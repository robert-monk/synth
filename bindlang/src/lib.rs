extern crate proc_macro;
extern crate quote;
extern crate syn;

use proc_macro::TokenStream;
use quote::quote;
use syn::*;

use std::collections::HashMap;
use std::sync::{Arc, Mutex};

fn ident(s: &str) -> Ident {
    syn::parse_str(s).unwrap()
}

/// all we need to wrap a method
struct MethodSig {
    name: String,
    args: usize,
    inputs: String,
    self_ty: Option<String>,
}

impl MethodSig {
    // create a tuple expr
    fn to_tuple(&self) -> Expr {
        let fn_name = &self.name;
        let fn_ident = ident(&self.name);
        let inputs = &self.inputs;
        let names: Vec<_> = (0..self.args).map(|i| ident(&format!("a{}", i))).collect();
        let mut exprs = names.iter().enumerate().map(|(i, name)| {
            parse_quote! {
                ::lang_bindings::FromValue::from_value(&KeyPath::Index(#i, None), #name)?
            }
        });
        let (call, is_method): (Expr, bool) = if let Some(ty) = &self.self_ty {
            let ty_ident = ident(ty);
            if self.args > 0 {
                let a0: &Ident = &names[0];
                let _: Expr = exprs.next().unwrap();
                (
                    parse_quote! {
                        <#ty_ident as ::lang_bindings::FromValue>::from_value(&::lang_bindings::KeyPath::Index(0, None), #a0)?. #fn_ident(#(#exprs),*)
                    },
                    true,
                )
            } else {
                (parse_quote! { #ty_ident :: #fn_ident() }, false)
            }
        } else {
            (parse_quote! { #fn_ident(#(#exprs),*) }, false)
        };
        parse_quote! {
            (
                #fn_name,
                ::koto_runtime::ExternalFunction::new(|vm, args| {
                    match vm.get_args(args) {
                        [#(#names),*] => Ok(::lang_bindings::IntoValue::into_value(#call)),
                        args => ::lang_bindings::fn_type_error(#fn_name, #inputs, args),
                    }
                }, #is_method),
            )
        }
    }
}

// We need some structures to keep stuff around
#[derive(Default)]
struct Context {
    bare_fns: Vec<MethodSig>,
    modules: HashMap<String, Vec<MethodSig>>,
    vtables: HashMap<String, Vec<MethodSig>>,
    types: HashMap<String, String>,
}

lazy_static::lazy_static! {
    static ref CONTEXT: Arc<Mutex<Context>> = Arc::new(Mutex::new(Context::default()));
}

fn fn_binding(sig: &Signature, module: Option<String>, self_ty: Option<&Type>) {
    let fn_ident = &sig.ident;
    let inputs = &sig.inputs;
    let method_sig = MethodSig {
        name: fn_ident.to_string(),
        args: inputs.len(),
        inputs: quote!(#inputs).to_string(),
        self_ty: self_ty.map(|ty| quote!(#ty).to_string()),
    };
    let ctx = &mut *CONTEXT.lock().unwrap();
    if self_ty.is_some() {
        if sig.receiver().is_some() {
            &mut ctx.vtables
        } else {
            &mut ctx.modules
        }
        .entry(method_sig.self_ty.clone().unwrap())
        .or_insert_with(Vec::new)
    } else if let Some(m) = module {
        ctx.modules.entry(m).or_insert_with(Vec::new)
    } else {
        &mut ctx.bare_fns
    }
    .push(method_sig)
}

fn get_attr_parens(attr: &Attribute) -> String {
    attr.tokens
        .to_string()
        .trim_matches(&['(', ')'][..])
        .to_owned()
}

fn get_module(attrs: &[Attribute]) -> Option<String> {
    attrs.iter().find_map(|a| {
        if a.path.get_ident().map_or(false, |i| i == "bindlang") {
            Some(get_attr_parens(a))
        } else {
            None
        }
    })
}

static DERIVES: &[(&str, (&str, usize))] = &[
    ("Default", ("default", 0)),
    ("Clone", ("clone", 1)),
    ("Display", ("to_string", 1)),
    //TODO: map other traits
];

fn get_derives(ty: String, attrs: &[Attribute]) {
    for attr in attrs {
        if attr.path.get_ident().map_or(false, |i| i == "derive") {
            for derive in get_attr_parens(attr).split(',') {
                let derive = derive.trim_matches(char::is_whitespace);
                for (trt, (name, args)) in DERIVES {
                    if *trt == derive {
                        let args = *args;
                        let ctx = &mut CONTEXT.lock().unwrap();
                        if args == 0 {
                            ctx.modules
                                .entry(ty.clone())
                                .or_insert_with(Vec::new)
                                .push(MethodSig {
                                    name: name.to_string(),
                                    args,
                                    inputs: String::new(),
                                    self_ty: Some(ty.clone()),
                                })
                        } else {
                            ctx.vtables.entry(ty.clone()).or_insert_with(Vec::new).push(
                                MethodSig {
                                    name: name.to_string(),
                                    args,
                                    inputs: "self".to_string(), //TODO: add to DERIVES if we add traits w/ more args
                                    self_ty: Some(ty.clone()),
                                },
                            );
                        }
                        break;
                    };
                }
            }
        }
    }
}

fn create_map(items: &[MethodSig]) -> Expr {
    let members = items.iter().map(|sig| sig.to_tuple());
    parse_quote! {
        ::koto_runtime::ValueMap::with_data(IntoIterator::into_iter([
            #(#members),*
        ]).map(|(k,v): (&str, ::koto_runtime::ExternalFunction)| (
            ::koto_runtime::ValueKey::from(::koto_runtime::Value::Str(k.into())),
            ::koto_runtime::Value::ExternalFunction(v)
        )).collect::<::koto_runtime::ValueHashMap>())
    }
}

fn create_vtable(vtable: &Ident, items: &[MethodSig]) -> Item {
    let map = create_map(items);
    parse_quote! {
        lazy_static::lazy_static! {
            static ref #vtable: ::koto_runtime::ValueMap = #map;
        }
    }
}

fn create_module(prelude: &Ident, name: &str, items: &[MethodSig]) -> Stmt {
    let map = create_map(items);
    parse_quote! {
        #prelude.add_map(#name, #map);
    }
}

//TODO: We may want to allow generics at some point, but we'd need to introduce a new type to parse them
fn create_binding(ty_name: &str, _generics: &str, ty_vtable: &Ident) -> impl Iterator<Item = Item> {
    let ty: Ident = ident(ty_name);
    IntoIterator::into_iter(//if generics.is_empty() {
        [
            parse_quote! {
                impl ::koto_runtime::ExternalValue for #ty {
                    fn value_type(&self) -> String {
                        #ty_name.into()
                    }
                }
            },
            parse_quote! {
                impl ::lang_bindings::FromValue for #ty {
                    fn from_value(
                        key_path: &::lang_bindings::KeyPath<'_>,
                        value: &::koto_runtime::Value,
                    ) -> std::result::Result<Self, ::koto_runtime::RuntimeError> {
                        if let ::koto_runtime::Value::ExternalValue(exval, ..) = value {
                            if let Some(v) = exval.as_ref().write().downcast_mut::<Self>() {
                                Ok(v.clone())
                            } else {
                                ::koto_runtime::runtime_error!(
                                    "Invalid type for external value, found '{}'",
                                    exval.as_ref().read().value_type(),
                                )
                            }
                        } else {
                            ::koto_runtime::runtime_error!("Expected external value at {}", key_path)
                        }
                    }
                }
            },
            parse_quote! {
                impl ::lang_bindings::IntoValue for #ty {
                    fn into_value(self) -> ::koto_runtime::Value {
                        ::koto_runtime::Value::make_external_value(self, #ty_vtable.clone())
                    }
                }
            },
        ]
/*     } else {
        let params: Punctuated<GenericParam, syn::Token![,]>::parse_terminated(generics).unwrap();
        [
            parse_quote! {        
                impl<#params> ::koto_runtime::ExternalValue<#params> for #ty {
                    fn value_type(&self) -> String {
                        stringify!(#ty).into()
                    }
                }
            },
            parse_quote! {        
                impl<#params> ::lang_bindings::FromValue<#params> for #ty {
                    fn from_value(
                        key_path: &::lang_bindings::KeyPath<'_>,
                        value: &::koto_runtime::Value,
                    ) -> Result<Self, ::koto_runtime::RuntimeError> {
                        if let ::koto_runtime::Value::ExternalValue(exval, ..) = value {
                            if let Some(v) = exval.as_ref().write().downcast_mut::<Self>() {
                                Ok(v.clone())
                            } else {
                                koto_runtime::runtime_error!(
                                    "Invalid type for external value, found '{}'",
                                    exval.as_ref().read().value_type(),
                                )
                            }
                        } else {
                            koto_runtime::runtime_error!("Expected external value at {}", key_path)
                        }
                    }
                }
            },
            parse_quote! {
                impl ::lang_bindings::IntoValue for #ty {
                    fn into_value(self) -> Value {
                        ::koto_runtime::Value::make_external_value(self, #ty_vtable.clone())
                    }
                }
            },
        ]
    }*/)
}

#[proc_macro]
pub fn bindlang_main(mut code: TokenStream) -> TokenStream {
    let Context {
        ref bare_fns,
        ref modules,
        ref vtables,
        ref types,
    } = *CONTEXT.lock().unwrap();
    let prelude = ident("prelude");
    let vtable_idents = vtables
        .keys()
        .map(|ty| (ty.to_string(), vtable_ident(ty)))
        .collect::<HashMap<String, Ident>>();
    let vtable_items = vtables
        .iter()
        .map(|(name, items)| create_vtable(&vtable_idents[name], items,));
    let type_bindings = types
        .iter()
        .flat_map(|(name, generics)| create_binding(name, generics, &vtable_idents[name]));
    let prelude_map = create_map(bare_fns);
    let module_stmts = modules
        .iter()
        .map(|(name, items)| create_module(&prelude, name, items));
    //TODO we may insert the "synth" string below in the _code tokens
    code.extend(TokenStream::from(quote! {
        #(#vtable_items)*
        #(#type_bindings)*
        fn bindlang_init(#prelude: &mut ::koto_runtime::ValueMap) {
            #prelude.add_map("synth", #prelude_map);
            #(#module_stmts)*
        }
    }));
    code
}

fn vtable_ident(ty: &str) -> Ident {
    ident(&format!("__BINDLANG_VTABLE_{}__", ty))
}

#[proc_macro_attribute]
pub fn bindlang(_attrs: TokenStream, code: TokenStream) -> TokenStream {
    let code_cloned = code.clone();
    let input = parse_macro_input!(code_cloned as Item);
    match &input {
        //TODO: bind trait impls
        Item::Impl(ItemImpl {
            attrs,
            trait_: None,
            self_ty,
            items,
            ..
        }) => {
            for item in items {
                if let ImplItem::Method(ImplItemMethod { ref sig, .. }) = item {
                    fn_binding(sig, get_module(attrs), Some(self_ty));
                }
                //TODO: Do we want or need to bind other items? E.g. statics?
            }
        }
        Item::Fn(ItemFn {
            ref attrs, ref sig, ..
        }) => {
            fn_binding(sig, get_module(attrs), None);
        }
        Item::Struct(ItemStruct {
            attrs,
            ident: ty,
            generics: Generics { params, .. },
            ..
        })
        | Item::Enum(ItemEnum {
            attrs,
            ident: ty,
            generics: Generics { params, .. },
            ..
        }) => {
            // record the type, derives and generics (as String)
            let ty_string = ty.to_string();
            get_derives(ty_string.clone(), attrs);
            CONTEXT.lock().unwrap().types.insert(ty_string, quote!(#params).to_string());
        }
        _ => (), //TODO: Report a usable error
    }
    // we emit the code as is
    code
}
