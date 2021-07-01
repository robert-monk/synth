extern crate proc_macro;
extern crate quote;
extern crate syn;

use proc_macro::TokenStream;
use quote::quote;
use syn::*;

use std::sync::atomic::{AtomicUsize, Ordering};

// A counter that gets incremented each time we bind something
static LANG_BINDING_COUNT: AtomicUsize = AtomicUsize::new(0);

fn ident(s: &str) -> Ident {
    syn::parse_str(s).unwrap()
}

fn vtable_ident(ty: &str) -> Ident {
    ident(&format!("__BINDLANG_VTABLE_{}__", ty))
}

fn next_binding_fn(block: Block) -> Item {
    let binding_count = LANG_BINDING_COUNT.fetch_add(1, Ordering::SeqCst);
    let binding_fn_ident = ident(&format!("__bindlang_{}__", binding_count));
    let call_last_fn: Stmt = if let Some(last_count) = binding_count.checked_sub(1) {
        let last_fn_ident = ident(&format!("__bindlang_{}__", last_count));
        parse_quote! {
            #last_fn_ident(prelude);
        }
    } else {
        parse_quote! { {} }
    };
    let stmts = block.stmts;
    parse_quote! {
        #[inline(always)]
        fn #binding_fn_ident(prelude: &mut ::koto_runtime::ValueMap) {
            #call_last_fn
            #(#stmts);*
        }
    }
}

/// enrich the "prelude" with the bound items
#[proc_macro]
pub fn bindlang_init(code: TokenStream) -> TokenStream {
    let prelude = parse_macro_input!(code as Ident);
    if let Some(binding_count) = LANG_BINDING_COUNT.load(Ordering::SeqCst).checked_sub(1) {
        let binding_fn_ident = ident(&format!("__bindlang_{}__", binding_count));
        TokenStream::from(quote! {
            #binding_fn_ident(&mut #prelude)
        })
    } else {
        TokenStream::default()
    }
}

fn add_fn_binding(
    map: &Ident,
    Signature { ident: ref fn_ident, inputs, .. }: &Signature,
    self_ty: Option<&Type>) -> Stmt {
    let fn_name = fn_ident.to_string();
    let inputs_len = inputs.len();
    let names: Vec<_> = (0..inputs_len)
        .map(|i| ident(&format!("a{}", i)))
        .collect();
    let mut exprs = names
        .iter()
        .enumerate()
        .map(|(i, name)| parse_quote!(FromValue::from_value(&KeyPath::Index(#i, None), #name,)?));
    let call: Expr = if let Some(ty) = self_ty {
        let _: Option<Expr> = exprs.next();
        if inputs.first().map_or(false, |a| matches!(a, FnArg::Receiver(_))) {
            parse_quote!(#ty :: from_value(&KeyPath::Index(0, None), a0)?. #fn_ident(#(#exprs),*))
        } else {
            parse_quote!(#ty :: #fn_ident(#(#exprs),*))
        }
    } else {
        parse_quote!(#fn_ident(#(#exprs),*))
    };
    parse_quote! {
        #map.add_fn(
            #fn_name,
            |vm, args| {
                match vm.get_args(args) {
                    [#(#names),*] => Ok(#call.into_value()),
                    args => fn_type_error(#fn_name, stringify!(#inputs), args),
                }
            }
        );
    }
}

fn bind_fn(sig: &Signature, self_ty: Option<&Type>) -> Item {
    let add_function = add_fn_binding(&ident("prelude"), sig, self_ty);
    next_binding_fn(parse_quote! {{
        use lang_bindings::{FromValue, IntoValue, KeyPath, fn_type_error};
        #add_function
    }})
}

fn type_to_string(ty: &Type) -> String {
    if let Type::Path(ref qpath) = ty {
        qpath.path.segments.last().expect("an ident").ident.to_string()
    } else {
        panic!("Cannot bind an impl on an unnamed type");
    }
}

fn bind_item(input: Item) -> TokenStream {
    TokenStream::from(match &input {
        //TODO: bind trait impls
        Item::Impl(ItemImpl { trait_: None, self_ty, ref items, ..}) => {
            let ty_str = type_to_string(&self_ty);
            let vtable_map = ident("map"); //vtable_ident(&ty_str);
            let module = ident("module"); //TODO: How would that work with generics?
            let mut vtable_members = Vec::new();
            let mut module_members = Vec::new();
            for item in items {
                if let ImplItem::Method(ImplItemMethod { ref sig, ..}) = item {
                    if sig.receiver().is_some() { // method => vtable
                        vtable_members.push(add_fn_binding(&vtable_map, sig, Some(&self_ty)));
                    } else { // selfless => module
                        module_members.push(add_fn_binding(&module, sig, Some(&self_ty)));
                    }
                }
                //TODO: Do we want or need to bind other items? E.g. statics?
            }
            
            let setup_vtable: Vec<Item> = if vtable_members.is_empty() {
                vec![]
            } else {
                let vtable = vtable_ident(&ty_str);
                vec![parse_quote! {
                    lazy_static::lazy_static! {
                        static ref #vtable: ::koto_runtime::ValueMap = {
                            use lang_bindings::{FromValue, IntoValue, KeyPath, fn_type_error};
                            let mut #vtable_map = ::koto_runtime::ValueMap::new();
                            #(#vtable_members)*
                            #vtable_map
                        };
                    }
                }]
            };
            let setup_module: Vec<Item> = if module_members.is_empty() {
                vec![]
            } else {
                vec![next_binding_fn(parse_quote! {{
                    use lang_bindings::{FromValue, IntoValue, KeyPath, fn_type_error};
                    let mut #module = ::koto_runtime::ValueMap::new();
                    #(#module_members)*
                    prelude.add_map(#ty_str, #module);
                }})]
            };

            quote! { #input #(#setup_vtable)* #(#setup_module)* }
        }
        Item::Fn(ItemFn { ref sig, .. }) => {
            let binding = bind_fn(sig, None);
            quote! { #input #binding }
        },
        _ => quote! { #input const NO_BINDLANG_FOR_THIS: () = (); },
    })
}

#[proc_macro_attribute]
pub fn bindlang(_attrs: TokenStream, code: TokenStream) -> TokenStream {
    let input = parse_macro_input!(code as Item);
    bind_item(input)
}

#[proc_macro_derive(ExternalValue)]
pub fn external_value(code: TokenStream) -> TokenStream {
    let input = parse_macro_input!(code as Item);
    TokenStream::from(match input {
        Item::Struct(ItemStruct { ident: ref ty, generics: Generics { ref params, .. }, .. }) | 
        Item::Enum(ItemEnum { ident: ref ty, generics: Generics { ref params, .. }, .. }) => {
            let ty_vtable = vtable_ident(&ty.to_string());
            if params.is_empty() {
                quote! {
                    impl ::koto_runtime::ExternalValue for #ty {
                        fn value_type(&self) -> String {
                            stringify!(#ty).into()
                        }
                    }
                    
                    use koto_runtime::runtime_error;
                    
                    impl ::lang_bindings::FromValue for #ty {
                        fn from_value(
                            key_path: &::lang_bindings::KeyPath<'_>,
                            value: &::koto_runtime::Value,
                        ) -> Result<Self, ::koto_runtime::RuntimeError> {
                            if let ::koto_runtime::Value::ExternalValue(exval, ..) = value {
                                if let Some(v) = exval.as_ref().write().downcast_mut::<Self>() {
                                    Ok(v.clone())
                                } else {
                                    runtime_error!(
                                        "Invalid type for external value, found '{}'",
                                        exval.as_ref().read().value_type(),
                                    )
                                }
                            } else {
                                runtime_error!("Expected external value at {}", key_path)
                            }
                        }
                    }

                    impl ::lang_bindings::IntoValue for #ty {
                        fn into_value(self) -> Value {
                            ::koto_runtime::Value::make_external_value(self, #ty_vtable.clone())
                        }
                    }
                }
            } else {
                quote! {
                    impl<#params> ::koto_runtime::ExternalValue<#params> for #ty {
                        fn value_type(&self) -> String {
                            stringify!(#ty).into()
                        }
                    }

                    use koto_runtime::runtime_error;

                    impl<#params> ::lang_bindings::FromValue<#params> for #ty {
                        fn from_value(
                            key_path: &::lang_bindings::KeyPath<'_>,
                            value: &::koto_runtime::Value,
                        ) -> Result<Self, ::koto_runtime::RuntimeError> {
                            if let ::koto_runtime::Value::ExternalValue(exval, ..) = value {
                                if let Some(v) = exval.as_ref().write().downcast_mut::<Self>() {
                                    Ok(v.clone())
                                } else {
                                    runtime_error!(
                                        "Invalid type for external value, found '{}'",
                                        exval.as_ref().read().value_type(),
                                    )
                                }
                            } else {
                                runtime_error!("Expected external value at {}", key_path)
                            }
                        }
                    }
                    
                    impl ::lang_bindings::IntoValue for #ty {
                        fn into_value(self) -> Value {
                            ::koto_runtime::Value::make_external_value(self, #ty_vtable.clone())
                        }
                    }

                }
            }
        },
        _ => panic!("Cannot derive ExternalValue for anything but structs or enums"),
    })
}
