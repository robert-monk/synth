#![feature(
    format_args_capture,
    async_closure,
    map_first_last,
    box_patterns,
    error_iter
)]
#![allow(type_alias_bounds)]
#![deny(warnings)]

#[macro_use]
extern crate log;

#[macro_use]
extern crate anyhow;

#[macro_use]
extern crate lazy_static;

#[allow(unused_imports)]
#[macro_use]
extern crate serde_json;

extern crate humantime_serde;

#[macro_use]
pub mod error;
pub use error::Error;

#[macro_use]
pub mod schema;
pub use schema::{Content, Name, Namespace};

pub mod graph;
pub use graph::Graph;

pub mod compile;
pub use compile::{Compile, Compiler};

bindlang::bindlang_main! {
    use crate::graph::{string::Locale, Value};
}
/* 
lazy_static::lazy_static! {
    static ref __BINDLANG_VTABLE_Value__: koto_runtime::ValueMap =
            ::koto_runtime::ValueMap::with_data(
                IntoIterator::into_iter([(
                    "clone",
                    ::koto_runtime::ExternalFunction::new(
                        |vm, args| match vm.get_args(args) {
                            [a0] => Ok(::lang_bindings::IntoValue::into_value(
                                <Value as ::lang_bindings::FromValue>::from_value(
                                    &::lang_bindings::KeyPath::Index(0, None),
                                    a0,
                                )?
                                .clone(),
                            )),
                            args => ::lang_bindings::fn_type_error("clone", "self", args),
                        },
                        true,
                    ),
                )])
                .map(|(k, v)| {
                    (
                        ::koto_runtime::Value::Str(k.into()).into(),
                        ::koto_runtime::Value::ExternalFunction(v),
                    )
                })
                .collect::<::koto_runtime::ValueHashMap>(),
            );
}
impl ::koto_runtime::ExternalValue for Value {
    fn value_type(&self) -> String {
        "Value".into()
    }
}
impl ::lang_bindings::FromValue for Value {
    fn from_value(
        key_path: &::lang_bindings::KeyPath<'_>,
        value: &::koto_runtime::Value,
    ) -> std::result::Result<Self, ::koto_runtime::RuntimeError> {
        if let ::koto_runtime::Value::ExternalValue(exval, ..) = value {
            if let Some(v) = exval.as_ref().write().downcast_mut::<Self>() {
                Ok(v.clone())
            } else {
                Err({
                    ::koto_runtime::RuntimeError::from_string({
                        format!("Invalid type for external value, found '{}'",
                            &exval.as_ref().read().value_type())
                    })
                })
            }
        } else {
            Err(
                ::koto_runtime::RuntimeError::from_string(
                    format!("Expected external value at {}",key_path)
                )
            )
        }
    }
}
impl ::lang_bindings::IntoValue for Value {
    fn into_value(self) -> ::koto_runtime::Value {
        ::koto_runtime::Value::make_external_value(self, __BINDLANG_VTABLE_Value__.clone())
    }
}
fn bindlang_init(prelude: &mut ::koto_runtime::ValueMap) {
    prelude.add_map(
        "synth",
        ::koto_runtime::ValueMap::with_data(
            IntoIterator::into_iter([])
                .map(|(k, v): (&str, ::koto_runtime::ExternalFunction)| {
                    (
                        ::koto_runtime::ValueKey::from(::koto_runtime::Value::Str(k.into())),
                        ::koto_runtime::Value::ExternalFunction(v),
                    )
                })
                .collect::<::koto_runtime::ValueHashMap>(),
        ),
    );
}
*/