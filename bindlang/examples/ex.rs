use bindlang::{bindlang, bindlang_init, ExternalValue};
use koto_runtime::Value;
use std::fmt::{Display, Formatter, Result as FmtResult};

#[bindlang]
pub fn bound(_b: bool, _i: i64, _f: f32, _u: u8) {
    // nothing to see here
}

#[bindlang]
pub fn anotherone() {
    // still nothing
}

#[derive(Clone, Debug, Default, ExternalValue)]
pub struct MyType;

impl Display for MyType {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "{:?}", self)
    }
}

#[bindlang]
impl MyType {
    pub fn new() -> Self { MyType }
    
    pub fn answer(&self) -> usize {
        42
    }
}

fn main() {
    let mut prelude = koto_runtime::ValueMap::new();
    bindlang_init!(prelude);
    println!("It runs");
}
