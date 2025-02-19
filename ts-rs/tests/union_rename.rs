#![allow(dead_code)]

use ts_rs::TS;

#[derive(TS)]
#[ts(rename_all = "lowercase")]
#[ts(rename = "SimpleEnum")]
enum RenamedEnum {
    #[ts(rename = "ASDF")]
    A,
    B,
    C,
}

#[test]
fn test_simple_enum() {
    assert_eq!(
        RenamedEnum::decl().unwrap(),
        r#"enum SimpleEnum { A = "ASDF", B = "b", C = "c" }"#
    )
}
