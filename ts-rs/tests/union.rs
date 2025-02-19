#![allow(dead_code)]

use ts_rs::TS;

#[derive(TS)]
enum SimpleEnum {
    #[ts(rename = "asdf")]
    A,
    B,
    C,
}

#[test]
fn test_empty() {
    #[derive(TS)]
    enum Empty {}

    assert_eq!(Empty::decl().unwrap(), "type Empty = never;")
}

#[test]
fn test_simple_enum() {
    assert_eq!(
        SimpleEnum::decl().unwrap(),
        r#"enum SimpleEnum { A = "asdf", B = "B", C = "C" }"#
    )
}
