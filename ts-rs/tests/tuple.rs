#![allow(unused)]

use ts_rs::TS;

#[test]
fn test_tuple() {
    type Tuple = (String, i32, (i32, i32));
    assert_eq!("[string, number, [number, number]]", Tuple::name());
}

#[test]
fn test_decl() {
    type Tuple = (String, i32, (i32, i32));
    assert!(Tuple::decl().is_none());
}

#[test]
fn test_newtype() {
    #[derive(TS)]
    struct NewType(String);

    assert_eq!("type NewType = string;", NewType::decl().unwrap());
}

#[test]
fn test_tuple_newtype() {
    #[derive(TS)]
    struct TupleNewType(String, i32, (i32, i32));
    assert_eq!(
        "type TupleNewType = [string, number, [number, number]];",
        TupleNewType::decl().unwrap()
    )
}
