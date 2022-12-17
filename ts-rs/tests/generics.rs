#![allow(dead_code)]

use std::{
    collections::{BTreeMap, HashSet},
    fmt::Debug,
    rc::Rc,
};

use ts_rs::TS;

#[derive(TS)]
struct Generic<T>
where
    T: TS,
{
    value: T,
    values: Vec<T>,
}

#[derive(TS)]
struct GenericAutoBound<T> {
    value: T,
    values: Vec<T>,
}

#[derive(TS)]
struct GenericAutoBound2<T>
where
    T: PartialEq,
{
    value: T,
    values: Vec<T>,
}

#[derive(TS)]
struct Container {
    foo: Generic<u32>,
    bar: Box<HashSet<Generic<u32>>>,
    baz: Box<BTreeMap<String, Rc<Generic<String>>>>,
}

macro_rules! declare {
    ($(#[$meta:meta])* $name:ident { $($fident:ident: $t:ty),+ $(,)? }) => {
        $(#[$meta])*
        struct $name {
            $(pub $fident: $t),+
        }
    }
}

declare! {
    #[derive(TS)]
    TypeGroup {
        foo: Vec<Container>,
    }
}

#[test]
fn test() {
    assert_eq!(
        TypeGroup::decl().unwrap(),
        "interface TypeGroup { foo: Array<Container>, }",
    );

    assert_eq!(
        Generic::<()>::decl().unwrap(),
        "interface Generic<T> { value: T, values: Array<T>, }"
    );

    assert_eq!(
        GenericAutoBound::<()>::decl().unwrap(),
        "interface GenericAutoBound<T> { value: T, values: Array<T>, }"
    );

    assert_eq!(
        GenericAutoBound2::<()>::decl().unwrap(),
        "interface GenericAutoBound2<T> { value: T, values: Array<T>, }"
    );

    assert_eq!(
        Container::decl().unwrap(),
        "interface Container { foo: Generic<number>, bar: Array<Generic<number>>, baz: Record<string, Generic<string>>, }"
    );
}

#[test]
fn generic_enum() {
    #[derive(TS)]
    enum Generic<A, B, C> {
        A(A),
        B(B, B, B),
        C(Vec<C>),
        D(Vec<Vec<Vec<A>>>),
        E { a: A, b: B, c: C },
        X(Vec<i32>),
        Y(i32),
        Z(Vec<Vec<i32>>),
    }

    assert_eq!(
        Generic::<(), (), ()>::decl().unwrap(),
        r#"type Generic<A, B, C> = { A: A } | { B: [B, B, B] } | { C: Array<C> } | { D: Array<Array<Array<A>>> } | { E: { a: A, b: B, c: C, } } | { X: Array<number> } | { Y: number } | { Z: Array<Array<number>> };"#
    )
}

#[test]
fn generic_newtype() {
    #[derive(TS)]
    struct NewType<T>(Vec<Vec<T>>);

    assert_eq!(
        NewType::<()>::decl().unwrap(),
        r#"type NewType<T> = Array<Array<T>>;"#
    );
}

#[test]
fn generic_tuple() {
    #[derive(TS)]
    struct Tuple<T>(T, Vec<T>, Vec<Vec<T>>);

    assert_eq!(
        Tuple::<()>::decl().unwrap(),
        r#"type Tuple<T> = [T, Array<T>, Array<Array<T>>];"#
    );
}

#[test]
fn generic_struct() {
    #[derive(TS)]
    struct Struct<T> {
        a: T,
        b: (T, T),
        c: (T, (T, T)),
        d: [T; 3],
        e: [(T, T); 3],
        f: Vec<T>,
        g: Vec<Vec<T>>,
        h: Vec<[(T, T); 3]>,
    }

    assert_eq!(
        Struct::<()>::decl().unwrap(),
        "interface Struct<T> { a: T, b: [T, T], c: [T, [T, T]], d: Array<T>, e: Array<[T, T]>, f: Array<T>, g: Array<Array<T>>, h: Array<Array<[T, T]>>, }"
    )
}

#[test]
#[ignore]
// https://github.com/Aleph-Alpha/ts-rs/issues/56 TODO
fn inline() {
    #[derive(TS)]
    struct Generic<T> {
        t: T,
    }

    #[derive(TS)]
    struct Container {
        g: Generic<String>,
        #[ts(inline)]
        gi: Generic<String>,
        #[ts(flatten)]
        t: Generic<String>,
    }

    assert_eq!(
        Generic::<()>::decl().unwrap(),
        "interface Generic<T> { t: T, }"
    );
    assert_eq!(
        Container::decl().unwrap(),
        "interface Container { g: Generic<string>, gi: { t: string }, t: string, }"
    );
}

#[test]
fn default() {
    #[derive(TS)]
    struct A<T = String> {
        t: T,
    }
    assert_eq!(
        A::<()>::decl().unwrap(),
        "interface A<T = string> { t: T, }"
    );

    #[derive(TS)]
    struct B<U = Option<A<i32>>> {
        u: U,
    }
    assert_eq!(
        B::<()>::decl().unwrap(),
        "interface B<U = Option<A<number>>> { u: U, }"
    );
    assert!(B::<()>::dependencies()
        .values()
        .any(|dep| dep.ts_name == "A"));

    #[derive(TS)]
    struct Y {
        a1: A,
        a2: A<i32>,
        // https://github.com/Aleph-Alpha/ts-rs/issues/56
        // TODO: fixme
        // #[ts(inline)]
        // xi: X,
        // #[ts(inline)]
        // xi2: X<i32>
    }
    assert_eq!(Y::decl().unwrap(), "interface Y { a1: A, a2: A<number>, }")
}

#[test]
fn name_with_generics() {
    #[derive(TS)]
    struct A<T: ToString = i32> {
        t: T,
    }
    assert_eq!(A::<i32>::name_with_generics(), "A<number>");

    #[derive(TS)]
    struct B<T, U>(T, U);
    assert_eq!(B::<A, String>::name_with_generics(), "B<A<number>, string>");
}

#[test]
fn test_dependencies_generics() {
    #[derive(TS)]
    struct D {
        d: B,
    }

    #[derive(TS)]
    struct B {
        b: C,
    }

    #[derive(TS)]
    struct C {
        c: u32,
    }

    #[derive(TS)]
    struct A<T = C> {
        t: T,
    }
    // A depends on C because it's generic is C by default
    assert_eq!(A::<i32>::dependencies().len(), 1);
    // A<B> depends on C (see above) and on B because it's
    // generic is B. B also depends on C, but we don't have
    // duplicate dependencies.
    assert_eq!(A::<B>::dependencies().len(), 2);
    // Depends on C, B and D
    assert_eq!(A::<D>::dependencies().len(), 3);
}

#[test]
fn trait_bounds() {
    #[derive(TS)]
    struct A<T: ToString = i32> {
        t: T,
    }
    assert_eq!(
        A::<i32>::decl().unwrap(),
        "interface A<T = number> { t: T, }"
    );

    #[derive(TS)]
    struct B<T: ToString + Debug + Clone + 'static>(T);
    assert_eq!(B::<&'static str>::decl().unwrap(), "type B<T> = T;");

    #[derive(TS)]
    enum C<T: Copy + Clone + PartialEq, K: Copy + PartialOrd = i32> {
        A { t: T },
        B(T),
        C,
        D(T, K),
    }
    assert_eq!(
        C::<&'static str, i32>::decl().unwrap(),
        "type C<T, K = number> = { A: { t: T, } } | { B: T } | \"C\" | { D: [T, K] };"
    );

    #[derive(TS)]
    struct D<T: ToString, const N: usize> {
        t: [T; N],
    }

    assert_eq!(
        D::<&str, 41>::decl().unwrap(),
        "interface D<T> { t: Array<T>, }"
    )
}
