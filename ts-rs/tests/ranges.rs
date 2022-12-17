use std::ops::{Range, RangeInclusive};

use ts_rs::{Dependencies, Dependency, TS};

#[derive(TS)]
struct Inner(i32);

#[derive(TS)]
#[allow(dead_code)]
struct RangeTest {
    a: Range<u32>,
    b: Range<&'static str>,
    c: Range<Range<i32>>,
    d: RangeInclusive<u32>,
    e: Range<Inner>,
}

#[test]
fn range() {
    assert_eq!(
        RangeTest::decl().unwrap(),
        "interface RangeTest { a: { start: number, end: number, }, b: { start: string, end: string, }, c: { start: { start: number, end: number, }, end: { start: number, end: number, }, }, d: { start: number, end: number, }, e: { start: Inner, end: Inner, }, }"
    );
    let mut deps = Dependencies::new();
    deps.add::<Range<()>>();
    deps.add::<RangeInclusive<()>>();
    deps.add::<Inner>();
    assert_eq!(RangeTest::dependencies(), deps);
}
