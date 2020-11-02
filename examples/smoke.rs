use maplit::btreeset;
use negatable_set::NegatableSet;
use std::collections::BTreeSet;

type Test = NegatableSet<BTreeSet<u64>>;

fn main() {
    let a = Test::from(btreeset! { 3,4,5 });
    let b = Test::from(btreeset! { 1,2,3 });
    let c = !&b;
    let d = &a | &c;
    println!("{:?} {:?} {:?} {:?}", a, b, c, d);
}
