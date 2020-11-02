use core::{
    fmt,
    fmt::{Debug, Write},
    hash::Hash,
    mem,
    ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Not, Sub, SubAssign},
};
#[cfg(feature = "serde")]
use serde::{
    de::{Deserialize, Deserializer},
    ser::{Serialize, Serializer},
};
#[cfg(feature = "vc")]
use smallvec::Array;
use std::{
    collections::{BTreeSet, HashSet},
    hash::BuildHasher,
};
#[cfg(feature = "vc")]
use vec_collections::VecSet;
#[cfg(test)]
#[macro_use]
mod test_macros;

#[derive(Default)]
pub struct NegatableSet<I> {
    elements: I,
    negated: bool,
}

#[cfg(feature = "serde")]
impl<A> Serialize for NegatableSet<A>
where
    A: MutableSet + Serialize,
{
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        (&self.elements, &self.negated).serialize(serializer)
    }
}

#[cfg(feature = "serde")]
impl<'de, A> Deserialize<'de> for NegatableSet<A>
where
    A: MutableSet + Deserialize<'de>,
{
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let (elements, negated) = <(A, bool)>::deserialize(deserializer)?;
        Ok(Self::new(elements, negated))
    }
}

impl<I: Clone> Clone for NegatableSet<I> {
    fn clone(&self) -> Self {
        Self {
            elements: self.elements.clone(),
            negated: self.negated,
        }
    }
}

impl<I: Hash> Hash for NegatableSet<I> {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        self.elements.hash(state);
        self.negated.hash(state);
    }
}

impl<I: PartialEq> PartialEq for NegatableSet<I> {
    fn eq(&self, other: &Self) -> bool {
        self.elements == other.elements && self.negated == other.negated
    }
}

impl<I: Eq> Eq for NegatableSet<I> {}

impl<I: MutableSet> Debug for NegatableSet<I> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.negated {
            f.write_char('!')?;
        }
        f.debug_set().entries(self.elements.iter()).finish()
    }
}

impl<I: MutableSet + Default> NegatableSet<I> {
    pub fn constant(value: bool) -> Self {
        Self::new(I::default(), value)
    }

    pub fn empty() -> Self {
        false.into()
    }

    pub fn all() -> Self {
        true.into()
    }
}

impl<I: MutableSet> NegatableSet<I> {
    fn new(elements: I, negated: bool) -> Self {
        Self { elements, negated }
    }

    pub fn is_empty(&self) -> bool {
        !self.negated && self.elements.is_empty()
    }

    pub fn is_all(&self) -> bool {
        self.negated && self.elements.is_empty()
    }

    pub fn into_elements(self) -> I {
        self.elements
    }

    pub fn elements(&self) -> &I {
        &self.elements
    }

    pub fn elements_mut(&mut self) -> &mut I {
        &mut self.elements
    }
}

pub trait MutableSet {
    type Item: Debug + 'static;

    fn iter<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Self::Item> + 'a>;

    fn is_empty(&self) -> bool;

    fn contains(&self, value: &Self::Item) -> bool;

    fn insert(&mut self, value: Self::Item);

    fn remove(&mut self, value: &Self::Item);

    fn is_subset(&self, rhs: &Self) -> bool;

    fn is_superset(&self, rhs: &Self) -> bool;

    fn is_disjoint(&self, rhs: &Self) -> bool;
}

#[cfg(feature = "vc")]
impl<T: Ord + Debug + 'static, A: Array<Item = T>> MutableSet for VecSet<A> {
    type Item = T;

    fn iter<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Self::Item> + 'a> {
        Box::new(VecSet::iter(self))
    }

    fn is_empty(&self) -> bool {
        VecSet::is_empty(self)
    }

    fn contains(&self, value: &Self::Item) -> bool {
        VecSet::contains(self, value)
    }

    fn insert(&mut self, value: Self::Item) {
        VecSet::insert(self, value)
    }

    fn remove(&mut self, value: &Self::Item) {
        VecSet::remove(self, value)
    }

    fn is_subset(&self, rhs: &Self) -> bool {
        VecSet::is_subset(self, rhs)
    }

    fn is_superset(&self, rhs: &Self) -> bool {
        VecSet::is_superset(self, rhs)
    }

    fn is_disjoint(&self, rhs: &Self) -> bool {
        VecSet::is_disjoint(self, rhs)
    }
}

impl<T: Ord + Debug + 'static> MutableSet for BTreeSet<T> {
    type Item = T;

    fn iter<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Self::Item> + 'a> {
        Box::new(BTreeSet::iter(self))
    }

    fn is_empty(&self) -> bool {
        BTreeSet::is_empty(self)
    }

    fn contains(&self, value: &Self::Item) -> bool {
        BTreeSet::contains(self, value)
    }

    fn insert(&mut self, value: Self::Item) {
        BTreeSet::insert(self, value);
    }

    fn remove(&mut self, value: &Self::Item) {
        BTreeSet::remove(self, value);
    }

    fn is_subset(&self, rhs: &Self) -> bool {
        BTreeSet::is_subset(self, rhs)
    }

    fn is_superset(&self, rhs: &Self) -> bool {
        BTreeSet::is_superset(self, rhs)
    }

    fn is_disjoint(&self, rhs: &Self) -> bool {
        BTreeSet::is_disjoint(self, rhs)
    }
}

impl<T: Hash + Eq + Debug + 'static, S: BuildHasher + Default> MutableSet for HashSet<T, S> {
    type Item = T;

    fn iter<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Self::Item> + 'a> {
        Box::new(HashSet::iter(self))
    }

    fn is_empty(&self) -> bool {
        HashSet::is_empty(self)
    }

    fn contains(&self, value: &Self::Item) -> bool {
        HashSet::contains(self, value)
    }

    fn insert(&mut self, value: Self::Item) {
        HashSet::insert(self, value);
    }

    fn remove(&mut self, value: &Self::Item) {
        HashSet::remove(self, value);
    }

    fn is_subset(&self, rhs: &Self) -> bool {
        HashSet::is_subset(self, rhs)
    }

    fn is_superset(&self, rhs: &Self) -> bool {
        HashSet::is_superset(self, rhs)
    }

    fn is_disjoint(&self, rhs: &Self) -> bool {
        HashSet::is_disjoint(self, rhs)
    }
}

impl<I: MutableSet + Default> From<bool> for NegatableSet<I> {
    fn from(value: bool) -> Self {
        Self::constant(value)
    }
}

impl<I: MutableSet> From<I> for NegatableSet<I> {
    fn from(value: I) -> Self {
        Self::new(value, false)
    }
}

impl<I: MutableSet> NegatableSet<I> {
    pub fn contains(&self, value: &I::Item) -> bool {
        self.negated ^ self.elements.contains(value)
    }

    pub fn insert(&mut self, that: I::Item) {
        if !self.negated {
            self.elements.insert(that)
        } else {
            self.elements.remove(&that)
        }
    }

    pub fn is_superset(&self, that: &Self) -> bool {
        !self.is_subset(that)
    }

    pub fn is_subset(&self, that: &Self) -> bool {
        match (self.negated, that.negated) {
            (false, false) => self.elements.is_subset(&that.elements),
            (false, true) => self.elements.is_disjoint(&that.elements),
            (true, false) => false,
            (true, true) => self.elements.is_superset(&that.elements),
        }
    }

    pub fn is_disjoint(&self, that: &Self) -> bool {
        match (self.negated, that.negated) {
            (false, false) => self.elements.is_disjoint(&that.elements),
            (false, true) => self.elements.is_subset(&that.elements),
            (true, false) => self.elements.is_superset(&that.elements),
            (true, true) => false,
        }
    }
}

impl<I: MutableSet> NegatableSet<I>
where
    I::Item: Ord + Clone,
{
    pub fn remove(&mut self, that: &I::Item) {
        if self.negated {
            self.elements.insert(that.clone())
        } else {
            self.elements.remove(that)
        }
    }
}

impl<'a, I: MutableSet + 'a> BitAnd for &'a NegatableSet<I>
where
    &'a I: BitAnd<Output = I>,
    &'a I: BitOr<Output = I>,
    &'a I: Sub<Output = I>,
{
    type Output = NegatableSet<I>;
    fn bitand(self, that: Self) -> Self::Output {
        match (self.negated, that.negated) {
            // intersection of elements
            (false, false) => Self::Output::new(&self.elements & &that.elements, false),
            // remove elements from self
            (false, true) => Self::Output::new(&self.elements - &that.elements, false),
            // remove elements from that
            (true, false) => Self::Output::new(&that.elements - &self.elements, false),
            // union of elements
            (true, true) => Self::Output::new(&that.elements | &self.elements, true),
        }
    }
}

impl<I: MutableSet> BitAndAssign for NegatableSet<I>
where
    I: BitAndAssign,
    I: BitOrAssign,
    I: SubAssign,
{
    fn bitand_assign(&mut self, that: Self) {
        match (self.negated, that.negated) {
            // intersection of elements
            (false, false) => {
                self.elements &= that.elements;
                self.negated = false;
            }
            // remove elements from self
            (false, true) => {
                self.elements -= that.elements;
                self.negated = false;
            }
            // remove elements from that
            (true, false) => {
                let mut that = that;
                mem::swap(&mut that.elements, &mut self.elements);
                self.elements -= that.elements;
                self.negated = false;
            }
            // union of elements
            (true, true) => {
                self.elements |= that.elements;
                self.negated = true;
            }
        };
    }
}

impl<'a, I: MutableSet> BitOr for &'a NegatableSet<I>
where
    &'a I: BitAnd<Output = I>,
    &'a I: BitOr<Output = I>,
    &'a I: Sub<Output = I>,
{
    type Output = NegatableSet<I>;
    fn bitor(self, that: Self) -> Self::Output {
        match (self.negated, that.negated) {
            // union of elements
            (false, false) => Self::Output::new(&self.elements | &that.elements, false),
            // remove holes from that
            (false, true) => Self::Output::new(&that.elements - &self.elements, true),
            // remove holes from self
            (true, false) => Self::Output::new(&self.elements - &that.elements, true),
            // intersection of holes
            (true, true) => Self::Output::new(&that.elements & &self.elements, true),
        }
    }
}

impl<I: MutableSet> BitOrAssign for NegatableSet<I>
where
    I: BitAndAssign,
    I: BitOrAssign,
    I: SubAssign,
{
    fn bitor_assign(&mut self, that: Self) {
        match (self.negated, that.negated) {
            // union of elements
            (false, false) => {
                self.elements |= that.elements;
                self.negated = false;
            }
            // remove holes from that
            (false, true) => {
                let mut that = that;
                mem::swap(&mut that.elements, &mut self.elements);
                self.elements -= that.elements;
                self.negated = true;
            }
            // remove holes from self
            (true, false) => {
                self.elements -= that.elements;
                self.negated = true;
            }
            // intersection of holes
            (true, true) => {
                self.elements &= that.elements;
                self.negated = true;
            }
        };
    }
}

impl<'a, I: MutableSet> BitXor for &'a NegatableSet<I>
where
    &'a I: BitXor<Output = I>,
{
    type Output = NegatableSet<I>;
    fn bitxor(self, that: Self) -> Self::Output {
        Self::Output::new(&self.elements ^ &that.elements, self.negated ^ that.negated)
    }
}

impl<I: MutableSet> BitXorAssign for NegatableSet<I>
where
    I: BitXorAssign,
{
    fn bitxor_assign(&mut self, that: Self) {
        self.elements ^= that.elements;
        self.negated ^= that.negated;
    }
}

#[allow(clippy::suspicious_arithmetic_impl)]
impl<'a, I: MutableSet> Sub for &'a NegatableSet<I>
where
    &'a I: BitAnd<Output = I>,
    &'a I: BitOr<Output = I>,
    &'a I: Sub<Output = I>,
{
    type Output = NegatableSet<I>;
    fn sub(self, that: Self) -> Self::Output {
        match (self.negated, that.negated) {
            // intersection of elements
            (false, false) => Self::Output::new(&self.elements - &that.elements, false),
            // keep only holes of that
            (false, true) => Self::Output::new(&self.elements & &that.elements, false),
            // add holes from that
            (true, false) => Self::Output::new(&self.elements | &that.elements, true),
            // union of elements
            (true, true) => Self::Output::new(&that.elements - &self.elements, false),
        }
    }
}

impl<I: MutableSet> SubAssign for NegatableSet<I>
where
    I: BitAndAssign,
    I: BitOrAssign,
    I: SubAssign,
{
    fn sub_assign(&mut self, that: Self) {
        match (self.negated, that.negated) {
            // intersection of elements
            (false, false) => {
                self.elements -= that.elements;
                self.negated = false;
            }
            // keep only holes of that
            (false, true) => {
                self.elements &= that.elements;
                self.negated = false;
            }
            // add holes from that
            (true, false) => {
                self.elements |= that.elements;
                self.negated = true;
            }
            // union of elements
            (true, true) => {
                let mut that = that;
                mem::swap(&mut that.elements, &mut self.elements);
                self.elements -= that.elements;
                self.negated = false;
            }
        }
    }
}

impl<'a, I: MutableSet + Clone> Not for &'a NegatableSet<I> {
    type Output = NegatableSet<I>;
    fn not(self) -> Self::Output {
        Self::Output::new(self.elements.clone(), !self.negated)
    }
}

impl<I: MutableSet + Clone> Not for NegatableSet<I> {
    type Output = NegatableSet<I>;
    fn not(self) -> Self::Output {
        Self::Output::new(self.elements, !self.negated)
    }
}

#[cfg(test)]
mod tests {
    #[allow(dead_code)]
    use super::*;
    use quickcheck::*;
    use quickcheck_macros::quickcheck;
    use vec_collections::VecSet;

    #[cfg(feature = "vc")]
    type Test = NegatableSet<VecSet<[i64; 4]>>;

    #[cfg(feature = "vc")]
    impl<T: Arbitrary + Ord + Copy + Default + Debug> Arbitrary for NegatableSet<VecSet<[T; 4]>> {
        fn arbitrary<G: Gen>(g: &mut G) -> Self {
            let mut elements: Vec<T> = Arbitrary::arbitrary(g);
            elements.truncate(2);
            let negated: bool = Arbitrary::arbitrary(g);
            NegatableSet::new(elements.into(), negated)
        }
    }

    impl<T: Arbitrary + Ord + Copy + Default + Debug> Arbitrary for NegatableSet<BTreeSet<T>> {
        fn arbitrary<G: Gen>(g: &mut G) -> Self {
            let mut elements: Vec<T> = Arbitrary::arbitrary(g);
            elements.truncate(2);
            let negated: bool = Arbitrary::arbitrary(g);
            NegatableSet::new(elements.into_iter().collect(), negated)
        }
    }

    #[allow(dead_code)]
    /// just a helper to get good output when a check fails
    fn print_on_failure_unary<E: Debug, R: Eq + Debug>(x: E, expected: R, actual: R) -> bool {
        let res = expected == actual;
        if !res {
            println!("x:{:?} expected:{:?} actual:{:?}", x, expected, actual);
        }
        res
    }

    #[cfg(feature = "vc")]
    fn binary_op(a: &Test, b: &Test, r: &Test, op: impl Fn(bool, bool) -> bool) -> bool {
        let mut samples: BTreeSet<i64> = BTreeSet::new();
        samples.extend(a.elements.iter().cloned());
        samples.extend(b.elements.iter().cloned());
        samples.insert(core::i64::MIN);
        samples.iter().all(|e| {
            let expected = op(a.contains(e), b.contains(e));
            let actual = r.contains(e);
            if expected != actual {
                println!(
                    "{:?}!={:?} at {:?} {:?} {:?} {:?}",
                    expected, actual, e, a, b, r
                );
            }
            expected == actual
        })
    }

    #[cfg(feature = "vc")]
    fn binary_property(a: &Test, b: &Test, r: bool, op: impl Fn(bool, bool) -> bool) -> bool {
        let mut samples: BTreeSet<i64> = BTreeSet::new();
        samples.extend(a.elements.iter().cloned());
        samples.extend(b.elements.iter().cloned());
        samples.insert(core::i64::MIN);
        if r {
            samples.iter().all(|e| {
                let expected = op(a.contains(e), b.contains(e));
                if !expected {
                    println!(
                        "{:?} is false at {:?}\na {:?}\nb {:?}\nr {:?}",
                        expected, e, a, b, r
                    );
                }
                expected
            })
        } else {
            samples.iter().any(|e| !op(a.contains(e), b.contains(e)))
        }
    }

    quickcheck::quickcheck! {

        #[cfg(feature = "serde")]
        fn serde_roundtrip(reference: NegatableSet<BTreeSet<u64>>) -> bool {
            let bytes = serde_json::to_vec(&reference).unwrap();
            let deser = serde_json::from_slice(&bytes).unwrap();
            reference == deser
        }

        #[cfg(feature = "vc")]
        fn is_disjoint_sample(a: Test, b: Test) -> bool {
            binary_property(&a, &b, a.is_disjoint(&b), |a, b| !(a & b))
        }

        #[cfg(feature = "vc")]
        fn is_subset_sample(a: Test, b: Test) -> bool {
            binary_property(&a, &b, a.is_subset(&b), |a, b| !a | b)
        }

        #[cfg(feature = "vc")]
        fn union_sample(a: Test, b: Test) -> bool {
            binary_op(&a, &b, &(&a | &b), |a, b| a | b)
        }

        #[cfg(feature = "vc")]
        fn intersection_sample(a: Test, b: Test) -> bool {
            binary_op(&a, &b, &(&a & &b), |a, b| a & b)
        }

        #[cfg(feature = "vc")]
        fn xor_sample(a: Test, b: Test) -> bool {
            binary_op(&a, &b, &(&a ^ &b), |a, b| a ^ b)
        }

        #[cfg(feature = "vc")]
        fn diff_sample(a: Test, b: Test) -> bool {
            binary_op(&a, &b, &(&a - &b), |a, b| a & !b)
        }
    }

    #[cfg(feature = "vc")]
    bitop_assign_consistent!(Test);

    #[cfg(feature = "vc")]
    bitop_symmetry!(Test);

    #[cfg(feature = "vc")]
    bitop_empty!(Test);

    #[cfg(feature = "vc")]
    bitop_sub_not_all!(Test);
}
