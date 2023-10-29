mod debug_evaluator {
    use crate::common::evaluator::small_step::*;
    use core::fmt::Display;

    pub struct DebugEvaluator<Stmt> {
        evaluator: Box<dyn SmallStepEvaluator<Stmt = Stmt>>,
    }
    impl<Stmt> DebugEvaluator<Stmt> {
        pub fn new(evaluator: impl SmallStepEvaluator<Stmt = Stmt> + 'static) -> Self {
            Self {
                evaluator: Box::new(evaluator),
            }
        }
    }
    impl<Stmt: Clone + Display> SmallStepEvaluator for DebugEvaluator<Stmt> {
        type Stmt = Stmt;
        fn eval1(&self, t: &Stmt) -> Option<Self::Stmt> {
            let result = self.evaluator.eval1(t);
            println!(
                "eval1({})   =   {}",
                t,
                result
                    .as_ref()
                    .map(|t| t.to_string())
                    .unwrap_or_else(|| "None".to_owned())
            );
            result
        }
    }
}

mod common {
    use std::fmt::Display;

    use crate::common::evaluator::small_step::*;
    use crate::untyped_lambda::core::ChurchValue;
    use crate::untyped_lambda::meta_named::syntax::TermParser;
    use crate::untyped_lambda::named::ast::NamedTerm;
    use crate::untyped_lambda::nameless::indexed::ast::IndexedNamelessTerm;
    use crate::untyped_lambda::nameless::indexed::evaluator::call_by_value::Evaluator as NamelessEvaluator;

    use super::debug_evaluator::DebugEvaluator;

    pub fn to_church_value(t: IndexedNamelessTerm) -> ChurchValue {
        let inner_evaluator = DebugEvaluator::new(NamelessEvaluator::new(true));
        t.to_church_value(&inner_evaluator, 30, 5)
    }

    pub fn assert_church_value_base<Evaluator: SmallStepEvaluator>(
        e: &Evaluator,
        t: &Evaluator::Stmt,
        expected: &str,
    ) where
        Evaluator::Stmt: Display + Clone + Into<IndexedNamelessTerm>,
    {
        let inner_evaluator = DebugEvaluator::new(NamelessEvaluator::new(true));
        let t = e.eval_iter(t).eval_with_limit(30).unwrap();
        let v = Into::<IndexedNamelessTerm>::into(t).to_church_value(&inner_evaluator, 30, 5);
        assert_eq!(v.to_string(), expected);
    }

    pub fn parse(s: &str) -> NamedTerm {
        TermParser::new().parse(s).unwrap()
    }
    pub fn unit() -> NamedTerm {
        parse(r"\x. x")
    }
    pub fn church_true() -> NamedTerm {
        parse(r"\x y. x")
    }
    pub fn church_first() -> NamedTerm {
        parse(r"\a b. a")
    }
    pub fn church_second() -> NamedTerm {
        parse(r"\a b. b")
    }
    pub fn church_succ() -> NamedTerm {
        parse(r"\n f x. f ((n f) x)")
    }
    pub fn fix_cbv() -> NamedTerm {
        parse(r"\f. (\x. f \y. (x x) y) (\x. f \y. (x x) y)")
    }
    pub fn fix_cbn() -> NamedTerm {
        parse(r"\f. (\x. f (x x)) (\x. f (x x))")
    }
    pub fn church_pair() -> NamedTerm {
        parse(r"\a b f. f a b")
    }
    pub fn church_0() -> NamedTerm {
        parse(r"\f x. x")
    }
    pub fn church_1() -> NamedTerm {
        parse(r"\f x. f x")
    }
    pub fn church_3() -> NamedTerm {
        parse(r"\f x. f (f (f x))")
    }
    // 2 * 2 * 2 * 2
    pub fn church_16_with_eval() -> NamedTerm {
        parse(r"(\two f x. two (two (two (two f))) x) \f x. f (f x)")
    }
    // (2^2)^2
    pub fn church_16_with_eval_powering() -> NamedTerm {
        parse(r"(\two. two two two) \f x. f (f x)")
    }
    pub fn church_nat_stream_cbv() -> NamedTerm {
        parse(
            format!(
                r"{fix} (\f n. {pair} n (\_. f ({succ} n))) {zero}",
                fix = fix_cbv(),
                pair = church_pair(),
                succ = church_succ(),
                zero = church_0()
            )
            .as_str(),
        )
    }
    pub fn church_nat_stream_cbn() -> NamedTerm {
        parse(
            format!(
                r"{fix} (\f n. {pair} n (\_. f ({succ} n))) {zero}",
                fix = fix_cbn(),
                pair = church_pair(),
                succ = church_succ(),
                zero = church_0()
            )
            .as_str(),
        )
    }
    pub fn church_nat_infinite_cbv() -> NamedTerm {
        parse(
            format!(
                r"{fix} (\f n. \g. g n (f ({succ} n))) {zero}",
                fix = fix_cbv(),
                succ = church_succ(),
                zero = church_0()
            )
            .as_str(),
        )
    }
    pub fn church_nat_infinite_cbn() -> NamedTerm {
        // call-by-name style infinite size pair cannot be inferred because the parts of pair is
        // coming to arguments, but arguments are evaluated preceding the substitution.
        parse(
            format!(
                r"{fix} (\f n. {pair} n (f ({succ} n))) {zero}",
                fix = fix_cbv(),
                pair = church_pair(),
                succ = church_succ(),
                zero = church_0()
            )
            .as_str(),
        )
    }

    pub fn assert_nat_stream<Term>(stream: NamedTerm, e: &impl SmallStepEvaluator<Stmt = Term>)
    where
        Term: From<NamedTerm> + Into<IndexedNamelessTerm> + Clone,
    {
        let mut s = stream;
        for i in 0..5 {
            let t = <Term as From<NamedTerm>>::from(s.apply(church_first()));

            let step_limit = 100;
            let t = e
                .eval_iter(&t)
                .eval_with_limit(step_limit)
                .unwrap_or_else(|| panic!("eval steps should be less than {}", step_limit));

            let t = <Term as Into<IndexedNamelessTerm>>::into(t);
            assert_eq!(to_church_value(t), ChurchValue::Nat(i));
            s = s.apply(church_second()).apply(unit());
        }
    }
}

mod indexed_nameless_cbv {
    use super::common::*;
    use super::debug_evaluator::DebugEvaluator;
    use crate::untyped_lambda::named::ast::NamedTerm;
    use crate::untyped_lambda::nameless::indexed::ast::IndexedNamelessTerm;
    use crate::untyped_lambda::nameless::indexed::evaluator::call_by_value::Evaluator;

    fn evaluator() -> DebugEvaluator<IndexedNamelessTerm> {
        DebugEvaluator::new(Evaluator::new(false))
    }
    fn assert_church_value(t: NamedTerm, expected: &str) {
        assert_church_value_base(&evaluator(), &t.into(), expected);
    }

    #[test]
    fn test_church_value_true() {
        assert_church_value(church_true(), "true");
    }

    #[test]
    fn test_church_value_0() {
        assert_church_value(church_0(), "0");
    }

    #[test]
    fn test_church_value_1() {
        assert_church_value(church_1(), "1");
    }

    #[test]
    fn test_church_value_3() {
        assert_church_value(church_3(), "3");
    }

    #[test]
    fn test_church_value_16_with_eval() {
        assert_church_value(church_16_with_eval(), "16");
    }

    #[test]
    fn test_church_value_16_with_eval_powering() {
        assert_church_value(church_16_with_eval_powering(), "16");
    }

    #[test]
    fn test_church_value_nat_stream() {
        assert_nat_stream(church_nat_stream_cbv(), &evaluator());
    }

    #[test]
    fn test_church_value_nat_infinite() {
        assert_church_value(church_nat_infinite_cbv(), "(0, (1, (2, (3, (4, ...)))))");
    }
}

mod indexed_nameless_cbn {
    use super::common::*;
    use super::debug_evaluator::DebugEvaluator;
    use crate::untyped_lambda::named::ast::NamedTerm;
    use crate::untyped_lambda::nameless::indexed::ast::IndexedNamelessTerm;
    use crate::untyped_lambda::nameless::indexed::evaluator::call_by_name::Evaluator;

    fn evaluator() -> DebugEvaluator<IndexedNamelessTerm> {
        DebugEvaluator::new(Evaluator::new(false))
    }
    fn assert_church_value(t: NamedTerm, expected: &str) {
        assert_church_value_base(&evaluator(), &t.into(), expected);
    }

    #[test]
    fn test_church_value_true() {
        assert_church_value(church_true(), "true");
    }

    #[test]
    fn test_church_value_0() {
        assert_church_value(church_0(), "0");
    }

    #[test]
    fn test_church_value_1() {
        assert_church_value(church_1(), "1");
    }

    #[test]
    fn test_church_value_3() {
        assert_church_value(church_3(), "3");
    }

    #[test]
    fn test_church_value_16_with_eval() {
        assert_church_value(church_16_with_eval(), "16");
    }

    #[test]
    fn test_church_value_16_with_eval_powering() {
        assert_church_value(church_16_with_eval_powering(), "16");
    }
}

mod named_cbv {
    use super::common::*;
    use super::debug_evaluator::DebugEvaluator;
    use crate::untyped_lambda::named::ast::NamedTerm;
    use crate::untyped_lambda::named::evaluator::call_by_value::Evaluator;

    fn evaluator() -> DebugEvaluator<NamedTerm> {
        DebugEvaluator::new(Evaluator::new(false))
    }
    fn assert_church_value(t: NamedTerm, expected: &str) {
        assert_church_value_base(&evaluator(), &t, expected);
    }

    #[test]
    fn test_church_value_true() {
        assert_church_value(church_true(), "true");
    }

    #[test]
    fn test_church_value_0() {
        assert_church_value(church_0(), "0");
    }

    #[test]
    fn test_church_value_1() {
        assert_church_value(church_1(), "1");
    }

    #[test]
    fn test_church_value_3() {
        assert_church_value(church_3(), "3");
    }

    #[test]
    fn test_church_value_16_with_eval() {
        assert_church_value(church_16_with_eval(), "16");
    }

    #[test]
    fn test_church_value_16_with_eval_powering() {
        assert_church_value(church_16_with_eval_powering(), "16");
    }

    #[test]
    fn test_church_value_nat_infinite_cbv() {
        assert_church_value(church_nat_infinite_cbv(), "(0, (1, (2, (3, (4, ...)))))");
    }
}

mod named_cbn {
    use super::common::*;
    use super::debug_evaluator::DebugEvaluator;
    use crate::untyped_lambda::named::ast::NamedTerm;
    use crate::untyped_lambda::named::evaluator::call_by_name::Evaluator;

    fn evaluator() -> DebugEvaluator<NamedTerm> {
        DebugEvaluator::new(Evaluator::new(false))
    }
    fn assert_church_value(t: NamedTerm, expected: &str) {
        assert_church_value_base(&evaluator(), &t, expected);
    }

    #[test]
    fn test_church_value_true() {
        assert_church_value(church_true(), "true");
    }

    #[test]
    fn test_church_value_0() {
        assert_church_value(church_0(), "0");
    }

    #[test]
    fn test_church_value_1() {
        assert_church_value(church_1(), "1");
    }

    #[test]
    fn test_church_value_3() {
        assert_church_value(church_3(), "3");
    }

    #[test]
    fn test_church_value_16_with_eval() {
        assert_church_value(church_16_with_eval(), "16");
    }

    #[test]
    fn test_church_value_16_with_eval_powering() {
        assert_church_value(church_16_with_eval_powering(), "16");
    }

    #[test]
    fn test_church_value_nat_stream_cbv() {
        assert_nat_stream(church_nat_stream_cbv(), &evaluator());
    }

    #[test]
    fn test_church_value_nat_stream_cbn() {
        assert_nat_stream(church_nat_stream_cbn(), &evaluator());
    }

    #[test]
    fn test_church_value_nat_infinite_cbv() {
        assert_church_value(church_nat_infinite_cbv(), "(0, (1, (2, (3, (4, ...)))))");
    }

    #[test]
    fn test_church_value_nat_infinite_cbn() {
        assert_church_value(church_nat_infinite_cbn(), "?");
    }
}
