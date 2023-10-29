use std::env;
use tapl_systems::common::diagnostic::{Spanned, SpannedWithDiagnostics};
use tapl_systems::format::ctx_for_err;
use tapl_systems::format::ToCode;
use tapl_systems::typed_lambda::unrestricted::cc::core::{Parser, Source, TypeChecker};
// use tapl_systems::typed_lambda::unrestricted::meta::ast::{
//     Context, ContextStack, Identifier, Kind as MetaKind, Pattern, QualifiedIdentifier,
//     Source as MetaSource, SubstitutableWithTerm, SubstitutableWithType, Term as MetaTerm, TermBind,
//     TermDefMap, Type as MetaType, TypeBind,
// };

pub fn main() {
    let s = env::args().nth(1);
    match s {
        Some(s) => cc_s(s.as_str()),
        None => println!("arg not found"),
    }
}

fn cc_s(code: &str) {
    println!("====================");
    // println!("code: {}", code);
    let SpannedWithDiagnostics {
        value: source,
        span,
        diagnostics,
    } = Parser::new(code)
        .start_parse_source()
        .into_result()
        .unwrap_or_else(|e| {
            e.diagnostics.iter().for_each(|d| println!("{}", d));
            panic!("parse error");
        });
    if !diagnostics.is_empty() {
        diagnostics.iter().for_each(|d| println!("{}", d));
        panic!("parse error");
    }
    let source = Source::try_new(Spanned::new(span, source)).unwrap_or_else(|e| {
        e.iter().for_each(|d| println!("{}", d));
        panic!("Source parse error");
    });
    // println!("source: {}", source.to_code());
    let Spanned {
        value: (term, mut ctx),
        ..
    } = source.map(|e| {
        e.into_term().unwrap_or_else(|e| {
            e.iter().for_each(|d| println!("{}", d));
            panic!("term parse error");
        })
    });
    // let tc = TypeChecker::new();
    // let e = Evaluator::new(ctx.term_def_map());
    let typ = TypeChecker::typing(&mut ctx, &term).unwrap_or_else(|e| {
        e.iter().for_each(|d| println!("{}", d));
        panic!("typing error");
    });
    // let typ = tc.eval_type(&mut ctx, &typ).unwrap_or_else(|e| {
    //     e.iter().for_each(|d| println!("{}", d));
    //     panic!("type eval error");
    // });
    let typ_s = typ.value().clone().into_inner().to_code(&ctx_for_err());
    println!("{}", typ_s);

    let t = TypeChecker::eval_term(&mut ctx, &term).unwrap_or_else(|e| {
        e.iter().for_each(|d| println!("{}", d));
        panic!("term eval error");
    });
    println!("--->  {}", t.to_code(&ctx_for_err()));
    // if expected_term != "SKIP" {
    //     let term = e
    //         .eval_iter(&term)
    //         .map_then_eval_with_limit(100, |e| {
    //             e.map(|e| {
    //                 println!("STEP: {}", e.value.0.to_code());
    //                 e
    //             })
    //         })
    //         .expect("evaluation limit exceeded");
    //     assert_eq!(
    //         term.value().clone().into_inner().to_code(),
    //         expected_term,
    //         "expected {} ->* {}",
    //         code,
    //         expected_term,
    //     );
    //     let typ2 = tc.typing(&mut ctx, term.clone()).unwrap_or_else(|e| {
    //         e.iter().for_each(|d| println!("{}", d));
    //         panic!("typing error");
    //     });
    //     assert_eq!(
    //         typ2.value.into_meta_type().to_code(),
    //         expected_typ,
    //         "{:?} : {:?}",
    //         term.value().clone().into_inner().to_code(),
    //         expected_typ
    //     );
    // }
}

// fn f1() {
//     use tapl_systems::typed_lambda::unrestricted::meta::tokenizer::{Token, Tokenizer};
//     let input = r#"
//             // comment
//             let x = 1;
//             lambda x: Nat. x
//         "#;
//     let mut tokenizer = Tokenizer::new(input);
//     for token in tokenizer.take_while(|token| &Token::EOF != token.value()) {
//         println!("{:?}", token);
//     }
// }
//
// fn f2() {
//     use tapl_systems::typed_lambda::unrestricted::meta::parser::*;
//     let mut parser = Parser::new(
//         r#"
//         type VecNat10 = Vec[nat](10).
//         def x = f[VecNat10](if true { 1 } else { 2 }; 3).
//         type mymap = map[nat][bool].
//         ind nat {
//             o: nat,
//             s: nat -> nat -> nat,
//         }
//         let x = (unit; x);
//         x(unit);
//         (unit; unit);
//         unit;
//         unit;
//         (
//             unit;
//             unit;
//             unit;
//             x
//         )(unit)
//     "#,
//     );
//     let term = parser.parse_source().into_result().unwrap();
//     println!("{:?}", term);
//     println!("{}", term.value.to_code());
// }
