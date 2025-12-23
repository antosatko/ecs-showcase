// use chumsky::prelude::*;

// /// An AST (Abstract Syntax Tree) for Brainfuck instructions
// #[derive(Clone, Debug)]
// enum Instr {
//     Left,
//     Right,
//     Incr,
//     Decr,
//     Read,
//     Write,
//     Loop(Vec<Self>), // In Brainfuck, `[...]` loop instructions contain any number of instructions
// }

// /// A function that generates a Brainfuck parser
// pub(crate) fn brainfuck<'a>() -> impl Parser<'a, &'a str, Vec<Instr>> {
//     // Brainfuck syntax is recursive: each instruction can contain many sub-instructions (via `[...]` loops)
//     recursive(|bf| {
//         choice((
//             // All of the basic instructions are just single characters
//             just('<').to(Instr::Left),
//             just('>').to(Instr::Right),
//             just('+').to(Instr::Incr),
//             just('-').to(Instr::Decr),
//             just(',').to(Instr::Read),
//             just('.').to(Instr::Write),
//             // Loops are strings of Brainfuck instructions, delimited by square brackets
//             bf.delimited_by(just('['), just(']')).map(Instr::Loop),
//         ))
//         // Brainfuck instructions appear sequentially, so parse as many as we need
//         .repeated()
//         .collect()
//     })
// }

// pub(crate) enum LangAst<'src> {
//     /// example:
//     ///
//     /// scheduler main {
//     ///     systems {
//     ///         init
//     ///         update
//     ///         listeners.clockEvent
//     ///     }
//     ///     resources {
//     ///         Clock.fromMs(1000.0/60.0)
//     ///         Keyboard
//     ///     }
//     /// }
//     Scheduler {
//         name: &'src str,
//         systems: Vec<&'src str>,
//         resources: Vec<Expression<'src>>,
//     },
// }

// pub(crate) enum Expression<'src> {
//     Value(Value<'src>),
//     Binary(Box<Expression<'src>>, Box<Expression<'src>>, Operators),
// }

// #[derive(Debug, Clone, Copy)]
// pub(crate) enum Operators {
//     Add,
//     Sub,
//     Mul,
//     Div,
// }

// pub(crate) struct Value<'src> {
//     pub root: ValueRoots<'src>,
//     pub rest: Vec<ValueRest<'src>>,
// }

// pub(crate) enum ValueRoots<'src> {
//     Ident(&'src str),
//     // place for literals etc.. ignore for now
// }

// pub(crate) enum ValueRest<'src> {
//     Filed(&'src str),
//     Call {
//         field: &'src str,
//         params: Vec<Expression<'src>>,
//     },
// }

// pub(crate) fn operator<'src>() -> impl Parser<'src, &'src str, Operators> {
//     choice((
//         just("+").to(Operators::Add),
//         just("-").to(Operators::Sub),
//         just("*").to(Operators::Mul),
//         just("/").to(Operators::Div),
//     ))
//     .padded()
// }

// pub(crate) fn value<'src>(
//     expr: impl Parser<'src, &'src str, Expression<'src>> + Clone,
// ) -> impl Parser<'src, &'src str, Expression<'src>> {
//     let ident = text::ascii::ident().padded();

//     let root = ident.map(ValueRoots::Ident);

//     let call = ident
//         .then(
//             expr.clone()
//                 .separated_by(just(",").padded())
//                 .delimited_by(just("(").padded(), just(")").padded()),
//         )
//         .map(|(field, params)| ValueRest::Call { field, params });

//     let field = ident.map(ValueRest::Filed);

//     let rest = just(".").padded().ignore_then(choice((call, field)));

//     root.then(rest.repeated())
//         .map(|(root, rest)| Expression::Value(Value { root, rest }))
// }

// pub(crate) fn expression<'src>() -> impl Parser<'src, &'src str, Expression<'src>> {
//     recursive(|expr| {
//         let atom = value(expr.clone());

//         let term = atom.clone().foldl(
//             choice((just("*").to(Operators::Mul), just("/").to(Operators::Div)))
//                 .then(atom)
//                 .repeated(),
//             |lhs, (op, rhs)| Expression::Binary(Box::new(lhs), Box::new(rhs), op),
//         );

//         term.foldl(
//             choice((just("+").to(Operators::Add), just("-").to(Operators::Sub)))
//                 .then(term.clone())
//                 .repeated(),
//             |lhs, (op, rhs)| Expression::Binary(Box::new(lhs), Box::new(rhs), op),
//         )
//         .padded()
//     })
// }
