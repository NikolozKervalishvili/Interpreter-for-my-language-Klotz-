use crate::lexer::*;

struct Expr;
struct Literal;
struct Unary;
struct Binary;
struct Grouping;

trait IsNode {}

#[macro_export]
macro_rules! impl_IsNode {
    ($($name:ty),+) => {
        $(
        impl IsNode for $name {}
        )+
    }
}

impl_IsNode!(Expr, Literal, Unary, Binary, Grouping);

struct Node<T: IsNode> {
    data: T,
    children: Vec<Node<T>>,
}

struct Parser {
    tokens: Vec<Token>,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens }
    }

    // fn parse(&mut self) {
    //     match self.tokens[0]._type {

    //     }
    // }
}
