//! Our AST consists of Nodes, statements, and expressions:
//! A node is just any element in the tree.  Some nodes are statements, and others are expressions.
//! Expressions evaluate to a value, and statements do not.

use crate::lexer::Token;

#[derive(Debug)]
pub enum Node {
    Statement(Statement),
    Expression(Expression),
}

#[derive(Debug)]
pub enum Statement {
    Program(Program),
    Let(LetStatement),
    // An "expression statement" is just a wrapper for an expression.  A program is a series of
    // statements, some of which could be expressions. For example "2 + 2;" is a valid statement
    // that could form a complete program, but it's also an expression that evalulates to 4.
    Expression(Expression),
}

#[derive(Debug)]
pub enum Expression {
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Expression,
}

#[derive(Debug)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

#[derive(Debug)]
pub struct IntegerLiteral {
    pub token: Token,
    // for now, we will represent numbers as strings in the AST and then evalulate them in the
    // interpreter
    pub value: String,
}
