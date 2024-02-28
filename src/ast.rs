//! Our AST consists of Nodes, which can be either statements or expressions:
//! A node is just any element in the tree.  Some nodes are statements, and others are expressions.
//! Expressions evaluate to a value, and statements do not.

use crate::lexer::Token;

#[derive(Debug)]
pub enum Statement<'ast> {
    Program(Program<'ast>),
    Let(LetStatement<'ast>),
    // An "expression statement" is just a wrapper for an expression.  A program is a series of
    // statements, some of which could be expressions. For example "2 + 2;" is a valid statement
    // that could form a complete program, but it's also an expression that evalulates to 4.
    Expression(Expression<'ast>),
}

#[derive(Debug)]
pub enum Expression<'ast> {
    Identifier(Identifier<'ast>),
    IntegerLiteral(IntegerLiteral<'ast>),
}

#[derive(Debug)]
pub struct Program<'ast> {
    pub statements: Vec<Statement<'ast>>,
}

#[derive(Debug)]
pub struct LetStatement<'ast> {
    pub start_token: &'ast Token,
    pub name: Identifier<'ast>,
    pub value: Expression<'ast>,
}

#[derive(Debug)]
pub struct Identifier<'ast> {
    pub start_token: &'ast Token,
    pub value: String,
}

#[derive(Debug)]
pub struct IntegerLiteral<'ast> {
    pub start_token: &'ast Token,
    // for now, we will represent numbers as strings in the AST and then evalulate them in the
    // interpreter
    pub value: String,
}
