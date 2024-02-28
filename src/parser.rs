#![allow(dead_code)]

use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

use crate::{
    ast::{Program, Statement},
    lexer::{Lexer, Token, TokenType},
};

/// Collects code into a vec of tokens.  Should really probably just go in the lexer module.
pub fn tokens_from_code(code: &str) -> Vec<Token> {
    let lexer = Lexer::new(code);
    let tokens: Vec<Token> = lexer.collect();

    tokens
}

#[derive(Debug, PartialEq, PartialOrd)]
pub enum Precendence {
    Lowest,
    Equals,      // ==
    LessGreater, // < >
    Sum,         // +
    Product,     // *
    Prefix,      // -x
    FnCall,      // foo(x)
}

#[derive(Error, Debug, Diagnostic)]
pub enum InterpreterError {
    #[error("Syntax Error")]
    #[diagnostic()]
    SyntaxError {
        #[label("at token")]
        location: SourceSpan,

        #[help]
        explanation: String,
    },

    #[error("Internal Error")]
    #[diagnostic()]
    InternalError {
        #[label("at token")]
        location: SourceSpan,

        #[help]
        explanation: String,
    },
}

/// Represents the top level error emitted by our parser, which contains the source code for the
/// file being parsed, as well as references into that source file the one or more parse errors
/// being emitted.
#[derive(Error, Debug, Diagnostic)]
#[error("ParserError")]
pub struct ParserError {
    #[source_code]
    src: NamedSource<String>,
    #[related]
    errors: Vec<InterpreterError>,
}

fn parse<'ast>(
    src: NamedSource<String>,
    tokens: &'ast Vec<Token>,
) -> Result<Program<'ast>, ParserError> {
    let mut program = Program {
        statements: Vec::new(),
    };

    let mut error = ParserError {
        src,
        errors: Vec::new(),
    };

    let mut tokens_slice = &tokens[..];

    if tokens_slice.len() == 0 {
        return Ok(program);
    }

    let last_token = tokens_slice.last().unwrap();
    let eof_span = last_token.span;

    while tokens_slice.len() > 0 {
        match parse_statement(tokens_slice, eof_span) {
            Ok((stmt, remaining)) => {
                program.statements.push(stmt);
                tokens_slice = remaining;
            }
            Err((mut err, remaining)) => {
                error.errors.append(&mut err);
                tokens_slice = remaining;
            }
        }
    }

    if error.errors.len() > 0 {
        return Err(error);
    }

    Ok(program)
}

// In the event of an error, a parser is expected to fast-forward to the terminal token of that kind
// of expression before returning an error.
type ParseResult<'a, T> = Result<(T, &'a [Token]), (Vec<InterpreterError>, &'a [Token])>;

fn parse_statement(tokens: &[Token], eof_span: SourceSpan) -> ParseResult<Statement> {
    match tokens.get(0) {
        // parse let statement
        Some(token) if token.token_type == TokenType::Let => todo!(),
        // parse expression statement
        Some(token) => todo!(),
        // This is an internal error because the parser should never knowingly call parse_statement
        // without there being any tokens left.
        _ => Err((
            vec![InterpreterError::InternalError {
                location: eof_span,
                explanation:
                    "Unexpected EOF in parse_statement.  This is an internal parser error."
                        .to_string(),
            }],
            tokens,
        )),
    }
}

fn parse_let_statement(tokens: &[Token], eof_span: SourceSpan) -> ParseResult<Statement> {
    let start_token = expect_token(tokens, TokenType::Let, eof_span)?;
    _ = expect_token(tokens, TokenType::Assignment, eof_span)?;

    todo!()
}

fn expect_token<'ast>(
    tokens: &'ast [Token],
    token_type: TokenType,
    eof_span: SourceSpan,
) -> ParseResult<&'ast Token> {
    match tokens.get(0) {
        // if the token is what we expect, advance the slice and return the token
        Some(token) if token.token_type == token_type => Ok((token, &tokens[1..])),
        // otherwise return an appropriately formatted error:
        Some(token) => Err((
            vec![InterpreterError::SyntaxError {
                location: token.span,
                explanation: format!(
                    "Unexpected token {:?}, expected {:?}",
                    token.token_type, token_type
                ),
            }],
            tokens,
        )),
        _ => Err((
            vec![InterpreterError::SyntaxError {
                location: eof_span,
                explanation: format!("Unexpected EOF, expected {:?}", token_type),
            }],
            tokens,
        )),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_let_statements() {
        let code = r#"
let x = 23;
        "#;

        // the lifetime of this variable is the lifetime of our AST, because our AST just points
        // back into this vec:
        let tokens = tokens_from_code(code);
    }
}
