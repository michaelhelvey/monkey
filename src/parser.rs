use std::env::current_exe;

use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

use crate::{
    ast::{Expression, Identifier, IntegerLiteral, LetStatement, Program, Statement},
    lexer::{Lexer, Token, TokenType},
};

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
#[error("ParseError")]
pub struct ParseError {
    #[source_code]
    src: NamedSource<String>,
    #[related]
    errors: Vec<InterpreterError>,
}

pub struct Parser {
    tokens: Vec<Token>,
    current_position: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current_position: 0,
        }
    }

    /// Creates a new parser from a raw code string
    pub fn from_code(code: &str) -> Self {
        let lexer = Lexer::new(code);
        let tokens: Vec<Token> = lexer.collect();

        Self::new(tokens)
    }

    pub fn parse(&mut self, source: NamedSource<String>) -> Result<Program, ParseError> {
        let mut program = Program {
            statements: Vec::new(),
        };

        let mut error = ParseError {
            src: source,
            errors: Vec::new(),
        };

        while let Some(token) = self.tokens.get(self.current_position) {
            match parse_statement(token, &self.tokens[self.current_position..]) {
                Ok((consumed, stmt)) => {
                    self.current_position += consumed;
                    program.statements.push(stmt);
                }
                Err((consumed, e)) => {
                    self.current_position += consumed;
                    error.errors.push(e);
                }
            };
        }

        if error.errors.len() > 0 {
            return Err(error);
        }

        Ok(program)
    }
}

type ParseFnResult<T> = Result<(usize, T), (usize, InterpreterError)>;

fn parse_statement(current_token: &Token, tokens: &[Token]) -> ParseFnResult<Statement> {
    match current_token.token_type {
        TokenType::Let => parse_let_statement(current_token, tokens),
        _ => parse_expression_statement(current_token, tokens),
    }
}

macro_rules! match_next_token {
    ($current_span:expr, $tokens:expr, $consumed:expr, $type:expr) => {{
        eprintln!("match_next_token: {:?}", $tokens);
        let ident = match $tokens.get($consumed + 1) {
            Some(token) if token.token_type == $type => {
                $consumed += 1;
                token
            }
            Some(token) => {
                return Err((
                    $consumed,
                    InterpreterError::SyntaxError {
                        location: token.span,
                        explanation: format!(
                            "Unexpected token {:?}, expected {}",
                            token.token_type,
                            stringify!($type)
                        ),
                    },
                ));
            }
            None => {
                return Err((
                    $consumed,
                    InterpreterError::SyntaxError {
                        location: $current_span,
                        explanation: "Unexpected EOF while parsing let statement".to_string(),
                    },
                ));
            }
        };

        ident
    }};
}

fn parse_let_statement(current_token: &Token, tokens: &[Token]) -> ParseFnResult<Statement> {
    eprintln!("parse_let_statement");
    let let_token = current_token;
    let mut position = 0;

    let ident_token = match_next_token!(
        current_token.span,
        &tokens[position..],
        position,
        TokenType::Identifier
    );
    dbg!(ident_token, position);

    let eq_token = match_next_token!(
        ident_token.span,
        &tokens[position..],
        position,
        TokenType::Assignment
    );

    dbg!(eq_token);

    eprintln!("consumed: {}", position);

    let (consumed, right) = parse_expression(eq_token, &tokens[position..], Precendence::Lowest)?;
    position += consumed;

    Ok((
        position,
        Statement::Let(LetStatement {
            token: let_token.clone(),
            name: Identifier {
                token: ident_token.clone(),
                value: ident_token.literal.clone(),
            },
            value: right,
        }),
    ))
}

fn parse_expression_statement(current_token: &Token, tokens: &[Token]) -> ParseFnResult<Statement> {
    let mut position = 0;
    let (consumed, expr) = parse_expression(current_token, tokens, Precendence::Lowest)?;
    position += consumed;

    // Peek the next token, and if it's a semicolon, advance, but if not, do not error, because we
    // want to support optional semicolons:
    match tokens.get(position + 1) {
        Some(token) if token.token_type == TokenType::Semicolon => {
            position += 1;
        }
        _ => {}
    };

    Ok((position, Statement::Expression(expr)))
}

fn parse_expression(
    current_token: &Token,
    tokens: &[Token],
    precedence: Precendence,
) -> ParseFnResult<Expression> {
    match current_token.token_type {
        TokenType::Identifier => {
            // TODO: it would be cool if tokens didn't need to be clone (e.g. they could just be
            // pointers into the AST which would own them, and the AST would be guaranteed to live
            // as long as the program was running)
            let current_token = current_token.clone();
            Ok((
                1,
                Expression::Identifier(Identifier {
                    value: current_token.literal.clone(),
                    token: current_token,
                }),
            ))
        }
        TokenType::Number => {
            let current_token = current_token.clone();
            Ok((
                1,
                Expression::IntegerLiteral(IntegerLiteral {
                    value: current_token.literal.clone(),
                    token: current_token,
                }),
            ))
        }
        _ => Err((
            0,
            InterpreterError::InternalError {
                location: current_token.span,
                explanation: format!(
                    "No prefix parser available for token of type {:?}",
                    current_token.token_type
                ),
            },
        )),
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::Statement;

    use super::*;

    #[test]
    fn test_parse_let_statements() {
        let code = r#"
let x = 5;
let y = 10;
let foobar = 838383;
        "#;

        let mut parser = Parser::from_code(code);
        let program = parser
            .parse(NamedSource::new("(inline)", code.to_string()))
            .unwrap();

        let expected_identifiers = ["x", "y", "foobar"];

        assert_eq!(program.statements.len(), 3);

        for (idx, expected) in expected_identifiers.iter().enumerate() {
            let stmt = program.statements.get(idx).unwrap();

            let Statement::Let(stmt) = stmt else {
                panic!(
                    "Expected statement of type {} received {:?}",
                    stringify!(Statement::Let),
                    stmt
                );
            };

            assert_eq!(stmt.name.value.as_str(), *expected);
        }
    }
}
