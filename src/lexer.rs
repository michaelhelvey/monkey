use miette::SourceSpan;
use unicode_segmentation::UnicodeSegmentation;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    // Operators:
    Equals,     // =
    NotEquals,  // !=
    Assignment, // ==
    Plus,
    Minus,
    Mult,
    Divide,
    Semicolon,
    Negation,

    LT,
    GT,

    // Punctuation:
    ParenOpen,
    ParenClose,
    BraceOpen,
    BraceClose,
    Comma,

    // Fallthrough cases:
    Number,
    Identifier,

    // Keywords:
    Let,
    Fn,
    If,
    Else,
    Return,

    // Illegal:
    Illegal,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
    pub span: SourceSpan,
}

impl Token {
    fn new(literal: impl Into<String>, token_type: TokenType, start: usize, end: usize) -> Self {
        Self {
            literal: literal.into(),
            token_type,
            span: (start..end).into(),
        }
    }
}

pub struct Lexer<'a> {
    graphemes: Vec<(usize, &'a str)>,
    current_position: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        let graphemes: Vec<(usize, &'a str)> = src.grapheme_indices(true).collect();

        Self {
            graphemes,
            current_position: 0,
        }
    }
}

fn is_identifier_character(c: &str) -> bool {
    c.chars().fold(true, |acc, curr| {
        let result = match curr {
            c if c.is_whitespace() => false,
            c if c.is_digit(10) => false,
            '_' => true,
            c if c.is_ascii_punctuation() => false,
            _ => true,
        };

        acc && result
    })
}

fn is_digit_character(c: &str) -> bool {
    c.chars().fold(true, |acc, curr| curr.is_digit(10) && acc)
}

fn is_whitespace(c: &str) -> bool {
    c.chars()
        .fold(true, |acc, curr| curr.is_whitespace() && acc)
}

fn get_identifier_type(lit: &str) -> TokenType {
    match lit {
        "let" => TokenType::Let,
        "fn" => TokenType::Fn,
        "return" => TokenType::Return,
        "if" => TokenType::If,
        "else" => TokenType::Else,
        _ => TokenType::Identifier,
    }
}

// Advances self.graphemes and returns a tuple of how many bytes it advanced and the resulting string.
macro_rules! advance_pred {
    ($self:ident, $pred:ident, $start:expr, $start_lit:expr) => {{
        let mut end = *$start;
        let mut ch = *$start_lit;
        let mut result = String::new();
        while $pred(ch) {
            result.push_str(ch);
            $self.current_position += 1;
            let Some((next_end, next_ch)) = $self.graphemes.get($self.current_position) else {
                break;
            };

            ch = *next_ch;
            end = *next_end;
        }

        // Once we've reached the end, we've advanced one grapheme too far, so go back:
        $self.current_position -= 1;

        (end, result)
    }};
}

macro_rules! parse_two_char_token {
    ($self:expr, $current_start:expr, $current_lit:expr, $peek_char:literal, $matches_result:expr, $fallback_result:expr) => {{
        match $self.graphemes.get($self.current_position + 1) {
            Some((next_start, next_lit @ $peek_char)) => {
                $self.current_position += 1;
                let result = format!("{}{}", *$current_lit, *next_lit);
                Token::new(result, $matches_result, *$current_start, *next_start)
            }
            _ => Token::new(
                *$current_lit,
                $fallback_result,
                *$current_start,
                *$current_start,
            ),
        }
    }};
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        // skip whitespace:
        while let Some(current_ch) = self.graphemes.get(self.current_position) {
            if is_whitespace(current_ch.1) {
                self.current_position += 1;
                continue;
            }

            break;
        }

        let Some(current_ch) = self.graphemes.get(self.current_position) else {
            return None;
        };

        let token = match current_ch {
            (start, lit @ "=") => parse_two_char_token!(
                self,
                start,
                lit,
                "=",
                TokenType::Equals,
                TokenType::Assignment
            ),
            (start, lit @ "!") => parse_two_char_token!(
                self,
                start,
                lit,
                "=",
                TokenType::NotEquals,
                TokenType::Negation
            ),
            (start, lit @ "+") => Token::new(*lit, TokenType::Plus, *start, *start),
            (start, lit @ "-") => Token::new(*lit, TokenType::Minus, *start, *start),
            (start, lit @ "*") => Token::new(*lit, TokenType::Mult, *start, *start),
            (start, lit @ "/") => Token::new(*lit, TokenType::Divide, *start, *start),
            (start, lit @ ">") => Token::new(*lit, TokenType::GT, *start, *start),
            (start, lit @ "<") => Token::new(*lit, TokenType::LT, *start, *start),
            (start, lit @ ";") => Token::new(*lit, TokenType::Semicolon, *start, *start),
            (start, lit @ ",") => Token::new(*lit, TokenType::Comma, *start, *start),
            (start, lit @ "(") => Token::new(*lit, TokenType::ParenOpen, *start, *start),
            (start, lit @ ")") => Token::new(*lit, TokenType::ParenClose, *start, *start),
            (start, lit @ "{") => Token::new(*lit, TokenType::BraceOpen, *start, *start),
            (start, lit @ "}") => Token::new(*lit, TokenType::BraceClose, *start, *start),
            (start, lit) if is_identifier_character(lit) => {
                let (end, result) = advance_pred!(self, is_identifier_character, start, lit);
                let token_type = get_identifier_type(&result);
                Token::new(result, token_type, *start, end)
            }
            (start, lit) if is_digit_character(lit) => {
                let (end, result) = advance_pred!(self, is_digit_character, start, lit);
                Token::new(result, TokenType::Number, *start, end)
            }
            (start, lit) => Token::new(*lit, TokenType::Illegal, *start, *start),
        };

        self.current_position += 1;

        Some(token)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_token_eq {
        ($token:expr, $token_type:expr, $lit:literal) => {{
            let Some(token) = $token else {
                panic!("Expected to get token, received None");
            };

            assert_eq!(token.token_type, $token_type);
            assert_eq!(token.literal, $lit);
        }};
    }

    #[test]
    fn test_lexer() {
        let code = r#"
let x = 23;

let add = fn(x, y) {
    return x +y;
}

== ! !=
        "#;

        let mut lexer = Lexer::new(code);
        assert_token_eq!(lexer.next(), TokenType::Let, "let");
        assert_token_eq!(lexer.next(), TokenType::Identifier, "x");
        assert_token_eq!(lexer.next(), TokenType::Assignment, "=");
        assert_token_eq!(lexer.next(), TokenType::Number, "23");
        assert_token_eq!(lexer.next(), TokenType::Semicolon, ";");
        assert_token_eq!(lexer.next(), TokenType::Let, "let");
        assert_token_eq!(lexer.next(), TokenType::Identifier, "add");
        assert_token_eq!(lexer.next(), TokenType::Assignment, "=");
        assert_token_eq!(lexer.next(), TokenType::Fn, "fn");
        assert_token_eq!(lexer.next(), TokenType::ParenOpen, "(");
        assert_token_eq!(lexer.next(), TokenType::Identifier, "x");
        assert_token_eq!(lexer.next(), TokenType::Comma, ",");
        assert_token_eq!(lexer.next(), TokenType::Identifier, "y");
        assert_token_eq!(lexer.next(), TokenType::ParenClose, ")");
        assert_token_eq!(lexer.next(), TokenType::BraceOpen, "{");
        assert_token_eq!(lexer.next(), TokenType::Return, "return");
        assert_token_eq!(lexer.next(), TokenType::Identifier, "x");
        assert_token_eq!(lexer.next(), TokenType::Plus, "+");
        assert_token_eq!(lexer.next(), TokenType::Identifier, "y");
        assert_token_eq!(lexer.next(), TokenType::Semicolon, ";");
        assert_token_eq!(lexer.next(), TokenType::BraceClose, "}");
        assert_token_eq!(lexer.next(), TokenType::Equals, "==");
        assert_token_eq!(lexer.next(), TokenType::Negation, "!");
        assert_token_eq!(lexer.next(), TokenType::NotEquals, "!=");
    }

    #[test]
    fn test_is_identifier_character() {
        let tests = [
            ("a", true),
            ("2", false),
            ("🙌", true),
            ("-", false),
            ("_", true),
        ];

        for (ch, expected) in tests {
            eprintln!("{} == {}", ch, expected);
            assert_eq!(is_identifier_character(ch), expected);
        }
    }
}
