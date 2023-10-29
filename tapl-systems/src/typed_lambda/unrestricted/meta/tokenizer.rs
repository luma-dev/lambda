use crate::common::diagnostic::{Location, Span, Spanned};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    Identifier(String),
    Pattern(String),

    Comment(String),

    // Nat values
    NumberDec(String),
    NumberOct(String),
    NumberHex(String),
    NumberBin(String),

    // Bool operators
    If,
    Else,

    // Fixpoint operators
    Fix,

    // Declarators
    Let,
    Def,
    Type,
    Axiom,

    // Induction
    Ind,
    Match,

    // Built-in types
    Forall,
    Exists,

    // Type
    Pi,

    // Recursive type
    Mu,

    // Built-in term operators
    All,
    Ex,

    // Grouping punctuations
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    // Other symbols
    Comma,
    Colon,
    ColonColon,
    Semicolon,
    Dot,
    Equal,
    Wildcard,
    Asterisk,
    Slash,
    Dollar,
    Backslash,
    Percent,
    Exclamation,

    RightArrowSingle,
    RightArrowDouble,

    Undefined(char),

    EOF,
}

pub struct Tokenizer<'a> {
    iter: std::iter::Peekable<std::str::Chars<'a>>,
    pos: usize,
    line: usize,
    col: usize,
}
impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            iter: input.chars().peekable(),
            pos: 0,
            line: 1,
            col: 1,
        }
    }
    fn next(&mut self) -> Option<char> {
        let c = self.iter.next();
        if let Some(c) = c {
            self.pos += 1;
            if c == '\n' {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
        }
        c
    }
    fn peek(&mut self) -> Option<char> {
        self.iter.peek().cloned()
    }
    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek() {
            if c.is_whitespace() {
                self.next();
            } else {
                break;
            }
        }
    }
    fn read_comment(&mut self) -> String {
        let mut comment = String::new();
        while let Some(c) = self.peek() {
            if c == '\n' {
                break;
            } else {
                comment.push(c);
                self.next();
            }
        }
        comment
    }
    fn read_identifier(&mut self, c: char) -> String {
        let mut id = String::new();
        id.push(c);
        while let Some(c) = self.peek() {
            if c.is_alphanumeric() || c == '_' {
                id.push(c);
                self.next();
            } else {
                break;
            }
        }
        id
    }
    fn read_number_dec(&mut self, c: char) -> String {
        let mut num = String::new();
        num.push(c);
        while let Some(c) = self.peek() {
            if c.is_numeric() {
                num.push(c);
                self.next();
            } else {
                break;
            }
        }
        num
    }
    fn read_number_hex(&mut self) -> String {
        let mut num = String::new();
        while let Some(c) = self.peek() {
            if c.is_numeric() || ('a'..='f').contains(&c) || ('A'..='F').contains(&c) {
                num.push(c);
                self.next();
            } else {
                break;
            }
        }
        num
    }
    fn read_number_oct(&mut self) -> String {
        let mut num = String::new();
        while let Some(c) = self.peek() {
            if ('0'..='7').contains(&c) {
                num.push(c);
                self.next();
            } else {
                break;
            }
        }
        num
    }
    fn read_number_bin(&mut self) -> String {
        let mut num = String::new();
        while let Some(c) = self.peek() {
            if c == '0' || c == '1' {
                num.push(c);
                self.next();
            } else {
                break;
            }
        }
        num
    }
    fn read_token(&mut self) -> Spanned<Token> {
        self.skip_whitespace();
        let start = Location::new(self.line, self.col);
        let c = match self.next() {
            Some(c) => c,
            None => {
                return Spanned::new(
                    Span::new(
                        Location::new(self.line, self.col + 1),
                        Location::new(self.line, self.col + 1),
                    )
                    .into(),
                    Token::EOF,
                )
            }
        };
        let token = match c {
            '(' => Token::LParen,
            ')' => Token::RParen,
            '{' => Token::LBrace,
            '}' => Token::RBrace,
            '[' => Token::LBracket,
            ']' => Token::RBracket,
            ',' => Token::Comma,
            ':' => {
                if let Some(':') = self.peek() {
                    self.next();
                    Token::ColonColon
                } else {
                    Token::Colon
                }
            }
            ';' => Token::Semicolon,
            '.' => Token::Dot,
            '/' => {
                if let Some('/') = self.peek() {
                    self.next();
                    let comment = self.read_comment();
                    Token::Comment(comment)
                } else {
                    Token::Slash
                }
            }
            '$' => Token::Dollar,
            '\\' => Token::Backslash,
            '-' => match self.peek() {
                Some('>') => {
                    self.next();
                    Token::RightArrowSingle
                }
                _ => Token::Undefined(c),
            },
            '=' => match self.peek() {
                Some('>') => {
                    self.next();
                    Token::RightArrowDouble
                }
                _ => Token::Equal,
            },
            '*' => Token::Asterisk,
            '%' => Token::Percent,
            '!' => Token::Exclamation,
            '_' => Token::Wildcard,
            'a'..='z' | 'A'..='Z' => {
                let id = self.read_identifier(c);
                match id.as_str() {
                    "let" => Token::Let,
                    "def" => Token::Def,
                    "type" => Token::Type,
                    "axiom" => Token::Axiom,

                    "ind" => Token::Ind,
                    "match" => Token::Match,

                    "pi" => Token::Pi,

                    "mu" => Token::Mu,

                    "forall" => Token::Forall,
                    "exists" => Token::Exists,

                    "if" => Token::If,
                    "else" => Token::Else,

                    "all" => Token::All,
                    "ex" => Token::Ex,

                    "fix" => Token::Fix,

                    _ => Token::Identifier(id),
                }
            }
            '0'..='9' => match (c, self.peek()) {
                ('0', Some('x')) => {
                    self.next();
                    let num = self.read_number_hex();
                    Token::NumberHex(num)
                }
                ('0', Some('o')) => {
                    self.next();
                    let num = self.read_number_oct();
                    Token::NumberOct(num)
                }
                ('0', Some('b')) => {
                    self.next();
                    let num = self.read_number_bin();
                    Token::NumberBin(num)
                }
                _ => {
                    let num = self.read_number_dec(c);
                    Token::NumberDec(num)
                }
            },
            _ => Token::Undefined(c),
        };
        let end = Location::new(self.line, self.col);
        Spanned::new(Span::new(start, end).into(), token)
    }
}
impl<'a> Iterator for Tokenizer<'a> {
    type Item = Spanned<Token>;
    fn next(&mut self) -> Option<Self::Item> {
        let token = self.read_token();
        Some(token)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tokens(input: &str) -> Vec<Token> {
        Tokenizer::new(input)
            .take_while(|t| t.value() != &Token::EOF)
            .map(|t| t.value().clone())
            .collect()
    }

    #[test]
    fn test_tokenizer() {
        assert_eq!(
            tokens("let x = 1;"),
            vec![
                Token::Let,
                Token::Identifier("x".to_string()),
                Token::Equal,
                Token::NumberDec("1".to_string()),
                Token::Semicolon,
            ]
        );
    }
}
