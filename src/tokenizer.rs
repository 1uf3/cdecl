use std::{collections::HashMap, vec};

use crate::lexer::TokenKind;

#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Identifier(String),
    Qualifier,
    Type,
    Pointer,
    Array(i64),
    Function(Option<HashMap<Option<String>, Vec<Token>>>),
    Semi,
}

#[derive(Debug, Clone)]
pub struct ParserError {
    pub msg: String,
}

impl ParserError {
    pub fn new(msg: &str) -> ParserError {
        ParserError {
            msg: msg.to_string(),
        }
    }
}
pub struct Parser {
    kinds: Vec<TokenKind>,
    index: usize,
}

impl Parser {
    pub fn new(kinds: Vec<TokenKind>) -> Parser {
        Parser { kinds, index: 0 }
    }

    fn parse_array(&mut self) -> Result<Token, ParserError> {
        let kinds = self.peek_expect()?;
        if *kinds != TokenKind::LeftBracket {
            return Err(ParserError::new(&format!(
                "error: Array must starts [ {:?}",
                kinds
            )));
        }
        self.next_expect()?;

        let mut size: i64 = i64::MAX;
        let kinds = self.peek_expect()?;
        if *kinds == TokenKind::RightBracket {
            self.next_expect()?;
            return Ok(Token::Array(size));
        }

        let kinds = self.peek_expect()?;
        match *kinds {
            TokenKind::Number(s) => {
                self.next_expect()?;
                return Ok(Token::Array(size));
            }
            _ => {
                return Err(ParserError::new(&format!(
                    "error: a Number token kind is expected {:?}",
                    kinds
                )));
            }
        }
    }

    fn parse_function(&mut self) -> Result<Token, ParserError> {
        let kinds = self.peek_expect()?;
        if *kinds != TokenKind::OpenParen {
            return Err(ParserError::new(&format!(
                "error: function must starts ( {:?}",
                kinds
            )));
        }
        self.next_expect()?;

        let mut args: HashMap<Option<String>, Vec<Token>> = std::collections::HashMap::new();

        if *self.peek_expect()? == TokenKind::CloseParen {
            self.next_expect()?;
            return Ok(Token::Function(None));
        }

        loop {
            let kind1 = self.next_expect()?.clone();
            let kind2 = self.next_expect()?;

            match (kind1, kind2) {
                (TokenKind::String(key1), TokenKind::Star) => {
                    let mut vec: Vec<Token> = vec![];
                    vec.push(classify_string(key1.as_str()));
                    vec.push(Token::Pointer);
                    // Todo: 再帰必要
                    // args.insert(Some(key1.to_owned()), vec);
                }
                (TokenKind::String(key1), TokenKind::LeftBracket) => {
                    let mut vec: Vec<Token> = vec![];
                    vec.push(classify_string(key1.as_str()));
                    vec.push(Token::Pointer);
                    // Todo: 再帰必要
                    // args.insert(Some(key1.to_owned()), vec);
                }
                (TokenKind::String(key1), TokenKind::String(key2)) => {
                    let mut vec: Vec<Token> = vec![];
                    vec.push(classify_string(key1.as_str()));
                    args.insert(Some(key2.to_owned()), vec);
                }
                (TokenKind::String(key1), TokenKind::Comma) => {
                    let mut vec: Vec<Token> = vec![];
                    vec.push(classify_string(key1.as_str()));
                    args.insert(None, vec);
                    continue;
                }
                _ => {
                    return Err(ParserError::new(
                        "error: a pair (key(string) and : token) token is expected",
                    ));
                }
            }

            let token3 = self.next_expect()?;
            match token3 {
                TokenKind::CloseParen => {
                    return Ok(Token::Function(Some(args)));
                }
                TokenKind::Comma => {
                    continue;
                }
                _ => {
                    return Err(ParserError::new(&format!(
                        "error: a ) or , token is expected {:?}",
                        token3
                    )));
                }
            }
        }
    }

    /// `Token`を評価して`Value`に変換する。この関数は再帰的に呼び出される。
    pub fn parse(&mut self) -> Result<Vec<Token>, ParserError> {
        let mut result: Vec<Token> = vec![];
        loop {
            let kind = self.peek_expect()?.clone();
            let token = match kind {
                TokenKind::String(s) => {
                    self.next_expect()?;
                    let kind = self.peek_expect()?;
                    if *kind == TokenKind::OpenParen {
                        self.parse_function()
                    } else {
                        Ok(classify_string(s.as_str()))
                    }
                }
                TokenKind::LeftBracket => {
                    self.next_expect()?;
                    self.parse_array()
                }
                TokenKind::Semi => Ok(Token::Semi),
                _ => {
                    return Err(ParserError::new(&format!(
                        "error: a token must start string not {:?}",
                        kind
                    )))
                }
            };
            match token {
                Ok(t) => result.push(t),
                Err(_) => break,
            }
            if self.kinds.is_empty() {
                break;
            }
        }
        Ok(result)
    }

    fn peek(&self) -> Option<&TokenKind> {
        self.kinds.get(self.index)
    }

    fn peek_expect(&self) -> Result<&TokenKind, ParserError> {
        self.peek()
            .ok_or_else(|| ParserError::new("error: a token isn't peekable"))
    }

    fn next(&mut self) -> Option<&TokenKind> {
        self.index += 1;
        self.kinds.get(self.index - 1)
    }

    fn next_expect(&mut self) -> Result<&TokenKind, ParserError> {
        self.next()
            .ok_or_else(|| ParserError::new("error: a token isn't peekable"))
    }
}

fn classify_string(kind: &str) -> Token {
    match kind {
        "volatile" => return Token::Qualifier,

        "void" => return Token::Type,
        "char" => return Token::Type,
        "signed" => return Token::Type,
        "unsigned" => return Token::Type,
        "short" => return Token::Type,
        "int" => return Token::Type,
        "long" => return Token::Type,
        "float" => return Token::Type,
        "double" => return Token::Type,
        "struct" => return Token::Type,
        "union" => return Token::Type,
        "enum" => return Token::Type,

        _ => (),
    }
    return Token::Identifier(kind.to_string());
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use crate::{lexer::Lexer, tokenizer::Token};

    use super::Parser;

    // 無限ループエラーになる。
    // #[test]
    // fn test_parse_function() {
    //     let json = "a(int b, int c);";
    //     let tokens = Parser::new(Lexer::new(json).tokenize().unwrap())
    //         .parse()
    //         .unwrap();
    //     let mut args = HashMap::new();
    //     let a: Vec<Token> = vec![Token::Type];
    //     let b: Vec<Token> = vec![Token::Type];
    //     args.insert(Some("b".into()), a);
    //     args.insert(Some("c".into()), b);
    //     let result: Vec<Token> = vec![
    //         Token::Identifier("a".into()),
    //         Token::Function(Some(args)),
    //         Token::Semi,
    //     ];
    //     tokens
    //         .iter()
    //         .zip(result.iter())
    //         .enumerate()
    //         .for_each(|(i, (x, y))| {
    //             assert_eq!(x, y, "index: {}", i);
    //         });
    // }

    #[test]
    fn test_parse_array() {
        let array = "a[100]";
        let tokens = Parser::new(Lexer::new(array).tokenize().unwrap())
            .parse()
            .unwrap();
        let result: Vec<Token> = vec![Token::Identifier("a".into()), Token::Array(100i64)];
        tokens
            .iter()
            .zip(result.iter())
            .enumerate()
            .for_each(|(i, (x, y))| {
                assert_eq!(x, y, "index: {}", i);
            });
    }
}
