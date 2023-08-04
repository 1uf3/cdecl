use std::collections::HashMap;

use crate::lexer::TokenKind;

#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Identifier(String),
    Qualifier(String),
    Type(String),
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

    pub fn parse(&mut self) -> Result<Vec<Token>, ParserError> {
        let mut result: Vec<Token> = vec![];
        loop {
            let kind = self.peek_expect()?.clone();
            let token = match kind {
                TokenKind::String(s) => {
                    let token = classify_string(&s);
                    self.next_expect()?;
                    let kind = self.peek_expect()?;
                    if token == Token::Identifier(s) {
                        if *kind == TokenKind::OpenParen {
                            result.push(token);
                            self.parse_function()
                        } else {
                            Ok(token)
                        }
                    } else {
                        Ok(token)
                    }
                }
                TokenKind::LeftBracket => {
                    self.next_expect()?;
                    self.parse_array()
                }
                TokenKind::Star => {
                    self.next_expect()?;
                    Ok(Token::Pointer)
                }
                TokenKind::Semi => {
                    self.next_expect()?;
                    result.push(Token::Semi);
                    break;
                }
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

fn classify_string(s: &str) -> Token {
    match s {
        "volatile" => return Token::Qualifier("volatile".into()),
        "const" => return Token::Qualifier("const".into()),

        "void" => return Token::Type("void".into()),
        "char" => return Token::Type("char".into()),
        "signed" => return Token::Type("signed".into()),
        "unsigned" => return Token::Type("unsinged".into()),
        "short" => return Token::Type("short".into()),
        "int" => return Token::Type("int".into()),
        "long" => return Token::Type("long".into()),
        "float" => return Token::Type("float".into()),
        "double" => return Token::Type("double".into()),
        "struct" => return Token::Type("struct".into()),
        "union" => return Token::Type("union".into()),
        "enum" => return Token::Type("enum".into()),

        _ => (),
    }
    return Token::Identifier(s.to_string());
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use crate::{lexer::Lexer, tokenizer::Token};

    use super::Parser;

    #[test]
    fn test_parse_empty_function() {
        let json = "a();";
        let tokens = Parser::new(Lexer::new(json).tokenize().unwrap())
            .parse()
            .unwrap();
        let result: Vec<Token> = vec![
            Token::Identifier("a".into()),
            Token::Function(None),
            Token::Semi,
        ];
        tokens
            .iter()
            .zip(result.iter())
            .enumerate()
            .for_each(|(i, (x, y))| {
                assert_eq!(x, y, "index: {}", i);
            });
    }

    #[test]
    fn test_parse_function() {
        let json = "int main(int argc, char** argv);";
        let tokens = Parser::new(Lexer::new(json).tokenize().unwrap())
            .parse()
            .unwrap();
        let mut args = HashMap::new();
        let a: Vec<Token> = vec![Token::Type("int".into())];
        let b: Vec<Token> = vec![Token::Type("char".into()), Token::Pointer, Token::Pointer];
        args.insert(Some("argc".into()), a);
        args.insert(Some("argv".into()), b);
        let result: Vec<Token> = vec![
            Token::Type("int".into()),
            Token::Identifier("main".into()),
            Token::Function(Some(args)),
            Token::Semi,
        ];
        tokens
            .iter()
            .zip(result.iter())
            .enumerate()
            .for_each(|(i, (x, y))| {
                assert_eq!(x, y, "index: {}", i);
            });
    }

    #[test]
    fn test_tokenize_complex_struct_function() {
        let obj = "struct a **c[10](int **p);";
        let tokens = Parser::new(Lexer::new(obj).tokenize().unwrap())
            .parse()
            .unwrap();
        let mut args = HashMap::new();
        let a: Vec<Token> = vec![Token::Type("int".into()), Token::Pointer, Token::Pointer];
        args.insert(Some("p".into()), a);
        let result = [
            Token::Type("struct".into()),
            Token::Identifier("a".into()),
            Token::Pointer,
            Token::Pointer,
            Token::Identifier("c".into()),
            Token::Array(10i64),
            Token::Function(Some(args)),
            Token::Semi,
        ];
        tokens
            .iter()
            .zip(result.iter())
            .enumerate()
            .for_each(|(i, (x, y))| {
                assert_eq!(x, y, "index: {}", i);
            });
    }

    #[test]
    fn test_parse_empty_array() {
        let empty_array = "[];";
        let tokens = Parser::new(Lexer::new(empty_array).tokenize().unwrap())
            .parse()
            .unwrap();
        let result = vec![Token::Array(i64::MAX), Token::Semi];
        tokens
            .iter()
            .zip(result.iter())
            .enumerate()
            .for_each(|(i, (x, y))| {
                assert_eq!(x, y, "index: {}", i);
            });
    }

    #[test]
    fn test_parse_array() {
        let array = "a[100];";
        let tokens = Parser::new(Lexer::new(array).tokenize().unwrap())
            .parse()
            .unwrap();
        let result: Vec<Token> = vec![
            Token::Identifier("a".into()),
            Token::Array(100i64),
            Token::Semi,
        ];
        tokens
            .iter()
            .zip(result.iter())
            .enumerate()
            .for_each(|(i, (x, y))| {
                assert_eq!(x, y, "index: {}", i);
            });
    }

    #[test]
    fn test_tokenize_complex_char_pointer() {
        let obj = "char * const **next;";
        let tokens = Parser::new(Lexer::new(obj).tokenize().unwrap())
            .parse()
            .unwrap();
        let result_tokens = [
            Token::Type("char".into()),
            Token::Pointer,
            Token::Qualifier("const".into()),
            Token::Pointer,
            Token::Pointer,
            Token::Identifier("next".into()),
            Token::Semi,
        ];
        tokens
            .iter()
            .zip(result_tokens.iter())
            .enumerate()
            .for_each(|(i, (x, y))| {
                assert_eq!(x, y, "index: {}", i);
            });
    }
}
