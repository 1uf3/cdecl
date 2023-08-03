#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    WhiteSpace,
    String(String),
    Number(i64),
    OpenParen,
    CloseParen,
    LeftBracket,
    RightBracket,
    Star,
    Comma,
    Semi,
}

pub struct Lexer<'a> {
    chars: std::iter::Peekable<std::str::Chars<'a>>,
}

#[derive(Debug)]
pub struct LexerError {
    pub msg: String,
}

impl LexerError {
    fn new(msg: &str) -> LexerError {
        LexerError {
            msg: msg.to_string(),
        }
    }
}

impl<'a> Lexer<'a> {
    pub fn new(input: &str) -> Lexer {
        Lexer {
            chars: input.chars().peekable(),
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<TokenKind>, LexerError> {
        let mut tokens = vec![];
        while let Some(token) = self.next_token()? {
            match token {
                TokenKind::WhiteSpace => {}
                _ => {
                    tokens.push(token);
                }
            }
        }
        Ok(tokens)
    }

    fn next_return_token(&mut self, token: TokenKind) -> Option<TokenKind> {
        self.chars.next();
        Some(token)
    }

    fn next_token(&mut self) -> Result<Option<TokenKind>, LexerError> {
        match self.chars.peek() {
            Some(c) => match c {
                c if c.is_whitespace() || *c == '\n' => {
                    Ok(self.next_return_token(TokenKind::WhiteSpace))
                }
                '(' => Ok(self.next_return_token(TokenKind::OpenParen)),
                ')' => Ok(self.next_return_token(TokenKind::CloseParen)),
                '[' => Ok(self.next_return_token(TokenKind::LeftBracket)),
                ']' => Ok(self.next_return_token(TokenKind::RightBracket)),
                ',' => Ok(self.next_return_token(TokenKind::Comma)),
                ';' => Ok(self.next_return_token(TokenKind::Semi)),
                '*' => Ok(self.next_return_token(TokenKind::Star)),

                c if c.is_numeric() => self.parse_number_token(),
                c if c.is_alphanumeric() => self.parse_string_token(),
                _ => Err(LexerError::new(&format!("error: an unexpected char {}", c))),
            },
            None => Ok(None),
        }
    }

    fn parse_number_token(&mut self) -> Result<Option<TokenKind>, LexerError> {
        let mut number_str = String::new();
        while let Some(&c) = self.chars.peek() {
            if c.is_numeric() {
                self.chars.next();
                number_str.push(c);
            } else {
                break;
            }
        }

        match number_str.parse::<i64>() {
            Ok(number) => Ok(Some(TokenKind::Number(number))),
            Err(e) => Err(LexerError::new(&format!("error: {}", e))),
        }
    }

    fn parse_string_token(&mut self) -> Result<Option<TokenKind>, LexerError> {
        let mut identifier = String::new();
        while let Some(&c) = self.chars.peek() {
            if c.is_alphanumeric() {
                self.chars.next();
                identifier.push(c);
            } else {
                break;
            }
        }
        return Ok(Some(TokenKind::String(identifier)));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_number() {
        //integer
        let num = "1234567890";
        let tokens = Lexer::new(num).tokenize().unwrap();
        assert_eq!(tokens[0], TokenKind::Number(1234567890i64));
    }

    #[test]
    fn test_string() {
        let s = "char";
        let tokens = Lexer::new(s).tokenize().unwrap();
        assert_eq!(tokens[0], TokenKind::String("char".to_string()));
    }

    #[test]
    fn test_tokenize_default_main() {
        let obj = "int main()";
        let tokens = Lexer::new(obj).tokenize().unwrap();
        let result_tokens = [
            TokenKind::String("int".into()),
            TokenKind::String("main".into()),
            TokenKind::OpenParen,
            TokenKind::CloseParen,
        ];
        tokens
            .iter()
            .zip(result_tokens.iter())
            .enumerate()
            .for_each(|(i, (x, y))| {
                assert_eq!(x, y, "index: {}", i);
            });
    }

    #[test]
    fn test_tokenize_complex_char_pointer() {
        let obj = "char * const *(*next)();";
        let tokens = Lexer::new(obj).tokenize().unwrap();
        let result_tokens = [
            TokenKind::String("char".into()),
            TokenKind::Star,
            TokenKind::String("const".into()),
            TokenKind::Star,
            TokenKind::OpenParen,
            TokenKind::Star,
            TokenKind::String("next".into()),
            TokenKind::CloseParen,
            TokenKind::OpenParen,
            TokenKind::CloseParen,
            TokenKind::Semi,
        ];
        tokens
            .iter()
            .zip(result_tokens.iter())
            .enumerate()
            .for_each(|(i, (x, y))| {
                assert_eq!(x, y, "index: {}", i);
            });
    }

    #[test]
    fn test_tokenize_complex_struct() {
        let obj = "struct a *(*c[10])(int **p);";
        let tokens = Lexer::new(obj).tokenize().unwrap();
        let result_tokens = [
            TokenKind::String("struct".into()),
            TokenKind::String("a".into()),
            TokenKind::Star,
            TokenKind::OpenParen,
            TokenKind::Star,
            TokenKind::String("c".into()),
            TokenKind::LeftBracket,
            TokenKind::Number(10),
            TokenKind::RightBracket,
            TokenKind::CloseParen,
            TokenKind::OpenParen,
            TokenKind::String("int".into()),
            TokenKind::Star,
            TokenKind::Star,
            TokenKind::String("p".into()),
            TokenKind::CloseParen,
            TokenKind::Semi,
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
