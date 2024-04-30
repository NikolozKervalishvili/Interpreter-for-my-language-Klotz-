use std::fmt;
use std::iter::{from_fn, once, Peekable};
use std::str::Chars;

pub type LexResult<T> = Result<T, LexError>;

#[derive(Debug)]
pub struct Lexer<'a> {
    // the txt input as an peekable iterator
    source: Peekable<Chars<'a>>,
    // raw character pos
    pos: usize,
    token_pos: usize,
    // brackets, curlies etc.
    bracket_stack: Vec<char>,
    line: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(f: &'a String) -> Self {
        Self {
            source: f.chars().peekable(),
            pos: 0,
            token_pos: 0,
            bracket_stack: Vec::new(),
            line: 0,
        }
    }

    /*
        -- method overloading --
        [self.source.next()] => self.next()
        [self.source.next_if()] => self.next_if()
        so [self.pos] and can be updated
    */
    fn next(&mut self) -> Option<char> {
        let n = self.source.next();
        if n.is_some() {
            self.pos += 1;
        }
        n
    }

    fn next_if(&mut self, f: impl FnOnce(&char) -> bool) -> Option<char> {
        let n = self.source.next_if(f);
        if n.is_some() {
            self.pos += 1;
        }
        n
    }
    // ----

    /*
        main method of the lexer
        the lexer creates the tokenstream for the
        next step, the parser
    */
    pub fn lex(&mut self) -> LexResult<Vec<Token>> {
        let mut tokens = Vec::new();
        // [self.next()] increases [self.pos]
        while let Some(c) = self.next() {
            let token = match c {
                '0'..='9' => self.take_number(c)?,
                // symbols which u can combine with '=' at the end
                '+' | '=' | '-' | '*' | '/' | '<' | '>' => self.take_operator(c),
                // symbols where u cant
                ';' | ':' | '\\' | '~' | '!' | '_' | '.' => Token::new(
                    TokenType::Operator,
                    c.to_string(),
                    self.token_pos,
                    self.line,
                ),
                // closings
                ')' | ']' | '}' => self.take_precedence(c)?,
                // opening
                '(' | '[' | '{' => {
                    self.bracket_stack.push(c);
                    Token::new(
                        TokenType::Precedence,
                        c.to_string(),
                        self.token_pos,
                        self.line,
                    )
                }
                ' ' => continue,
                /*
                    windows uses \r\n for a newline,
                    so \r and \n decrement [self.pos],
                    because a newline is not included in the raw pos
                */
                '\r' => {
                    self.pos -= 1;
                    continue;
                }
                '\n' => {
                    self.pos -= 1;
                    self.line += 1;
                    continue;
                }
                '"' | '\'' => self.take_string(c)?,
                c if c.is_alphabetic() => self.take_keyword(c),
                // the chars not implemented yet, e.g. &
                other => panic!("not yet implementet: {other}"),
            };
            self.token_pos += 1;
            tokens.push(token);
            // check for open brackets
        }
        // if the stack is not empty, an opening is unclosed
        if !self.bracket_stack.is_empty() {
            return Err(LexError::OpenBracketLeft(
                *self.bracket_stack.last().unwrap(),
            ));
        }
        Ok(tokens)
    }

    fn take_number(&mut self, c: char) -> LexResult<Token> {
        // consumes until non alphanumeric char (excluding dot for floats)
        let value = once(c)
            .chain(from_fn(|| {
                self.source.next_if(|c| c.is_alphanumeric() || *c == '.')
            }))
            .collect::<String>();
        // try to parse to float to check if value is a number
        match value.parse::<f64>() {
            Ok(num) => Ok(Token::new(
                if value.parse::<u32>().is_ok() {
                    TokenType::Int
                } else {
                    TokenType::Float
                },
                num.to_string(),
                self.token_pos,
                self.line,
            )),
            Err(_) => Err(LexError::InvalidNumber(value.to_string())),
        }
    }

    /*
        consumes until quotationmark is consumed again
        possible quotation marks: " and '
    */
    fn take_string(&mut self, c: char) -> LexResult<Token> {
        let value = from_fn(|| self.next_if(|&ch| ch != c)).collect();
        if self.next().is_none() {
            return Err(LexError::QuotationNotEnd(c, self.pos));
        }
        Ok(Token::new(
            TokenType::String,
            value,
            self.token_pos,
            self.line,
        ))
    }

    fn take_keyword(&mut self, c: char) -> Token {
        let value = once(c)
            .chain(from_fn(|| self.next_if(|&ch| ch.is_alphanumeric())))
            .collect::<String>();
        match value.as_ref() {
            // keywords implemented yet
            "if" | "else" | "let" => {
                Token::new(TokenType::Keyword, value, self.token_pos, self.line)
            }
            // if keyword not found it takes it as variable
            _ => Token::new(TokenType::Variable, value, self.token_pos, self.line),
        }
    }

    fn take_operator(&mut self, c: char) -> Token {
        Token::new(
            TokenType::Operator,
            once(c).chain(self.next_if(|&ch| ch == '=')).collect(),
            self.token_pos,
            self.line,
        )
    }

    /*
        match last openening from stack [self.bracket_stack]
        with the top of the stack, if matching: pop the top of the stack
    */
    fn take_precedence(&mut self, c: char) -> LexResult<Token> {
        if self.bracket_stack.last()
            != match c {
                '}' => Some(&'{'),
                ']' => Some(&'['),
                ')' => Some(&'('),
                _ => unreachable!(),
            }
        {
            return Err(LexError::PrecedenceNotMatch(c, self.pos));
        }
        self.bracket_stack.pop();
        Ok(Token::new(
            TokenType::Precedence,
            c.to_string(),
            self.token_pos,
            self.line,
        ))
    }
}

#[derive(Debug)]
pub struct Token {
    pub _type: TokenType,
    value: String, // content of the token, e.g. "12"
    // token pos from the lexer struct
    pos: usize,
    line: usize,
}

impl Token {
    fn new(_type: TokenType, value: String, pos: usize, line: usize) -> Self {
        Self {
            _type,
            value,
            pos,
            line,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum TokenType {
    Int,
    Operator,
    String,
    Keyword,
    Variable,
    // () {} [], not only precedence
    Precedence,
    Float,
}

pub enum LexError {
    InvalidNumber(String),
    QuotationNotEnd(char, usize),
    PrecedenceNotMatch(char, usize),
    OpenBracketLeft(char),
}

impl fmt::Debug for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            /*
                marks all non digit chars with a star,
                exception: the first dot
                TODO: make this more compact
            */
            LexError::InvalidNumber(s) => {
                // iterator which only consumes first item (first dot)
                let mut first_dot_pos = s
                    .chars()
                    .enumerate()
                    .filter(|(_, c)| *c == '.')
                    .map(|t| t.0)
                    .take(1);
                writeln!(f, "The stars mark the wrong chars: ")?;
                writeln!(f, "{}", s)?;
                write!(
                    f,
                    "{}",
                    s.chars()
                        .enumerate()
                        .map(|(i, c)| {
                            if c.is_alphabetic() || c == '.' && Some(i) != first_dot_pos.next() {
                                '*'
                            } else {
                                ' '
                            }
                        })
                        .collect::<String>()
                )?;
            }
            LexError::OpenBracketLeft(c) => {
                write!(f, "Unclosed Opening: {c}")?;
            }
            LexError::QuotationNotEnd(c, pos) => {
                write!(f, "This Quotation: [{c}] on pos: [{pos}] is unclosed")?;
            }
            LexError::PrecedenceNotMatch(c, pos) => {
                write!(f, "This Opening: [{c}] on pos: [{pos}] is doesnt match")?;
            }
        }
        Ok(())
    }
}
