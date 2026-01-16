use crate::{common::CodeLocation, tokens::{Keyword, Operator, Token, TokenDetail}};


pub struct LexError {
    pub loc: CodeLocation,
    pub message: String,
}

pub fn extract_tokens(source: String) -> Result<Vec<Token>, LexError> {
    let mut tokens: Vec<Token> = vec![];

    let mut iter = source.chars().enumerate().peekable();
    
    loop {
        match iter.peek() {
            None => {
                let loc = match tokens.last() {
                    Some(t) => CodeLocation { start_index: t.loc.end_index, end_index: t.loc.end_index },
                    None => CodeLocation{start_index:0, end_index:0},
                };
                tokens.push(Token { loc, t: TokenDetail::EOF, text: "".to_string() });
                return Ok(tokens);

            }
            Some((_i, _char)) => {
                let i = _i.clone();
                let char = _char.clone();
                match char {
                    // whitespace
                    ' ' | '\t' | '\r' | '\n' => {
                        iter.next();
                    },

                    // comments and divide operators
                    '/' => {
                        iter.next();
                        if iter.peek().is_some_and(|(_, char2)| *char2 == '/') {
                            // comment
                            let mut text = "//".to_string();
                            let mut j = i + 1;
                            iter.next();
                            while match iter.peek() {
                                None | Some((_, '\n')) => false,
                                Some((_j, char3)) => {
                                    j = *_j;
                                    text += &char3.to_string();
                                    true
                                }
                            } {iter.next();}
                            tokens.push(Token { loc: CodeLocation { start_index: i, end_index: j+1 }, t: TokenDetail::Comment(text.clone()), text });

                        } else {
                            // divide
                            tokens.push(Token { loc: CodeLocation { start_index: i, end_index: i+1 }, t: TokenDetail::Operator(Operator::ForwardSlash), text: char.to_string() });
                        }
                    }

                    // keywords and identifiers
                    'a'..='z' | 'A'..='Z' => {
                        let mut text = char.to_string();
                        let mut j = i + 1;
                        iter.next();
                        while match iter.peek() {
                            Some((_j, char2 )) if matches!(char2, 'a'..='z' | 'A'..='Z' | '0'..='9') => {
                                j = *_j;
                                text += &char2.to_string();
                                true
                            },
                            _ => false   
                        } {iter.next();}
                        let detail = match text.as_str() {
                            "main" => TokenDetail::Keyword(Keyword::Main),
                            "const" => TokenDetail::Keyword(Keyword::Const),
                            "let" => TokenDetail::Keyword(Keyword::Let),
                            "byte" => TokenDetail::Keyword(Keyword::Byte),
                            "void" => TokenDetail::Keyword(Keyword::Void),
                            "forever" => TokenDetail::Keyword(Keyword::Forever),
                            "while" => TokenDetail::Keyword(Keyword::While),
                            "do" => TokenDetail::Keyword(Keyword::Do),
                            "if" => TokenDetail::Keyword(Keyword::If),
                            "else" => TokenDetail::Keyword(Keyword::Else),
                            "flag" => TokenDetail::Keyword(Keyword::Flag),
                            "break" => TokenDetail::Keyword(Keyword::Break),
                            "continue" => TokenDetail::Keyword(Keyword::Continue),
                            ident => TokenDetail::Identifier(ident.to_string())
                        };
                        tokens.push(Token { loc: CodeLocation { start_index: i, end_index: j+1 }, t: detail, text });
                    },

                    // integer and binary literals
                    '0'..='9' => {
                        let mut text = char.to_string();
                        let mut j = i+1;
                        iter.next();
                        let num = match iter.peek() {
                            Some((_j, 'b')) if char == '0' => {
                                // bits in the form 0bxxxxxxxx
                                let mut num: u8 = 0;
                                j = *_j;
                                text += "b";
                                iter.next();
                                for k in (0..=7).rev() {
                                    match iter.peek() {
                                        Some((_j, '0')) => {
                                            j = *_j;
                                            text += "0";
                                        },
                                        Some((_j, '1')) => {
                                            j = *_j;
                                            text += "1";
                                            num += u8::pow(2, k);
                                        }
                                        _ => return Err(LexError { loc: CodeLocation { start_index: i, end_index: j+1 }, message: "Binary number must be 8 digits".to_string() })
                                    }
                                    iter.next();
                                }
                                num
                            }
                            Some((_j, char2)) if matches!(char2, '0'..='9') => {
                                // second char is a numerical digit - expect 0 or more digits to follow
                                j = *_j;
                                text += &char2.to_string();
                                iter.next();
                                while match iter.peek() {
                                    Some((_j, char3)) if matches!(char3, '0'..='9') => {
                                        j = *_j;
                                        text += &char3.to_string();
                                        true
                                    }
                                    _ => false
                                } {iter.next();}
                                match text.parse::<u8>() {
                                    Ok(num) => num,
                                    Err(_) => return Err(LexError { loc: CodeLocation { start_index: i, end_index: j+1 }, message: format!("Integer literal greater than 255") })
                                }
                            }
                            _ => {
                                // 1-digit number
                                match text.parse::<u8>() {
                                    Ok(num) => num,
                                    Err(_) => return Err(LexError { loc: CodeLocation { start_index: i, end_index: j+1 }, message: format!("Integer literal must not be greater than 255") })
                                }
                            }
                        };

                        tokens.push(Token { loc: CodeLocation { start_index: i, end_index: j+1 }, t: TokenDetail::Number(num), text })
  
                    }

                    // unambiguous 1-char tokens
                    ';' | '[' | ']' | '(' | ')' | '{' | '}' | ',' | ':' | '+' | '-' | '~' | '*' | '@' | '^' | '%' | '_' | '!' => {
                        iter.next();
                        let detail = match char {
                            ';' => TokenDetail::SemiColon, 
                            '[' => TokenDetail::LeftBracket,
                            ']' => TokenDetail::RightBracket,
                            '(' => TokenDetail::LeftParenthesis,
                            ')' => TokenDetail::RightParenthesis,
                            '{' => TokenDetail::LeftBrace,
                            '}' => TokenDetail::RightBrace,
                            ',' => TokenDetail::Comma,
                            ':' => TokenDetail::Colon,
                            '+' => TokenDetail::Operator(Operator::Plus),
                            '-' => TokenDetail::Operator(Operator::Minus),
                            '~' => TokenDetail::Operator(Operator::Tilde),
                            '*' => TokenDetail::Operator(Operator::Asterix),
                            '@' => TokenDetail::Operator(Operator::Arobase),
                            '^' => TokenDetail::Operator(Operator::Caret),
                            '%' => TokenDetail::Operator(Operator::Percent),
                            '_' => TokenDetail::Operator(Operator::Underscore),
                            '!' => TokenDetail::Operator(Operator::Exclaimation),
                            _ => panic!("unambiguous 1-char token missing from match statement")
                        };
                        tokens.push(Token { loc: CodeLocation { start_index: i, end_index: i+1 }, t: detail, text: char.to_string() });
                    },

                    // signed and unsigned inequalities
                    '<' | '>' => {
                        let mut text = String::new();
                        let mut j;

                        match iter.peek() {
                            Some((_j, '<' )) => {
                                j = *_j;
                                text += "<";
                                iter.next();
                                match iter.peek() {
                                    Some((_j, '<')) => {
                                        j = *_j;
                                        text += "<";
                                        iter.next();
                                    },
                                    _ => ()
                                }
                            }
                            Some((_j, '>' )) => {
                                j = *_j;
                                text += ">";
                                iter.next();
                                match iter.peek() {
                                    Some((_j, '>')) => {
                                        j = *_j;
                                        text += ">";
                                        iter.next();
                                    },
                                    _ => ()
                                }
                            },
                            _ => panic!()
                        }
                        match iter.peek() {
                            Some((_j, '=')) => {
                                j = *_j;
                                text += "=";
                                iter.next();
                            }
                            _ => ()
                        }

                        let detail = match text.as_str() {
                            "<" => TokenDetail::Operator(Operator::UnsignedLessThan),
                            "<<" => TokenDetail::Operator(Operator::SignedLessThan),
                            "<=" => TokenDetail::Operator(Operator::UnsignedLessThanOrEqualTo),
                            "<<=" => TokenDetail::Operator(Operator::SignedLessThanOrEqualTo),
                            ">" => TokenDetail::Operator(Operator::UnsignedGreaterThan),
                            ">>" => TokenDetail::Operator(Operator::SignedGreaterThan),
                            ">=" => TokenDetail::Operator(Operator::UnsignedGreaterThanOrEqualTo),
                            ">>=" => TokenDetail::Operator(Operator::SignedGreaterThanOrEqualTo),
                            _ => panic!()
                        };
                        tokens.push(Token { loc: CodeLocation { start_index: i, end_index: j+1 }, t: detail, text });
                    },

                    // different meaning if doubled
                    '=' | '&' | '|' => {
                        let mut j = i;
                        let mut text = char.to_string();
                        iter.next();
                        match iter.peek() {
                            Some((_j, char2)) if *char2 == char => {
                                j = *_j;
                                text += &char2.to_string();
                                iter.next();
                            },
                            _ => ()
                        }
                        let detail = match text.as_str() {
                            "=" => TokenDetail::Equals,
                            "==" => TokenDetail::Operator(Operator::Equality),
                            "&" => TokenDetail::Operator(Operator::Ampersand),
                            "&&" => TokenDetail::Operator(Operator::DoubleAmpersand),
                            "|" => TokenDetail::Operator(Operator::Bar),
                            "||" => TokenDetail::Operator(Operator::DoubleBar),
                            _ => panic!()
                        };
                        tokens.push(Token { loc: CodeLocation { start_index: i, end_index: j + text.len() }, t: detail, text })
                    }

                    // invalid character
                    _ => {
                        return Err(LexError { loc: CodeLocation { start_index: i, end_index: i+1 }, message: "Unexpected character".to_string() })
                    }
                }
            }
        }
    }
}