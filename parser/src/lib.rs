pub mod scanner;
pub mod token;

use crate::token::TokenType;
use ast::{
    blob_type::BlobType,
    expr::{Expr, ExprBinaryOp, ExprCall, ExprUnaryOp},
    op_type::{BinaryOpType, UnaryOpType},
    stmt::{Stmt, StmtAssign, StmtFuncDecl, StmtIf, StmtIfElse, StmtVarDecl, StmtWhile},
    FileCoords,
};
use std::{
    fmt::Display,
    fs::File,
    io::{BufReader, Read},
};

use self::{scanner::Scanner, token::Token};

const CHUNK_SIZE: usize = 4096;
const INTIAL_CAPACITY: usize = 1024;

type StmtResult = Result<Stmt, ParserError>;
type ExprResult = Result<Expr, ParserError>;
type TokenRefResult<'a> = Result<&'a Token, ParserError>;

#[derive(Debug)]
pub enum ParserStatus {
    Succeeded(Vec<Stmt>),
    Failed,
    IOError,
}

#[derive(Debug)]
pub enum ParserError {
    /// This error is used only when reading the chunk fails.
    IOError(String, FileCoords),
    WrongToken(String, Token),
    TypeError(String, Token),
    Scope(String, Token),
    EOF,
}

impl ParserError {
    fn print(&self) {
        match self {
            Self::WrongToken(msg, token) => {
                eprintln!(
                    "[ERROR] Line {} Col {}: {}. '{:?}'",
                    token.file_coords.line, token.file_coords.col, msg, token.token_type
                );
            }
            Self::TypeError(msg, token) => {
                eprintln!(
                    "[ERROR] Line{}: {}. '{:?}'",
                    token.file_coords.line, msg, token.token_type
                );
            }
            Self::Scope(msg, token) => {
                eprintln!("[ERROR] Line {}: {}", token.file_coords.line, msg);
            }
            Self::IOError(msg, token) => {
                eprintln!("FAIL: {}. Last token {:?}", msg, token);
            }
            Self::EOF => {
                eprintln!("[ERROR] Reached end of file in incomplete state",);
            }
        };
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ScopeType {
    Func,
    If,
    Else,
    While,
    Block,
}

#[derive(Debug, Clone)]
struct Scope {
    scope_type: ScopeType,
    line: usize,
}

impl Scope {
    fn new(scope_type: ScopeType, line: usize) -> Scope {
        Scope { scope_type, line }
    }
}

impl Display for Scope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} at line {}", self.scope_type, self.line)
    }
}

/// Inteded for use in `Parser`. It checks if the current token's `TokenType` matches the given branches.
/// If so, it advances to the next token and it executed the given expression. If not it goes to
/// the default branch and executes it wihtout advancing to the next token.
///
/// Usefull when a different action is needed for each token type and they all require to advance
/// to the next one.
///
/// Propagates error of type `ParserError`.
///
/// Example usage
/// ```
/// action_and_advance_by_token_type!(
///     self;
///     If => self.if_else_stmt(),
///     default => self.assignment_stmt()
/// )
/// ```
/// generates
/// ```
/// match self.peek_tokne()?.token_type {
///     TokenType::If => {
///         let _ = self.next_token()?;
///         self.if_else_stmt()
///     },
///     _ => self.assignment_stmt(),
/// }
/// ```
macro_rules! action_and_advance_by_token_type {
    ($self:ident; $($token_type:ident => $action:expr),+ ; default => $default:expr) => {
        match $self.peek_token()?.token_type {
            $(
                TokenType::$token_type => {
                    let _ = $self.next_token()?;
                    $action
                }
            )+
            _ => $default
        }
    }
}

/// Inteded for use in `Parser`. Pushes the given `ScopeType` to the stack and executes the action.
/// The action is a block which should return a `Stmt` object. Then it pops the scope from the stack.
///
/// Example
/// ```
/// stmt_scope!(self; While; {self.while_stmt()?})
/// ```
macro_rules! stmt_scope {
    ($self:ident; $scope_type:ident; $action:block) => {{
        $self
            .scopes
            .push(Scope::new(ScopeType::$scope_type, $self.get_line()));
        let stmt: Stmt = $action;
        $self.scopes.pop();
        stmt
    }};
}

/// Executes the given action if it is inside the scope of a function.
/// Returns a `ParserError` otherwise.
macro_rules! in_func {
    ($self:ident; $msg:literal; $action:expr) => {
        if $self
            .scopes
            .iter()
            .any(|scope| scope.scope_type == ScopeType::Func)
        {
            $action
        } else {
            return Err(ParserError::Scope(
                String::from($msg),
                $self.peek_token()?.clone(),
            ));
        }
    };
}

/// Executes the given action if it is NOT inside the scope of a function.
/// Returns a `ParserError` otherwise.
macro_rules! not_in_func {
    ($self:ident; $msg:literal; $action:expr) => {
        if !$self
            .scopes
            .iter()
            .any(|scope| scope.scope_type == ScopeType::Func)
        {
            $action
        } else {
            return Err(ParserError::Scope(
                String::from($msg),
                $self.peek_token()?.clone(),
            ));
        }
    };
}

/// Top down parser.
///
/// Parsing rules
/// ```
/// // Statement grammar
/// stmt -> func_decl_stmt / return_stmt / if_else_stmt / var_decl_stmt / while_stmt / block_stmt / assignment_stmt
/// func_decl_stmt -> "func" "(" (IDENTIFIER type_expr ("," IDENTIFIER type_expr)*)? ")" type_expr? block_stmt
/// return_stmt -> "return" expr ";"
/// if_else_stmt -> "if" "(" expr ")" block_stmt ("else" block_stmt | "else" if_else_stmt)?
/// var_decl_stmt -> "var" IDENTIFIER type_expr? "=" expr ";"
/// while_stmt -> "while" "(" expr ")" block_stmt
/// block_stmt -> "{" stmt* "}"
/// assignment_stmt -> ((IDENTIFIER "=")? expr | IDENTIFIER) ";"
///
/// // Expression grammar
/// expr -> logic_or
/// logic_or -> logic_and ("||" logic_and)?
/// logic_and -> comparison ("&&" comparison)?
/// comparison -> term (("<" | ">" | "<=" | ">=" | "==" | "!=") term)?
/// term -> factor (("+" | "-") factor)*
/// factor -> unary (("*" | "/") unary)*
/// unary -> ("!" | "-") unary | call
/// call -> primary | IDENTIFIER ("(" arguments? ")")?
/// arguments -> expr ("," expr )*
/// primary -> NUMBER | IDENTIFIER | "(" expression ")"
/// type_expr -> ":" IDENTIFIER | "i32"
/// ```
pub struct Parser {
    scanner: Scanner,
    reader: BufReader<File>,
    tokens: Vec<Token>,
    token_index: usize,
    scopes: Vec<Scope>,
}

impl Parser {
    pub fn new(file_path: &str) -> Result<Parser, std::io::Error> {
        Ok(Parser {
            scanner: Scanner::new(),
            reader: BufReader::new(File::open(file_path)?),
            tokens: Vec::with_capacity(INTIAL_CAPACITY),
            token_index: 0,
            scopes: Vec::with_capacity(INTIAL_CAPACITY),
        })
    }

    pub fn parse(&mut self) -> ParserStatus {
        let mut stmts: Vec<Stmt> = Vec::new();
        let mut errored = false;
        let mut io_error = false;

        loop {
            match self.peek_token() {
                Err(err) if matches!(&err, ParserError::IOError(_, _)) => {
                    err.print();
                    io_error = true;
                    break;
                }
                Ok(token) => {
                    if token.token_type == TokenType::Blank {
                        let _ = self.next_token();
                        continue;
                    }
                    if token.token_type == TokenType::EOF {
                        break;
                    }
                }
                Err(_) => unreachable!("Unexpected error type. This branch only expectes IOError"),
            }
            match self.stmt() {
                Ok(stmt) => stmts.push(stmt),
                Err(err) => {
                    errored = true;
                    err.print();
                    match err {
                        ParserError::EOF => {
                            self.scopes.iter().for_each(|scope| {
                                eprintln!("\tDid not end scope of {}", scope);
                            });
                        }
                        _ => {}
                    }
                    self.scopes.clear();

                    match self.sync() {
                        Err(err) if matches!(&err, ParserError::IOError(_, _)) => {
                            err.print();
                            io_error = true;
                            break;
                        }
                        _ => {}
                    };
                }
            };
        }

        if io_error {
            return ParserStatus::IOError;
        }
        match errored {
            true => ParserStatus::Failed,
            false => ParserStatus::Succeeded(stmts),
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    /// Statement rules
    ///////////////////////////////////////////////////////////////////////////

    fn stmt(&mut self) -> StmtResult {
        Ok(action_and_advance_by_token_type!(
            self;
            Func => not_in_func!(
                self;
                "Cannot declare a function inside another function";
                stmt_scope!(self; Func; {self.func_decl_stmt()?})
            ),
            Return => in_func!(
                self;
                "Return statement must be inside a function";
                self.return_stmt()?
            ),
            If => in_func!(
                self;
                "If statement must be inside a function";
                self.if_else_stmt()?
            ),
            Var => in_func!(
                self;
                "Var statement must be inside a function";
                self.var_decl_stmt()?
            ),
            While => in_func!(
                self;
                "While statement must be inside a function";
                stmt_scope!(self; While; {self.while_stmt()?})
            ),
            LeftBrace => in_func!(
                self;
                "Block statement must be inside a function";
                stmt_scope!(self; Block; {self.block_stmt()?})
            );
            default => in_func!(
                self;
                "Statement must be inside a function";
                self.assignment_stmt()?
            )
        ))
    }

    fn func_decl_stmt(&mut self) -> StmtResult {
        let line = self.get_line();
        let func_name = self.consume_identifier_and_get_lexeme("Expected function name")?;

        self.consume(
            TokenType::LeftParen,
            "Expected opening '(' for function argument declarations",
        )?;
        let mut args = Vec::new();
        let mut first_pass = true;
        while self.peek_token()?.token_type != TokenType::RightParen {
            if !first_pass {
                self.consume(
                    TokenType::Comma,
                    "Expecting ',' to seperate function arguments",
                )?;
            }
            let arg_name = self.consume_identifier_and_get_lexeme("Expected argument name")?;
            let arg_type = self.type_expr(
                &format!(
                    "Expected variable type for function argument '{}'",
                    arg_name
                ),
                true,
            )?;
            args.push((arg_name, arg_type.unwrap()));
            first_pass = false;
        }
        self.consume(
            TokenType::RightParen,
            "Expected closing ')' for function argument declrations",
        )?;
        let return_type: Option<BlobType> = self.type_expr(
            &format!("Expected return type for function '{}'", func_name),
            false,
        )?;

        self.consume(
            TokenType::LeftBrace,
            "Expected opening '{' for function body",
        )?;
        let func_body = self.block_stmt()?;

        if func_name == "main" {
            return Ok(Stmt::MainDecl(StmtFuncDecl::new(
                func_name,
                args,
                return_type,
                Box::new(func_body),
                line,
            )));
        }
        Ok(Stmt::FuncDecl(StmtFuncDecl::new(
            func_name,
            args,
            return_type,
            Box::new(func_body),
            line,
        )))
    }

    fn return_stmt(&mut self) -> StmtResult {
        let expr = self.expr()?;
        self.consume(TokenType::Semicolon, "Expected ';' to end return statement")?;
        Ok(Stmt::Return(expr))
    }

    fn if_else_stmt(&mut self) -> StmtResult {
        let line = self.get_line();
        let if_stmt = stmt_scope!(self; If; {
            self.consume(
                TokenType::LeftParen,
                "Expected opening '(' for if condition",
                )?;
            if self.peek_token()?.token_type == TokenType::RightParen {
                return Err(ParserError::WrongToken(
                        String::from("Expected opening '(' for if condition"),
                        self.next_token()?.clone(),
                        ));
            }
            let condition = self.expr()?;
            self.consume(
                TokenType::RightParen,
                "Expected closing ')' for if conditon",
                )?;

            self.consume(TokenType::LeftBrace, "Expected opening '{' for if body")?;
            let if_clause = self.block_stmt()?;

            Stmt::If(StmtIf::new(condition, Box::new(if_clause), line))
        });

        // Check for `else` and `else if` chains
        if self.peek_token()?.token_type == TokenType::Else {
            self.next_token()?;
            let else_clause = action_and_advance_by_token_type!(
                self;
                If => self.if_else_stmt(),
                LeftBrace => {
                    Ok(stmt_scope!(self; Else; {
                        self.block_stmt()?
                    }))
                };
                default => Err(ParserError::WrongToken(
                    String::from("Expected another 'if' statement or opening '{' for else body"),
                    self.peek_token()?.clone(),
                ))
            )?;
            match if_stmt {
                Stmt::If(stmt_if) => {
                    return Ok(Stmt::IfElse(StmtIfElse::new(
                        stmt_if.condition,
                        Box::new(*stmt_if.clause),
                        Box::new(else_clause),
                        line,
                    )))
                }
                _ => unreachable!("How the heck did it make the if statement into something else?"),
            }
        }

        Ok(if_stmt)
    }

    fn var_decl_stmt(&mut self) -> StmtResult {
        let line = self.get_line();
        let var_name = self.consume_identifier_and_get_lexeme("Expected variable name")?;
        let var_type =
            self.type_expr(&format!("Expected variable type for '{}'", var_name), false)?;

        self.consume(
            TokenType::Equal,
            "Must give an intial value for var declaration",
        )?;

        let expr = self.expr()?;
        self.consume(TokenType::Semicolon, "Expected ';'")?;

        Ok(Stmt::VarDecl(StmtVarDecl::new(
            var_name.clone(),
            var_type,
            expr,
            line,
        )))
    }

    fn while_stmt(&mut self) -> StmtResult {
        let line = self.get_line();
        self.consume(
            TokenType::LeftParen,
            "Expected opening '(' for while condition",
        )?;
        if self.peek_token()?.token_type == TokenType::RightParen {
            return Err(ParserError::WrongToken(
                String::from("Expected while condition"),
                self.next_token()?.clone(),
            ));
        }
        let condition = self.expr()?;
        self.consume(
            TokenType::RightParen,
            "Expected closing')' for while conditon",
        )?;
        self.consume(TokenType::LeftBrace, "Expected opening '{' for while body")?;
        let block = self.block_stmt()?;

        Ok(Stmt::While(StmtWhile::new(
            condition,
            Box::new(block),
            line,
        )))
    }

    /// Consume a "{" befor calling this function
    fn block_stmt(&mut self) -> StmtResult {
        let mut block_stmts = Vec::new();
        while self.peek_token()?.token_type != TokenType::RightBrace {
            block_stmts.push(self.stmt()?);
        }
        self.consume(TokenType::RightBrace, "Expected closing'}' for block")?;
        Ok(Stmt::Block(block_stmts))
    }

    fn assignment_stmt(&mut self) -> StmtResult {
        match self.expr()? {
            Expr::Identifier(ident) => {
                let line = self.get_line();
                if self.peek_token()?.token_type == TokenType::Equal {
                    self.consume(TokenType::Equal, "Expected '='")?;
                    let to_expr = self.expr()?;
                    self.consume(TokenType::Semicolon, "Expected ';' after expression")?;
                    return Ok(Stmt::Assign(StmtAssign::new(ident, to_expr, line)));
                }
                // TODO print warning here for unimpactfull instruction
                self.consume(TokenType::Semicolon, "Expected ';' after expression")?;
                Ok(Stmt::ExprStmt(Expr::Identifier(ident)))
            }
            other => {
                // TODO add checks for unimpactfull instructions
                self.consume(TokenType::Semicolon, "Expected ';' after expression")?;
                Ok(Stmt::ExprStmt(other))
            }
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    /// Expression rules
    ///////////////////////////////////////////////////////////////////////////

    fn expr(&mut self) -> ExprResult {
        self.logic_or()
    }

    fn logic_or(&mut self) -> ExprResult {
        let possible_left_term = self.logic_and()?;
        let line = self.get_line();
        if self.match_token(TokenType::PipePipe)? {
            let bin_op_type = BinaryOpType::Or;
            let right_term = self.logic_and()?;
            return Ok(Expr::BinaryOp(ExprBinaryOp::new(
                Box::new(possible_left_term),
                bin_op_type,
                Box::new(right_term),
                line,
            )));
        }
        Ok(possible_left_term)
    }

    fn logic_and(&mut self) -> ExprResult {
        let possible_left_term = self.comparison()?;
        let line = self.get_line();
        if self.match_token(TokenType::AmpersandAmpersand)? {
            let bin_op_type = BinaryOpType::And;
            let right_term = self.comparison()?;
            return Ok(Expr::BinaryOp(ExprBinaryOp::new(
                Box::new(possible_left_term),
                bin_op_type,
                Box::new(right_term),
                line,
            )));
        }
        Ok(possible_left_term)
    }

    fn comparison(&mut self) -> ExprResult {
        let possible_left_term = self.term()?;
        let line = self.get_line();
        if let Some(token_type) = self.match_tokens(vec![
            TokenType::Less,
            TokenType::Greater,
            TokenType::LessEqual,
            TokenType::GreaterEqual,
            TokenType::EqualEqual,
            TokenType::BangEqual,
        ])? {
            let bin_op_type = token_type.get_bin_op_type();
            let right_term = self.term()?;
            return Ok(Expr::BinaryOp(ExprBinaryOp::new(
                Box::new(possible_left_term),
                bin_op_type,
                Box::new(right_term),
                line,
            )));
        }
        Ok(possible_left_term)
    }

    fn term(&mut self) -> ExprResult {
        let mut expr = self.factor()?;
        while let Some(token_type) = self.match_tokens(vec![TokenType::Plus, TokenType::Minus])? {
            let line = self.get_line();
            let bin_op_type = token_type.get_bin_op_type();
            let right_term = self.factor()?;
            expr = Expr::BinaryOp(ExprBinaryOp::new(
                Box::new(expr),
                bin_op_type,
                Box::new(right_term),
                line,
            ));
        }
        Ok(expr)
    }

    fn factor(&mut self) -> ExprResult {
        let mut expr = self.unary()?;
        while let Some(token_type) = self.match_tokens(vec![TokenType::Star, TokenType::Slash])? {
            let line = self.get_line();
            let bin_op_type = token_type.get_bin_op_type();
            let right_term = self.unary()?;
            expr = Expr::BinaryOp(ExprBinaryOp::new(
                Box::new(expr),
                bin_op_type,
                Box::new(right_term),
                line,
            ));
        }
        Ok(expr)
    }

    fn unary(&mut self) -> ExprResult {
        action_and_advance_by_token_type!(
            self;
            Bang => {
                let line = self.get_line();
                let term = self.unary()?;
                Ok(Expr::UnaryOp(ExprUnaryOp::new(UnaryOpType::Not, Box::new(term), line)))
            },
            Minus => {
                let line = self.get_line();
                let term = self.unary()?;
                Ok(Expr::UnaryOp(ExprUnaryOp::new(UnaryOpType::Negate, Box::new(term), line)))
            };
            default => self.call()
        )
    }

    fn call(&mut self) -> ExprResult {
        let possible_callee = self.primary()?;
        if self.peek_token()?.token_type == TokenType::LeftParen {
            if let Expr::Identifier(ident) = possible_callee {
                self.consume(TokenType::LeftParen, "Expected opening '('")?;
                let args = self.arguments()?;
                return Ok(Expr::Call(ExprCall::new(ident, args, self.get_line())));
            }
            return Err(ParserError::WrongToken(
                String::from("Call expression requires an identifier"),
                self.peek_token()?.clone(),
            ));
        }
        Ok(possible_callee)
    }

    fn arguments(&mut self) -> Result<Vec<Expr>, ParserError> {
        let mut args = Vec::new();
        let mut first_pass = true;
        while self.peek_token()?.token_type != TokenType::RightParen {
            if !first_pass {
                self.consume(TokenType::Comma, "Expecting ',' to seperate call arguments")?;
            }
            args.push(self.expr()?);
            first_pass = false;
        }
        self.consume(TokenType::RightParen, "Expected closing ')'")?;
        Ok(args)
    }

    fn primary(&mut self) -> ExprResult {
        let token = self.next_token()?;
        match token.token_type {
            TokenType::Number => Ok(Expr::Number(token.lexeme.as_ref().unwrap().clone())),
            TokenType::Identifier => Ok(Expr::Identifier(token.lexeme.as_ref().unwrap().clone())),
            TokenType::LeftParen => {
                let expr = self.expr()?;
                self.consume(TokenType::RightParen, "Missing closing ')'")?;
                Ok(expr)
            }
            TokenType::EOF => Err(ParserError::EOF),
            _ => Err(ParserError::WrongToken(
                String::from("Can't start statement or expression with this token"),
                token.clone(),
            )),
        }
    }

    ///
    /// Checks if next token is ':'. If so it consumes it and consumes the next token as a BlobType
    /// If `required` is true it will expect to have a type and will return an `Err(ParserError)` if
    /// no type is found.
    /// ```
    /// type_expr -> ":" IDENTIFIER | "i32"
    /// ```
    fn type_expr(
        &mut self,
        err_msg: &str,
        required: bool,
    ) -> Result<Option<BlobType>, ParserError> {
        if self.match_token(TokenType::Colon)? {
            return match self.peek_token()?.token_type {
                TokenType::I32 => {
                    let _ = self.next_token()?;
                    Ok(Some(BlobType::I32))
                }
                TokenType::Identifier => Ok(Some(BlobType::Custom(
                    self.next_token()?.lexeme.as_ref().unwrap().clone(),
                ))),
                _ => Err(ParserError::TypeError(
                    String::from(err_msg),
                    self.peek_token()?.clone(),
                )),
            };
        }
        if required {
            return Err(ParserError::TypeError(
                String::from(err_msg),
                self.peek_token()?.clone(),
            ));
        }
        Ok(None)
    }

    ///////////////////////////////////////////////////////////////////////////
    /// Parer helper methods
    ///////////////////////////////////////////////////////////////////////////

    /// Iterates to the next token and returns an error if it is not of the given type
    fn consume(&mut self, token_type: TokenType, msg: &str) -> TokenRefResult {
        let token = self.next_token()?;
        if token.token_type != token_type {
            return Err(ParserError::WrongToken(String::from(msg), token.clone()));
        }
        Ok(token)
    }

    fn consume_identifier_and_get_lexeme(&mut self, msg: &str) -> Result<String, ParserError> {
        Ok(String::from(
            self.consume(TokenType::Identifier, msg)?
                .lexeme
                .as_ref()
                .unwrap(),
        ))
    }

    /// Iterates to the next token if token is of given type.
    fn match_token(&mut self, token_type: TokenType) -> Result<bool, ParserError> {
        if self.peek_token()?.token_type == token_type {
            let _ = self.next_token()?;
            return Ok(true);
        }
        Ok(false)
    }

    /// Iterates to the next token if token is in the given types.
    /// Returns an `Option<TokenType>` for the found token and iterates
    /// to the next one if a match is found.
    fn match_tokens(
        &mut self,
        token_types: Vec<TokenType>,
    ) -> Result<Option<TokenType>, ParserError> {
        let peek_token_type = self.peek_token()?.token_type.clone();
        for token_type in token_types {
            if peek_token_type == token_type {
                let _ = self.next_token()?;
                return Ok(Some(token_type));
            }
        }
        Ok(None)
    }

    /// Syncronize to a stable state after a parsing error
    fn sync(&mut self) -> Result<(), ParserError> {
        loop {
            match self.peek_token()?.token_type {
                TokenType::Semicolon | TokenType::RightBrace => {
                    self.next_token()?;
                    break;
                }
                TokenType::EOF => break,
                _ => self.next_token()?,
            };
        }
        Ok(())
    }

    /// Returns the next token in the iteration, does not iterate to the next one
    fn peek_token(&mut self) -> TokenRefResult {
        if self.token_index >= self.tokens.len() {
            return self.read_next_chunk(false);
        }

        Ok(self.tokens.get(self.token_index).unwrap())
    }

    /// Returns the next token in the iteration, iterate to the next one
    fn next_token(&mut self) -> TokenRefResult {
        if self.token_index >= self.tokens.len() {
            return self.read_next_chunk(true);
        }
        let token = self.tokens.get(self.token_index).unwrap();
        self.token_index += 1;
        Ok(token)
    }

    fn read_next_chunk(&mut self, iterate_to_next: bool) -> TokenRefResult {
        let mut chunk: Vec<u8> = vec![0; CHUNK_SIZE];
        return match self.reader.read(&mut chunk) {
            Ok(bytes_read) => {
                if bytes_read == 0 {
                    self.tokens.push(Token::eof(self.scanner.get_coords()));
                    return Ok(self.tokens.last().unwrap());
                }

                self.tokens = self
                    .scanner
                    .scan(&String::from_utf8_lossy(&chunk[..bytes_read]));
                if iterate_to_next {
                    self.token_index = 1;
                } else {
                    self.token_index = 0;
                }
                if self.tokens.is_empty() {
                    self.tokens.push(Token::blank(self.scanner.get_coords()));
                }
                return Ok(self.tokens.get(0).unwrap());
            }
            Err(_) => Err(ParserError::IOError(
                String::from("Failed to read next chunk from line"),
                self.tokens
                    .last()
                    .unwrap_or(&Token::eof(self.scanner.get_coords()))
                    .file_coords
                    .clone(),
            )),
        };
    }

    fn get_line(&self) -> usize {
        self.tokens
            .get(self.token_index - 1)
            .unwrap_or_else(|| self.tokens.get(self.token_index).unwrap())
            .file_coords
            .line
    }
}
