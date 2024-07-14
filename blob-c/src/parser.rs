use std::{collections::HashMap, fmt::Display};

use blob_common::{error, file_coords::FileCoords, info};

use crate::{
    ast::{
        btype::BType,
        expr::{
            Expr, ExprBinaryOp, ExprBool, ExprCall, ExprGetProperty, ExprI32, ExprIdenifier,
            ExprString, ExprStructInstance, ExprUnaryOp,
        },
        op::BinaryOp,
        stmt::{
            Stmt, StmtAssign, StmtBlock, StmtExpr, StmtFuncDecl, StmtIf, StmtIfElse, StmtReturn,
            StmtStructDecl, StmtVarDecl, StmtWhile, VarTypeInfo,
        },
        Ast,
    },
    token::{Token, TokenType},
};

#[derive(Debug, Clone, PartialEq, Eq)]
enum ParserError {
    InvalidScope(String, FileCoords),
    WrongToken(String, FileCoords),
    EOF,
}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::WrongToken(msg, file_coords) | Self::InvalidScope(msg, file_coords) => {
                write!(f, "{}:{} {}", file_coords.line, file_coords.col, msg)
            }
            Self::EOF => write!(f, "Reached end of file before completing statement"),
        }
    }
}

type StmtResult = Result<Stmt, ParserError>;
type ExprResult = Result<Expr, ParserError>;
type TokenRefResult<'a> = Result<&'a Token, ParserError>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Scope {
    Func,
}

/// Top down parser.
///
/// Parsing rules
/// ```ignore
/// // Statement grammar
/// stmt -> stmt_func_decl / stmt_return / stmt_if_else / stmt_var_decl / stmt_while / stmt_block / stmt_assignment
/// stmt_func_decl -> "func" IDENTIFIER "(" (IDENTIFIER type_expr ("," IDENTIFIER type_expr)*)? ")" type_expr? block_stmt
/// stmt_return -> "return" expr ";"
/// stmt_struct_decl -> "struct" IDENTIFIER "{" (IDENTIFIER type_expr ("," IDENTIFIER type_expr)*)? "}"
/// stmt_if_else -> "if" "(" expr ")" block_stmt ("else" block_stmt | "else" if_else_stmt)?
/// stmt_var_decl -> "var" IDENTIFIER expr_type? "=" expr ";"
/// stmt_while -> "while" "(" expr ")" block_stmt
/// stmt_block -> "{" stmt* "}"
/// stmt_assignment -> (IDENTIFIER ("." IDENTIFIER)? "=")? expr ";"
///
/// // Expression grammar
/// expr -> expr_boolean_or
/// expr_boolean_or -> boolean_and ("||" expr_boolean_and)?
/// expr_boolean_and -> bitwise_or ("&&" expr_bitwise_or)?
/// expr_bitwise_or -> bitwise_and ("|" expr_bitwise_and)?
/// expr_bitwise_and -> comparision_eq ("&" expr_comparison_eq)?
/// expr_comparison_eq -> comparison (("==" | "!=") expr_comparison)?
/// expr_comparison -> term (("<" | ">" | "<=" | ">=" | "==" | "!=") expr_term)?
/// expr_term -> factor (("+" | "-") expr_factor)*
/// expr_factor -> unary (("*" | "/") expr_unary)*
/// expr_unary -> ("!" | "-") expr_unary | expr_call
/// expr_call -> expr_primary | IDENTIFIER ("(" expr_arguments? ")")?
/// expr_arguments -> expr ("," expr )*
/// expr_primary -> expr_struct_instance | I32 |  STRING | TRUE | FALSE | "(" expr ")" | expr_struct
/// expr_struct -> IDENTIFIER (expr_struct_instance | expr_get_property)?
/// expr_sturct_instance -> "{" (IDENTIFIER: expr ("," IDENTIFIER: expr)*)? "}"
/// expr_get_property -> "." IDENTIFIER
/// expr_type -> ":" IDENTIFIER | "i32" | "str" | "bool"
/// ```
struct Parser {
    tokens: Vec<Token>,
    index: usize,
    scopes: Vec<Scope>,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens,
            index: 0,
            scopes: Vec::with_capacity(8),
        }
    }

    fn parse(&mut self) -> Option<Ast> {
        let mut stmts = vec![];

        let mut errored = false;
        loop {
            let peek_token_result = self.peek_token();
            if peek_token_result.is_err() {
                break;
            }
            if self.peek_token().unwrap().token_type == TokenType::EOF {
                break;
            }
            match self.stmt() {
                Ok(stmt) => stmts.push(stmt),
                Err(err) => {
                    errored = true;
                    error!("{}", err);
                    match self.sync() {
                        Err(parser_error) => {
                            error!("{}", parser_error);
                            break;
                        }
                        _ => {}
                    }
                }
            }
        }

        if errored {
            return None;
        }

        Some(stmts)
    }

    ///////////////////////////////////////////////////////////////////////////
    /// Statement rules
    ///////////////////////////////////////////////////////////////////////////

    fn stmt(&mut self) -> StmtResult {
        match self.peek_token()?.token_type {
            TokenType::Func => self.stmt_func_decl(),
            TokenType::Return => self.stmt_return(),
            TokenType::Struct => self.stmt_struct_decl(),
            TokenType::If => self.stmt_if_else(),
            TokenType::Var => self.stmt_var_decl(),
            TokenType::While => self.stmt_while(),
            TokenType::LeftBrace => self.stmt_block(),
            _ => self.stmt_assignment(),
        }
    }

    fn stmt_func_decl(&mut self) -> StmtResult {
        self.err_if_in_scope(Scope::Func)?;
        self.scopes.push(Scope::Func);

        self.consume(TokenType::Func)?;
        let ident = self
            .consume(TokenType::Identifier)?
            .lexeme
            .as_ref()
            .unwrap()
            .clone();
        self.consume(TokenType::LeftParen)?;
        let mut args = vec![];
        let mut first_iter = true;
        while !self.match_exact(TokenType::RightParen)? {
            if !first_iter {
                self.consume(TokenType::Comma)?;
            }
            let ident = self
                .consume(TokenType::Identifier)?
                .lexeme
                .as_ref()
                .unwrap()
                .clone();
            let btype = self.consume_type()?;
            args.push(VarTypeInfo { ident, btype });
            first_iter = false;
        }

        let ret_type;
        if self.peek_token()?.token_type == TokenType::Colon {
            ret_type = self.consume_type()?;
        } else {
            ret_type = BType::None;
        }

        let stmt_func_decl = Stmt::FuncDecl(StmtFuncDecl {
            ident,
            args,
            ret_type,
            body: Box::new(self.stmt_block()?),
        });
        self.scopes.pop();
        Ok(stmt_func_decl)
    }

    fn stmt_return(&mut self) -> StmtResult {
        self.err_if_not_in_func()?;

        self.consume(TokenType::Return)?;
        if self.match_exact(TokenType::Semicolon)? {
            return Ok(Stmt::Return(StmtReturn { expr: None }));
        }
        let expr = self.expr()?;
        self.consume(TokenType::Semicolon)?;
        Ok(Stmt::Return(StmtReturn { expr: Some(expr) }))
    }

    fn stmt_struct_decl(&mut self) -> StmtResult {
        self.consume(TokenType::Struct)?;
        let ident = self
            .consume(TokenType::Identifier)?
            .lexeme
            .as_ref()
            .unwrap()
            .clone();
        self.consume(TokenType::LeftBrace)?;

        let mut fields = vec![];
        let mut first_iter = true;
        while !self.match_exact(TokenType::RightBrace)? {
            if !first_iter {
                self.consume(TokenType::Comma)?;
            }
            let ident = self
                .consume(TokenType::Identifier)?
                .lexeme
                .as_ref()
                .unwrap()
                .clone();
            let btype = self.consume_type()?;
            fields.push(VarTypeInfo { ident, btype });
            first_iter = false;
        }

        Ok(Stmt::StructDecl(StmtStructDecl { ident, fields }))
    }

    fn stmt_if_else(&mut self) -> StmtResult {
        self.err_if_not_in_func()?;

        self.consume(TokenType::If)?;
        self.consume(TokenType::LeftParen)?;
        let condition = self.expr()?;
        self.consume(TokenType::RightParen)?;
        let body = self.stmt_block()?;

        if self.match_exact(TokenType::Else)? {
            return match self.peek_token()?.token_type {
                TokenType::LeftBrace => Ok(Stmt::IfElse(StmtIfElse {
                    condition,
                    if_body: Box::new(body),
                    else_body: Box::new(self.stmt_block()?),
                })),
                TokenType::If => Ok(Stmt::IfElse(StmtIfElse {
                    condition,
                    if_body: Box::new(body),
                    else_body: Box::new(self.stmt_if_else()?),
                })),
                _ => Err(ParserError::WrongToken(
                    "Expected 'if' or '{' after 'else'".to_string(),
                    self.get_prev_file_coords(),
                )),
            };
        }

        Ok(Stmt::If(StmtIf {
            condition,
            body: Box::new(body),
        }))
    }

    fn stmt_var_decl(&mut self) -> StmtResult {
        self.err_if_not_in_func()?;

        self.consume(TokenType::Var)?;
        let ident = self
            .consume(TokenType::Identifier)?
            .lexeme
            .as_ref()
            .unwrap()
            .clone();
        let btype;
        if self.peek_token()?.token_type == TokenType::Colon {
            btype = self.consume_type()?;
        } else {
            btype = BType::None;
        }
        self.consume(TokenType::Equal)?;
        let expr = self.expr()?;
        self.consume(TokenType::Semicolon)?;
        Ok(Stmt::VarDecl(StmtVarDecl { ident, btype, expr }))
    }

    fn stmt_while(&mut self) -> StmtResult {
        self.err_if_not_in_func()?;

        self.consume(TokenType::While)?;
        self.consume(TokenType::LeftParen)?;
        let condition = self.expr()?;
        self.consume(TokenType::RightParen)?;
        let body = self.stmt_block()?;

        Ok(Stmt::While(StmtWhile {
            condition,
            body: Box::new(body),
        }))
    }

    fn stmt_block(&mut self) -> StmtResult {
        self.err_if_not_in_func()?;

        self.consume(TokenType::LeftBrace)?;
        let mut stmts = vec![];
        while !self.match_exact(TokenType::RightBrace)? {
            stmts.push(self.stmt()?);
        }
        Ok(Stmt::Block(StmtBlock { stmts }))
    }

    fn stmt_assignment(&mut self) -> StmtResult {
        self.err_if_not_in_func()?;

        let expr = self.expr()?;
        if let Expr::Identifier(expr_identifier) = &expr {
            if let Some(_) = self.match_any(vec![TokenType::Equal])? {
                let expr_assign_to = self.expr()?;
                let stmt_assign = Stmt::Assign(StmtAssign {
                    ident: expr_identifier.ident.clone(),
                    property: None,
                    expr: expr_assign_to,
                });
                self.consume(TokenType::Semicolon)?;
                return Ok(stmt_assign);
            }
        } else if let Expr::GetProperty(expr_get_property) = &expr {
            if let Some(_) = self.match_any(vec![TokenType::Equal])? {
                let expr_assign_to = self.expr()?;
                let stmt_assign = Stmt::Assign(StmtAssign {
                    ident: expr_get_property.ident.clone(),
                    property: Some(expr_get_property.property.clone()),
                    expr: expr_assign_to,
                });
                self.consume(TokenType::Semicolon)?;
                return Ok(stmt_assign);
            }
        }
        self.consume(TokenType::Semicolon)?;
        Ok(Stmt::Expr(StmtExpr { expr }))
    }

    ///////////////////////////////////////////////////////////////////////////
    /// Expression rules
    ///////////////////////////////////////////////////////////////////////////

    fn expr(&mut self) -> ExprResult {
        self.expr_boolean_or()
    }

    fn expr_boolean_or(&mut self) -> ExprResult {
        let mut expr = self.expr_boolean_and()?;
        while self.match_exact(TokenType::PipePipe)? {
            let file_coords = self.get_prev_file_coords();
            let right_term = self.expr_boolean_and()?;
            expr = Expr::BinaryOp(ExprBinaryOp {
                left: Box::new(expr),
                op: BinaryOp::BooleanOr,
                right: Box::new(right_term),
                file_coords,
            });
        }
        Ok(expr)
    }

    fn expr_boolean_and(&mut self) -> ExprResult {
        let mut expr = self.expr_bitwise_or()?;
        while self.match_exact(TokenType::AmpersandAmpersand)? {
            let file_coords = self.get_prev_file_coords();
            let right_term = self.expr_bitwise_or()?;
            expr = Expr::BinaryOp(ExprBinaryOp {
                left: Box::new(expr),
                op: BinaryOp::BooleanAnd,
                right: Box::new(right_term),
                file_coords,
            });
        }
        Ok(expr)
    }

    fn expr_bitwise_or(&mut self) -> ExprResult {
        let mut expr = self.expr_bitwise_and()?;
        while self.match_exact(TokenType::Pipe)? {
            let file_coords = self.get_prev_file_coords();
            let right_term = self.expr_bitwise_and()?;
            expr = Expr::BinaryOp(ExprBinaryOp {
                left: Box::new(expr),
                op: BinaryOp::BitwiseOr,
                right: Box::new(right_term),
                file_coords,
            });
        }
        Ok(expr)
    }

    fn expr_bitwise_and(&mut self) -> ExprResult {
        let mut expr = self.expr_comparison_eq()?;
        while self.match_any(vec![TokenType::Ampersand])?.is_some() {
            let file_coords = self.get_prev_file_coords();
            let right_term = self.expr_comparison_eq()?;
            expr = Expr::BinaryOp(ExprBinaryOp {
                left: Box::new(expr),
                op: BinaryOp::BitwiseAnd,
                right: Box::new(right_term),
                file_coords,
            });
        }
        Ok(expr)
    }

    fn expr_comparison_eq(&mut self) -> ExprResult {
        let mut expr = self.expr_comparison()?;
        while let Some(token_type) =
            self.match_any(vec![TokenType::EqualEqual, TokenType::BangEqual])?
        {
            let file_coords = self.get_prev_file_coords();
            let right_term = self.expr_comparison()?;
            expr = Expr::BinaryOp(ExprBinaryOp {
                left: Box::new(expr),
                op: token_type.get_bin_op_type(),
                right: Box::new(right_term),
                file_coords,
            });
        }
        Ok(expr)
    }

    fn expr_comparison(&mut self) -> ExprResult {
        let mut expr = self.expr_term()?;
        while let Some(token_type) = self.match_any(vec![
            TokenType::Less,
            TokenType::Greater,
            TokenType::LessEqual,
            TokenType::GreaterEqual,
        ])? {
            let file_coords = self.get_prev_file_coords();
            let right_term = self.expr_term()?;
            expr = Expr::BinaryOp(ExprBinaryOp {
                left: Box::new(expr),
                op: token_type.get_bin_op_type(),
                right: Box::new(right_term),
                file_coords,
            });
        }
        Ok(expr)
    }

    fn expr_term(&mut self) -> ExprResult {
        let mut expr = self.expr_factor()?;
        while let Some(token_type) = self.match_any(vec![TokenType::Plus, TokenType::Minus])? {
            let file_coords = self.get_prev_file_coords();
            let right_term = self.expr_factor()?;
            expr = Expr::BinaryOp(ExprBinaryOp {
                left: Box::new(expr),
                op: token_type.get_bin_op_type(),
                right: Box::new(right_term),
                file_coords,
            });
        }
        Ok(expr)
    }

    fn expr_factor(&mut self) -> ExprResult {
        let mut expr = self.expr_unary()?;
        while let Some(token_type) = self.match_any(vec![TokenType::Star, TokenType::Slash])? {
            let file_coords = self.get_prev_file_coords();
            let right_term = self.expr_unary()?;
            expr = Expr::BinaryOp(ExprBinaryOp {
                left: Box::new(expr),
                op: token_type.get_bin_op_type(),
                right: Box::new(right_term),
                file_coords,
            });
        }
        Ok(expr)
    }

    fn expr_unary(&mut self) -> ExprResult {
        if let Some(token_type) = self.match_any(vec![TokenType::Bang, TokenType::Minus])? {
            let term = self.expr_unary()?;
            return Ok(Expr::UnaryOp(ExprUnaryOp {
                op: token_type.get_unary_op_type(),
                term: Box::new(term),
                file_coords: self.get_prev_file_coords(),
            }));
        }
        self.expr_call()
    }

    fn expr_call(&mut self) -> ExprResult {
        let term = self.expr_primary()?;
        if self.peek_token()?.token_type == TokenType::LeftParen {
            if let Expr::Identifier(ExprIdenifier { ident, file_coords }) = term {
                self.consume(TokenType::LeftParen)?;
                let args = self.expr_arguments()?;
                return Ok(Expr::Call(ExprCall {
                    name: ident,
                    args,
                    file_coords,
                }));
            }
            return Err(ParserError::WrongToken(
                "Expected identifier after '('".to_string(),
                self.get_prev_file_coords(),
            ));
        }
        Ok(term)
    }

    fn expr_arguments(&mut self) -> Result<Vec<Expr>, ParserError> {
        let mut args = vec![];
        let mut first_pass = true;
        while self.peek_token()?.token_type != TokenType::RightParen {
            if !first_pass {
                self.consume(TokenType::Comma)?;
            }
            args.push(self.expr()?);
            first_pass = false;
        }
        self.consume(TokenType::RightParen)?;
        Ok(args)
    }

    fn expr_primary(&mut self) -> ExprResult {
        let token = self.next_token()?.clone();
        match token.token_type {
            TokenType::I32 => Ok(Expr::I32(ExprI32 {
                value: token.lexeme.as_ref().unwrap().parse::<i32>().unwrap(),
                file_coords: token.file_coords,
            })),
            TokenType::Identifier => {
                if self.match_exact(TokenType::LeftBrace)? {
                    return self.expr_sturct_instance(token.lexeme.unwrap(), token.file_coords);
                } else if self.match_exact(TokenType::Dot)? {
                    return self.expr_get_property(token.lexeme.unwrap(), token.file_coords);
                }
                Ok(Expr::Identifier(ExprIdenifier {
                    ident: token.lexeme.as_ref().unwrap().clone(),
                    file_coords: token.file_coords,
                }))
            }
            TokenType::String => Ok(Expr::String(ExprString {
                value: token.lexeme.as_ref().unwrap().clone(),
                file_coords: token.file_coords,
            })),
            TokenType::True => Ok(Expr::Bool(ExprBool {
                value: true,
                file_coords: token.file_coords,
            })),
            TokenType::False => Ok(Expr::Bool(ExprBool {
                value: false,
                file_coords: token.file_coords,
            })),
            TokenType::LeftParen => {
                let expr = self.expr()?;
                self.consume(TokenType::RightParen)?;
                Ok(expr)
            }
            _ => Err(ParserError::WrongToken(
                format!(
                    "Can't start statement or expression with {:?} token",
                    token.token_type
                ),
                token.file_coords,
            )),
        }
    }

    fn expr_sturct_instance(&mut self, ident: String, file_coords: FileCoords) -> ExprResult {
        let mut fields: HashMap<String, Expr> = Default::default();
        let mut first_iter = true;
        while !self.match_exact(TokenType::RightBrace)? {
            if !first_iter {
                self.consume(TokenType::Comma)?;
            }
            let ident = self
                .consume(TokenType::Identifier)?
                .lexeme
                .as_ref()
                .unwrap()
                .clone();
            self.consume(TokenType::Colon)?;
            let expr = self.expr()?;
            fields.insert(ident, expr);
            first_iter = false;
        }

        Ok(Expr::StructInstance(ExprStructInstance {
            ident,
            fields,
            file_coords,
        }))
    }

    fn expr_get_property(&mut self, ident: String, file_coords: FileCoords) -> ExprResult {
        let ident_token = self.consume(TokenType::Identifier)?;
        Ok(Expr::GetProperty(ExprGetProperty {
            ident,
            property: ident_token.lexeme.as_ref().unwrap().clone(),
            file_coords,
        }))
    }

    fn consume_type(&mut self) -> Result<BType, ParserError> {
        self.consume(TokenType::Colon)?;
        Ok(self
            .consume_any(vec![
                TokenType::BTypeBool,
                TokenType::BTypeI32,
                TokenType::BTypeStr,
                TokenType::Identifier,
            ])?
            .get_btype())
    }

    ///////////////////////////////////////////////////////////////////////////
    /// Parser helper methods
    ///////////////////////////////////////////////////////////////////////////

    fn next_token(&mut self) -> TokenRefResult {
        match self.tokens.get(self.index) {
            Some(token) => {
                self.index += 1;
                Ok(token)
            }
            None => Err(ParserError::EOF),
        }
    }

    fn peek_token(&mut self) -> TokenRefResult {
        match self.tokens.get(self.index) {
            Some(token) => Ok(token),
            None => Err(ParserError::EOF),
        }
    }

    fn consume(&mut self, token_type: TokenType) -> TokenRefResult {
        let token = self.next_token()?;
        if token.token_type != token_type {
            return Err(ParserError::WrongToken(
                format!(
                    "Expected token of type {:?} but got {:?}.",
                    token_type, token.token_type
                ),
                token.file_coords,
            ));
        }
        Ok(token)
    }

    fn consume_any(&mut self, token_types: Vec<TokenType>) -> TokenRefResult {
        let token = self.next_token()?;
        if !token_types.contains(&token.token_type) {
            return Err(ParserError::WrongToken(
                format!(
                    "Expected token of any type {:?} but got {:?}.",
                    token_types, token.token_type
                ),
                token.file_coords,
            ));
        }
        Ok(token)
    }

    fn match_exact(&mut self, token_type: TokenType) -> Result<bool, ParserError> {
        if self.peek_token()?.token_type == token_type {
            let _ = self.next_token()?;
            return Ok(true);
        }
        return Ok(false);
    }

    fn match_any(&mut self, token_types: Vec<TokenType>) -> Result<Option<TokenType>, ParserError> {
        if token_types.contains(&self.peek_token()?.token_type) {
            return Ok(Some(self.next_token()?.token_type));
        }
        Ok(None)
    }

    fn get_prev_file_coords(&self) -> FileCoords {
        self.tokens.get(self.index - 1).unwrap().file_coords
    }

    fn get_current_file_coords(&self) -> FileCoords {
        self.tokens.get(self.index).unwrap().file_coords
    }

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

    fn err_if_in_scope(&self, scope: Scope) -> Result<(), ParserError> {
        if self.scopes.iter().find(|s| **s == scope).is_none() {
            return Ok(());
        }
        Err(ParserError::InvalidScope(
            format!("Can't have this statement inside '{:?}' scope", scope),
            self.get_current_file_coords(),
        ))
    }

    fn err_if_not_in_func(&self) -> Result<(), ParserError> {
        if self.scopes.iter().find(|s| **s == Scope::Func).is_some() {
            return Ok(());
        }
        Err(ParserError::InvalidScope(
            format!("Statement must be inside of a '{:?}' scope", Scope::Func),
            self.get_current_file_coords(),
        ))
    }
}

pub fn parse(tokens: Vec<Token>) -> Ast {
    info!("Parsing...");
    match Parser::new(tokens).parse() {
        Some(ast) => {
            info!("Parsing complete!");
            ast
        }
        _ => {
            error!("Parsing failed!");
            std::process::exit(1);
        }
    }
}
