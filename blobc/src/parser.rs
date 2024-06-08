use std::fmt::Display;

use crate::{
    ast::{
        expr::{
            Expr, ExprBinaryOp, ExprBitwiseOp, ExprBool, ExprBooleanOp, ExprCall, ExprCmpOp, ExprI64, ExprIdenifier, ExprString, ExprUnaryOp
        },
        stmt::{Stmt, StmtExpr},
    },
    common::FileCoords,
    token::{Token, TokenType},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParserError {
    WrongToken(String),
}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::WrongToken(msg) => write!(f, "{}", msg),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ScopeType {
    Func,
    If,
    Else,
    While,
    Block,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Scope {
    scope_type: ScopeType,
    file_coords: FileCoords,
}

type StmtResult = Result<Stmt, ParserError>;
type ExprResult = Result<Expr, ParserError>;
type TokenRefResult<'a> = Result<&'a Token, ParserError>;

struct Parser {
    tokens: Vec<Token>,
    index: usize,
    scopes: Vec<Scope>,
}

/// Top down parser.
///
/// Parsing rules
/// ```
/// // Statement grammar
/// stmt -> stmt_func_decl / stmt_return / stmt_if_else / stmt_var_decl / stmt_while / stmt_block / stmt_assignment
/// stmt_func_decl -> "func" "(" (IDENTIFIER type_expr ("," IDENTIFIER type_expr)*)? ")" type_expr? block_stmt
/// stmt_return -> "return" expr ";"
/// stmt_if_else -> "if" "(" expr ")" block_stmt ("else" block_stmt | "else" if_else_stmt)?
/// stmt_var_decl -> "var" IDENTIFIER type_expr? "=" expr ";"
/// stmt_while -> "while" "(" expr ")" block_stmt
/// stmt_block -> "{" stmt* "}"
/// stmt_assignment -> ((IDENTIFIER "=")? expr | IDENTIFIER) ";"
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
/// expr_primary -> I64 | IDENTIFIER | STRING | TRUE | FALSE | "(" expr ")"
/// expr_type -> ":" IDENTIFIER | "i64" | "str" | "bool"
/// ```
impl Parser {
    fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens,
            index: 0,
            scopes: vec![],
        }
    }

    fn parse(&mut self) -> Result<Vec<Stmt>, ParserError> {
        let mut stmts = vec![];

        loop {
            if self.peek_token().token_type == TokenType::EOF {
                break;
            }
            match self.stmt() {
                Ok(stmt) => stmts.push(stmt),
                Err(err) => {
                    println!("{}", err);
                }
            }
        }

        Ok(stmts)
    }

    fn stmt(&mut self) -> StmtResult {
        let expr = self.expr()?;
        self.consume(TokenType::Semicolon)?;
        Ok(Stmt::Expr(StmtExpr { expr }))
    }

    fn expr(&mut self) -> ExprResult {
        self.expr_boolean_or()
    }

    fn expr_boolean_or(&mut self) -> ExprResult {
        let mut expr = self.expr_boolean_and()?;
        while let Some(token_type) = self.match_any(vec![TokenType::PipePipe]) {
            let file_coords = self.get_prev_file_coords();
            let right_term = self.expr_boolean_and()?;
            expr = Expr::BooleanOp(ExprBooleanOp {
                left: Box::new(expr),
                op: token_type.get_boolean_op_type(),
                right: Box::new(right_term),
                file_coords,
            });
        }
        Ok(expr)
    }

    fn expr_boolean_and(&mut self) -> ExprResult {
        let mut expr = self.expr_bitwise_or()?;
        while let Some(token_type) = self.match_any(vec![TokenType::AmpersandAmpersand]) {
            let file_coords = self.get_prev_file_coords();
            let right_term = self.expr_bitwise_or()?;
            expr = Expr::BooleanOp(ExprBooleanOp {
                left: Box::new(expr),
                op: token_type.get_boolean_op_type(),
                right: Box::new(right_term),
                file_coords,
            });
        }
        Ok(expr)
    }

    fn expr_bitwise_or(&mut self) -> ExprResult {
        let mut expr = self.expr_bitwise_and()?;
        while let Some(token_type) = self.match_any(vec![TokenType::Ampersand]) {
            let file_coords = self.get_prev_file_coords();
            let right_term = self.expr_bitwise_and()?;
            expr = Expr::BitwiseOp(ExprBitwiseOp {
                left: Box::new(expr),
                op: token_type.get_bitwise_op_type(),
                right: Box::new(right_term),
                file_coords,
            });
        }
        Ok(expr)
    }

    fn expr_bitwise_and(&mut self) -> ExprResult {
        let mut expr = self.expr_comparison_eq()?;
        while let Some(token_type) = self.match_any(vec![TokenType::Ampersand]) {
            let file_coords = self.get_prev_file_coords();
            let right_term = self.expr_comparison_eq()?;
            expr = Expr::BitwiseOp(ExprBitwiseOp {
                left: Box::new(expr),
                op: token_type.get_bitwise_op_type(),
                right: Box::new(right_term),
                file_coords,
            });
        }
        Ok(expr)
    }

    fn expr_comparison_eq(&mut self) -> ExprResult {
        let mut expr = self.expr_comparison()?;
        while let Some(token_type) =
            self.match_any(vec![TokenType::EqualEqual, TokenType::BangEqual])
        {
            let file_coords = self.get_prev_file_coords();
            let right_term = self.expr_comparison()?;
            expr = Expr::CmpOp(ExprCmpOp {
                left: Box::new(expr),
                op: token_type.get_cmp_op_type(),
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
        ]) {
            let file_coords = self.get_prev_file_coords();
            let right_term = self.expr_term()?;
            expr = Expr::CmpOp(ExprCmpOp {
                left: Box::new(expr),
                op: token_type.get_cmp_op_type(),
                right: Box::new(right_term),
                file_coords,
            });
        }
        Ok(expr)
    }

    fn expr_term(&mut self) -> ExprResult {
        let mut expr = self.expr_factor()?;
        while let Some(token_type) = self.match_any(vec![TokenType::Plus, TokenType::Minus]) {
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
        while let Some(token_type) = self.match_any(vec![TokenType::Star, TokenType::Slash]) {
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
        if let Some(token_type) = self.match_any(vec![TokenType::Bang, TokenType::Minus]) {
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
        if self.peek_token().token_type == TokenType::LeftParen {
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
            ));
        }
        Ok(term)
    }

    fn expr_arguments(&mut self) -> Result<Vec<Expr>, ParserError> {
        let mut args = vec![];
        let mut first_pass = true;
        while self.peek_token().token_type != TokenType::RightParen {
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
        let token = self.next_token();
        match token.token_type {
            TokenType::I64 => Ok(Expr::I64(ExprI64 {
                value: token.lexeme.as_ref().unwrap().parse::<i64>().unwrap(),
                file_coords: token.file_coords,
            })),
            TokenType::Identifier => Ok(Expr::Identifier(ExprIdenifier {
                ident: token.lexeme.as_ref().unwrap().clone(),
                file_coords: token.file_coords,
            })),
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
            _ => Err(ParserError::WrongToken(format!(
                "Can't start statement or expression with {:?} token",
                token.token_type
            ))),
        }
    }

    //fn type_expr(&mut self) -> ExprResult {}

    ///////////////////////////////////////////////////////////////////////////
    /// Parer helper methods
    ///////////////////////////////////////////////////////////////////////////

    fn next_token(&mut self) -> &Token {
        let token = self.tokens.get(self.index).unwrap();
        self.index += 1;
        token
    }

    fn peek_token(&mut self) -> &Token {
        self.tokens.get(self.index).unwrap()
    }

    fn consume(&mut self, token_type: TokenType) -> TokenRefResult {
        let token = self.next_token();
        if token.token_type != token_type {
            return Err(ParserError::WrongToken(format!(
                "Expected token of type {:?} but got {:?}.",
                token_type, token.token_type
            )));
        }
        Ok(token)
    }

    fn match_any(&mut self, token_types: Vec<TokenType>) -> Option<TokenType> {
        if token_types.contains(&self.peek_token().token_type) {
            return Some(self.next_token().token_type);
        }
        None
    }

    fn get_prev_file_coords(&self) -> FileCoords {
        self.tokens.get(self.index - 1).unwrap().file_coords
    }
}

pub fn parse(tokens: Vec<Token>) -> Result<Vec<Stmt>, ParserError> {
    println!("Parsing...");
    let result = Parser::new(tokens).parse();
    println!("Parsing complete!");
    result
}
