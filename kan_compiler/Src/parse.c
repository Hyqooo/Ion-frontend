// So here is a plan:
// 1. Create syntactic structure that allows us to parse program
// 2. Provide api to construct ast

// Actually, I think what returning types we need will become more clear further

Stmt *parse_stmt();
Expr *parse_unary_expr();

const char *token_kind_names[] = {
    [TOKEN_EOF] = "EOF",
    [TOKEN_COLON] = ":",
    [TOKEN_LPAREN] = "(",
    [TOKEN_RPAREN] = ")",
    [TOKEN_LBRACE] = "{",
    [TOKEN_RBRACE] = "}",
    [TOKEN_LBRACKET] = "[",
    [TOKEN_RBRACKET] = "]",
    [TOKEN_COMMA] = ",",
    [TOKEN_DOT] = ".",
    [TOKEN_QUESTION] = "?",
    [TOKEN_SEMICOLON] = ";",
    [TOKEN_KEYWORD] = "keyword",
    [TOKEN_INT] = "int",
    [TOKEN_FLOAT] = "float",
    [TOKEN_STRING] = "string",
    [TOKEN_NAME] = "name",
    [TOKEN_NEG] = "~",
    [TOKEN_NOT] = "!",
    [TOKEN_MUL] = "*",
    [TOKEN_DIV] = "/",
    [TOKEN_MOD] = "%",
    [TOKEN_AND] = "&",
    [TOKEN_LSHIFT] = "<<",
    [TOKEN_RSHIFT] = ">>",
    [TOKEN_ADD] = "+",
    [TOKEN_SUB] = "-",
    [TOKEN_OR] = "|",
    [TOKEN_XOR] = "^",
    [TOKEN_EQ] = "==",
    [TOKEN_NOTEQ] = "!=",
    [TOKEN_LT] = "<",
    [TOKEN_GT] = ">",
    [TOKEN_LTEQ] = "<=",
    [TOKEN_GTEQ] = ">=",
    [TOKEN_AND_AND] = "&&",
    [TOKEN_OR_OR] = "||",
    [TOKEN_ASSIGN] = "=",
    [TOKEN_ADD_ASSIGN] = "+=",
    [TOKEN_SUB_ASSIGN] = "-=",
    [TOKEN_OR_ASSIGN] = "|=",
    [TOKEN_AND_ASSIGN] = "&=",
    [TOKEN_XOR_ASSIGN] = "^=",
    [TOKEN_MUL_ASSIGN] = "*=",
    [TOKEN_DIV_ASSIGN] = "/=",
    [TOKEN_MOD_ASSIGN] = "%=",
    [TOKEN_LSHIFT_ASSIGN] = "<<=",
    [TOKEN_RSHIFT_ASSIGN] = ">>=",
    [TOKEN_INC] = "++",
    [TOKEN_DEC] = "--",
    [TOKEN_COLON_ASSIGN] = ":=",
};

inline bool is_token(TokenKind kind) {
    return token.kind == kind;
}

inline bool is_token_eof() {
    return token.kind == TOKEN_EOF;
}

bool expect_token(TokenKind expected_token) {
    if (is_token(expected_token)) {
        read_token();
        return true;
    } else {
        // TODO: unknown token
        printf("Expected token %s, got %s", token_kind_names[expected_token], token_kind_names[token.kind]);
        return false;
    }
}

bool match_token(TokenKind kind) {
    if (is_token(kind)) {
        read_token();
        return true;
    } else {
        return false;
    }
}

bool match_keyword(const char *keyword) {
    if (is_keyword(keyword)) {
        return true;
    } else {
        return false;
    }
}

inline bool is_cmp_op() {
    return is_token(TOKEN_LT) || is_token(TOKEN_GT) || is_token(TOKEN_LTEQ) || is_token(TOKEN_GTEQ) || is_token(TOKEN_EQ) || is_token(TOKEN_NOTEQ);
}

inline bool is_add_op() {
    return is_token(TOKEN_ADD) || is_token(TOKEN_SUB) || is_token(TOKEN_XOR);
}

inline bool is_mul_op() {
    return is_token(TOKEN_MUL) || is_token(TOKEN_DIV) || is_token(TOKEN_MOD) || is_token(TOKEN_AND) || is_token(TOKEN_LSHIFT) || is_token(TOKEN_RSHIFT);
}

inline bool is_unary_op() {
    return is_token(TOKEN_ADD) ||
        is_token(TOKEN_SUB) ||
        is_token(TOKEN_MUL) ||
        is_token(TOKEN_AND) ||
        is_token(TOKEN_NEG) ||
        is_token(TOKEN_INC) ||
        is_token(TOKEN_DEC);
}

inline bool is_assign_op(){
    return token.kind >= TOKEN_FIRST_ASSIGN && token.kind <= TOKEN_LAST_ASSIGN;
}

Expr *parse_expr();
Typespec *parse_type_base();
Typespec *parse_type();

const char *parse_name(){
    const char *name = token.name;
    expect_token(TOKEN_NAME);
    return name;
}

Typespec *parse_type_func_param(){
    Typespec *type = parse_type();
    if (match_token(TOKEN_COLON)){
        if (type->kind != TYPESPEC_NAME){
            // Error: Colons in parameters of func types must be preceded by names
            assert(0);
        }
        type = parse_type();
    }
    return type;
}

Typespec *parse_type_func(){
    Typespec **args = NULL;
    bool has_varargs = false;
    expect_token(TOKEN_LPAREN);
    if (!is_token(TOKEN_RPAREN)){
        buf_push(args, parse_type_func_param());
        while (match_token(TOKEN_COMMA)){
            if (match_token(TOKEN_ELLIPSIS)){
                if (has_varargs){
                    // Error: Multiple ellipsis instances in function type
                    assert(0);
                }
                has_varargs = true;
            } else {
                if (has_varargs){
                    // Error: Ellipsis must be last paremeter in function type
                    assert(0);
                }
                buf_push(args, parse_type_func_param());
            }
        }
    }
    expect_token(TOKEN_RPAREN);
    Typespec *ret = NULL;
    if (match_token(TOKEN_COLON)){
        ret = parse_type();
    }
    return new_typespec_func(args, buf_len(args), ret, has_varargs);
}

Typespec *parse_type(){
    Typespec *type = parse_type_base();
    while (is_token(TOKEN_LBRACKET) || is_token(TOKEN_MUL) || is_keyword(keyword_const)){
        if (match_token(TOKEN_LBRACKET)){
            Expr *size = NULL;
            if (!is_token(TOKEN_RBRACKET)){
                size = parse_expr();
            }
            expect_token(TOKEN_RBRACKET);
            type = new_typespec_array(type, size);
        } else if (match_keyword(keyword_const)){
            type = new_typespec_const(type);
        } else {
            assert(is_token(TOKEN_MUL));
            read_token();
            // NOTE: I don't remember if I want to do pointers or not
            //type = new_typespec_ptr(type);
        }
    }
    return type;
}

Typespec *parse_type_base() {
    if (is_token(TOKEN_NAME)){
        const char **names = NULL;
        buf_push(names, token.name);
        read_token();
        while (match_token(TOKEN_DOT)){
            buf_push(names, parse_name());
        }
        return new_typespec_name(names, buf_len(names));
    } else if (match_keyword(keyword_func)){
        return parse_type_func();
    } else if (match_token(TOKEN_LPAREN)){
        Typespec *type = parse_type();
        expect_token(TOKEN_RPAREN);
        return type;
    } else if (match_token(TOKEN_LBRACE)){
        assert(0);
        //return parse_type_tuple();
        return NULL;
    } else {
        // Error: Unexpected token in type
        assert(0);
        return NULL;
    }
}

Expr *parse_paren_expr() {
    expect_token(TOKEN_LPAREN);
    Expr *expr = parse_expr();
    expect_token(TOKEN_RPAREN);
    return expr;
}

CompoundField parse_compound_field_expr(){
    if (match_token(TOKEN_LBRACKET)){
        Expr *index = parse_expr();
        expect_token(TOKEN_RBRACKET);
        expect_token(TOKEN_ASSIGN);
        return (CompoundField){FIELD_INDEX, parse_expr(), .index = index};
    } else {
        Expr *expr = parse_expr();
        if (match_token(TOKEN_ASSIGN)){
            if (expr->kind != EXPR_NAME){
                // Error: Named initializer in compound literal must be preceded by field name
                assert(0);
            }
            return (CompoundField) {
                FIELD_NAME, parse_expr(), .name = expr->name
            };
        } else {
            return (CompoundField){FIELD_DEFAULT, expr};
        }
    }
}

Expr *parse_compound_expr(Typespec *type) {
    expect_token(TOKEN_LBRACE);
    CompoundField *fields = NULL;
    while (!is_token(TOKEN_RBRACE)) {
        buf_push(fields, parse_compound_field_expr());
        if (!match_token(TOKEN_COMMA)) {
            break;
        }
    }
    expect_token(TOKEN_RBRACE);
    return new_compound_expr(type, fields, buf_len(fields));
}

Expr *parse_operand_expr() {
    if (is_token(TOKEN_INT)) {
        unsigned long long val = token.int_val;
        read_token();
        return new_int_expr(val);
    } else if (is_token(TOKEN_FLOAT)) {
        const char *start = token.start;
        const char *end = token.end;
        double val = token.float_val;
        read_token();
        return new_float_expr(start, end, val);
    } else if (is_token(TOKEN_STRING)) {
        const char *val = token.str_val;
        read_token();
        return new_str_expr(val);
    } else if (is_token(TOKEN_NAME)) {
        const char *name = token.name;
        read_token();
        if (is_token(TOKEN_LBRACE)) {
            return parse_compound_expr(new_typespec_name(&name, 1));
        } else {
            return new_name_expr(name);
        }
    } else if (is_token(TOKEN_LBRACE)) {
        return parse_compound_expr(NULL);
    } else if (match_token(TOKEN_LPAREN)) {
        if (match_token(TOKEN_COLON)) {
            Typespec *type = parse_type();
            expect_token(TOKEN_RPAREN);
            if (is_token(TOKEN_LBRACE)) {
                return parse_compound_expr(type);
            } else {
                // Why is there no 'cast' word?
                return new_cast_expr(type, parse_unary_expr());
            }
        } else {
            Expr *expr = parse_expr();
            expect_token(TOKEN_RPAREN);
            return new_paren_expr(expr);
        }
    } else {
        // Error: unexpected token in epxression
        assert(0);
        return NULL;
    }
}

Expr *parse_base_expr() {
    Expr *expr = parse_operand_expr();
    while (is_token(TOKEN_LPAREN) || is_token(TOKEN_LBRACKET) || is_token(TOKEN_DOT) || is_token(TOKEN_INC) || is_token(TOKEN_DEC)) {
        if (match_token(TOKEN_LPAREN)) {
            Expr **args = NULL;
            if (!is_token(TOKEN_RPAREN)) {
                buf_push(args, parse_expr());
                while (match_token(TOKEN_COMMA)) {
                    buf_push(args, parse_expr());
                }
            }
            expect_token(TOKEN_RPAREN);
            expr = new_call_expr(expr, args, buf_len(args));
        } else if (match_token(TOKEN_LBRACKET)) {
            Expr *index = parse_expr();
            expect_token(TOKEN_RBRACKET);
            expr = new_index_expr(expr, index);
        } else if (is_token(TOKEN_DOT)) {
            read_token();
            const char *field = token.name;
            expect_token(TOKEN_NAME);
            expr = new_field_expr(expr, field);
        } else {
            assert(is_token(TOKEN_INC) || is_token(TOKEN_DEC));
            TokenKind op = token.kind;
            read_token();
            expr = new_modify_expr(op, true, expr);
        }
    }
    return expr;
}

Expr *parse_unary_expr() {
    if (is_unary_op()) {
        TokenKind op = token.kind;
        read_token();
        if (op == TOKEN_INC || op == TOKEN_DEC) {
            return new_modify_expr(op, false, parse_unary_expr());
        } else {
            return new_unary_expr(op, parse_unary_expr());
        }
    } else {
        return parse_base_expr();
    }
}

Expr *parse_mul_expr() {
    Expr *expr = parse_unary_expr();
    while (is_mul_op()) {
        TokenKind op = token.kind;
        read_token();
        expr = new_binary_expr(op, expr, parse_unary_expr());
    }
    return expr;
}

Expr *parse_add_expr() {
    Expr *expr = parse_mul_expr();
    while (is_add_op()) {
        TokenKind op = token.kind;
        read_token();
        expr = new_binary_expr(op, expr, parse_mul_expr());
    }
    return expr;
}

Expr *parse_cmp_expr() {
    Expr *expr = parse_add_expr();
    while (is_cmp_op()) {
        TokenKind op = token.kind;
        read_token();
        expr = new_binary_expr(op, expr, parse_add_expr());
    }
    return expr;
}

Expr *parse_and_expr() {
    Expr *expr = parse_cmp_expr();
    while (match_token(TOKEN_AND_AND)) {
        expr = new_binary_expr(TOKEN_AND_AND, expr, parse_cmp_expr());
    }
    return expr;
}

Expr *parse_or_expr() {
    Expr *expr = parse_and_expr();
    while (match_token(TOKEN_OR_OR)) {
        expr = new_binary_expr(TOKEN_OR_OR, expr, parse_and_expr());
    }
    return expr;
}

Expr *parse_ternary_expr() {
    Expr *expr = parse_or_expr();
    if (match_token(TOKEN_QUESTION)) {
        Expr *then_expr = parse_ternary_expr();
        expect_token(TOKEN_COLON);
        Expr *else_expr = parse_ternary_expr();
        expr = new_ternary_expr(expr, then_expr, else_expr);
    }
    
    return expr;
}

Expr *parse_expr() {
    return parse_ternary_expr();
}

StmtList parse_stmt_block() {
    expect_token(TOKEN_LBRACE);
    Stmt **stmts = NULL;
    while (!is_token_eof() && !is_token(TOKEN_RBRACE)) {
        buf_push(stmts, parse_stmt());
    }
    expect_token(TOKEN_RBRACE);
    return new_stmt_list(stmts, buf_len(stmts));
}

Stmt *parse_init_stmt(Expr *left) {
    if (match_token(TOKEN_COLON_ASSIGN)) {
        if (left->kind != EXPR_NAME) {
            // Error: := must be preceded by a name
            return NULL;
        }
        return new_stmt_init(left->name, NULL, parse_expr(), false);
    } else {
        return NULL;
    }
}

// NOTE: This kind of statements is
// Assignments
// Any other expressions that doesn't contain any keywords (?)
Stmt *parse_simple_stmt() {
    Expr *expr = parse_expr();
    // This is lvalue
    Stmt *stmt = parse_init_stmt(expr);
    if (!stmt) {
        if (is_assign_op()) {
            TokenKind op = token.kind;
            read_token();
            stmt = new_stmt_assign(op, expr, parse_expr());
        } else {
            stmt = new_stmt_expr(expr);
        }
    }
    return stmt;
}

Stmt *parse_stmt_if() {
    expect_token(TOKEN_LPAREN);
    Expr *cond = parse_expr();
    Stmt *init = parse_init_stmt(cond);
    if (init) {
        // So it allows: 'if (x := 42; x >= 0)' 
        if (match_token(TOKEN_SEMICOLON)) {
            cond = parse_expr();
        } else {
            cond = NULL;
        }
    }
    expect_token(TOKEN_RPAREN);
    StmtList then_block = parse_stmt_block();
    StmtList else_block = { 0 };
    ElseIf *elseifs = NULL;
    while (match_keyword(keyword_else)) {
        if (!match_keyword(keyword_if)) {
            else_block = parse_stmt_block();
            break;
        }
        Expr *elseif_cond = parse_paren_expr();
        StmtList elseif_block = parse_stmt_block();
        buf_push(elseifs, ((ElseIf){ elseif_cond, elseif_block }));
    }
    return new_stmt_if(init, cond, then_block, elseifs, buf_len(elseifs), else_block);
}

Stmt *parse_stmt_while() {
    Expr *cond = parse_paren_expr();
    return new_stmt_while(cond, parse_stmt_block());
}

Stmt *parse_stmt_for() {
    Stmt *init = NULL;
    Expr *cond = NULL;
    Stmt *next = NULL;
    // This means that we allow: for { ... } == for (;;) { ... }
    if (!is_token(TOKEN_LBRACE)) {
        expect_token(TOKEN_LPAREN);
        if (!is_token(TOKEN_SEMICOLON)) {
            init = parse_simple_stmt();
        }
        if (match_token(TOKEN_SEMICOLON)) {
            if (!is_token(TOKEN_SEMICOLON)) {
                cond = parse_expr();
            }
            if (match_token(TOKEN_SEMICOLON)) {
                if (!is_token(TOKEN_RPAREN)) {
                    next = parse_simple_stmt();
                    if (next->kind == STMT_INIT) {
                        // Error: Init statements are not allowed in for-statement's next clause
                        assert(0);
                    }
                }
            }
        }
        expect_token(TOKEN_RPAREN);
    }
    return new_stmt_for(init, cond, next, parse_stmt_block());
}

SwitchCasePattern parse_switch_case_pattern() {
    Expr *start = parse_expr();
    Expr *end = NULL;
    if (match_token(TOKEN_ELLIPSIS)) {
        end = parse_expr();
    }
    return (SwitchCasePattern) { start, end };
}

SwitchCase parse_stmt_switch_case() {
    SwitchCasePattern *patterns = NULL;
    bool is_default = false;
    bool is_first_case = true;
    while (is_keyword(keyword_case) || is_keyword(keyword_default)) {
        if (match_keyword(keyword_case)) {
            if (!is_first_case) {
                // Warning: Use comma-separated expressions to match multiple values with one case label
                is_first_case = false;
            }
            buf_push(patterns, parse_switch_case_pattern());
            while (match_token(TOKEN_COMMA)) {
                buf_push(patterns, parse_switch_case_pattern());
            }
        } else {
            assert(is_keyword(keyword_default));
            read_token();
            if (is_default) {
                // Error: Duplicate default labels in same switch clause
                assert(0);
            }
            is_default = true;
        }
        expect_token(TOKEN_COLON);
    }
    Stmt **stmts = NULL;
    while (!is_token_eof() && !is_token(TOKEN_RBRACE) && !is_keyword(keyword_case) && !is_keyword(keyword_default)) {
        buf_push(stmts, parse_stmt());
    }
    return (SwitchCase) { patterns, buf_len(patterns), is_default, new_stmt_list(stmts, buf_len(stmts)) };
}

Stmt *parse_stmt_switch() {
    Expr *expr = parse_paren_expr();
    SwitchCase *cases = NULL;
    expect_token(TOKEN_LBRACE);
    while (!is_token_eof() && !is_token(TOKEN_RBRACE)) {
        buf_push(cases, parse_stmt_switch_case());
    }
    expect_token(TOKEN_RBRACE);
    return new_stmt_switch(expr, cases, buf_len(cases));
}

// TODO: UNTESTED
Stmt *parse_stmt() {
    Stmt *stmt = NULL;
    if (match_keyword(keyword_if)) {
        stmt = parse_stmt_if();
    } else if (match_keyword(keyword_while)) {
        stmt = parse_stmt_while();
    } else if (match_keyword(keyword_for)) {
        stmt = parse_stmt_for();
    } else if (match_keyword(keyword_switch)) {
        stmt = parse_stmt_switch();
    } else if (is_token(TOKEN_LBRACE)) {
        stmt = new_stmt_block(parse_stmt_block());
    } else if (match_keyword(keyword_break)) {
        expect_token(TOKEN_SEMICOLON);
        stmt = new_stmt_break();
    } else if (match_keyword(keyword_continue)) {
        expect_token(TOKEN_SEMICOLON);
        // Each (new_*) call can be provided with pos within a file for more accurate error signaling
        stmt = new_stmt_continue();
    } else if (match_keyword(keyword_return)) {
        Expr *expr = NULL;
        if (!is_token(TOKEN_SEMICOLON)) {
            expr = parse_expr();
        }
        expect_token(TOKEN_SEMICOLON);
        stmt = new_stmt_return(expr);
    } else {
        stmt = parse_simple_stmt();
        expect_token(TOKEN_SEMICOLON);
    }
    return stmt;
}

EnumItem parse_decl_enum_item(){
    const char *name = parse_name();
    Expr *init = NULL;
    if (match_token(TOKEN_ASSIGN)){
        init = parse_expr();
    }
    return (EnumItem){name, init};
}

Decl *parse_decl_enum() {
    const char *name = NULL;
    if (is_token(TOKEN_NAME)){
        name = parse_name();
    }
    Typespec *type = NULL;
    if (match_token(TOKEN_ASSIGN)){
        type = parse_type();
    }
    expect_token(TOKEN_LBRACE);
    EnumItem *items = NULL;
    while (!is_token(TOKEN_RBRACE)){
        buf_push(items, parse_decl_enum_item());
        if(!match_token(TOKEN_COMMA)){
            break;
        }
    }
    expect_token(TOKEN_RBRACE);
    return new_decl_enum(name, type, items, buf_len(items));
}

AggregateItem parse_decl_aggregate_item();

Aggregate *parse_aggregate(AggregateKind kind){
    expect_token(TOKEN_LBRACE);
    AggregateItem *items = NULL;
    while (!is_token_eof() && !is_token(TOKEN_RBRACE)){
        buf_push(items, parse_decl_aggregate_item());
    }
    expect_token(TOKEN_RBRACE);
    return new_aggregate(kind, items, buf_len(items));
}


AggregateItem parse_decl_aggregate_item() {
    if (match_keyword(keyword_struct)){
        return (AggregateItem) {
            .kind = AGGREGATE_ITEM_SUBAGGREGATE,
            .subaggregate = parse_aggregate(AGGREGATE_STRUCT)
        };
    } else if (match_keyword(keyword_union)){
        return (AggregateItem) {
            .kind = AGGREGATE_ITEM_SUBAGGREGATE,
            .subaggregate = parse_aggregate(AGGREGATE_UNION),
        };
    } else {
        const char **names = NULL;
        buf_push(names, parse_name());
        while (match_token(TOKEN_COMMA)){
            buf_push(names, parse_name());
        }
        expect_token(TOKEN_COLON);
        Typespec *type = parse_type();
        expect_token(TOKEN_SEMICOLON);
        return (AggregateItem){
            .kind = AGGREGATE_ITEM_FIELD,
            .names = names,
            .num_names = buf_len(names),
            .type = type,
        };
    }
}

Decl *parse_decl_aggregate(DeclKind kind) {
    assert(kind == DECL_STRUCT || kind == DECL_UNION);
    const char *name = NULL;
    AggregateKind aggregate_kind = kind == DECL_STRUCT ? AGGREGATE_STRUCT : AGGREGATE_UNION;
    if (match_token(TOKEN_SEMICOLON)){
        Decl *decl = new_decl_aggregate(kind, name, new_aggregate(aggregate_kind, NULL, 0));
        decl->is_incomplete = true;
        return decl;
    } else {
        return new_decl_aggregate(kind, name, parse_aggregate(aggregate_kind));
    }
}

Decl *parse_decl_const() {
    const char *name = parse_name();
    Typespec *type = NULL;
    if (match_token(TOKEN_COLON)){
        type = parse_type();
    }
    expect_token(TOKEN_ASSIGN);
    Expr *expr = parse_expr();
    expect_token(TOKEN_SEMICOLON);
    return new_decl_const(name, type, expr);
}

Decl *parse_decl_var() {
    const char *name = parse_name();
    if (match_token(TOKEN_ASSIGN)){
        Expr *expr = parse_expr();
        expect_token(TOKEN_SEMICOLON);
        // We don't know a type of a variable yet
        return new_decl_var(name, NULL, expr);
    } else if (match_token(TOKEN_COLON)){
        Typespec *type = parse_type();
        Expr *expr = NULL;
        if (match_token(TOKEN_ASSIGN)){
            expr = parse_expr();
        }
        expect_token(TOKEN_SEMICOLON);
        return new_decl_var(name, type, expr);
    } else {
        // Error: Expected : or = after var
        assert(0);
        return NULL;
    }
}

FuncParam parse_decl_func_param() {
    const char *name = parse_name();
    expect_token(TOKEN_COLON);
    Typespec *type = parse_type();
    return (FuncParam){name, type};
}

Decl *parse_decl_func(){
    const char *name = parse_name();
    expect_token(TOKEN_LPAREN);
    FuncParam *params = NULL;
    bool has_varargs = false;
    Typespec *varargs_type = NULL;
    if (!is_token(TOKEN_RPAREN)){
        buf_push(params, parse_decl_func_param());
        while (match_token(TOKEN_COMMA)){
            if (match_token(TOKEN_ELLIPSIS)){
                if (has_varargs){
                    // Error: Multiple ellipsis in function declaration
                    assert(0);
                }
                if (!is_token(TOKEN_RPAREN)){
                    varargs_type = parse_type();
                }
                has_varargs = true;
            } else {
                if (has_varargs){
                    // Error: Ellipsis must be last paremter in function delcaration
                    assert(0);
                }
                buf_push(params, parse_decl_func_param());
            }
        }
    }
    expect_token(TOKEN_RPAREN);
    Typespec *ret_type = NULL;
    if (match_token(TOKEN_COLON)){
        ret_type = parse_type();
    }
    StmtList block = {0};
    bool is_incomplete;
    if (match_token(TOKEN_SEMICOLON)){
        is_incomplete = true;
    } else {
        block = parse_stmt_block();
        is_incomplete = false;
    }
    Decl *decl =new_decl_func(name, params, buf_len(params), ret_type, has_varargs, varargs_type, block);
    decl->is_incomplete = is_incomplete;
    return decl;
}

Decl *parse_decl_opt(){
    if (match_keyword(keyword_enum)){
        return parse_decl_enum();
    } else if (match_keyword(keyword_struct)){
        return parse_decl_aggregate(DECL_STRUCT);
    } else if (match_keyword(keyword_union)){
        return parse_decl_aggregate(DECL_UNION);
    } else if (match_keyword(keyword_const)){
        return parse_decl_const();
    } else if (match_keyword(keyword_func)){
        return parse_decl_func();
    } else if (match_keyword(keyword_var)){
        return parse_decl_var();
    } else {
        return NULL;
    }
}