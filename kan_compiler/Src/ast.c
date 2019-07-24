Arena ast_arena;

size_t ast_memory_usage;

void *ast_alloc(size_t size){
    assert(size != 0);
    void *ptr = arena_alloc(&ast_arena, size);
    memset(ptr, 0, size);
    ast_memory_usage += size;
    return ptr;
}

void *ast_dup(const void *src, size_t size){
    if (size == 0){
        return NULL;
    }
    void *ptr = arena_alloc(&ast_arena, size);
    memcpy(ptr, src, size);
    return ptr;
}

#define AST_DUP(x) ast_dup(x, num_##x * sizeof(*x)) 

Typespec *new_typespec(TypespecKind kind){
    Typespec *t = ast_alloc(sizeof(Typespec));
    t->kind = kind;
    return t;
}

Typespec *new_typespec_func(Typespec **args, size_t num_args, Typespec *ret){
    Typespec *t = new_typespec(TYPESPEC_FUNC);
    t->func.args = AST_DUP(args);
    t->func.num_args = num_args;
    t->func.ret = ret;
    return t;
}

Typespec *new_typespec_ptr(Typespec *elem){
    Typespec *t = new_typespec(TYPESPEC_NAME);
    t->ptr.elem = elem;
    return t;
}

Typespec *new_typespec_array(Typespec *elem, Expr *size){
    Typespec *t = new_typespec(TYPESPEC_ARRAY);
    t->array.elem = elem;
    t->array.size = size;
    return t;
}

Typespec *new_typespec_name(const char *name){
    Typespec *t = new_typespec(TYPESPEC_NAME);
    t->name = name;
    return t;
}

Expr *new_expr(ExprKind kind){
    Expr *e = ast_alloc(sizeof(Expr));
    e->kind = kind;
    return e;
}

Expr *new_ternary_expr(Expr *cond, Expr *then_expr, Expr *else_expr){
    Expr *e = new_expr(EXPR_TERNARY);
    e->ternary.cond = cond;
    e->ternary.then_expr = then_expr;
    e->ternary.else_expr = else_expr;
    return e;
}

Expr *new_binary_expr(TokenKind op, Expr *left, Expr *right){
    Expr *e = new_expr(EXPR_BINARY);
    e->binary.op = op;
    e->binary.left = left;
    e->binary.right = right;
    return e;
}

Expr *new_unary_expr(TokenKind op, Expr *expr){
    Expr *e = new_expr(EXPR_UNARY);
    e->unary.op = op;
    e->unary.expr = expr;
    return e;
}

Expr *new_field_expr(Expr *expr, const char *name){
    Expr *e = new_expr(EXPR_FIELD);
    e->field.expr = expr;
    e->field.name = name;
    return e;
}

Expr *new_index_expr(Expr *expr, Expr *index){
    Expr *e = new_expr(EXPR_INDEX);
    e->index.expr = expr;
    e->index.index = index;
    return e;
}

Expr *new_call_expr(Expr *expr, Expr **args, size_t num_args){
    Expr *e = new_expr(EXPR_CALL);
    e->call.expr = expr;
    // NOTE: Why do we need to duplicate args?
    e->call.args = AST_DUP(args);
    e->call.num_args = num_args;
    return e;
}

Expr *new_cast_expr(Typespec *type, Expr *expr){
    Expr *e = new_expr(EXPR_CAST);
    e->cast.type = type;
    e->cast.expr = expr;
    return e;
}

Expr *new_name_expr(const char *name){
    Expr *e = new_expr(EXPR_NAME);
    e->name = name;
    return e;
}

Expr *new_str_expr(const char *str_val){
    Expr *e = new_expr(EXPR_STRING);
    e->str_val = str_val;
    return e;
}

Expr *new_float_expr(const char *start, const char *end, double val){
    Expr *e = new_expr(EXPR_FLOAT);
    e->float_val = val;
    return e;
}

Expr *new_int_expr(unsigned long long val){
    Expr *e = new_expr(EXPR_INT);
    e->int_val = val;
    return e;
}

Expr *new_compound_expr(Typespec *type, CompoundField *fields, size_t num_fields){
    Expr *e = new_expr(EXPR_COMPOUND);
    e->compound.type = type;
    e->compound.fields = AST_DUP(fields);
    e->compound.num_fields = num_fields;
    return e;
}

Stmt *new_stmt(StmtKind kind){
    Stmt *s = ast_alloc(sizeof(Stmt));
    s->kind = kind;
    return s;
}

Stmt *new_stmt_return(Expr *expr){
    Stmt *s = new_stmt(STMT_RETURN);
    s->expr = expr;
    return s;
}

Stmt *new_stmt_continue(){
    return new_stmt(STMT_CONTINUE);
}

Stmt *new_stmt_break(){
    return new_stmt(STMT_BREAK);
}

Stmt *new_stmt_block(StmtList block){
    Stmt *s = new_stmt(STMT_BLOCK);
    s->block = block;
    return s;
}

Stmt *new_stmt_switch(Expr *expr, SwitchCase *cases, size_t num_cases){
    Stmt *s = new_stmt(STMT_SWITCH);
    s->stmt_switch.expr = expr;
    s->stmt_switch.cases = AST_DUP(cases);
    s->stmt_switch.num_cases = num_cases;
    return s;
}

Stmt *new_stmt_for(Stmt *init, Expr *cond, Stmt *next, StmtList block){
    Stmt *s = new_stmt(STMT_FOR);
    s->stmt_for.init = init;
    s->stmt_for.cond = cond;
    s->stmt_for.next = next;
    s->stmt_for.block = block;
    return s;
}

Stmt *new_stmt_while(Expr *cond, StmtList block){
    Stmt *s = new_stmt(STMT_WHILE);
    s->stmt_while.cond = cond;
    s->stmt_while.block = block;
    return s;
}

Stmt *new_stmt_if(Stmt *init, Expr *cond, StmtList then_block, ElseIf *elseifs, size_t num_elseifs, StmtList else_block){
    Stmt *s = new_stmt(STMT_IF);
    s->stmt_if.init = init;
    s->stmt_if.cond = cond;
    s->stmt_if.then_block = then_block;
    s->stmt_if.elseifs = AST_DUP(elseifs);
    s->stmt_if.num_elseifs = num_elseifs;
    s->stmt_if.else_block = else_block;
    return s;
}

Stmt *new_stmt_expr(Expr *expr){
    Stmt *s = new_stmt(STMT_EXPR);
    s->expr = expr;
    return s;
}

Stmt *new_stmt_assign(TokenKind op, Expr *left, Expr *right){
    Stmt *s = new_stmt(STMT_ASSIGN);
    s->assign.op = op;
    s->assign.left = left;
    s->assign.right = right;
    return s;
}

Stmt *new_stmt_init(const char *name, Typespec *type, Expr *expr, bool is_undef){
    Stmt *s = new_stmt(STMT_INIT);
    s->init.name = name;
    s->init.type = type;
    s->init.expr = expr;
    s->init.is_undef = is_undef;
    return s;
}

StmtList new_stmt_list(Stmt **stmts, size_t num_stmts){
    return (StmtList){AST_DUP(stmts), num_stmts};
}

Decl *new_decl(DeclKind kind, const char *name){
    Decl *d = ast_alloc(sizeof(Decl));
    d->kind = kind;
    d->name = name;
    return d;
}

Decl *new_decl_enum(const char *name, Typespec *type, EnumItem *items, size_t num_items){
    Decl *d = new_decl(DECL_ENUM, name);
    d->enum_decl.items = AST_DUP(items);
    d->enum_decl.num_items = num_items;
    return d;
}

Aggregate *new_aggregate(AggregateKind kind, AggregateItem *items, size_t num_items){
    Aggregate *aggregate = ast_alloc(sizeof(Aggregate));
    aggregate->kind = kind;
    aggregate->items = AST_DUP(items);
    aggregate->num_items = num_items;
    return aggregate;
}

Decl *new_decl_aggregate(DeclKind kind, const char *name, AggregateItem *items, size_t num_items){
    assert(kind == DECL_STRUCT || kind == DECL_UNION);
    Decl *d = new_decl(kind, name);
    d->aggregate.items = AST_DUP(items);
    d->aggregate.num_items = num_items;
    return d;
}

Decl *new_decl_const(const char *name, Typespec *type, Expr *expr){
    Decl *d = new_decl(DECL_CONST, name);
    d->const_decl.expr = expr;
    return d;
}

Decl *new_decl_var(const char *name, Typespec *type, Expr *expr){
    Decl *d = new_decl(DECL_VAR, name);
    d->var.type = type;
    d->var.expr = expr;
    return d;
}

Decl *new_decl_func(const char *name, FuncParam *params, size_t num_params, Typespec *ret_type, StmtList block){
    Decl *d = new_decl(DECL_FUNC, name);
    d->func_decl.params = AST_DUP(params);
    d->func_decl.num_params = num_params;
    d->func_decl.ret_type = ret_type;
    d->func_decl.block = block;
    return d;
}