void print_typespec(Typespec *type);
void print_decl(Decl *decl);
void print_stmt_block(StmtList block);

int indent;

void print_newline(){
    printf("\n%.*s", 2*indent, "                                                                      ");
}

void print_expr(Expr *expr){
    Expr *e = expr;
    switch(e->kind){
        case EXPR_INT:{
            printf("%llu", e->int_lit.val);
            break;
        }
        case EXPR_FLOAT:{
            printf("%f", e->float_lit.val);
            break;
        }
        case EXPR_STRING:{
            printf("\"%s\"", e->string_lit.val);
            break;
        }
        case EXPR_NAME:{
            printf("%s", e->name);
            break;
        }
        case EXPR_CAST:{
            printf("(cast ");
            print_typespec(e->cast.type);
            printf(" ");
            print_expr(e->cast.expr);
            printf(")");
            break;
        }
        case EXPR_CALL:{
            printf("(");
            print_expr(e->call.expr);
            for (Expr **it = e->call.args; it != e->call.args + e->call.num_args; it++){
                printf(" ");
                print_expr(*it);
            }
            printf(" ");
            break;
        }
        case EXPR_INDEX:{
            printf("(index ");
            print_expr(e->index.expr);
            printf(" ");
            print_expr(e->index.index);
            printf(")");
            break;
        }
        case EXPR_FIELD:{
            printf("(field ");
            print_expr(e->field.expr);
            printf(" %s", e->field.name);
            break;
        }
        case EXPR_COMPOUND:{
            printf("(compound ");
            if (e->compound.type){
                print_typespec(e->compound.type);
            } else {
                printf("nil");
            }
            for (CompoundField *it = e->compound.fields; it != e->compound.fields + e->compound.num_fields; it++){
                printf(" ");
                if (it->kind == FIELD_DEFAULT){
                    printf("(nil ");
                } else if(it->kind == FIELD_NAME){
                    printf("(name %s ", it->name);
                } else {
                    assert(it->kind == FIELD_INDEX);
                    printf("(index ");
                    print_expr(it->index);
                    printf(" ");
                }
                print_expr(it->init);
                printf(")");
            }
            printf(")");
            break;
        }
        case EXPR_UNARY:{
            printf("(%s ", token_kind_name(e->unary.op));
            print_expr(e->unary.expr);
            printf(")");
            break;
        }
        case EXPR_BINARY:{
            printf("(%s ", token_kind_name(e->binary.op));
            print_expr(e->binary.left);
            printf(" ");
            print_expr(e->binary.right);
            printf(")");
            break;
        }
        case EXPR_TERNARY:{
            printf("(? ");
            print_expr(e->ternary.cond);
            printf(" ");
            print_expr(e->ternary.then_expr);
            printf(" ");
            print_expr(e->ternary.else_expr);
            printf(")");
            break;
        }
        default:{
            assert(0);
            break;
        }
    }
}

void print_aggregate_decl(Decl *decl){
    Decl *d = decl;
    switch(d->kind){
        case DECL_ENUM:{
            printf("(enum %s", d->name);
            indent++;
            for (EnumItem *it = d->enum_decl.items; it != d->enum_decl.items + d->enum_decl.num_items; it++){
                print_newline();
                printf("(%s ", it->name);
                if (it->init){
                    print_expr(it->init);
                } else {
                    printf("nil");
                }
                printf(")");
            }
            indent--;
            printf(")");
            break;
        }
        case DECL_STRUCT:{
            printf("(struct %s", d->name);
            indent++;
            print_aggregate_decl(d);
            indent--;
            printf(")");
            break;
        }
        case DECL_UNION:{
            printf("(union %s", d->name);
            indent++;
            print_aggregate_decl(d);
            indent--;
            printf(")");
            break;
        }
        case DECL_VAR:{
            printf("(var %s ", d->name);
            if (d->var_decl.type){
                print_typespec(d->var_decl.type);
            } else {
                printf("nil");
            }
            printf(" ");
            if (d->var_decl.expr){
                print_expr(d->var_decl.expr);
            } else {
                printf("nil");
            }
            printf(")");
            break;
        }
        case DECL_CONST:{
            printf("(const %s ", d->name);
            print_expr(d->const_decl.expr);
            printf(")");
            break;
        }
        case DECL_FUNC:{
            printf("(func %s ", d->name);
            printf("(");
            for (FuncParam *it = d->func_decl.params; it != d->func_decl.params + d->func_decl.num_params; it++){
                printf(" %s ", it->name);
                print_typespec(it->type);
            }
            printf(" ) ");
            if (d->func_decl.ret_type){
                print_typespec(d->func_decl.ret_type);
            } else {
                printf("nil");
            }
            indent++;
            print_newline();
            print_stmt_block(d->func_decl.block);
            indent--;
            printf(")");
            break;
        }
        default:{
            assert(0);
            break;
        }
    }
}

void print_typespec(Typespec *type){
    Typespec *t = type;
    switch(t->kind){
        case TYPESPEC_NAME:{
            for (const char **it = t->names; it != t->names + t->num_names; it++){
                printf("%s", *it);
            }
            break;
        }
        case TYPESPEC_FUNC:{
            printf("(func (");
            for (Typespec **it = t->func.args; it != t->func.args + t->func.num_args; it++){
                printf(" ");
                print_typespec(*it);
            }
            break;
        }
        default:{
            assert(0);
            break;
        }
    }
}

void print_stmt(Stmt *stmt){
    Stmt *s = stmt;
    switch(s->kind){
        case STMT_DECL:{
            print_decl(s->decl);
            break;
        }
        case STMT_RETURN:{
            printf("(return");
            if (s->expr){
                printf(" ");
                print_expr(s->expr);
            }
            printf(")");
            break;
        }
        case STMT_BREAK:{
            printf("(break");
            break;
        }
        case STMT_CONTINUE:{
            printf("(continue");
            break;
        }
        case STMT_BLOCK:{
            print_stmt_block(s->block);
            break;
        }
        case STMT_IF:{
            printf("(if");
            print_expr(s->stmt_if.cond);
            indent++;
            print_newline();
            print_stmt_block(s->stmt_if.then_block);
            for (ElseIf *it = s->stmt_if.elseifs; it != s->stmt_if.elseifs + s->stmt_if.num_elseifs; it++){
                print_newline();
                printf("elseif ");
                print_expr(it->cond);
                print_newline(it->cond);
                print_newline();
                print_stmt_block(it->block);
            }
            if (s->stmt_if.else_block.num_stmts != 0){
                print_newline();
                printf("else ");
                print_newline();
                print_stmt_block(s->stmt_if.else_block);
            }
            indent--;
            printf(")");
            break;
        }
        case STMT_WHILE:{
            printf("(while ");
            print_expr(s->stmt_while.cond);
            indent++;
            print_newline();
            print_stmt_block(s->stmt_while.block);
            indent--;
            printf(")");
            break;
        }
        case STMT_FOR:{
            printf("(for ");
            print_stmt(s->stmt_for.init);
            print_expr(s->stmt_for.cond);
            print_stmt(s->stmt_for.next);
            indent++;
            print_newline();
            print_stmt_block(s->stmt_for.block);
            indent--;
            printf(")");
            break;
        }
        case STMT_SWITCH:{
            printf("(switch ");
            print_expr(s->stmt_switch.expr);
            indent++;
            for (SwitchCase *it = s->stmt_switch.cases; it != s->stmt_switch.cases + s->stmt_switch.num_cases; it++){
                print_newline();
                printf("(case (%s", it->is_default ? " default" : "");
                print_stmt_block(it->block);
                printf(" ) ");
                indent++;
                print_newline();
                print_stmt_block(it->block);
                indent--;
            }
            indent--;
            printf(")");
            break;
        }
        case STMT_ASSIGN:{
            printf("(%s ", token_kind_names[s->assign.op]);
            print_expr(s->assign.left);
            if (s->assign.right){
                printf(" ");
                print_expr(s->assign.right);
            }
            printf(")");
            break;
        }
        case STMT_INIT:{
            printf("(:= %s ", s->init.name);
            print_expr(s->init.expr);
            printf(")");
            break;
        }
        case STMT_EXPR:{
            print_expr(s->expr);
            break;
        }
        default:{
            assert(0);
            break;
        }
    }
}

void print_stmt_block(StmtList block){
    printf("(block");
    indent++;
    for (Stmt **it = block.stmts; it != block.stmts + block.num_stmts; it++){
        print_newline();
        print_stmt(*it);
    }
    indent--;
    printf(")");
}

void print_decl(Decl *decl){
    switch(decl->kind){
        case DECL_ENUM:{
            printf("(enum %s", decl->name);
            indent++;
            for (EnumItem *it = decl->enum_decl.items; it != decl->enum_decl.items + decl->enum_decl.num_items; it++){
                print_newline();
                printf("(%s ", it->name);
                if (it->init){
                    print_expr(it->init);
                } else {
                    printf("nil");
                }
                printf(")");
            }
            indent--;
            printf(")");
            break;
        }
        case DECL_STRUCT:{
            printf("(struct %s", decl->name);
            indent++;
            print_aggregate_decl(decl);
            indent--;
            printf(")");
            break;
        }
        case DECL_UNION: {
            printf("(union %s", decl->name);
            indent++;
            print_aggregate_decl(decl);
            indent--;
            printf(")");
            break;
        }
        case DECL_VAR: {
            printf("(var %s ", decl->name);
            if (decl->var_decl.type){
                print_typespec(decl->var_decl.type);
            } else {
                printf("nil");
            }
            printf(" ");
            if (decl->var_decl.expr){
                print_expr(decl->var_decl.expr);
            } else {
                printf("nil");
            }
            printf(")");
            break;
        }
        case DECL_CONST:{
            printf("(typedef %s ", decl->name);
            print_expr(decl->const_decl.expr);
            printf(")");
            break;
        }
        case DECL_FUNC:{
            printf("(func %s ", decl->name);
            printf("(");
            for (FuncParam *it = decl->func_decl.params; it != decl->func_decl.params + decl->func_decl.num_params; it++){
                printf(" %s ", it->name);
                print_typespec(it->type);
            }
            printf(" ) ");
            if (decl->func_decl.ret_type){
                print_typespec(decl->func_decl.ret_type);
            } else {
                printf("nil");
            }
            indent++;
            print_newline();
            print_stmt_block(decl->func_decl.block);
            indent--;
            printf(")");
            break;
        }
        default:{
            assert(0);
            break;
        }
    }
}