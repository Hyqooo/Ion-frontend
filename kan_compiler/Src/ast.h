typedef struct Typespec Typespec;
typedef struct Expr Expr;
typedef struct Decl Decl;
typedef struct Stmt Stmt;

typedef enum TypespecKind {
    TYPESPEC_NONE,
    TYPESPEC_NAME,
    TYPESPEC_FUNC,
    TYPESPEC_ARRAY,
    TYPESPEC_PTR, // (?)
    TYPESPEC_CONST,
    TYPESPEC_TUPLE,
} TypespecKind;

struct Typespec {
    TypespecKind kind;
    Typespec *base;
    union {
        struct {
            const char **names;
            size_t num_names;
        };
        Expr *num_elems;
        struct {
            Typespec **args;
            size_t num_args;
            bool has_varargs;
            Typespec *ret;
        } func;
    };
};

typedef enum ExprKind {
    EXPR_NONE,
    EXPR_PAREN,
    EXPR_INT,
    EXPR_FLOAT,
    EXPR_STRING,
    EXPR_NAME,
    EXPR_CAST,
    EXPR_CALL,
    EXPR_INDEX,
    EXPR_FIELD,
    EXPR_COMPOUND,
    EXPR_UNARY,
    EXPR_BINARY,
    EXPR_TERNARY,
    EXPR_MODIFY, // Increment and decrement
}ExprKind;

typedef enum CompoundFieldKind {
    FIELD_DEFAULT,
    FIELD_NAME,
    FIELD_INDEX
}CompoundFieldKind;

typedef struct CompoundField {
    CompoundFieldKind kind;
    Expr *init;
    union {
        const char *name;
        Expr *index;
    };
}CompoundField;

struct Expr {
    ExprKind kind;
    union {
        const char *name;
        struct {
            unsigned long long val;
        }int_lit;
        struct {
            const char *start;
            const char *end;
            double val;
        }float_lit;
        struct {
            const char *val;
        }string_lit;
        struct {
            Expr *expr;
        }paren_expr;
        struct {
            TokenKind op;
            Expr *left;
            Expr *right;
        }binary;
        struct {
            Typespec *type;
            CompoundField *fields;
            size_t num_fields;
        }compound;
        struct {
            Expr *cond;
            Expr *then_expr;
            Expr *else_expr;
        }ternary;
        struct {
            TokenKind op;
            Expr *expr;
        }unary;
        struct {
            TokenKind op;
            bool post;
            Expr *expr;
        }modify;
        struct {
            Expr *expr;
            const char *name;
        }field;
        struct {
            Expr *expr;
            Expr **args;
            size_t num_args;
        }call;
        struct {
            Expr *expr;
            Expr *index;
        }index;
        struct {
            Typespec *type;
            Expr *expr;
        }cast;
    };
};

typedef struct StmtList {
    Stmt **stmts;
    size_t num_stmts;
} StmtList;

typedef struct ElseIf {
    Expr *cond;
    StmtList block;
}ElseIf;

typedef enum StmtKind {
    STMT_NONE,
    STMT_DECL,
    STMT_RETURN,
    STMT_BREAK,
    STMT_CONTINUE,
    STMT_BLOCK,
    STMT_IF,
    STMT_WHILE,
    STMT_FOR,
    STMT_SWITCH,
    STMT_ASSIGN,
    STMT_INIT,
    STMT_EXPR
}StmtKind;

typedef struct SwitchCase SwitchCase;
struct Stmt {
    StmtKind kind;
    union {
        Expr *expr;
        Decl *decl;
        StmtList block;
        struct {
            Stmt *init;
            Expr *cond;
            StmtList then_block;
            ElseIf *elseifs;
            size_t num_elseifs;
            StmtList else_block;
        } stmt_if;
        struct {
            Expr *cond;
            StmtList block;
        } stmt_while;
        struct {
            Expr *expr;
            SwitchCase *cases;
            size_t num_cases;
        } stmt_switch;
        struct {
            Stmt *init;
            Expr *cond;
            Stmt *next;
            StmtList block;
        } stmt_for;
        struct {
            const char *name;
            Typespec *type;
            Expr *expr;
            bool is_undef;
        } init;
        struct {
            TokenKind op;
            Expr *left;
            Expr *right;
        } assign;
    };
};

// I don't understand what is the purpose of this struct
typedef struct SwitchCasePattern {
    Expr *start;
    Expr *end;
} SwitchCasePattern;

struct SwitchCase {
    SwitchCasePattern *patterns;
    size_t num_patterns;
    bool is_default;
    StmtList block;
};

typedef enum DeclKind {
    DECL_NONE,
    DECL_ENUM, 
    DECL_STRUCT,
    DECL_UNION,
    DECL_VAR,
    DECL_CONST,
    DECL_FUNC,
} DeclKind;

typedef enum AggregateItemKind {
    AGGREGATE_ITEM_NONE,
    AGGREGATE_ITEM_FIELD,
    AGGREGATE_ITEM_SUBAGGREGATE
}AggregateItemKind;

typedef struct AggregateItem {
    AggregateItemKind kind;
    union {
        struct {
            const char **names;
            size_t num_names;
            Typespec *type;
        };
        struct Aggregate *subaggregate;
    };
} AggregateItem;

typedef enum AggregateKind {
    AGGREGATE_NONE,
    AGGREGATE_STRUCT,
    AGGREGATE_UNION
} AggregateKind;

typedef struct Aggregate {
    AggregateKind kind;
    AggregateItem *items;
    size_t num_items;
} Aggregate;

typedef struct EnumItem {
    const char *name;
    Expr *init;
} EnumItem;

typedef struct FuncParam {
    const char *name;
    Typespec *type;
} FuncParam;

struct Decl {
    DeclKind kind;
    const char *name;
    bool is_incomplete;
    union {
        Typespec *type;
        EnumItem *items;
        size_t num_items;
    } enum_decl;
    Aggregate *aggregate;
    union {
        Typespec *type;
        Expr *expr;
    } const_decl;
    union {
        Typespec *type;
        Expr *expr;
    } var_decl;
    union {
        FuncParam *params;
        size_t num_params;
        Typespec *ret_type;
        bool has_varargs;
        Typespec *varargs_type;
        StmtList block;
    } func_decl;
};