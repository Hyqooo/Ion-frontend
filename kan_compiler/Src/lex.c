const char *keyword_if;
const char *keyword_else;
const char *keyword_while;
const char *keyword_break;
const char *keyword_continue;
const char *keyword_switch;
const char *keyword_case;
const char *keyword_default;
const char *keyword_func;
const char *keyword_return;
const char *keyword_enum;
const char *keyword_union;
const char *keyword_struct;
const char *keyword_const;
const char *keyword_var;
const char *keyword_cast;

typedef enum TokenKind {
    TOKEN_EOF,
    TOKEN_COLON,
    TOKEN_LPAREN,
    TOKEN_RPAREN,
    TOKEN_LBRACE,
    TOKEN_RBRACE,
    TOKEN_LBRACKET,
    TOKEN_RBRACKET,
    TOKEN_COMMA,
    TOKEN_DOT,
    TOKEN_QUESTION,
    TOKEN_SEMICOLON,
    TOKEN_KEYWORD,
    TOKEN_INT,
    TOKEN_FLOAT, // Unsupported
    TOKEN_NAME,
    TOKEN_NOT,
    TOKEN_NEG,
    // Multiplicative precedence
    TOKEN_MUL,
    TOKEN_DIV,
    TOKEN_MOD,
    TOKEN_LSHIFT,
    TOKEN_RSHIFT,
    TOKEN_AND,
    // Additive precedence
    TOKEN_ADD,
    TOKEN_SUB,
    TOKEN_XOR,
    TOKEN_OR,
    // Comparative precedence
    TOKEN_EQ,
    TOKEN_NOTEQ,
    TOKEN_LT,
    TOKEN_GT, 
    TOKEN_LTEQ,
    TOKEN_RTEQ,

    TOKEN_AND_AND,
    TOKEN_OR_OR,
    // Assignment operators
    TOKEN_ASSIGN,
    TOKEN_ADD_ASSIGN,
    TOKEN_SUB_ASSIGN,
    TOKEN_MOD_ASSIGN,
    TOKEN_MUL_ASSIGN,
    TOKEN_DIV_ASSIGN,
    TOKEN_LSHIFT_ASSIGN,
    TOKEN_RSHIFT_ASSING,

    TOKEN_INC,
    TOKEN_DEC,
    TOKEN_COLON_ASSIGN
}TokenKind;

// Supports only integer decimal values
typedef struct Token {
    TokenKind kind;
    const char *start;
    const char *end;
    union {
        int64_t int_val;
        const char *name;
        const char *str_val;
    };
}Token;

Token token;
const char *stream;

const char *first_keyword;
const char *last_keyword;
const char **keywords;
#define KEYWORD(name) keyword_##name = str_intern(#name); buf_push(keywords, keyword_##name)

void init_keywords() {
    static bool is_initialized;
    if (is_initialized) {
        return;
    }
    KEYWORD(if);
    KEYWORD(else);
    KEYWORD(while);
    KEYWORD(break);
    KEYWORD(continue);
    KEYWORD(switch);
    KEYWORD(case);
    KEYWORD(default);
    KEYWORD(func);
    KEYWORD(return);
    KEYWORD(enum);
    KEYWORD(union);
    KEYWORD(struct);
    KEYWORD(const);
    KEYWORD(var);
    KEYWORD(cast);
    first_keyword = keyword_if;
    last_keyword = keyword_cast;
    is_initialized = true;
}

inline bool is_keyword(const char *name) {
    return (name >= first_keyword && name <= last_keyword) ? true : false;
}

void read_token() {
    switch (*stream) {
        case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h': case 'i': case 'j':
        case 'k': case 'l': case 'm': case 'n': case 'o': case 'p': case 'q': case 'r': case 's': case 't':
        case 'u': case 'v': case 'w': case 'x': case 'y': case 'z':
        case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I': case 'J':
        case 'K': case 'L': case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T':
        case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z':
        case '_':{
            const char *start = stream;
            while (isalnum(*stream) || *stream == '_') {
                stream++;
            }
            token.kind = is_keyword(str_intern_range(start, stream)) ? TOKEN_KEYWORD : TOKEN_NAME;
        }
    }
}