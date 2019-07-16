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
    TOKEN_GTEQ,

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
    TOKEN_RSHIFT_ASSIGN,
    TOKEN_XOR_ASSIGN,
    TOKEN_OR_ASSIGN,
    TOKEN_AND_ASSIGN,
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

#define CASE1(c, k) \
    case c: \
        token.kind = k; \
        stream++; \
        break

#define CASE2(c1, k1, c2, k2) \
    case c1: \
        token.kind = k1; \
        stream++; \
        if (*stream == c2) { \
            token.kind = k2; \
            stream++; \
        } \
        break
        
#define CASE3(c1, k1, c2, k2, c3, k3) \
    case c1: \
        token.kind = k1; \
        stream++; \
        if (*stream == c2) { \
            token.kind = k2; \
            stream++; \
        } else if (*stream == c3) { \
            token.kind = k3; \
            stream++; \
        } \
        break

void read_token() {
repeat:
    token.start = stream;
    switch (*stream) {
        case '\t': case '\b': case '\n': case '\v': case '\r': case ' ':
            stream++;
            goto repeat;
            break;
        case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9': case '0': {

            // CLEANUP
            // TODO add string and char literals, add floating point numbers 
            int64_t val = 0;
            while (isdigit(*stream)) {
                val += *stream - '0';
                val *= 10;
                stream++;
            }
            token.kind = TOKEN_INT;
            token.int_val = val / 10;
            break;
        }
        case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h': case 'i': case 'j':
        case 'k': case 'l': case 'm': case 'n': case 'o': case 'p': case 'q': case 'r': case 's': case 't':
        case 'u': case 'v': case 'w': case 'x': case 'y': case 'z':
        case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I': case 'J':
        case 'K': case 'L': case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T':
        case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z':
        case '_': {
            const char *start = stream;
            while (isalnum(*stream) || *stream == '_') {
                stream++;
            }
            token.name = str_intern_range(start, stream);
            token.kind = is_keyword(token.name) ? TOKEN_KEYWORD : TOKEN_NAME;
            break;
        }
        case '<': {
            token.kind = TOKEN_LT;
            stream++;
            if (*stream == '<') {
                token.kind = TOKEN_LSHIFT;
                stream++;
                if (*stream == '=') {
                    token.kind = TOKEN_LSHIFT_ASSIGN;
                    stream++;
                }
            } else if (*stream == '=') {
                token.kind = TOKEN_LTEQ;
                stream++;
            }
            break;
        }
        case '>': {
            token.kind = TOKEN_GT;
            stream++;
            if (*stream == '>') {
                token.kind = TOKEN_RSHIFT;
                stream++;
                if (*stream == '=') {
                    token.kind = TOKEN_RSHIFT_ASSIGN;
                    stream++;
                }
            
            } else if (*stream == '=') {
                token.kind = TOKEN_GTEQ;
                stream++;
            }
            break;
        }
        CASE1('{', TOKEN_LBRACE);
        CASE1('}', TOKEN_RBRACE);
        CASE1('[', TOKEN_LBRACKET);
        CASE1(']', TOKEN_RBRACKET);
        CASE1('(', TOKEN_LPAREN);
        CASE1(')', TOKEN_RPAREN);
        CASE1('\0', TOKEN_EOF);
        CASE1(',', TOKEN_COMMA);
        CASE1(';', TOKEN_SEMICOLON);
        CASE1('.', TOKEN_DOT);
        CASE1('?', TOKEN_QUESTION);
        CASE1('!', TOKEN_NOT);
        CASE1('~', TOKEN_NEG);
        CASE2('/', TOKEN_DIV, '=', TOKEN_DIV_ASSIGN);
        CASE2('%', TOKEN_MOD, '=', TOKEN_MOD_ASSIGN);
        CASE2('*', TOKEN_MUL, '=', TOKEN_MUL_ASSIGN);
        CASE2('^', TOKEN_XOR, '=', TOKEN_XOR_ASSIGN);
        CASE2(':', TOKEN_COLON, '=', TOKEN_COLON_ASSIGN);
        CASE2('=', TOKEN_ASSIGN, '=', TOKEN_EQ);
        CASE3('&', TOKEN_AND, '=', TOKEN_AND_ASSIGN, '&', TOKEN_AND_AND);
        CASE3('|', TOKEN_OR, '=', TOKEN_OR_ASSIGN, '|', TOKEN_OR_OR);
        CASE3('+', TOKEN_ADD, '=', TOKEN_ADD_ASSIGN, '+', TOKEN_INC);
        CASE3('-', TOKEN_SUB, '=', TOKEN_SUB_ASSIGN, '-', TOKEN_DEC);
    }
    token.end = stream;
}

void init_stream(const char *str) {
    stream = str;
    read_token();
}

#define my_for(last) for (int i = 0; i < last; i++)

#define as_int(val) \
    read_token(); \
    assert(token.kind == TOKEN_INT); \
    assert(token.int_val == val)

#define as_name(n) \
    read_token(); \
    assert(token.kind == TOKEN_NAME); \
    assert(str_intern(token.name) == str_intern(n))

#define as_keyword(n) \
    read_token(); \
    assert(token.kind == TOKEN_KEYWORD); \
    assert(str_intern(token.name) == str_intern(n))

#define as_kind(k) \
    read_token(); \
    assert(token.kind == k)

void test_lex() {
    init_stream("this is a good string");
    my_for(4)
        read_token();
    init_stream("  number 1234 gjj1234 009000998");
    my_for(3)
        read_token();
    init_stream("++ += (1 + 2)nnn");
    assert(token.kind == TOKEN_INC);
    as_kind(TOKEN_ADD_ASSIGN);
    as_kind(TOKEN_LPAREN);
    as_int(1);
    as_kind(TOKEN_ADD);
    as_int(2);
    as_kind(TOKEN_RPAREN);
    as_name("nnn");

    init_stream("if while(x < 3) x++;");
    assert(token.kind == TOKEN_KEYWORD);
    assert(str_intern(token.name) == str_intern("if"));
    assert(str_intern(token.name) != str_intern("rand"));
    as_keyword("while");
    as_kind(TOKEN_LPAREN);
    as_name("x");
    as_kind(TOKEN_LT);
    as_int(3);
    as_kind(TOKEN_RPAREN);
    as_name("x");
    as_kind(TOKEN_INC);
    as_kind(TOKEN_SEMICOLON);

    init_stream("x : int = 212; y : int = 23; if (x == y) y = 0;");
    assert(token.kind == TOKEN_NAME);
    assert(str_intern(token.name) == str_intern("x"));
    as_kind(TOKEN_COLON);
    as_name("int");
    as_kind(TOKEN_ASSIGN);
    as_int(212);
    as_kind(TOKEN_SEMICOLON);
    as_name("y");
    as_kind(TOKEN_COLON);
    as_name("int");
    as_kind(TOKEN_ASSIGN);
    as_int(23);
    as_kind(TOKEN_SEMICOLON);
    as_keyword("if");
    as_kind(TOKEN_LPAREN);
    as_name("x");
    as_kind(TOKEN_EQ);
    as_name("y");
    as_kind(TOKEN_RPAREN);
    as_name("y");
    as_kind(TOKEN_ASSIGN);
    as_int(0);
    as_kind(TOKEN_SEMICOLON);
}