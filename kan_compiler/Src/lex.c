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
    TOKEN_FLOAT,
    TOKEN_STRING,
    TOKEN_CHAR,
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
        double float_val;
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

// This array can be extended to hexadecimal numbers
uint8_t char_to_digit[256] = {
    ['0'] = 0,
    ['1'] = 1,
    ['2'] = 2,
    ['3'] = 3,
    ['4'] = 4,
    ['5'] = 5,
    ['6'] = 6,
    ['7'] = 7,
    ['8'] = 8,
    ['9'] = 9
};

uint8_t escape_to_char[256] = {
    ['n'] = '\n',
    ['r'] = '\r',
    ['b'] = '\b',
    ['v'] = '\v',
    ['t'] = '\t',
    ['a'] = '\a',
    ['0'] = '\0',
};

void process_str() {
    assert(*stream == '"');
    stream++;
    char *str = NULL;
    while (*stream && *stream != '"') {
        char val = *stream;
        if (val == '\n') {
            // Error: string literal cannot contain newline
            assert(0);
            break;
        } else if (val == '\\') {
            stream++;
            val = escape_to_char[*(unsigned char*)stream];
            if (val == 0 && *stream != '0') {
                // Error: Invalid string literal escape
                assert(0);
            }
        }
        buf_push(str, val);
        stream++;
    }
    if (*stream) {
        assert(*stream == '"');
        stream++;
    } else {
        // Error: Unxpected end of file withing string literal.
        assert(0);
    }
    buf_push(str, 0);
    token.kind = TOKEN_STRING;
    token.str_val = str;
}

void process_char() {
    assert(*stream == '\'');
    stream++;
    char val = 0;
    if (*stream == '\'') {
        // Error: char literal cannot be empty
        stream++;
        assert(0);
    } else if (*stream == '\n') {
        // Error: char literal cannot contain newline
        assert(0);
    } else if (*stream == '\\') {
        stream++;
        val = escape_to_char[*(unsigned char*)stream];
        if (val == 0 && *stream != 0) {
            // Error: Invalid char literal escape
            assert(0);
        }
        stream++;
    } else {
        val = *stream;
        stream++;
    }
    if (*stream != '\'') {
        // Error: expecting closing char quote 
        assert(0);
    } else {
        stream++;
    }
    token.kind = TOKEN_CHAR; // TODO: check if there's real need in token.mod as bitwise did.
    token.int_val = val;
}

void process_int() {
    uint64_t val = 0;
    while (1) {
        uint64_t digit = char_to_digit[*(unsigned char *)stream];
        if (digit == 0 && *stream != '0') {
            break;
        }
        if (val > (UINT64_MAX - digit) / 10) {
            // integer overflow
            while (isdigit(*stream)) {
                stream++;
            }
            val = 0;
            assert(0);
            break;
        }
        val = val * 10 + digit;
        stream++;
    }
    token.kind = TOKEN_INT;
    token.int_val = val;
}

void process_float() {
    const char *start = stream;
    while (isdigit(*stream)) {
        stream++;
    }
    if (*stream == '.') {
        stream++;
    }
    while (isdigit(*stream)) {
        stream++;
    }
    if (tolower(*stream) == 'e') {
        stream++;
        if (*stream == '+' || *stream == '-') {
            stream++;
        }
        if (!isdigit(*stream)) {
            // syntax error
            assert(0);
        }
        while (isdigit(*stream)) {
            stream++;
        }
    }
    double val = strtod(start, NULL);
    if (val == HUGE_VAL) {
        // TODO: test this
        assert(0);
        exit(1);
    }
    token.kind = TOKEN_FLOAT;
    token.float_val = val;
}

void read_token() {
repeat:
    token.start = stream;
    switch (*stream) {
        case '\t': case '\b': case '\n': case '\v': case '\r': case ' ':
            while (isspace(*stream)) {
                stream++;
            }
            goto repeat;
            break;
        case '\'': {
            process_char();
            break;
        }
        case '"': {
            process_str();
            break;
        }
        case '.': {
            if (isdigit(stream[1])) {
                process_float();
            } else {
                token.kind = TOKEN_DOT;
                stream++;
            }
            break;
        }
        case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9': case '0': {
            while (isdigit(*stream)) {
                stream++;
            }
            char c = *stream;
            stream = token.start;
            if (c == '.' || tolower(c) == 'e') {
                process_float();
            } else {
                process_int();
            }
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
        default: 
            assert(0);
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

#define as_float(val) \
    read_token(); \
    assert(token.kind == TOKEN_FLOAT); \
    assert(token.float_val == val)

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

#define as_char_literal(ch) \
    read_token(); \
    assert(token.kind == TOKEN_CHAR); \
    assert(token.int_val == (int64_t)ch)

#define as_string_literal(string) \
    read_token(); \
    assert(token.kind == TOKEN_STRING); \
    assert(str_intern(token.str_val) == str_intern(string))

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

    init_stream("'a' \"sadf12\" 'd' ");
    assert(token.kind == TOKEN_CHAR); 
    assert(token.int_val == (int64_t)'a');
    as_string_literal("sadf12");
    as_char_literal('d');
    
    init_stream("213.34 34 23e3  .34");
    assert(token.kind == TOKEN_FLOAT);
    assert(token.float_val == 213.34);
    as_int(34);
    as_float(23e3);
    as_float(.34);
}