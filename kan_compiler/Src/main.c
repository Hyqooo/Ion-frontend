#define _CRT_SECURE_NO_WARNINGS

#include <assert.h>
#include <ctype.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <math.h>

#include "util.c"
#include "lex.c"
#include "ast.h"
#include "ast.c"
#include "parse.c"
#include "print.c"

void test_keywords() {
    assert(is_keyword(str_intern("while")) == true);
    assert(is_keyword(str_intern("somethingelse")) == false);
}

void parse_test(){
    const char *decls[] = {
        "var x: char[256] = {1,2,3, ['a'] = 4}"
    };
    for (const char **it = decls; it != decls + sizeof(decls)/sizeof(*decls); it++){
        init_stream(*it);
        Decl *decl = parse_decl_opt();
        print_decl(decl);
        printf("\n");
    }
}

int main() {
    init_keywords();
    test_keywords();
    test_lex();
    
    token.kind = TOKEN_SUB;
    expect_token(TOKEN_SUB);
    
    buf_test();
    str_intern_test();
    parse_test();
    system("pause");
}