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
#include "resolve.c"

void test_keywords() {
    token.kind = TOKEN_KEYWORD;
    token.name = str_intern("while");
    assert(is_keyword(str_intern("while")) == true);
    assert(is_keyword(str_intern("somethingelse")) == false);
}

void parse_test(){
    const char *decls[] = {
        // NOTE: Isn't that should be resolved?
        "enum = int { TOKEN, POKEN };",
        "enum { };",
        "struct Be { x: Vector; size: int; };",
        "struct Vector { x, y: float; }",
        "var x: int = 3;",
        "var x: char[256] = {1,2,3, [4] = 4};",
        "func main() { return 3; }",
        "func main(argc: int, argv:char):int { return 3; }",
        "func main(argc: int, argv:char):int { x := 14; y := 4; y += x; return y; }",
        "func max(x:int, y:int):int { return x > y ? x : y; }",
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
    
    buf_test();
    str_intern_test();
    parse_test();
    system("pause");
}