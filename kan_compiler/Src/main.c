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

void test_keywords() {
    assert(is_keyword(str_intern("while")) == true);
    assert(is_keyword(str_intern("somethingelse")) == false);
}

int main() {
    init_keywords();
    test_keywords();
    test_lex();

    buf_test();
    str_intern_test();
    system("pause");
}