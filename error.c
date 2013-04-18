/*
 * Tscheme: A Tiny Scheme Interpreter
 * Copyright (c) 1995-2013 Takuo WATANABE (Tokyo Institute of Technology)
 * 
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 * 
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#include <stdio.h>
#include <setjmp.h>

#include "tscheme.h"

jmp_buf error_return;

void wta_error(char *fname, int argno) {
    fprintf(stderr, "ERROR: %s: Wrong type in arg %d\n", fname, argno);
    longjmp(error_return, NON_FATAL);
}

void wna_error(char *fname, int nargs) {
    fprintf(stderr, "ERROR: %s: Wrong number (%d) of args.\n", fname, nargs);
    longjmp(error_return, NON_FATAL);
}

void error0(char *message) {
    fprintf(stderr, "%s", message);
    longjmp(error_return, NON_FATAL);
}

void error1(char *message, char *arg) {
    fprintf(stderr, message, arg);
    longjmp(error_return, NON_FATAL);
}

void fatal_error(char *message) {
    fprintf(stderr, "%s", message);
    longjmp(error_return, FATAL);
}

int check_nargs(char *fname, SCM args, int min, int max) {
    if (!(IS_PAIR(args) || IS_NULL(args)))
        error0("wrong arguments");

    int i;
    for (i = 0; IS_PAIR(args); i++)
        args = CDR(args);

    if (!IS_NULL(args))
        error0("wrong arguments");
    if (!((min <= i) && (i <= max)))
        wna_error(fname, i);

    return i;
}
