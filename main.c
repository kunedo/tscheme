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
#include <stdlib.h>
#include <stdbool.h>
#include <unistd.h>
#include <getopt.h>
#include <setjmp.h>
#include <signal.h>

#include "tscheme.h"

#ifndef INIT_FILE
#define INIT_FILE "./init.scm"
#endif

static char *init_file = INIT_FILE;
static bool init_loaded = false;

void interrupt_handler(int sig) {
    fprintf(stderr, "Interrupted\n");
    longjmp(error_return, NON_FATAL);
}

void usage(char *me) {
    fprintf(stderr, "usage: %s [-i init_file]\n", me);
    exit(EXIT_FAILURE);
}

int main(int argc, char *argv[]) {
    int ch;
    SCM start;
    char *me = argv[0];

    while ((ch = getopt(argc, argv, "i:")) != -1) {
        switch (ch) {
        case 'i':
            init_file = optarg;
            break;
        default:
            usage(me);
        }
    }
    argc -= optind;
    if (argc > 0) usage(me);

    stack_start = (SCM)&start;

    init_storage(DEFAULT_NUMCELLS);
    init_subrs();
    init_io_subrs();

    printf(BANNER);

    switch (setjmp(error_return)) {
    case FATAL:
        exit(EXIT_FAILURE);
    case NON_FATAL:
        if (!init_loaded) {
            fprintf(stderr, "Error in init file.\n");
            exit(EXIT_FAILURE);
        }
        break;
    default:
        break;
    }

    signal(SIGINT, interrupt_handler);

    if (!init_loaded) {
        do_load(init_file);
        init_loaded = true;
    }

    /* (sys:toplevel) */
    evaluate(mk_pair(sym_toplevel, NIL), NIL);

    exit(EXIT_SUCCESS);
}
