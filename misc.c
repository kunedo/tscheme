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

int memq(SCM key, SCM list) {
    SCM l = list;
    while (!IS_NULL(l)) {
        if (!(IS_PAIR(l) || IS_NULL(l))) wta_error ("memq", 2);
        if EQ(key, CAR(l)) return 1;
        l = CDR(l);
    }
    return 0;
}
    
SCM map1(SCM (*f)(SCM), SCM list) {
    SCM l = list;
    if (IS_PAIR(l)) {
        SCM h, p;
        p = CONS(f(CAR(l)), NIL);
        h = p;
        l = CDR(l);
        while (!IS_NULL(l)) {
            SCM n = CONS(f(CAR(l)), NIL);
            CDR(p) = n;
            p = n;
        }
        return h;
    }
    else
        return NIL;
}


SCM last(SCM l) {
    SCM p = l;
    if (!(IS_PAIR(p) || IS_NULL(p)))
        wta_error("last", 1);
    if (IS_PAIR(p))
        while (IS_PAIR(CDR(p)))
            p = CDR(p);
    return p;
}
