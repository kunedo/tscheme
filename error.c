/*
  $Id: error.c,v 1.1.1.1 2004/09/09 07:59:50 takuo Exp $
  Tscheme: A Tiny Scheme Interpreter
  (Course material for I425 "Topics on Software Environment")
  Copyright (C) 1997,1998 by Takuo Watanabe <takuo@jaist.ac.jp>
  School of Information Science, Japan Advanced Institute of
  Science and Technology,
  1-1 Asahidai, Tatsunokuchi, Ishikawa 923-1292, Japan

  This program is free software; you can redistribute it and/or modify
  it under the terms of GNU General Public Licence as published by
  the Free Software Foundation; either version 1, or (at your option)
  any lator version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
 */

#include "tscheme.h"

jmp_buf error_return;

void wta_error (char *fname, int argno) {
    fprintf (stderr, "ERROR: %s: Wrong type in arg %d\n",
	     fname, argno);
    longjmp (error_return, NON_FATAL);
}

void wna_error (char *fname, int nargs) {
    fprintf (stderr, "ERROR: %s: Wrong number (%d) of args.\n",
	     fname, nargs);
    longjmp (error_return, NON_FATAL);
}

void fatal_error (char *message) {
    fprintf (stderr, message);
    longjmp (error_return, FATAL);
}

void error0 (char *message) {
    fprintf (stderr, message);
    longjmp (error_return, NON_FATAL);
}

void error1 (char *message, char *arg) {
    fprintf (stderr, message, arg);
    longjmp (error_return, NON_FATAL);
}

int check_nargs (char *fname, SCM args, int min, int max) {
    int i = 0;
    SCM x = args;

    if (!(IS_PAIR(x) || IS_NULL(x))) error0 ("wrong arguments");

    while (IS_PAIR(x)) {
	i++;
	x = CDR(x);
    }

    if (!IS_NULL(x)) error0 ("wrong arguments");

    if ((min <= i) && (i <= max)) {
	return i;
    }
    else {
	wna_error (fname, i);
	return i;
    }
}

