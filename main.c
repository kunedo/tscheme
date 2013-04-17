/*
  $Id: main.c,v 1.1.1.1 2004/09/09 07:59:50 takuo Exp $
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

#define USAGE "tscheme [-i init_file]"
#ifndef INIT_FILE
#define INIT_FILE "./init.scm"
#endif

void interrupt_handler (int sig);
void usage ();

static char *init_file = INIT_FILE;
static int init_loaded = 0;

extern char *optarg;
extern int optind;

int main (int argc, char **argv) {

    int ch;
    SCM start;

    while ((ch = getopt (argc, argv, "i:")) != EOF) {
	switch (ch) {
	  case 'i':
	    init_file = optarg;
	    break;
	  default:
	    usage ();
	}
    }
    argc -= optind;
    if (argc > 0) usage ();

    stack_start = (SCM)&start;

    init_storage (DEFAULT_NUMCELLS);
    init_subrs ();
    init_io_subrs ();

    printf (BANNER);

    switch (setjmp (error_return)) {
      case FATAL:
	exit (1);
      case NON_FATAL:
	if (!init_loaded) {
	    fprintf (stderr, "Error in init file.\n");
	    exit (1);
	}
	break;
      default:
	break;
    }

    signal (SIGINT, interrupt_handler);

    if (!init_loaded) {
	do_load (init_file);
	init_loaded = 1;
    }

    /* (sys:toplevel) */
    evaluate (mk_pair (sym_toplevel, NIL), NIL);

    exit(0);
}

void interrupt_handler (int sig) {
    fprintf (stderr, "Interrupt!\n");
    longjmp (error_return, NON_FATAL);
}

void usage () {
    fprintf (stderr, "usage: %s\n", USAGE);
    exit (1);
}
