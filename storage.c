/*
  $Id: storage.c,v 1.1.1.1 2004/09/09 07:59:50 takuo Exp $
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

static SCM heap_start, heap_end;
SCM free_list;
SCM stack_start;
static int mark_counter;

SCM obarray[DEFAULT_OBARRAY_SIZE];
long obarray_dim = DEFAULT_OBARRAY_SIZE;

SCM the_null_value, bool_true, bool_false;

SCM unbound_value, unspecified_value;
SCM
  stdin_value, stdout_value, stderr_value, eof_value, 
  sym_quote, sym_quasiquote, sym_unquote, sym_unquote_splicing,
  sym_lambda, sym_and, sym_or, sym_let, sym_let_star, sym_letrec,
  sym_begin, sym_do, sym_delay, sym_if, sym_cond, sym_case, sym_else,
  sym_set, sym_define, sym_dot;

SCM sym_toplevel;

static void gc_mark_locations (SCM *start, SCM *end);
static void gc_mark_locations_array (SCM *x, long n);
static void gc_mark (SCM p);
static void gc_sweep (void);


void gc (void) {

    SCM stack_end_var = NIL;
    jmp_buf save_regs;

    /* Inhibit signal interruption */
    signal (SIGINT, SIG_IGN);

    /* Start message */
    fprintf (stderr, "GC: start\n");

    /* Machine registers */
    fprintf (stderr, "GC: registers: ");
    setjmp (save_regs);
    gc_mark_locations ((SCM *)save_regs,
		       (SCM *)(((char *)save_regs) + sizeof (save_regs)));

    /* Stack */
    fprintf (stderr, "GC: stack:     ");
    gc_mark_locations ((SCM *)stack_start, (SCM *)&stack_end_var);

    /* Obarray */
    fprintf (stderr, "GC: obarray:   ");
    gc_mark_locations_array (obarray, obarray_dim);

    gc_sweep ();

    /* Resume signal settings */
    signal (SIGINT, interrupt_handler);
}

static void gc_mark_locations (SCM *start, SCM *end) {
    long n;

    if (start > end) {
	SCM *tmp;
	tmp = start;
	start = end;
	end = tmp;
    }
    n = end - start;
    gc_mark_locations_array (start, n);
}

static void gc_mark_locations_array (SCM *x, long n) {
    int j;
    SCM p;

    mark_counter = 0;
    for (j = 0; j < n; j++) {
	p = x[j];
	if ((p > heap_start) &&
	    (p < heap_end) &&
	    ((((char *)p) - ((char *)heap_start)) %
	     sizeof (struct object)) == 0 &&
	    !IS_TYPE(p, T_FREE_CELL)) {
	    gc_mark (p);
	}
    }
    fprintf (stderr, "%d cells marked.\n", mark_counter);
}

static void gc_mark (SCM p) {
    register SCM pp = p;

  gc_mark_loop:
    if (IS_IMM(pp) || MARKED(pp)) return;
    MARK(pp);
    mark_counter++;
    switch BOXED_TYPE(pp) {
      case T_PAIR:
	gc_mark (CAR(pp));
	pp = CDR(pp);
	goto gc_mark_loop;
      case T_SYMBOL:
	gc_mark (SYM_PNAME(pp));
	pp = SYM_VALUE(pp);
	goto gc_mark_loop;
      case T_CLOSURE:
	gc_mark (CLOSURE_ENV(pp));
	pp = CLOSURE_CODE(pp);
	goto gc_mark_loop;
      case T_ENV:
	pp = ENV(pp);
	goto gc_mark_loop;
      case T_NULL:
      case T_BOOL:
      case T_CHARACTER:
      case T_STRING:
      case T_SUBR0:
      case T_SUBR1:
      case T_SUBR2:
      case T_SUBR3:
      case T_SUBRN:
      case T_FSUBR:
      case T_PORT:
      case T_EOF_VALUE:
	break;
      default:
	fprintf (stderr, "DEBUG: Should not reach here! (tt=%d)\n",
		 TYPE(pp));
	break;
    }
}

static void gc_sweep (void) {
    SCM p;
    SCM new_free_list = NIL;
    int n = 0;

    for (p = heap_start; p < heap_end; ++p) {
	if (UNMARKED(p)) {
	    switch BOXED_TYPE(p) {
	      case T_STRING:
		free (STR_DATA(p));
		break;
	      case T_PORT:
		fclose (PORT_FPTR(p));
		fprintf (stderr, "file %s is closed\n", PORT_NAME(p));
		break;
	      default:
		break;
	    }
	    n++;
	    SET_BOXED_TYPE (p, T_FREE_CELL);
	    UNMARK(p);
	    CDR(p) = new_free_list;
	    new_free_list = p;
	}
	else {
	    UNMARK(p);
	}
    }
    free_list = new_free_list;
    if (n == 0)
      fatal_error ("GC: Sorry! NO memory! Bye!\n");
    else
      fprintf (stderr, "GC: %d cells collected.\n", n);
}

void init_storage (unsigned heapsize) {

    SCM ptr, next;
    int i, count;

    /* allocate unique values */

    /* () */
    if ((the_null_value = (SCM) malloc (sizeof (struct object))) == NULL)
      fatal_error ("malloc: ()");
    SET_BOXED_TYPE (the_null_value, T_NULL);
    the_null_value->as.null = (SCM)NULL;
    /* #t */
    if ((bool_true  = (SCM) malloc (sizeof (struct object))) == NULL)
      fatal_error ("malloc: #t");
    SET_BOXED_TYPE (bool_true, T_BOOL);
    bool_true->as.bool = 1;
    /* #f */
    if ((bool_false = (SCM) malloc (sizeof (struct object))) == NULL)
      fatal_error ("malloc: #f");
    SET_BOXED_TYPE (bool_false, T_BOOL);
    bool_false->as.bool = 0;
    /* EOF */
    if ((eof_value = (SCM) malloc (sizeof (struct object))) == NULL)
      fatal_error ("malloc: eof_value");
    SET_BOXED_TYPE (eof_value, T_EOF_VALUE);
    EOF_VALUE(eof_value) = EOF;

    /* allocate heap area */
    if ((heap_start = (SCM) malloc (sizeof (struct object) * heapsize))
	== NULL)
      fatal_error ("malloc: heap");
    heap_end   = heap_start + heapsize;

    /* initialize the free list */
    ptr = free_list = heap_start;
    count = 0;
    while (1) {
	SET_BOXED_TYPE (ptr, T_FREE_CELL);
	next = ptr + 1;
	count++;
	if (next < heap_end) {
	    CDR(ptr) = next;
	    ptr = next;
	}
	else {
	    CDR(ptr) = NIL;
	    break;
	}
    }

    /* initialize the obarray */
    for (i = 0; i < obarray_dim; i++) {
	obarray[i] = NIL;
    }

    /* special values and symbols */
    unbound_value = mk_symbol ("**UNBOUND**");
    SYM_VALUE(unbound_value) = unbound_value;
    unspecified_value = mk_symbol ("**UNSPECIFIED**");

    sym_quote = mk_symbol ("QUOTE");
    sym_quasiquote = mk_symbol ("QUASIQUOTE");
    sym_unquote = mk_symbol ("UNQUOTE");
    sym_unquote_splicing = mk_symbol ("UNQUOTE-SPLICING");
    sym_lambda = mk_symbol("LAMBDA");
    sym_and = mk_symbol("AND");
    sym_or = mk_symbol("OR");
    sym_let = mk_symbol("LET");
    sym_let_star = mk_symbol("LET*");
    sym_letrec = mk_symbol("LETREC");
    sym_begin = mk_symbol("BEGIN");
    sym_do = mk_symbol("DO");
    sym_delay = mk_symbol("DELAY");
    sym_if = mk_symbol("IF");
    sym_cond = mk_symbol("COND");
    sym_case = mk_symbol("CASE");
    sym_else = mk_symbol("ELSE");
    sym_set = mk_symbol("SET!");
    sym_define = mk_symbol("DEFINE");
    sym_dot = mk_symbol(".");

    sym_toplevel = mk_symbol("SYS:TOPLEVEL");

    NEWCELL(stdin_value, T_PORT);
    PORT_NAME(stdin_value) = "standard_input";
    PORT_FPTR(stdin_value) = stdin;
    SYM_VALUE(mk_symbol("STDIN")) = stdin_value;

    NEWCELL(stdout_value, T_PORT);
    PORT_NAME(stdout_value) = "standard_output";
    PORT_FPTR(stdout_value) = stdout;
    SYM_VALUE(mk_symbol("STDOUT")) = stdout_value;

    NEWCELL(stderr_value, T_PORT);
    PORT_NAME(stderr_value) = "standard_error";
    PORT_FPTR(stderr_value) = stderr;
    SYM_VALUE(mk_symbol("STDERR")) = stderr_value;
}

