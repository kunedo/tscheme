/*
  $Id: object.c,v 1.1.1.1 2004/09/09 07:59:50 takuo Exp $
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

/* pairs */

SCM mk_pair (register SCM car, register SCM cdr) {
    register SCM x;

    NEWCELL (x, T_PAIR);
    CAR (x) = car;
    CDR (x) = cdr;
    return x;
}

/* strings */

SCM mk_string (register char *string, register long dim) {
    register SCM x;
    char *newstr;

    if ((newstr = (char *) malloc ((unsigned)dim+1)) == NULL)
      fatal_error ("malloc: mk_string");
    strcpy (newstr, string);
    NEWCELL (x, T_STRING);
    STR_DIM (x) = dim;
    STR_DATA (x) = newstr;
    return x;
}

/* symbols */

SCM newsym (register SCM pname, register SCM value) {
    register SCM x;

    NEWCELL (x, T_SYMBOL);
    SYM_PNAME (x) = pname;
    SYM_VALUE (x) = value;
    return x;
}

SCM mk_symbol (char *name) {
    SCM sl, l, x;
    char *s;
    long hash, c;

    /* computre hash location */
    hash = 0;
    s = name;
    while ((c = *s++)) hash = ((hash * 17)^c) % obarray_dim;
    sl = obarray[hash];

    /* check if the symbol already exists */
    for (l = sl; !IS_NULL(l); l = CDR(l))
      if (strcmp (name, STR_DATA(SYM_PNAME(CAR(l)))) == 0)
	return CAR(l);

    /* allocate new symbol */
    x = newsym (mk_string (name, strlen(name)), unbound_value);
    obarray[hash] = mk_pair (x, sl);
    return x;
}

/* Subrs */

SCM mk_subr (char *name, SCM (*fun)(void), int nargs) {
    SCM subr, symbol;

    switch (nargs) {
      case 0:
	NEWCELL(subr, T_SUBR0);
	break;
      case 1:
	NEWCELL(subr, T_SUBR1);
	break;
      case 2:
	NEWCELL(subr, T_SUBR2);
	break;
      case 3:
	NEWCELL(subr, T_SUBR3);
	break;
      default:
	NEWCELL(subr, T_SUBRN);
	break;
    }
    symbol = mk_symbol (name);
    SYM_VALUE (symbol) = subr;
    SUBR_NAME (subr) = symbol;
    SUBR_FUN (subr) = fun;
    return subr;
}

SCM mk_fsubr (char *name, SCM (*fun)(void)) {
    SCM subr, symbol;

    NEWCELL (subr, T_FSUBR);
    symbol = mk_symbol (name);
    SYM_VALUE (symbol) = subr;
    SUBR_NAME (subr) = symbol;
    SUBR_FUN (subr) = fun;

    return subr;
}

/* Closures */

SCM mk_closure (SCM args, SCM code, SCM env) {
    SCM closure;

    NEWCELL (closure, T_CLOSURE);
    CLOSURE_CODE (closure) = mk_pair (args, code);
    CLOSURE_ENV (closure) = env;
    return closure;
}

