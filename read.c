/*
  $Id: read.c,v 1.1.1.1 2004/09/09 07:59:50 takuo Exp $
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

static SCM do_readr (FILE *fp);
static SCM do_readparen (FILE *fp);
static SCM do_readstring (FILE *fp);
static SCM do_readtoken(FILE *fp);
static int skip_spaces (FILE *f, char *eoferr);
static int is_number_str (char *s);

static char strbuf[STRBUF_SIZE];

SCM n_read (SCM args) {
    if (check_nargs ("read", args, 0, 1) == 0)
      return scm_read (stdin_value);
    else
      return scm_read (FIRST(args));
}

SCM scm_read (SCM port) {
    if (!IS_PORT(port)) wta_error ("read", 1);
    return do_read (PORT_FPTR(port));
}

SCM do_read (FILE *fp) {
    int c;
    c = skip_spaces (fp, (char *)NULL);
    if (c == EOF) return eof_value;
    ungetc (c, fp);
    return do_readr(fp);
}

static SCM do_readr (FILE *fp) {
     int c;
     SCM v;

     switch (c = skip_spaces (fp, "Unexpected EOF")) {
       case '(':
	 return do_readparen(fp);
       case ')':
	 error0 ("unexpexted close paren");
	 return unspecified_value;
       case '#':
	 if ((c = getc(fp)) == EOF) error0 ("unexpected EOF");
	 switch (c) {
	   case 't':
	   case 'T':
	     return bool_true;
	   case 'f':
	   case 'F':
	     return bool_false;
	   case '\\': 
	     if ((c = getc(fp)) == EOF) error0 ("unexpected EOF");
	     NEWCELL(v, T_CHARACTER);
	     v->as.character = c;
	     return v;
	   default:
	     error0 ("syntax");
	 }
       case '\'':
	 return mk_pair(sym_quote, mk_pair(do_readr(fp), NIL));
       case '`':
	 return mk_pair(sym_quasiquote, mk_pair(do_readr(fp), NIL));
       case ',':
	 if ((c = getc(fp)) == EOF) error0 ("unexpected EOF");
	 switch (c) {
	   case '@':
	     return mk_pair (sym_unquote_splicing,
			     mk_pair (do_readr (fp), NIL));
	   default:
	     ungetc (c, fp);
	     return mk_pair (sym_unquote, mk_pair (do_readr (fp), NIL));
	 }
       case '"':
	 return do_readstring(fp);
       default:
	 ungetc (c, fp);

	 return do_readtoken(fp);
     }
 }

static SCM do_readparen (FILE *fp) {
    int c;
    SCM tmp;

    c = skip_spaces (fp, "Unexpected EOF inside list");
    if (c == ')') return (NIL);
    ungetc(c,fp);
    tmp = do_readr(fp);
    if EQ(tmp, sym_dot) {
	tmp = do_readr(fp);
	c = skip_spaces (fp, "Unexpected EOF inside list");
	if (c != ')') error0 ("missing close paren");
	return(tmp);
    }
    return mk_pair (tmp, do_readparen(fp));
}

static SCM do_readstring (FILE *fp) {
    char c, *newstr;
    int i;
    i = 0;
    while ((c = getc(fp)) != EOF) {
	switch (c) {
	  case '\\':
	    c = getc(fp);
	    if (c == EOF) error0 ("Unexpected EOF");
	    switch (c) {
	      case 'n':
		strbuf[i] = '\n';
		break;
	      default:
		strbuf[i] = c;
		break;
	    }
	    break;
	  case '"':
	    if ((newstr = (char *)malloc(i+1)) == NULL)
	      fatal_error ("malloc: read");
	    strbuf[i] = '\0';
	    strcpy(newstr, strbuf);
	    return mk_string(newstr,i);
	  default:
	    strbuf[i] = c;
	    break;
	}
	i++;
    }
    error0 ("Unextected EOF");
    return unspecified_value;
}

static SCM do_readtoken(FILE *fp) {
    char c;
    int i;

    i = 0;
    while ((c = getc(fp)) != EOF) {
	if (i >= STRBUF_SIZE)
	  fatal_error ("I/O buffer size exceeded");
	switch (c) {
	  case '(':
	  case ')':
	  case '\'':
	  case '"':
	  case '`':
	  case ',':
	  case ' ':
	  case '\t':
	  case '\n':
	    ungetc(c,fp);
	    strbuf[i] = '\0';
	    if (is_number_str(strbuf)) {
		return ((SCM)MK_FIXNUM(atoi(strbuf)));
	    }
	    else
	      return (mk_symbol(strbuf));
	  default:
	    strbuf[i] = (islower(c) ? toupper(c) : c);
	    break;
	}
	i++;
    }
    error0 ("Unexptected EOF");
    return unspecified_value;
}


static int skip_spaces (FILE *f, char *eoferr) {
    int c, commentp;
    commentp = 0;
    while (1) {
	c = getc(f);
	if (c == EOF) {
	    if (eoferr) {
		error0 (eoferr);
	    }
	    else {
		return c;
	    }
	}
	if (commentp) {
	    if (c == '\n') {
		commentp = 0;
	    }
	}
	else if (c == ';') {
	    commentp = 1;
	}
	else if (!isspace(c)) {
	    return (c);
	}
    }
    return 0; /* dummy */
}

static int is_number_str (char *s) {
    char *p;
    p = s;
    if (p[0] == '-') p++;
    if (p[0] == '\0') return 0;
    while (p[0] != '\0') {
	if (p[0] < '0' || p[0] > '9') return 0;
	p++;
    }
    return 1;
}
