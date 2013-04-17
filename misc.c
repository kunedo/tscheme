/*
  $Id: misc.c,v 1.1.1.1 2004/09/09 07:59:50 takuo Exp $
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

int memq (SCM key, SCM list) {
    SCM l = list;
    while (!IS_NULL(l)) {
	if (!(IS_PAIR(l) || IS_NULL(l))) wta_error ("memq", 2);
	if EQ(key, CAR(l)) return 1;
	l = CDR(l);
    }
    return 0;
}
    
SCM map1 (SCM (*f)(SCM), SCM list) {
    SCM l = list;
    if IS_PAIR(l) {
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


SCM last (SCM l) {
    SCM p = l;
    if (!(IS_PAIR(p) || IS_NULL(p))) wta_error ("last", 1);
    if IS_PAIR(p)
	while IS_PAIR(CDR(p)) p = CDR(p);
    return p;
}
