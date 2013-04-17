# $Id: Makefile,v 1.1.1.1 2004/09/09 07:59:50 takuo Exp $
# Tscheme: A Tiny Scheme Interpreter
# (Course material for I425 "Topics on Software Environment")
# Copyright (C) 1997,1998 by Takuo Watanabe <takuo@jaist.ac.jp>
# School of Information Science, Japan Advanced Institute of
# Science and Technology,
# 1-1 Asahidai, Tatsunokuchi, Ishikawa 923-1292, Japan
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of GNU General Public Licence as published by
# the Free Software Foundation; either version 1, or (at your option)
# any lator version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#

# Use ANSI C compiler
CC = gcc
CFLAGS = -Wall -ansi -pedantic
DBGFLAGS = -g #-DDEBUG
OPTFLAGS = -O2
LDFLAGS=
#DEFFLAGS = -DINIT_FILE=\"$(INIT_FILE)\"
DEFFLAGS=

# PREFIX = /usr/lang
# PREFIX = /usr/local/lecture/is/i425/tscheme
PREFIX = /usr/local
BINDIR = $(PREFIX)/bin
# LIBDIR = $(PREFIX)/lib/tscheme
LIBDIR = $(PREFIX)/lib

HDRFILES = tscheme.h
SRCFILES = main.c storage.c object.c eval.c subrs.c io.c error.c misc.c read.c
OBJFILES = main.o storage.o object.o eval.o subrs.o io.o error.o misc.o read.o
SCMFILES = init.scm
TMPFILES = init1.scm # init0.scm
NAME = tscheme

INIT_FILE = $(LIBDIR)/init.scm


MKINIT_CMD = '(begin (load "simplify.scm") (sys:make-init "init.scm"))'
MKINIT_CMD0 = '(begin (load "simplify.scm") (sys:make-init "init0.scm"))'
MKINIT_CMD1 = '(begin (load "simplify.scm") (sys:make-init "init1.scm"))'
SCM = /usr/local/bin/scm

.c.o: $(HDRFILES)
	$(CC) $(CFLAGS) $(DEFFLAGS) $(DBGFLAGS) $(OPTFLAGS) -c $<

all: $(NAME) $(SCMFILES)

$(NAME): $(OBJFILES)
	$(CC) $(LDFLAGS) -o $(NAME) $(OBJFILES)

init.scm: init1.scm $(NAME)
	echo $(MKINIT_CMD) | ./$(NAME) -i init1.scm
	diff init.scm init1.scm

init1.scm: init0.scm $(NAME)
	echo $(MKINIT_CMD1) | ./$(NAME) -i init0.scm

init0.scm: init-src.scm simplify.scm
	echo $(MKINIT_CMD0) | $(SCM)

objects: $(OBJFILES)

install: $(NAME) $(SCMFILES)
	install -c -s $(NAME) $(BINDIR)
	install -c $(SCMFILES) $(LIBDIR)

clean:
	rm -f $(OBJFILES) $(NAME) $(SCMFILES) $(TMPFILES)

realclean: 
	rm -f $(OBJFILES) $(NAME) $(SCMFILES) $(TMPFILES) core *.core *~

# -*- EOF -*-

