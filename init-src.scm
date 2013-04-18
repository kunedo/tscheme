;;; Tscheme: A Tiny Scheme Interpreter
;;; Copyright (c) 1995-2013 Takuo WATANABE (Tokyo Institute of Technology)
;;; 
;;; Permission is hereby granted, free of charge, to any person obtaining
;;; a copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:
;;; 
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(define (scheme-implementation-type) 'tscheme)
(define (scheme-implementation-version) "Apr. 18, 2013")

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

(define (caaar x) (car (caar x)))
(define (caadr x) (car (cadr x)))
(define (cadar x) (car (cdar x)))
(define (caddr x) (car (cddr x)))
(define (cdaar x) (cdr (caar x)))
(define (cdadr x) (cdr (cadr x)))
(define (cddar x) (cdr (cdar x)))
(define (cdddr x) (cdr (cddr x)))

(define (caaaar x) (car (caaar x)))
(define (caaadr x) (car (caadr x)))
(define (caadar x) (car (cadar x)))
(define (caaddr x) (car (caddr x)))
(define (cadaar x) (car (cdaar x)))
(define (cadadr x) (car (cdadr x)))
(define (caddar x) (car (cddar x)))
(define (cadddr x) (car (cdddr x)))
(define (cdaaar x) (cdr (caaar x)))
(define (cdaadr x) (cdr (caadr x)))
(define (cdadar x) (cdr (cadar x)))
(define (cdaddr x) (cdr (caddr x)))
(define (cddaar x) (cdr (cdaar x)))
(define (cddadr x) (cdr (cdadr x)))
(define (cdddar x) (cdr (cddar x)))
(define (cddddr x) (cdr (cdddr x)))

(define first car)
(define second cadr)
(define third caddr)
(define fourth cadddr)
(define (fifth x) (car (cddddr x)))
(define (sixth x) (cadr (cddddr x)))
(define (seventh x) (caddr (cddddr x)))
(define (eighth x) (cadddr (cddddr x)))

(define (nth l n)
  (if (pair? l)
      (if (< n 1)
	  (car l)
	  (nth (cdr l) (-1+ n)))
      (error "nth: invalid argument")))

(define (equal? x y)   ; abridged version of 'equal?'.
  (cond ((eq? x y))
	((and (pair? x) (pair? y)
	      (equal? (car x) (car y))
	      (equal? (cdr x) (cdr y))))
	(else
	 #f)))

(define (assoc key alist)
  (cond ((null? alist)
	 #f)
	((eq? key (caar alist))
	 (car alist))
	(else
	 (assoc key (cdr alist)))))

(define (vector? x) #f)

(define (atom? x) (not (pair? x)))

(define (append . args)
  (define (append2 xs ys)
    (if (null? xs)
	ys
	(cons (car xs) (append2 (cdr xs) ys))))
  (let loop ((args args))
    (if (null? args)
	'()
	(append2 (car args) (loop (cdr args))))))

(define (reverse l)
  (define (rev1 l a)
    (if (null? l) a (rev1 (cdr l) (cons (car l) a))))
  (rev1 l '()))

(define (list* . args)
  (if (null? args)
      '()
      (append (butlast args) (last args))))

(define (butlast l)
  (cond ((null? l)
	 (error "butlast"))
	((null? (cdr l))
	 '())
	(else
	 (cons (car l) (butlast (cdr l))))))

(define (last l)
  (cond ((null? l)
	 (error "last"))
	((null? (cdr l))
	 (car l))
	(else
	 (last (cdr l)))))

(define (map1 f xs)
  (if (null? xs)
      '()
      (cons (f (car xs)) (map1 f (cdr xs)))))

(define (map f . args)
  (if (null? (car args))
      '()
      (let ((args1 (map1 car args))
	    (argsr (map1 cdr args)))
	(cons (apply f args1)
	      (apply map f argsr)))))

(define (expand-quasiquote e)
  (cond ((and (atom? e) (not (symbol? e))) e)
	((symbol? e) (list 'quote e))
	(else
	 (let loop ((l e) (a '()) (b '()))
	   (cond ((null? l)
		  (cons 'append
			(reverse (cons (cons 'list (reverse b)) a))))
		 (else
		  (if (pair? (car l))
		      (case (caar l)
			((unquote)
			 (loop (cdr l) a (cons (cadar l) b)))
			((unquote-splicing)
			 (loop (cdr l)
			       (cons (cadar l)
				     (cons (cons 'list (reverse b)) a))
                               
			       '()))
			(else
			 (loop (cdr l) a
			       (cons (expand-quasiquote (car l)) b))))
		      (loop (cdr l) a
			    (cons (expand-quasiquote (car l)) b)))))))))

(define (call-with-input-file infile f)
  (let ((inport (open-input-file infile)))
    (f inport)
    (close-input-port inport)))

(define (call-with-output-file outfile f)
  (let ((outport (open-output-file outfile)))
    (f outport)
    (close-output-port outport)))

(define gentemp
  (let ((*gentemp-counter* 0))
    (lambda ()
      (let ((s (string->symbol
		(string-append "SCM:" (number->string *gentemp-counter*)))))
	(set! *gentemp-counter* (+ *gentemp-counter* 1))
	s))))

(define *prompt* "> ")
(define *default-prompt* "> ")

(define (sys:prompt-and-read . args)
  (display (if (null? args) *default-prompt* (car args)))
  (read))

(define (sys:toplevel)
  (display *prompt*)
  (let ((input (read)))
    (cond ((or (eof-object? input) (eq? input 'bye))
           (display "Bye!")
           (newline))
          (else
           (write (sys:eval (sys:simplify input) '()))
           (newline)
           (sys:toplevel)))))

(define (load file)
  (call-with-input-file file
    (lambda (inport)
      (display "Loading ")
      (write file)
      (display " ... ")
      (let loop ((e (read inport)))
	(if (eof-object? e)
	    (begin
              (display "done.")
              (newline))
	    (begin
              (sys:eval (sys:simplify e) '())
              ;; (display ".")
              (loop (read inport))))))))

(define (eval x)
  (sys:eval (sys:simplify x) '()))

;;; -*- EOF -*-

