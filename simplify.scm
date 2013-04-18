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

;;; An expression simplifier

;;; Utility functions

(define (map1 f xs)
  (if (null? xs)
      '()
      (cons (f (car xs)) (map1 f (cdr xs)))))

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


;;; Simplifier

(define (sys:simplify exp)
  (cond ((boolean? exp) exp)
	((number?  exp) exp)
	((char?    exp) exp)
	((string?  exp) exp)
	((symbol?  exp) exp)
	((pair?    exp)
	 (let ((op (car exp)) (args (cdr exp)))
	   (cond ((eq? op 'quote)
		  exp)
		 ((eq? op 'lambda)
		  (list* 'lambda
			 (car args)
			 (sys:simplify-body (cdr args))))
		 ((eq? op 'let)
		  (if (symbol? (car args))
		      ;; (LET var ((var init)...) . body) =>
		      ;; (LETREC ((var (LAMBDA (var...) . body)))
		      ;;   (var init...))
		      (list 'letrec
			    (list (list (car args)
					(list* 'lambda
					       (sys:let-vars (cadr args))
					       (sys:simplify-body (cddr args)))))
			    (list* (car args)
				   (map1 sys:simplify (sys:let-exps (cadr args)))))
		      (list* 'let
			     (sys:simplify-let-bspecs (car args))
			     (sys:simplify-body (cdr args)))))
		 ((eq? op 'let*)
		  ;; (LET* ((var1 init1) (var2 init2)...) . body) =>
		  ;; (LET ((var1 init1))
		  ;;   (LET ((var2 init2)) (... . body)...))
		  (sys:simplify-let* (car args) (cdr args)))
		 ((eq? op 'letrec)
		  (list* 'letrec
			 (sys:simplify-let-bspecs (car args))
			 (sys:simplify-body (cdr args))))
		 ((eq? op 'if)
		  (list* 'if (map1 sys:simplify args)))
		 ((eq? op 'cond)
		  (list* 'cond
			 (map1 (lambda (clause)
				 (list*
				  (if (eq? (car clause) 'else)
				      'else
				      (sys:simplify (car clause)))
				  (map1 sys:simplify (cdr clause))))
			       args)))
		 ((eq? op 'case)
		  (list* 'case
			 (sys:simplify (car args))
			 (map1 (lambda (clause)
				 (list*
				  (car clause)
				  (map1 sys:simplify (cdr clause))))
			       (cdr args))))
		 ((eq? op 'and)
		  (list* 'and (map1 sys:simplify args)))
		 ((eq? op 'or)
		  (list* 'or (map1 sys:simplify args)))
		 ((eq? op 'do))
		 ((eq? op 'begin)
		  (list* 'begin
			 (sys:simplify-body args)))
		 ((eq? op 'set!)
		  (list 'set!
			(car args)
			(sys:simplify (cadr args))))
		 ((eq? op 'define)
		  (if (pair? (car args))
		      ;; (DEFINE (var . args) . body) =>
		      ;; (DEFINE var (LAMBDA args . body))
		      (list 'define
			    (caar args)
			    (list* 'lambda
				   (cdar args)
				   (sys:simplify-body (cdr args))))
		      (list 'define
			    (car args)
			    (sys:simplify (cadr args)))))
		 ((eq? op 'quasiquote)
		  (sys:expand-quasiquote (car args)))
		 (else
		  (map1 sys:simplify exp)))))
	(else	
	 (error "Unknown expression type."))))

(define (sys:simplify-let-bspecs bspecs)
  (map1 (lambda (bspec)
	  (list (car bspec)
		(sys:simplify (cadr bspec))))
	bspecs))

(define (sys:let-vars bspecs) (map1 car bspecs))

(define (sys:let-exps bspecs) (map1 cadr bspecs))

(define (sys:simplify-let* bspecs body)
  (if (null? bspecs)
      (sys:simplify-body body)
      (list* 'let
	     (list (list (caar bspecs)
			 (sys:simplify (cadar bspecs))))
	     (if (null? (cdr bspecs))
		 (sys:simplify-body body)
		 (list (sys:simplify-let* (cdr bspecs) body))))))

(define (sys:simplify-body body)
  (define (sys:simplify-body-define body bspecs)
    (cond ((null? body)
	   '())
	  ((and (list? (car body))
		(not (null? (car body)))
		(eq? (car (car body)) 'define))
	   (let ((var (if (pair? (cadr (car body)))
			  (caadr (car body))
			  (cadr (car body))))
		 (exp (if (pair? (cadr (car body)))
			  (list* 'lambda
				 (cdadr (car body))
				 (sys:simplify-body (cddr (car body))))
			  (sys:simplify (caddr (car body))))))
	     (sys:simplify-body-define
	      (cdr body)
	      (cons (list var exp) bspecs))))
	  (else
	   (if (null? bspecs)
	       (sys:simplify-body-others body)
	       (list
		(list* 'letrec
		       (reverse bspecs)
		       (sys:simplify-body-others body)))))))

  (define (sys:simplify-body-others body)
    (cond ((null? body)
	   '())
	  ((and (list? (car body))
		(not (null? (car body)))
		(eq? (car (car body)) 'define))
	   (error "Invalid local define."))
	  (else
	   (cons (sys:simplify (car body))
		 (sys:simplify-body-others (cdr body))))))

  (sys:simplify-body-define body '()))

(define (sys:expand-quasiquote e)
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
			       (cons (sys:expand-quasiquote (car l)) b))))
		      (loop (cdr l) a
			    (cons (sys:expand-quasiquote (car l)) b)))))))))


(define (sys:simplify-all inport outport)
  (let loop ((e (read inport)))
    (if (not (eof-object? e))
	(begin
	  (write (sys:simplify e) outport)
	  (newline outport)
	  (loop (read inport))))))

(define (sys:simplify-file infile outfile)
  (call-with-input-file infile
    (lambda (inport)
      (call-with-output-file outfile
	(lambda (outport)
	  (sys:simplify-all inport outport))))))

(define (sys:make-init outfile)
  (display "Making ")
  (display outfile)
  (newline)
  (call-with-output-file outfile
    (lambda (outport)
      (call-with-input-file "init-src.scm"
	(lambda (inport) (sys:simplify-all inport outport)))
      (call-with-input-file "simplify.scm"
	(lambda (inport) (sys:simplify-all inport outport))))))

;;; -*- EOF -*-
