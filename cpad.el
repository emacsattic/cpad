;;; cpad.el -- Calculation PAD - A tiny language for calculation in Emacs buffer.

;;; Licence: 
;;
;;  Copyright (C) 2002 Akimichi Tatsukawa <akimichi@mbox.co.jp>
;;        adapted from red.el which is originally developed by Masami Hagiya.
;;
;;  This file is NOT a part of GNU Emacs.
;;
;;  This program is free software; you can redistribute it and/or
;;  modify it under the terms of the GNU General Public License as
;;  published by the Free Software Foundation; either version 2 of
;;  the License, or (at your option) any later version.
;;     
;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;  GNU General Public License for more details.
;;     
;;  You should have received a copy of the GNU General Public
;;  License along with this program; if not, write to the Free
;;  Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;  MA 02111-1307, USA.
;;
;;  Please send suggestions and bug reports to <akimichi@mbox.co.jp>. 
;;  The latest version of this package should be available at
;;
;;     <URL:http://akimichi.homeunix.net/~emile/aki/program/elisp/>

;;  Author: Akimichi Tatsukawa <akimichi@mbox.co.jp>
;;  Keywords: calculator, math, functional language
;;  Version: $Id: cpad.el,v 1.6 2002/07/16 11:25:49 emile Exp emile $
;;  $Author: emile $
;;  $Date: 2002/07/16 11:25:49 $
;;  $Log: cpad.el,v $
;;  Revision 1.6  2002/07/16 11:25:49  emile
;;  Functions for debug message are all commented out.
;;
;;  Revision 1.5  2002/07/15 02:15:54  emile
;;  *** empty log message ***
;;
;;  Revision 1.4  2002/07/04 08:41:25  emile
;;  Before implementation of function call with units.
;;
;;  Revision 1.3  2002/06/15 08:31:13  emile
;;  Primitive unit calculation such as "18{mg} + 21{mg};" is capable.
;;
;;  Revision 1.2  2002/05/15 05:36:14  emile
;;  VAR changes to SYM
;;
;;  Revision 1.1  2002/05/14 03:58:36  emile
;;  Initial revision
;;


;;; Commentary:
;;    This package enables a calculation in Emacs buffer like a calculation pad.
;;    It immediately parse and calculate a expression, and replace it with the result.
;;    The language for calculation is a kind of function-oriented language(don't expect too much).
;;    Allowed data types are number(integer,float) and list(and unit).
;;    A simple standard library is provided, which contains limited features of arithmatic,list
;;    processing,statistical and clinical functions.
;;    And you can add your own function easily to enrich its facilities.
;;
;;    Its major drawback is performance. Owing to its recursive feature,it sometimes consumes so much time
;;    that even a moderate human brain can beat it.
;; Usage:
;;    Add following codes in your .emacs file.
;;
;;    (autoload 'cpad "cpad" "A calculation pad" t)
;;    (autoload 'cpad* "cpad" "A calculation pad" t)
;;    (global-set-key "\C-cr" 'cpad) ; you may change it to whatever key you like.
;;    (global-set-key "\C-cx" 'cpad*)
;; TODO:
;;      iterator macro
;;      curry function
;;      built-in functions for arithmatic,list,statistics,etc to improve performance
;;      exponential operator ^
;;      display argument
;;      query facility
;;      documentation
;;      unification
;;      environment for variable and its value --> maybe not
;;      stream list
;;      variable assignment --> maybe not
;;      parsing by eparser
;;      hooks for registration of user functions -> use defadvice

; (defvar cpad:version "0.1"
;   "Current version of CPAD.")

;;; Code:
(setq max-specpdl-size 100000)

(require 'cl)
(require 'units)
(require 'eieio)
;(require 'calc)
;(require 'calc-ext)
;(defvar calc-command-flags)


;;; ユーザーオプション User options
(defvar cpad:default-library-path "~/.cpad"
  "*Directory where the default cpad library is located")
;; (defvar cpad:user-function-hooks nil
;;   "*Hooks for user-defind functions")
;; (defvar cpad:function-prompt-regexp-user nil
;;   "*User definded Regular expression to search function definition")

;;; internal variables

;; (defvar cpad:function-prompt-regexp "^[ \t\n]*"
;;   "*Regular expression to search function definition")
;; (defvar cpad:function-prompt-regexp-lisp "^;[; \t\n]*"
;;   "*Regular expression to search function definition for lisp source embedding")
;; (defvar cpad:function-prompt-regexp-c "^[ \t\n]*/[/\* \t\n]+"
;;   "*Regular expression to search function definition for C source embedding")
;; (defvar cpad:function-prompt-regexp-ruby "^#[# \t\n]*"
;;   "*Regular expression to search function definition for ruby source embedding")
;; (defvar cpad:function-prompt-regexp-tex "^%[% \t\n]*"
;;   "*Regular expression to search function definition for tex source embedding")



;;; 環境 Enironment(not in use)

; (defvar cpad:global-environment nil "The cpad environment which stores variables and its values")

; (defun cpad:lookup-env (var env)
;   "Lookup a variable in environment"
;   (let* ((binding (assoc var env))
; 	 (val (cdr binding)))
;     (cond ((cpad:variablep val)
; 	   (cpad:variable-lookup val env))
; 	  ((null binding)		; Unbound variable, so the variable itself is returned.
; 	   var)
; 	  ((null val)			; null variable value.
; 	   nil)
; 	  (t val))))

; (defun cpad:variable-lookup (var env)
;   (let* ((binding (assoc var env))
; 	 (val (cdr binding)))
;     (cond ((cpad:variablep val)
; 	   (cpad:variable-lookup val env))
; 	  ((null binding)		; Unbound variable, so the variable itself is returned.
; 	   var)
; 	  ((null val)			; Null variable value.
; 	   nil)
; 	  (t val))))

; (defun cpad:variablep (item)
;   "A variable is of the form ?name, where name is a symbol."
;   (if (null item)
;       nil
;     (symbolp item)))

; (defun cpad:update-env (var val env)
;   "Update environment and return it"
;   (acons var val env))
    

; (defun cpad:reset-env (env)
;   "Reset environment"
;   (setq env nil))
  


;;; 字句解析 Lexical analyzer


(defconst cpad:scanner-func-re "[ \t\n]*([^)]*)[ \t\n]*:="
  "Scanner sertring for function definition.")


;;; 構文解析 parser
(defun cpad:alphap (c)
  "Check if argument 'C is an alphabet."
  (or (and (<= ?a c) (<= c ?z))
      (and (<= ?A c) (<= c ?Z))
      (= c ?_)))

(defun cpad:digitp (c)
  "Check if argument C is a digit."
  (and (<= ?0 c) (<= c ?9)))


(defun cpad:skip-white ()
  "Skip white and return the current point."
  (skip-chars-forward " \t\n")
  (point))

(defvar cpad:tokenp nil "Token point??")


(defun cpad:get-next-token ()
  "Retrive the next token and return it as a string."
  (setq cpad:tokenp (point))
  (let ((cpad:begin-point (cpad:skip-white))
	(c (following-char)))
    (cond ((= c 0) ";")
	  ((>= c 128) (forward-char 2)
	   (buffer-substring cpad:begin-point (point)))
	  ((= c ?\;) ";")
	  (t (cond ((or (= c ?\() (= c ?\,) (= c ?\)); (= c ?\;)
			(= c ?[) (= c ?]) (= c ?{) (= c ?}))
		    (forward-char))
		   ((or (= c ?\/) (= c ?\*))
		    (forward-char))
		   ((cpad:alphap c)
		    (skip-chars-forward "a-zA-Z_0-9'"))
		   ((cpad:digitp c)		; must be a number
		    (skip-chars-forward "0-9")
		    (let ((next-char (following-char)))	;note that 'following-char doesn't change the point
		      (cond ((= next-char ?e)
			     (if (looking-at "e-?[0-9]+")
				 (forward-char (- (match-end 0) (match-beginning 0) ))
			       (error "Improper exponential expression")))
			    ((= next-char ?.) ;must be a floating point number
			     (cond ((looking-at "\\.[0-9]+\\(e-?[0-9]+\\)?")
				    (forward-char (- (match-end 0) (match-beginning 0) )))
				   ((looking-at "\\.")
				    (forward-char (- (match-end 0) (match-beginning 0) )))))
			    (t		; must be an integer
			     ))))		
		   
		   (t
		    (skip-chars-forward "^a-zA-Z_0-9'(,);[] \t\n")))
	     (buffer-substring cpad:begin-point (point))))))

(defun cpad:look-next-token ()
  (let ((following-token (cpad:get-next-token)))
    (cpad:back-token)
    following-token))
	


(defun cpad:back-token ()
  (goto-char cpad:tokenp))

(defun cpad:check-next-token (token)
  (if (not (string= (cpad:get-next-token) token))
      (error "%s expected" token)))

(defun cpad:check-numargs (values n)
  (if (not (= (length values) n))
      (error "Number of arguments")))

(defun cpad:check-number (v)
  (if (not (numberp v))
      (error "Number expected but I got %s" (type-of v))))

(defun cpad:check-two-numbers (v1 v2)
  (if (or (not (numberp v1)) (not (numberp v2)))
      (error "Number expected I got %s and %s" (type-of v1) (type-of v2))))

(defun cpad:check-truth-value (v)
  (if (not (or (eq v 'TRUE) (eq v 'FALSE) (eq v 'UNKNOWN)))
      (error "Truth value expected but I got %s" (type-of v))))

(defun cpad:check-list (v)
  (if (and (not (eq v 'EMPTY))
	   (not (and (consp v) (not (eq (car v) 'LAM)))))
      (error "List expected")))

(defun cpad:check-nonempty-list (v)
  (if (not (and (consp v) (not (eq (car v) 'LAM))))
      (error "Nonempty list expected")))

(defun cpad:exp ()
  "Parse an expression and return it"
  (let ((begin (cpad:skip-white))
	(token (cpad:get-next-token)))
    (cond ((string= token "if")		;IF文のパース
	   (let ((cond (cpad:exp)))
	     (cpad:check-next-token "then")
	     (let ((then (cpad:exp)))
	       (cpad:check-next-token "else")
	       (let ((else (cpad:exp)))
		 (list 'IF begin (point) cond then else)))))
	  ((or (string= token "lam") (string= token "λ")) ;ラムダ式のパース
	   (cpad:check-next-token "(")
	   (let ((args (cpad:parse-func-args)))
	     (cpad:check-next-token ")")
	     (let ((body (cpad:exp)))
	       (list 'LAM begin (point) args body))))
	  (t (cpad:back-token)
	     (cpad:log-exp)))))
	     
(defun cpad:log-exp ()
  "Parse a logical expression and return it"
  (let ((next-token (cpad:get-next-token)))
    (cond ((string= next-token "~")
	   (let ((operand (cpad:rel-exp)))
	     (list 'LOG-NOT begin (point) next-token operand)))
	  (t
	   (cpad:back-token)
	   (let ((operand-l (cpad:rel-exp))
		 (op (cpad:get-next-token)))
	     (cond ((or (string= op "&") (string= op "|"))
		    (let ((operand-r (cpad:rel-exp)))
		      (list 'LOG-BIN-OP begin (point) op operand-l operand-r)))
		   (t
		    (cpad:back-token)
		    operand-l)))))))

(defun cpad:rel-exp ()
  "Parse an relative operator expression and return it"
  (let ((result (cpad:exp1))
	(op (cpad:get-next-token)))
    (cond ((or (string= op "==") (string= op "!=")
	       (string= op "<") (string= op ">")
	       (string= op "<=") (string= op ">="))
	   (let ((n2 (cpad:exp1)))
	     (list 'REL begin (point) op result n2)))
	  (t
	   (cpad:back-token)
	   result))))


(defun cpad:exp1 ()
  (let ((begin (cpad:skip-white))
	(result (cpad:exp2))
	op)
    (while (or (string= (setq op (cpad:get-next-token)) "+")
	       (string= op "-"))
      (let ((n2 (cpad:exp2)))
	(setq result (list 'ADD begin (point) op result n2))))
    (cpad:back-token)
    result))

(defun cpad:exp2 ()
  (let ((begin (cpad:skip-white))
	(result (cpad:exp3)) op)
    (while (or (string= (setq op (cpad:get-next-token)) "*")
	       (string= op "/"))
;      (message "cpad:exp2 op -> %s" op)
      (let ((result2 (cpad:exp3)))
	(setq result (list 'MUL begin (point) op result result2))))
    (cpad:back-token)
;    (message "cpad:exp2 result -> %s" result)
    result))
  
(defun cpad:exp3 ()
  (let ((begin (cpad:skip-white))
	(id (cpad:get-next-token)))
    (cond ((string= id "-")
	   (let ((result (cpad:exp3)))
	     (list 'MINUS begin (point) result)))
	  (t (cpad:back-token)
	     (cpad:exp4)))))

(defun cpad:exp4 ()
  (let* ((begin (cpad:skip-white))
	 (id (cpad:get-next-token))
	 (result (cond ((cpad:alphap (string-to-char id)) ;begin with an alphabet
			(cond ((string= id "true") ;parsing the constant
			       (list 'TRUE begin (point)))
			      ((string= id "false")
			       (list 'FALSE begin (point)))
			      ((string= id "nil")
			       (list 'LIST begin (point) '()))
			      (t
			       ;; <arg> ::= <sym> | <sym> "{" <unit> "}"  
			       (if (not (string= (cpad:look-next-token) "{"))
				   (list 'SYM begin (point) id) ;識別子 identifier
				 ;; unit argument
				 (let ((begin-unit (cpad:skip-white)))
				   (if (search-forward "}")
				       ;; symbol unit
				       (list 'UNIT begin (point) id (buffer-substring begin-unit (point)))
				     (error "Mismatched }")))))))
		       ;; quantity ::= <num> | <num> <unit>
		       ((cpad:digitp (string-to-char id)) ; parsing numeric or quantity
			(if (not (string= (cpad:look-next-token) "{"))
			    (list 'NUM begin (point) (string-to-int id))
					;		    (message "point: %s" (point))
			  (if (null (looking-at "{\\([^\}]+\\)}"))
			      (error "Mismatched }")
			    (forward-char (- (match-end 0) (match-beginning 0) ))
			    (let ((unit-message
				   (list 'UNIT begin (point) (string-to-int id) (buffer-substring (match-beginning 1) (match-end 1)))))
;			      (message "cpad:exp4: unit-message: %s" unit-message)
			      unit-message)
			    )))
		       ((string= id "(")	;()内のパース
			(cpad:skip-white)
			(if (string= (cpad:look-next-token) ")") ;無引数のパース
			    (list 'PAR begin (point) nil)
			  (let ((result (cpad:exp))) ;引数のパース
			    (cpad:check-next-token ")")
			    (list 'PAR begin (point) result))))
		       ((string= id "[")	;リストのパース
			(let ((args '()))
			  (cond ((string= (cpad:get-next-token) "]"))
				(t (cpad:back-token)
				   (setq args (cons (cpad:exp) args))
				   (while (string= (cpad:get-next-token) ",")
				     (setq args (cons (cpad:exp) args)))
				   (cpad:back-token)
				   (cpad:check-next-token "]")))
			  (list 'LIST begin (point) (nreverse args))))
		       ((string= id "\"")	;文字列のパース
			(if (search-forward "\"")
			    (list 'STR begin (point) (buffer-substring begin (point)))
			  (error "Mismatched \" ")))
		       (t (error "Illegal expression")))))
    (setq result (cpad:parse-funcall begin result))
    (cpad:back-token)
    result))

(defun cpad:parse-funcall (begin result)
  (while (string= (cpad:get-next-token) "(")	;関数のパース
      ;;without arguments
      (if (string= (cpad:get-next-token) ")")
	  (setq result (list 'APP begin (point) result nil))
	;;with arguments
	(let ((dummy (cpad:back-token))
	      (args (list (cpad:exp))))
	  (while (string= (cpad:get-next-token) ",")
	    (setq args (cons (cpad:exp) args)))
	  (cpad:back-token)
	  (cpad:check-next-token ")")
	  (setq result (list 'APP begin (point) result (nreverse args))))))
  result)



(defun cpad:parse-func-args ()		;関数の引数を処理する
  "Parse function arguments.
Parse function's arguments and return them as a list."
  (let ((id (cpad:get-next-token))
	(args '())
	(without-arg-p nil))
    (cond ((string= id ")")
	   (cpad:back-token)
	   nil)
	  ((cpad:alphap (string-to-char id))	;ここで引数を期待している
	   (let ((next-token (cpad:get-next-token)))
	     (if (not (string= next-token "{"))
		 (cpad:back-token)
	       ;; unit argument
	       (cpad:back-token)
	       (let ((begin-unit (point)))
		 (if (search-forward "}")
		     (setq id (concat id (buffer-substring begin-unit (point))))
		   (error "Mismatched {}"))))
	     (setq args (cons id args)))
	   ;; multiple arguments 未完成
	   (while (string= (cpad:get-next-token) ",")
	     (setq id (cpad:get-next-token))
	     (let ((next-token (cpad:look-next-token)))
	       (if (string= next-token "{") ;; unit argument
		   (let ((begin-unit (point)))
		     (if (search-forward "}")
			 (setq id (concat id (buffer-substring begin-unit (point))))
		       (error "Mismatched {}"))))
	       (setq args (cons id args))))
	   (cpad:back-token)
	   (nreverse args)))))



(defun cpad:parse-func-def ()
  "Parse function definition"
  (let ((parsed-func-def (cpad:exp)))
    (cpad:check-next-token ";")
    parsed-func-def))

(defun cpad:exp-begin (e)
  "Get the position of a beginning of expression"
  (nth 1 e))

(defun cpad:exp-end (e) (nth 2 e))

(defun cpad:exp-arg1 (e)
  "Get the id symbol or value of the expression 'E"
  (nth 3 e))

(defun cpad:exp-arg2 (e)
  "Get the supplementary value  of the expression 'E"
  (nth 4 e))

(defun cpad:exp-arg3 (e)
  "Get the supplementary value  of the expression 'E"
  (nth 5 e))

;;; evaluation
(defun cpad:evaluate (e)
  "Evaluate the expression.
Evaluate the expression 'E and return the evaluated value or nil if not evaluated."
  (let ((k (car e)))
    (cond
      ((eq k 'PAR)
       (cpad:evaluate (cpad:exp-arg1 e)))
      ((eq k 'SYM)
       (cpad:exp-arg1 e))
      ((eq k 'TRUE)
       'TRUE)
      ((eq k 'FALSE)
       'FALSE)
      ((eq k 'NUM)
       (cpad:exp-arg1 e))
      ((eq k 'UNIT)			;unit evaluation
;       (message "cpad:evaluate:e %s" e)
       (let ((num (cpad:exp-arg1 e))
	     (unt (cpad:exp-arg2 e)))
;	 (message "cpad:evaluate:num %s" num)
;	 (message "cpad:evaluate:unt %s" unt)
	 (let ((built-unit (SI:build-unit-string unt)))
;	   (message "cpad:evaluate built-unit -> %s" built-unit)
;	   (message "(same-class-p built-unit 'basic-units) -> %s" (same-class-p built-unit 'basic-units))
	   (cond ((same-class-p built-unit 'basic-units)
		  (SI:make-quantity (get-symbol built-unit) built-unit SI:void-prefix num))
		 (t
		  (SI:make-quantity (get-symbol built-unit) built-unit (get-prefix built-unit) num))))))
      ((eq k 'MINUS)
       (let ((v (cpad:evaluate (cpad:exp-arg1 e))))
	 (if v
	     (if (numberp v)
		 (if (> v 0) (- v) nil)
	       (progn (goto-char (cpad:exp-begin e))
		      (error "Number expected")))
	   nil)))
      ((eq k 'LIST)			;リストの評価
       (let ((args (cpad:exp-arg1 e))
	     (values '())
	     v)
	 (while (and args (setq v (cpad:evaluate (car args))))
	   (setq values (cons v values))
	   (setq args (cdr args)))
	 (if args
	     nil
	     (append (nreverse values) 'EMPTY))))
      ((eq k 'LAM)
       (cons 'LAM (buffer-substring (cpad:exp-begin e) (cpad:exp-end e))))
      ((eq k 'STR)			;文字列の処理
       (cons 'STR (buffer-substring (cpad:exp-begin e) (cpad:exp-end e))))
      (t nil))))

(defun cpad:reduciblep (e)
  "Check if the expression 'E is reducible."
  (if (not (cpad:evaluate e))
      t
    nil))

(defun cpad:insert-value (v)
  (cond ((stringp v) (insert v))
	((eq v 'TRUE) (insert "true"))
	((eq v 'FALSE) (insert "false"))
	((numberp v)
	 (cond ((< v 0)
		(insert "(")
		(insert (int-to-string v))
		(insert ")"))
	       (t (insert (int-to-string v)))))
	((and (consp v) (eq (car v) 'LAM))
	 (insert "(")
	 (insert (cdr v))
	 (insert ")"))
	((or (consp v) (eq v 'EMPTY))
	 (insert "[")
	 (while (not (eq v 'EMPTY))
	   (cpad:insert-value (car v))
	   (setq v (cdr v))
	   (if (not (eq v 'EMPTY)) (insert ",")))
	 (insert "]"))
	((same-class-p v 'quantity)
	 (insert (convert2string v)))
	((same-class-p v 'numeric)
	 (insert (get-value v)))
	(t
	 (error "cpad:insert-value: %s not implemented" v))))

(defun cpad:delete-exp (e)
  "Delete an expression.
Delete an expression 'E  from current buffer and move point to where the expression began."
  (delete-region (cpad:exp-begin e) (cpad:exp-end e))
  (goto-char (cpad:exp-begin e)))

(defvar cpad:show-cpadex t
  "Flag whether it shows expressions during calculation or not.")

(defvar cpad:begin-point 0)

(defun cpad:point-cpadex (e)
  (goto-char (cpad:exp-begin e))
  (if (and cpad:show-cpadex (/= cpad:begin-point (cpad:exp-begin e)))
      (sit-for 1)))

(defun cpad:need-no-par (e)
  "Check if an expression has paremeter?"
  (memq (car e) '(PAR SYM TRUE FALSE NUM UNIT APP LIST))) ;IF,REL,ADD,MUL,MINUS

(defvar cpad:var-count 0)

(defun cpad:reduce-exp (e no-par)
  "Reduce an expression.
The argument 'E' is the expression to be reduced."
  (let ((k (car e))
	(cpad:var-count 0))
    (cond
     ((eq k 'IF)			;IF statement
      (cpad:reduce-conditional-if e))
     ((eq k 'REL)			;比較演算子の処理
      (cpad:reduce-relative-op e))
     ((or (eq k 'ADD) (eq k 'MUL))	;算術演算子の処理
      (cpad:reduce-arith-op e))
     ((eq k 'MINUS)
      (let ((v (cpad:evaluate (cpad:exp-arg1 e))))
	(cond (v (cpad:point-cpadex e)
		 (cpad:check-number v)
		 (cpad:delete-exp e)
		 (insert (int-to-string (- v)))
		 t)
	      (t
	       (cpad:reduce-exp (cpad:exp-arg1 e) nil)
	       nil))))
     ((eq k 'LOG-NOT)
      (cpad:reduce-logical-not e))
     ((eq k 'LOG-BIN-OP)
      (cpad:reduce-logical-binop e))
     ((eq k 'APP)			;関数の処理部分か??
      (cpad:reduce-funcall e))
     ((eq k 'PAR)
       (cond ((cpad:reduce-exp (cpad:exp-arg1 e) t)
	      (delete-region (cpad:exp-begin e) (1+ (cpad:exp-begin e)))
	      (re-search-forward ")")
	      (delete-backward-char 1)
	      t)
	     (t nil)))
      ((eq k 'LIST)
       (let ((args (cpad:exp-arg1 e)))
	 (while (and args (cpad:evaluate (car args)))
	   (setq args (cdr args)))
	 (if args
	     (cpad:reduce-exp (car args) t))
	 nil))
      ((eq k 'STR)			;文字列の処理
       (cpad:delete-exp e)
       (cpad:insert-value (cpad:exp-arg1 e))
       t)
      (t
       (error "reduce:Not yet implemented %s" k)))))

;;; arithmatic


(defun cpad:sum (e values)
  "Compute summation"
  (let ((item (car values)))
    (cpad:check-numargs values 1)
    (cpad:check-list item)
    (cpad:delete-exp e)
    (if (eq item 'EMPTY)
	(cpad:insert-value 0)
      (let ((ans 0))
	(while (not (eq item 'EMPTY))
	  (setq ans (+ ans (car item)))
	  (setq item (cdr item))
	  )
	(cpad:insert-value ans)))))

(defun cpad:reduce-funcall (e)
  "Reduce a function call expression."
  (let ((fun (cpad:evaluate (cpad:exp-arg1 e)))
	(args (cpad:exp-arg2 e))
	(values '())
	v)
    (while (and args (setq v (cpad:evaluate (car args))))
      (setq values (cons v values))
      (setq args (cdr args)))
    (setq values (nreverse values))
    (cond ((null fun)
	   (cpad:reduce-exp (cpad:exp-arg1 e) nil) nil)
	  (args
	   (cpad:reduce-exp (car args) t)
	   nil)
	  ((progn (cpad:point-cpadex e) nil))
	  ((and (consp fun) (eq (car fun) 'LAM)) ;ラムダ式の処理 reduce lambda expression
	   (let (b
		 formals
		 insert-par)
	     (setq b (cpad:exp-arg1 e))
	     (while (eq (car b) 'PAR)
	       (setq b (cpad:exp-arg1 b)))
	     (if (not (eq (car b) 'LAM))
		 (error "Lam expected"))
	     (setq formals (cpad:exp-arg1 b))
	     (cpad:check-numargs values (length formals))
	     (setq b (cpad:exp-arg2 b))
	     (setq insert-par (and (not no-par)
				   (not (cpad:need-no-par b))))
	     (delete-region (cpad:exp-end b) (cpad:exp-end e))
	     (goto-char (cpad:exp-end b))
	     (if insert-par (insert ")"))
	     (save-excursion
	       (delete-region (cpad:exp-begin e) (cpad:exp-begin b))
	       (goto-char (cpad:exp-begin e))
	       (if insert-par (insert "("))
	       (cpad:substitute-args b (- (if insert-par
					      (1+ (cpad:exp-begin e))
					    (cpad:exp-begin e))
					  (cpad:exp-begin b))
				     formals values))
	     (cpad:need-no-par b)))
	  ((not (stringp fun))
	   (error "Function expected"))
	  ((string= fun "mod")	;このあたりが組み込み関数の実装部分か??
	   (cpad:check-numargs values 2)
	   (cpad:delete-exp e)
	   (cpad:insert-value (mod (car values) (car (cdr values))))
	   t)
	  ((string= fun "to_f")
	   (cpad:check-numargs values 1)
	   (cpad:delete-exp e)
	   (cpad:insert-value (float (car values)))
	   t)
	  ((string= fun "log")
	   (cond ((= (length values) 2)
		  (cpad:delete-exp e)
;		  (cpad:insert-value (log (car values) (car (cdr values)))))
		  (cpad:insert-value (calc-eval (format "log(%s %s)" (car (cdr values)) (car values)))))
		 ((= (length values) 1)
		  (cpad:delete-exp e)
;		  (cpad:insert-value (log (car values))))
		  (cpad:insert-value (calc-eval (format "log(%s)" (car values)))))
		 (t
		  (error "Number of arguments mismatched")))
	   t)
	  ((string= fun "log10")
	   (cpad:check-numargs values 1)
	   (cpad:delete-exp e)
	   (cpad:insert-value (log10 (car values)))
	   t)
	  ((string= fun "expt")
	   (cpad:check-numargs values 2)
	   (cpad:delete-exp e)
	   (let ((x (car values))
		 (y (car (cdr values)))
		 (result))
	     (cond ((> y 0)
		    (cpad:insert-value (expt x y)))
		   ((= y 0)
		    (cpad:insert-value 1))
		   ((< y 0)
		    (cpad:insert-value (/ 1.0 (expt x (- y)))))))
	   t)
	  ((string= fun "comb")
	   (cpad:check-numargs values 2)
	   (cpad:delete-exp e)
;	   (cpad:insert-value (cpad:comb (car values) (car (cdr values))))
	   (cpad:insert-value (calc-eval (format "choose(%s,%s)" (car values) (car (cdr values)))))
	   t)
	  ((string= fun "fact")
	   (cpad:check-numargs values 1)
	   (cpad:delete-exp e)
;	   (cpad:insert-value (cpad:factorial (car values)))
	   (cpad:insert-value (calc-eval (format "fact(%s)" (car values))))
	   t)
	  ((string= fun "exp")
	   (cpad:check-numargs values 1)
	   (cpad:delete-exp e)
;	   (cpad:insert-value (exp (car values)))
	   (cpad:insert-value (calc-eval (format "exp(%s)" (car values))))
	   t)
	  ((string= fun "sqrt")
	   (cpad:check-numargs values 1)
	   (cpad:delete-exp e)
;	   (cpad:insert-value (sqrt (car values)))
	   (cpad:insert-value (calc-eval (format "sqrt(%s)" (car values))))
	   t)
	  ;;記号処理関数 symbol processing functions
	  ((string= fun "eq")
	   (cpad:check-numargs values 2)
	   (cpad:delete-exp e)
	   (if (string= (car values) (car (cdr values)))
	       (insert "true")
	     (insert "false"))
	   t)
	  ;;リスト処理関数 list processing functions
	  ((string= fun "cons")
	   (cpad:check-numargs values 2)
	   (cpad:check-list (car (cdr values)))
	   (cpad:delete-exp e)
	   (cpad:insert-value (cons (car values) (car (cdr values))))
	   t)
	  ((string= fun "is_nil")
	   (cpad:check-numargs values 1)
	   (cpad:check-list (car values))
	   (cpad:delete-exp e)
	   (if (eq (car values) 'EMPTY)
	       (insert "true")
	     (insert "false"))
	   t)
	  ((string= fun "is_list")
	   (cpad:check-numargs values 1)
	   (cpad:delete-exp e)
	   (if (and (not (eq v 'EMPTY))
		    (not (and (consp v) (not (eq (car v) 'LAM)))))
	       (insert "false")
	     (insert "true"))
	   t)
	  ((string= fun "first")
	   (cpad:check-numargs values 1)
	   (cpad:check-nonempty-list (car values))
	   (cpad:delete-exp e)
	   (cpad:insert-value (car (car values)))
	   t)
	  ((string= fun "rest")
	   (cpad:check-numargs values 1)
	   (cpad:check-nonempty-list (car values))
	   (cpad:delete-exp e)
	   (cpad:insert-value (cdr (car values)))
	   t)
	  ((string= fun "length")
	   (cpad:check-numargs values 1)
	   (cpad:check-list (car values))
	   (cpad:delete-exp e)
	   (if (eq (car values) 'EMPTY)
	       (cpad:insert-value 0)
	     (let ((i 1))
	       (while (not (eq (cdr (car values)) 'EMPTY))
		 (setq values (list (cdr (car values))))
		 (setq i (+ i 1)))
	       (cpad:insert-value i)))
	   t)
	  ;;統計関数 statistical funcitons
	  ((string= fun "sum")
	   (cpad:sum e values)
	   t)
	  ;;単位処理関数 unit processing functions
	  ((string= fun "conv")
	   (cpad:check-numargs values 2)
	   (cpad:delete-exp e)
	   (cpad:insert-value (convert2string (SI:convert (car values) (car (cdr values)))))
	   t)
	  ((string= fun "coerce")
	   (cpad:check-numargs values 2)
	   (cpad:delete-exp e)
	   (cpad:insert-value (convert2string (SI:coerce (car values) (car (cdr values)))))
	   t)
	  ((string= fun "standardize")
	   (cpad:check-numargs values 1)
	   (cpad:delete-exp e)
	   (cpad:insert-value (convert2string (standardize (car values))))
	   t)
	  (t
	   ;;上に遡って関数定義をパース user-defined functions
;	   (message "cpad:reduce-exp: values -> %s " values)
;	   (message "cpad:reduce-exp: (car (cdr values)) -> %s " (car (cdr values)))
	   (if (not (cpad:search-func-def e no-par fun values)) ;カレントバッファを検索
	       (cpad:search-default-func-def e no-par fun values))	;標準ライブラリーを検索
	   ))))


(defun cpad:reduce-arith-op (e)
  "Reduce the arithmatic operator expression."
  (let ((op (cpad:exp-arg1 e))
	(v1 (cpad:evaluate (cpad:exp-arg2 e)))
	(v2 (cpad:evaluate (cpad:exp-arg3 e))))
;    (message "cpad:reduce-arith-op: (cpad:exp-arg2 e) -> %s" (cpad:exp-arg2 e))
;    (message "cpad:reduce-arith-op: (cpad:exp-arg3 e) -> %s" (cpad:exp-arg3 e))
;    (message "cpad:reduce-arith-op: v1: %s" v1)
;    (message "cpad:reduce-arith-op: v2: %s" v2)
    (cond ((and v1 v2)
	   (cpad:point-cpadex e)
	   (cpad:delete-exp e)
	   (cpad:insert-value (cond
			       ((string= op "+")
				(cond ((numberp v1)
				       (+ v1 v2))
				      ((obj-of-class-p v1 'quantity)
				       (assert (obj-of-class-p v2 'quantity))
;				       (message "cpad:reduce-exp: (add v1 v2) -> %s" (add v1 v2))
				       (add v1 v2))))
			       ((string= op "-")
				(cond ((numberp v1)
				       (- v1 v2))
				      ((obj-of-class-p v1 'quantity)
				       (assert (obj-of-class-p v2 'quantity))
;				       (message "cpad:reduce-exp: (subtract v1 v2) -> %s" (subtract v1 v2))
				       (subtract v1 v2))))
			       ((string= op "*")
				(cond ((numberp v1)
				       (cond ((numberp v2)
					      (* v1 v2))
					     ((obj-of-class-p v2 'quantity)
					      (multiply v2 v1))
					     (t
					      (errror "cpad:reduce-arith-op:"))))
				      ((obj-of-class-p v1 'quantity)
;				       (message "cpad:reduce-exp: (multiply v1 v2) -> %s" (multiply v1 v2))
				       (multiply v1 v2))))
			       ((string= op "/")
				(cond ((and (numberp v2) (zerop v2))
				       (error "Division by zero"))
				      ((numberp v1)
				       (if (and (integerp v1) (integerp v1))
					   (setq v1 (float v1)))
				       (/ v1 v2))
				      ((obj-of-class-p v1 'units)
				       (divide v1 v2))))))
	   t)
	  (v1
	   (cpad:reduce-exp (cpad:exp-arg3 e) nil) nil)
	  (t
	   (cpad:reduce-exp (cpad:exp-arg2 e) nil) nil))))

(defun cpad:reduce-relative-op (e)
  "Reduce the relative operator expression."
  (let ((op (cpad:exp-arg1 e))
	(v1 (cpad:evaluate (cpad:exp-arg2 e)))
	(v2 (cpad:evaluate (cpad:exp-arg3 e))))
    (cond ((and v1 v2)
	   (cpad:point-cpadex e)
	   (cpad:check-two-numbers v1 v2)
	   (cpad:delete-exp e)
	   (if (cond ((string= op "==") (= v1 v2))
		     ((string= op "<") (< v1 v2))
		     ((string= op ">") (> v1 v2))
		     ((string= op "<=") (<= v1 v2))
		     ((string= op ">=") (>= v1 v2))
		     ((string= op "!=") (/= v1 v2)))
	       (insert "true")
	     (insert "false"))
	   t)
	  (v1 (cpad:reduce-exp (cpad:exp-arg3 e) nil) nil)
	  (t (cpad:reduce-exp (cpad:exp-arg2 e) nil) nil))))

(defun cpad:reduce-conditional-if (e)
  "Reduce if statement"
  (let ((b (cpad:evaluate (cpad:exp-arg1 e))))	; 条件節を評価
    (cond ((null b)
	   (cpad:reduce-exp (cpad:exp-arg1 e) t) nil)
	  ((eq b 'TRUE)		; 真の場合
	   (cpad:point-cpadex e)
	   (goto-char (cpad:exp-end e))
	   (delete-region (cpad:exp-end (cpad:exp-arg2 e)) (cpad:exp-end e))
	   (delete-region (cpad:exp-begin e) (cpad:exp-begin (cpad:exp-arg2 e)))
	   (cpad:need-no-par (cpad:exp-arg2 e)))
	  ((eq b 'FALSE)		; 偽の場合
	   (cpad:point-cpadex e)
	   (goto-char (cpad:exp-end e))
	   (delete-region (cpad:exp-begin e) (cpad:exp-begin (cpad:exp-arg3 e)))
	   (cpad:need-no-par (cpad:exp-arg3 e)))
	  (t
	   (goto-char (cpad:exp-begin e))
	   (error "Illegal condition")))))

(defun cpad:reduce-logical-binop (e)
  "Reduce logical binary operator expression"
  (let ((operator (cpad:exp-arg1 e))
	(operand-l (cpad:evaluate (cpad:exp-arg2 e)))
	(operand-r (cpad:evaluate (cpad:exp-arg3 e))))
    (cond ((and operand-l operand-r)
	   (cpad:check-truth-value operand-l)
	   (cpad:check-truth-value operand-r)
	   (cpad:delete-exp e)
	   (if (cond ((string= operator "&")
		      (if (and (eq operand-l 'TRUE) (eq operand-r 'TRUE))
			  t
			nil))
		     ((string= operator "|")
		      (if (or (eq operand-l 'TRUE) (eq operand-r 'TRUE))
			  t
			nil))
		     (t
		      (error "Undefined logical operator %s" op)))
	       (insert "true")
	     (insert "false"))
	   t)
	  (operand-l (cpad:reduce-exp (cpad:exp-arg3 e) nil) nil)
	  (t
	   (cpad:reduce-exp (cpad:exp-arg2 e) nil) nil))))

(defun cpad:reduce-logical-not (e)
  "Reduce logical negation expression"
  (let ((v (cpad:evaluate (cpad:exp-arg2 e))))
    (cond (v
	   (let ((val
		  (cond ((eq v 'TRUE)
			 'FALSE)
			((eq v 'FALSE)
			 'TRUE)
			((eq v 'UNKNOWN)
			 'UNKNOWN)
			(t
			 (goto-char (cpad:exp-begin e))
			 (error "cpad:evaluate:Truth value expected")))))
	     (cpad:delete-exp e)
	     (cpad:insert-value val)
	     t))
	  (t				;ここの処理の意味が不明
	   (cpad:reduce-exp (cpad:exp-arg2 e) nil)
	   nil))))



(defun cpad:search-func-def (e no-par fun values)
  "Parse function definition.
Parse fuction definition backwardly in the current buffer and return t if found, nil if not found.
This and 'cpad:search-default-func-def function are pretty complicated,and have to be reconstructed someday."
  (catch 'escape
;    (message "cpad:search-func-def entered")
    (cond ((re-search-backward ;上に遡って関数定義をパース
	    (concat "^[ \t]*" fun cpad:scanner-func-re)
	    0 t))
	  (t
	   (goto-char (cpad:exp-begin e)) 
	   (throw 'escape nil)))
    (cpad:get-next-token)
    (cpad:check-next-token "(") ;関数定義内の引数のパース
    (let ((func-def nil)		;関数定義の本体
	  (func-args (cpad:parse-func-args))
	  (insert-par nil)		;括弧を挿入すべきか否かのフラグ
	  (flag-found nil))
;      (message "cpad:search-func-def: fun -> %s, func-args -> %s, values -> %s" fun func-args values)
      ;;引数の数 arity が合致しない場合、さらに上流を検索する
      (cond ((/= (length values) (length func-args)) 
	     (setq flag-found (cpad:search-func-def e no-par fun values))
	     (goto-char (cpad:exp-begin e))
	     (if flag-found
		 (throw 'escape t)
	        (throw 'escape nil))))
      ;;引数の数 arity が合致した場合
      (cpad:check-numargs values (length func-args))
      (cpad:check-next-token ")")
      (cpad:check-next-token ":=")
      (setq func-def (cpad:parse-func-def))			;関数の定義部分をパースする
      (setq insert-par (and (not no-par)
			    (not (cpad:need-no-par func-def))))
      ;; ここから原バッファーで処理する
      (goto-char (cpad:exp-begin e))
      (cpad:delete-exp e)
      (if insert-par (insert "("))
      (insert-buffer-substring (current-buffer)
			       (cpad:exp-begin func-def) (cpad:exp-end func-def))
      (if insert-par (insert ")"))
      (save-excursion
	;;実引数の仮引数への代入を実行する部分か??
	(cpad:substitute-args func-def			;func-defは関数定義
		    (- (if insert-par	
			   (1+ (cpad:exp-begin e))
			 (cpad:exp-begin e))
		       (cpad:exp-begin func-def))
		    func-args
		    values))
      (cpad:need-no-par func-def))
    t))

;; 変数 'cpad:default-library-path で指定された標準ライブラリーから関数定義を探す。
(defun cpad:search-default-func-def (e no-par fun values)
  "Parse function definition in default library.
Parse function definition in default library 'cpad:default-library-path.
'FIRST-TIME-P' argument stands for it is the first time call of function or not.
Return t if found, nil if not found."

    (let ((lib-buffer (get-file-buffer cpad:default-library-path))
	  (already-opened nil)
	  (original-buffer (buffer-name (current-buffer)))
	  (previous-mark (point))
	  (insert-par nil)		;括弧を挿入すべきか否かのフラグ
	  (func-def nil)		;関数定義の本体
	  (func-args nil))
      (cond (;; まだ開かれていないならば、新規に開く
	     (null lib-buffer)		
	     (setq already-opened nil)
	     (setq lib-buffer (find-file-noselect  cpad:default-library-path))) ;ファイルがない場合のチェックは??
	    (t
	     (setq already-opened t)))
      (save-excursion			;標準ライブラリーのバッファ上に移動
	(set-buffer lib-buffer)		; 間接バッファーを利用すべきか?
	(goto-char (point-max))
	(catch 'escape
	  ;;ここからループ。上に遡ってライブラリー上で関数定義をサーチ
	  (while (re-search-backward (concat "^[ \t]*" fun cpad:scanner-func-re) 0 t)
	    (cpad:get-next-token)
	    (cpad:check-next-token "(")
	    ;;関数定義内の引数のパース
	    (setq func-args (cpad:parse-func-args))
;	    (message "cpad:search-func-def: fun -> %s, values -> %s, func-args -> %s" fun values  func-args)
	    (cond ((= (length values) (length func-args))
		   ;;引数の数 arity が合致した場合
		   (cpad:check-numargs values (length func-args))
		   (cpad:check-next-token ")")
		   (cpad:check-next-token ":=")
		   (setq func-def (cpad:parse-func-def))			;関数の定義部分をパースする
		   (setq insert-par (and (not no-par)
					 (not (cpad:need-no-par func-def))))
		   (throw 'escape nil)) ;ループから大域脱出する
		  ;;引数の数 arity が合致しない場合、さらに上流を検索する
	      ))
	  ;;定義が見つからない場合
	  (goto-char (cpad:exp-begin e)) ;必要か??
	  (error "Definition %s with arity %s not found" fun (length values))
	  )				;escape
	)				;end of save-excursion
      ;;ここから原バッファーで処理する
      (goto-char (cpad:exp-begin e))
      (cpad:delete-exp e)
      (if insert-par (insert "("))
      (insert-buffer-substring lib-buffer
			       (cpad:exp-begin func-def) (cpad:exp-end func-def))
      (if insert-par (insert ")"))
      (save-excursion 
	;;実引数の仮引数への代入を実行する部分か??
	(cpad:substitute-args func-def			;func-defは関数定義
			      (- (if insert-par	
				     (1+ (cpad:exp-begin e))
				   (cpad:exp-begin e))
				 (cpad:exp-begin func-def))
			      func-args
			      values))
      (cpad:need-no-par func-def))
    t)

	   

;; カレントバッファが標準ライブラリであるか否かを調べる
(defun cpad:current-is-default-library-buffer-p ()
  "Check if the current buffer is the buffer of default library or not"
  (if (string= (buffer-name (current-buffer))
	       (buffer-name (get-file-buffer cpad:default-library-path)))
      t
    nil))
  
  
(defun cpad:substitute-args (func-body offset func-args values)
  "Substitute arguments with called values."
  (let ((k (car func-body)))
;    (message "cpad:substitute-args: func-body -> %s" func-body)
;    (message "cpad:substitute-args: func-args -> %s" func-args)
;    (message "cpad:substitute-args: values -> %s" values)
    (cond
      ((eq k 'APP)
       (let ((a (reverse (cpad:exp-arg2 func-body))))
	 (while a
;	   (message "cpad:substitute-args: a -> %s" a)
	   (cpad:substitute-args (car a) offset func-args values)
	   (setq a (cdr a))))
       (cpad:substitute-args (cpad:exp-arg1 func-body) offset func-args values))
      ((eq k 'SYM)
       (let ((stop nil))
	 (while (and (not stop) func-args) ;ここで仮引数を実引数に置換する
	   (cond ((string= (cpad:exp-arg1 func-body) (car func-args))
		  (delete-region (+ (cpad:exp-begin func-body) offset)
				 (+ (cpad:exp-end func-body) offset))
		  (goto-char (+ (cpad:exp-begin func-body) offset))
		  (cpad:insert-value (car values))
		  (setq stop t))
		 ;; 引数が単位付き引数の場合の処理 in case of unit argument
		 ((and (not (null (string-match "\\([^}]+\\){\\([^\}]+\\)}" (car func-args) 0)))
		       (equal (match-beginning 1) 0)
		       (match-string 2 (car func-args)))
		  (let ((matched-symbol (match-string 1 (car func-args)))
			(matched-unit-argument (match-string 2 (car func-args)))
			(value (car values)))
		    (cond ((string= (cpad:exp-arg1 func-body) matched-symbol)
			   (delete-region (+ (cpad:exp-begin func-body) offset)
					  (+ (cpad:exp-end func-body) offset))
			   (goto-char (+ (cpad:exp-begin func-body) offset))
			   ;;ここで単位を変換する
;			   (message "cpad:substitute-args: value -> %s" value)
;			   (message "cpad:substitute-args: matched-unit-argument -> %s" matched-unit-argument)
;			   (message "cpad:substitute-args: (SI:build-unit-string matched-unit-argument) -> %s" (SI:build-unit-string matched-unit-argument))
;			   (message "cpad:substitute-args: (convert2string (SI:convert value (SI:build-unit-string matched-unit-argument))) -> %s" (convert2string (SI:convert value (SI:build-unit-string matched-unit-argument))))
			   (cpad:insert-value (convert2string (SI:convert value (SI:build-unit-string matched-unit-argument))))
			   (setq stop t))
			  (t
			   (setq func-args (cdr func-args))
			   (setq values (cdr values))))))
		 (t
		  (setq func-args (cdr func-args))
		  (setq values (cdr values)))))))
      ((eq k 'UNIT)			;(list 'UNIT begin (point) id (buffer-substring begin-unit (point)))
;       (message "cpad:substitute-args:  (cpad:exp-arg1 func-body) -> %s" (cpad:exp-arg1 func-body))
;       (message "cpad:substitute-args:  (cpad:exp-arg2 func-body) -> %s" (cpad:exp-arg2 func-body))
;       (message "cpad:substitute-args:  (cpad:exp-arg3 func-body) -> %s" (cpad:exp-arg3 func-body))
       

       )
      ((eq k 'IF)
       (cpad:substitute-args (cpad:exp-arg3 func-body) offset func-args values)
       (cpad:substitute-args (cpad:exp-arg2 func-body) offset func-args values)
       (cpad:substitute-args (cpad:exp-arg1 func-body) offset func-args values))
      ((or (eq k 'REL) (eq k 'ADD) (eq k 'MUL))
       (cpad:substitute-args (cpad:exp-arg3 func-body) offset func-args values)
       (cpad:substitute-args (cpad:exp-arg2 func-body) offset func-args values))
      ((eq k 'LOG-BIN-OP)
       (cpad:substitute-args (cpad:exp-arg3 func-body) offset func-args values)
       (cpad:substitute-args (cpad:exp-arg2 func-body) offset func-args values))
      ((eq k 'LOG-NOT)
       (cpad:substitute-args (cpad:exp-arg2 func-body) offset func-args values))
      ((or (eq k 'PAR) (eq k 'MINUS))
       (cpad:substitute-args (cpad:exp-arg1 func-body) offset func-args values))
      ((eq k 'LIST)
       (let ((a (reverse (cpad:exp-arg1 func-body))))
	 (while a
	   (cpad:substitute-args (car a) offset func-args values)
	   (setq a (cdr a)))))
      ((eq k 'LAM)
       (let ((lam-args (cpad:exp-arg1 func-body)) (lam-variants '()))
	 (while lam-args
	   (setq cpad:var-count (+ cpad:var-count 1))
	   (setq lam-variants
		 (cons (concat "_" (int-to-string cpad:var-count))
		       lam-variants))
	   (setq lam-args (cdr lam-args)))
	 (setq lam-variants (nreverse lam-variants))
	 (cpad:substitute-args (cpad:exp-arg2 func-body) offset
		     (append (cpad:exp-arg1 func-body) func-args)
		     (append lam-variants values))
	 (goto-char (+ (cpad:exp-begin func-body) offset))
	 (re-search-forward "(")
	 (let ((p (point)))
	   (re-search-forward ")")
	   (delete-region (- p 1) (point))
	   (insert "(")
	   (while lam-variants
	     (insert (car lam-variants))
	     (setq lam-variants (cdr lam-variants))
	     (if lam-variants (insert ",")))
	   (insert ")")))))))

(defun cpad:once-reduce ()
  "Reduce once."
  (let ((cpad:begin-point (point))
	(e (cpad:exp))
	(cpad:show-cpadex t)
	(case-fold-search nil))
;    (message "cpad:once-reduce: e -> %s" e)
    (if (cpad:reduciblep e)			;(if (not (cpad:evaluate e))
	(cpad:reduce-exp e t))
    (goto-char cpad:begin-point)))

(defun cpad:repeat-reduce (n)
  (let ((cpad:begin-point (point))
	e
	(cpad:show-cpadex nil)
	(case-fold-search nil))
    (while (and (> n 0)
		(cpad:reduciblep (setq e (cpad:exp)))) ; (not (cpad:evaluate (setq e (cpad:exp)))))
      (cpad:reduce-exp e t)
      (goto-char cpad:begin-point)
      (sit-for 0)
      (setq n (1- n)))
    (goto-char cpad:begin-point)))

;;; ユーザー・インターフェイス Interactive functions
;;;###autoload
(defun cpad (n)
  (interactive "p")
  (if (= n 1)
      (cpad:once-reduce)
    (cpad:repeat-reduce n)))
;;;###autoload
(defun cpad* ()
  (interactive)
  (cpad:repeat-reduce 1000000))



;;; 構文テーブル syntax-table
(defvar cpad:syntax-table nil
  "Syntax table in use in cpad buffers.")

(if cpad:syntax-table
    ()
  (setq cpad:syntax-table (make-syntax-table))
  (modify-syntax-entry ?\  " " cpad:syntax-table) ;スペース文字をクラス空白に入れる。
  (modify-syntax-entry ?_ "_" cpad:syntax-table)
  (modify-syntax-entry ?\( "()" cpad:syntax-table)
  (modify-syntax-entry ?\) ")(" cpad:syntax-table)
  (modify-syntax-entry ?\[ "(]" cpad:syntax-table)
  (modify-syntax-entry ?\] ")[" cpad:syntax-table)
  (modify-syntax-entry ?\{ "(}" cpad:syntax-table)
  (modify-syntax-entry ?\} "){" cpad:syntax-table)
  (modify-syntax-entry ?\' "\"" cpad:syntax-table)
  (modify-syntax-entry ?\" "\"" cpad:syntax-table)
  (modify-syntax-entry ?\` "\"" cpad:syntax-table)
  (modify-syntax-entry ?# "<" cpad:syntax-table) ;コメント # ??
  (modify-syntax-entry ?\n ">" cpad:syntax-table) ;コメント終了??
  (modify-syntax-entry ?\\ "\\" cpad:syntax-table) ;エスケープ??
  (modify-syntax-entry ?$ "." cpad:syntax-table) ;句読点??
  (modify-syntax-entry ?? "_" cpad:syntax-table) ;シンボル構成可能??
  (modify-syntax-entry ?< "." cpad:syntax-table)
  (modify-syntax-entry ?> "." cpad:syntax-table)
  (modify-syntax-entry ?& "." cpad:syntax-table)
  (modify-syntax-entry ?| "." cpad:syntax-table)
  (modify-syntax-entry ?% "." cpad:syntax-table)
  (modify-syntax-entry ?= "." cpad:syntax-table)
  (modify-syntax-entry ?/ "." cpad:syntax-table)
  (modify-syntax-entry ?+ "." cpad:syntax-table)
  (modify-syntax-entry ?* "." cpad:syntax-table)
  (modify-syntax-entry ?- "." cpad:syntax-table)
  (modify-syntax-entry ?\; "." cpad:syntax-table)
  )

;;; 数学関数 mathematical functions


(defun cpad:gammln (xx)
  (if (< xx 1.0)
      (let ((z (- 1.0 xx)))
	(- (log (/ (* pi z) (sin (* pi z))))
	   (cpad:gammln (+ 1.0 z))))
      (let* ((z   (- xx 1.0))
	     (tmp (+ z 5.0 0.5)))
	(+ (* (log tmp) (+ z 0.5))
	   (- tmp)
	   (log (sqrt (* 2 pi)))
	   (log (+ 1.0
		   (/  76.18009173   (+ z 1.0))
		   (/ -86.50532033   (+ z 2.0))
		   (/  24.01409822   (+ z 3.0))
		   (/  -1.231739516  (+ z 4.0))
		   (/   0.120858003e-2 (+ z 5.0))
		   (/  -0.536382e-5    (+ z 6.0))))))))

(defun cpad:fgammln (xx)
  (assert (floatp xx) "argument must be a float")
  (if (< xx 1.0)
      (let ((z (- 1.0 xx)))
	(- (log (/ (* pi z)
		   (sin (* pi z))))
	   (cpad:fgammln (+ 1.0 z))))
      (let* ((z   (- xx 1.0))
	     (tmp (+ z 5.0 0.5)))
	(+ (* (log tmp) (+ z 0.5))
	   (- tmp)
	   (log (sqrt (* 2 pi)))
	   (log (+ 1.0
		   (/  76.18009173e0   (+ z 1.0))
		   (/ -86.50532033e0   (+ z 2.0))
		   (/  24.01409822e0   (+ z 3.0))
		   (/  -1.231739516e0  (+ z 4.0))
		   (/   0.120858003e-2 (+ z 5.0))
		   (/  -0.536382e-5    (+ z 6.0))))))))
(defun cpad:gamma-function (a)
  (typecase a
    (integer (exp (cpad:fgammln (float a))))
    (float (exp (cpad:fgammln        a )))
    (otherwise (assert "not implemented"))))

(defun cpad:log-gamma-function (a)
  (typecase a
    (integer (cpad:fgammln (float a)))
    (float (cpad:fgammln a))
    (otherwise (assert "not implemented"))))

(defun gamma-density (x a)
  (if (<= x 0.0)
      0.0
      (/ (* (expt x (1- a)) (exp (- x)))
	 (cpad:gamma-function a))))

(defun cpad:gamma-function-incomplete (a x)
  "Return incomplete gamma function P(a,x)"
  (if (or (<= a 0.0) (minusp x))
      (error "Invalid argument to gammp: ~F" (if (<= 0.0 a) a x))
     (if (< x (+ a 1.0))
 	(gser a x)			;use series
       (- 1.0 (gcf a x)))))		;use continued fraction


(defun gser (a x)
  "P(a,x) evaluated as series, also Ln(gamma)"
  (if (minusp x)
      (error "~F is less than 0" x)
    (if (zerop x) 0.0
      (do* ((gln (cpad:gammln a))
	    (itmax 100)
	    (eps 3.0e-7)
	    (ap a)
	    (sum (/ 1.0 a))
	    (del sum)
	    (n 1 (+ n 1)))
	  ((or (> n itmax)
	       (< (abs del) (* (abs sum) eps)))
	   (values (* sum (exp (+ (- x) (* a (log x)) (- gln)))) gln))
	(incf ap)
	(setf del (* del (/ x ap)))
	(incf sum del)))))

(defun gcf (a x)
  "Q(a,x) evaluated as continued fraction"
  (do* ((itmax 100)
	(eps 3.0e-7)
	(gln (cpad:fgammln a))
	(gold 0.0)			;previous value of g, tested for convergence
	(a0 1.0)
	(a1 x)
	(b0 0.0)
	(b1 1.0)			;setting up continued fraction
	(fac 1.0)
	(n 1 (+ n 1))
	(ana 0.0)
	(g 0.0)
	(anf 0.0))
      ((> n itmax) 
       (values (* g (exp (+ (- x) (* a (log x)) (- gln)))) gln))
    (setf ana (- n a))
    (setf a0 (* fac (+ a1 (* a0 ana))))
    (setf b0 (* fac (+ b1 (* b0 ana))))
    (setf anf (* fac n))
    (setf a1 (+ (* x a0) (* anf a1)))
    (setf b1 (+ (* x b0) (* anf b1)))
    (cond ((/= 0.0 a1)			;renormalize?
	   (setf fac (/ 1.0 a1))
	   (setf g (* b1 fac))
	   (if (< (abs (/ (- g gold) g)) eps)
	       (return (values (* g (exp (+ (- x) (* a (log x)) (- gln)))) gln)))))))

(defun cpad:gamma-cumulative (x a)
  (if (<= x 0.0)
      0.0
      (/ (cpad:gamma-function-incomplete a x)
	 (cpad:gamma-function a))))

;;;; Chi-Square Distribution

;;; CHI-SQUARE = (SUM (i 1 n) Yi**2)
;;; where the Yi are Normal(0, 1) independent RV's
;;; CHI-SQUARE >= 0

(defun cpad:chi-square-density (chi-sq n)
  (if (< chi-sq 0.0)
      0.0
      (let ((a (* 0.5 (float n))))
	(/ (* (expt chi-sq (1- a)) (exp (* -0.5 chi-sq)))
	   (* (expt 2.0 a)         (cpad:gamma-function a))))))

;;; CHI-SQUARE-CUMULATIVE is the same as GAMMA-CUMULATIVE(chi-sq/2 n/2)
;;;
(defun cpad:chi-square-cumulative (chi-sq n)
  (cpad:gamma-cumulative (/ chi-sq 2.0) (/ (float n) 2.0)))


(defun cpad:factorial (n)
  "Compute factorial"
  (cpad:factorial-iter 1 1 n))

(defun cpad:factorial-iter (product counter max-count)
  (if (> counter max-count)
      product
    (cpad:factorial-iter (* counter product)
			 (+ counter 1)
			 max-count)))
  
; (defun cpad:factorial (n)
;   "Compute factorial"
;   (let* ((i 1)
; 	 (fact 1)
; 	 (i-fact nil))
;     (while (< i n)
;       (setq i (1+ i))
;       (setq i-fact (* fact i))
;       (assert (>= i-fact fact) nil "Overflow in computing factorial")
;       (setq fact i-fact))
;     fact))

(defun cpad:comb (n k)
  "Compute combination,but result may not be accurate"
  (let ((n-k (- n k)))
    (if (< n-k k)
	(cpad:comb n n-k)				; n C k = n C n-k
      (do ((i n (1- i))
	   (j k (1- j))
	   (c1 1)
	   (c2 1))
	  ((<= j 0) (the integer (/ c1 c2)))
	;;桁溢れチェックは不十分
	(let ((c1i (* c1 i))
	      (c2j (* c2 j)))
	  (if (or (< c1i c1 ) (< c2j c2))
	      (error "Overflow in cpad:comb"))
	  (setq c1 c1i)
	  (setq c2 c2j))))))



;;; test

;;; cpad.el ends here


;;; Local Variables:
;;; End:

