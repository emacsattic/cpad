;;; eparser.el -- a small parser library in elisp

;;; Licence: 
;;
;; Copyright (C) 2002 Akimichi Tatsukawa <akimichi@mbox.co.jp>
;;
;; This file is NOT a part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;     
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;     
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.
;;
;; Please send suggestions and bug reports to <akimichi@mbox.co.jp>. 
;; The latest version of this package should be available at
;;
;;    <URL:http://akimichi.homeunix.net/~emile/program/elisp/>

;; Author: Akimichi Tatsukawa <akimichi@mbox.co.jp>
;; Keywords: parser,lexer
;; $Id: eparser.el,v 1.3 2002/07/16 11:25:04 emile Exp emile $
;; $Date: 2002/07/16 11:25:04 $
;; $Log: eparser.el,v $
;; Revision 1.3  2002/07/16 11:25:04  emile
;; Functions for debug message are all commented out.
;;
;; Revision 1.10  2002/05/02 02:48:26  emile
;; function ep:lexer-string
;;
;; Revision 1.9  2002/05/01 03:52:40  emile
;; *** empty log message ***
;;
;; Revision 1.8  2002/05/01 01:48:38  emile
;; *** empty log message ***
;;
;; Revision 1.7  2002/04/30 06:36:30  emile
;; *** empty log message ***
;;
;; Revision 1.6  2002/04/26 05:10:13  emile
;; *** empty log message ***
;;
;; Revision 1.2  2002/04/24 06:40:38  emile
;; *** empty log message ***
;;
;; Revision 1.5  2002/04/24 01:26:21  emile
;; *** empty log message ***
;;
;; Revision 1.4  2002/04/23 05:57:51  emile
;; *** empty log message ***
;;
;; Revision 1.3  2002/04/23 04:33:06  emile
;; *** empty log message ***
;;
;; Revision 1.2  2002/04/23 04:26:56  emile
;; *** empty log message ***
;;
;; Revision 1.1  2002/04/23 02:31:19  emile
;; Initial revision
;;

;;; Commentary:
;;   This library is a kind of parser generator of recursive descent parsing.
;;   It still remains early stage,so it is not easy to use.
;;   基本的な設計や関数名などは岩波講座ソフトウェア科学「プログラミング言語」(武市正人著)の9章を参考にし
;;   ています。
;;   The basic library design and function naming are mostly owed to the book "The programming language"
;;   (Iwanami software science series 4)  by Masato Takeichi.

;;; Usage:

;(require 'eparser)

;;; TODO:
;;    BNF parser
;;; Code:

(require 'cl)
;(require 'stream)

;;; user options
(defvar ep:seek-all-alternatives nil
  "*Flag to seek all alternatives at parsing.
Not tested. So if this flag is t, the parser may become very unstable.")

;;; util
;; utils for tagged-data manupulation
(defun ep:attach-tag (type-tag contents)
  (cons 'ep:tagged (cons type-tag contents)))

(defun ep:tagged-p (data)
  (if (eq (car data) 'ep:tagged)
      t
    nil))
      
(defun ep:type-tag (datum)
  (assert (ep:tagged-p datum) t "%s is not a tagged-data -- EP:TYPE-TAG" datum)
  (car (cdr datum)))
  
(defun ep:contents (datum)
  (assert (ep:tagged-p datum) t "%s is not a tagged-data -- EP:CONTENTS" datum)
  (cdr (cdr datum)))

; (defmacro ep:flip (proc)		;  (funcall (ep:flip (lambda (x) (print x))) '(1 2 3))
;   `#'(lambda (x y) (funcall ,proc y x)))


; (defmacro ep:foldl (proc) ; lst)	; (funcall (ep:foldl (ep:flip #'(lambda (x y) (list x y)))) '(num expr))
;   `#'(lambda (lst)
;        (cond ((null lst)
; 	      nil)
; 	     ((null (cdr lst))
; 	      (car lst))
; 	     (t
; 	      (funcall (ep:foldl ,proc)
; 		       (cons (funcall ,proc (car lst) (car (cdr lst)))
; 			     (cdr (cdr lst))))))))

; (defun ep:id (a)
;   a)


  

;;; lexer

;; token

;; token-table
(defvar ep:token-table nil "The token list")

(defun ep:define-token (token-type token-regex)
  (setq ep:token-table (cons (cons token-regex token-type) ep:token-table)))
;  `(defvar ep:token-table (cons (cons ,token-regex ,token-type) ep:token-table)))

(defun ep:define-subtoken-hook (matched-type matched-string)
  matched-type)

;; token-stream
;; token-stream data should be like (token1 token2 token3 ...)
(defun ep:look-nth-token (n token-stream)
  (nth n token-stream))

(defun ep:look-next-token (token-stream)
  (car token-stream))

(defun ep:rest-token (token-stream)
  (cdr token-stream))


(defun ep:lexer-region (start end &optional token-list carnivorous)
  "Lexer for the region.
If optional argument CARNIVOROUS is t, then lexer eats buffer as the process proceeds."
  (interactive "r")
  (save-excursion
    (let ((token-table (if token-list
		      token-list
		    ep:token-table))
	  (token-stream nil))
      (goto-char start)
      (while (< (point) end)
	(let ((matched-at-least-once nil))
	  (dolist (i token-table)
	    (if matched-at-least-once
		(return)
	      (if (looking-at (car i))
		  (let ((matched-str (buffer-substring (match-beginning 0) (match-end 0)))
			(matched-type (cdr i)))
		    (cond ((eq matched-type 'punctuation) ; ignore the punctuation
			   )
			  (t
			   (setq matched-type (ep:define-subtoken-hook matched-type matched-str))
			   (setq token-stream (cons (cons matched-type matched-str) token-stream))))
		    (setq matched-at-least-once t)
		    (if carnivorous
			(error "not implemented") ; eat
		      (goto-char (match-end 0)))))))	
	  ;; never matched
	  (if (null matched-at-least-once)
	      (error "Unknown token %s" (point))))
	)
;      (message "token-stream: %s" (reverse token-stream))
      (nreverse token-stream))))

(defun ep:lexer-string (str &optional token-list)
  "Lexer for the string.
Does not work properly!!"
  (let ((token-table (if token-list
			 token-list
		       ep:token-table))
	(token-stream nil)
	(index 0))
;    (message "(length str): %s" (length str))
    (while (< 0 (length str))
      (let ((matched-at-least-once nil))
	(dolist (i token-table)
	  (if matched-at-least-once
	      (return)
	    (let ((match-result (string-match (concat "^" (car i)) str)))
	      (if (not (null match-result))
		  (let ((matched-str (match-string 0 str))
			(matched-type (cdr i)))
;		    (message "matched-str:%s" matched-str)
;		    (message "match-result:%s" match-result)
;		    (message "(match-end 0):%s" (match-end 0))
		    (cond ((eq matched-type 'punctuation) ; skip the punctuation
			   )
			  (t
			   (setq matched-type (ep:define-subtoken-hook matched-type matched-str))
			   (setq token-stream (cons (cons matched-type matched-str) token-stream))))
		    (setq matched-at-least-once t)
		    (setq str (substring str (match-end 0) nil))
;		    (message "str:%s" str)
		    )))))
	;; never matched
	(if (null matched-at-least-once)
	    (error "Unknown token %s" str)))
      )
;    (message "token-stream: %s" (reverse token-stream))
    (nreverse token-stream)))


;;; token
;; 後に登録したほど優先して字句解析される The later defined token has the higher priority.
(ep:define-token 'punctuation "[ \t\n]+")	; スキップする文字もトークンとして登録する。
(ep:define-token 'integer "[+-]*[0-9]+")
(ep:define-token 'identifier "[a-zA-Z]\\([a-zA-Z]\\|[0-9]\\)*")
(ep:define-token 'open-paren "(")
(ep:define-token 'close-paren ")")



;;; parser

;; parser-message routines

(defun ep:construct-parser-message (syntax-tree token-list)
;  (list (list syntax-tree) token-list))
  (list syntax-tree token-list))

(defun ep:get-succeeded-token (parser-message)
  (car (car parser-message)))

(defun ep:get-token-stream (parser-message)
  (car (cdr (car parser-message))))


;; 基本関数 basic functions

(defun ep:succeed (succeeded-token rest-token-stream)
;  (ep:construct-parser-message succeeded-token rest-token-stream))
  (list (ep:construct-parser-message succeeded-token rest-token-stream)))

(defun ep:fail (token-stream)
  '())

(defmacro ep:satisfy (predicate); token-stream)
  `#'(lambda (token-stream)
       (cond ((null token-stream)
	      (ep:fail nil))
	     (t
	      (if (funcall ,predicate (ep:look-next-token token-stream))
		  (ep:succeed (ep:look-next-token token-stream) (ep:rest-token token-stream))
		(ep:fail nil))))))

(defmacro ep:seq (former-parser latter-parser); token-stream)
  `#'(lambda (token-stream)
       (let* ((former-result (funcall ,former-parser token-stream))
	      (new-token-stream (ep:get-token-stream former-result))
	      (former-result-token (ep:get-succeeded-token former-result)))
;	 (message "seq:former-result: %s" former-result)
;	 (message "seq:new-token-stream: %s" new-token-stream)
;	 (message "seq:former-result-token: %s" former-result-token)
	 (cond ((null former-result)
		nil)
	       (t
;		(message "seq:latter processing")
;		(message "seq:new-token-stream: %s" new-token-stream)
		(let* ((latter-result (funcall ,latter-parser new-token-stream))
		       (latter-new-token-stream (ep:get-token-stream latter-result))
		       (latter-result-token (ep:get-succeeded-token latter-result)))
;		  (message "seq:latter-result: %s" latter-result)
;		  (message "seq:latter-new-token-stream: %s" latter-new-token-stream)
;		  (message "seq:latter-result-token: %s" latter-result-token)
		  (cond ((null latter-result)
			 nil)
			(t
			 (let (
			       (result (list (list (list former-result-token latter-result-token) latter-new-token-stream))))
;			   (message "seq:result: %s" result)
			   result)))))))))



(defmacro ep:alt (former-parser latter-parser); token-stream)
  `#'(lambda (token-stream)
       (let ((former-result (funcall ,former-parser token-stream)))
;	 (message "alt:former-result: %s" former-result)
	 (if former-result
	     (if ep:seek-all-alternatives
		 (let ((latter-result (funcall ,latter-parser token-stream)))
;		   (message "alt:alternative:latter-result: %s" latter-result)
		   (list (car former-result) (car latter-result))) ; Is this right??
	       former-result)
	   (let ((latter-result (funcall ,latter-parser token-stream)))
;	     (message "alt:latter-result: %s" latter-result)
	     latter-result)))))

(defmacro ep:literal (str)
  `#'(lambda (token-list) (funcall (ep:satisfy #'(lambda (x) (string= ,str (cdr x))))
				   token-list)))
;   `#'(lambda (token-list) (funcall (ep:satisfy #'(lambda (x) (string= ,str (ep:contents x))))
; 				   token-list)))

(defmacro ep:token-type (type)
  `#'(lambda (token-list) (funcall (ep:satisfy #'(lambda (x) (eq ,type (car x))))
 				   token-list)))
;   `#'(lambda (token-list) (funcall (ep:satisfy #'(lambda (x) (eq ,type (ep:type-tag x))))
; 				   token-list)))

(defmacro ep:always-succeed ()
;  `#'(lambda (token-stream) (list (list nil token-stream))))
  `#'(lambda (token-stream) (list (list nil token-stream))))



(defmacro ep:x-seq (former-parser latter-parser); token-stream)
  `#'(lambda (token-stream)
       (let* ((former-result (funcall ,former-parser token-stream))
	      (new-token-stream (ep:get-token-stream former-result))
	      (former-result-token (ep:get-succeeded-token former-result)))
;	 (message "x-seq:former-result: %s" former-result)
;	 (message "x-seq:new-token-stream: %s" new-token-stream)
;	 (message "x-seq:former-result-token: %s" former-result-token)
	 (cond ((null former-result)
		nil)
	       (t
		(let* ((latter-result (funcall ,latter-parser new-token-stream))
		       (new-token-stream (ep:get-token-stream latter-result))
		       (latter-result-token (ep:get-succeeded-token latter-result)))
;		  (message "x-seq:latter-result: %s" latter-result)
;		  (message "x-seq:new-token-stream: %s" new-token-stream)
;		  (message "x-seq:latter-result-token: %s" latter-result-token)
		  (cond ((null latter-result)
			 nil)
			(t
			 (let (
			       (result (list (list (append nil latter-result-token) new-token-stream))))
;			   (message "x-seq:result: %s" result)
			   result)))))))))

(defmacro ep:seq-x (former-parser latter-parser); token-stream)
  `#'(lambda (token-stream)
       (let* ((former-result (funcall ,former-parser token-stream))
	      (new-token-stream (ep:get-token-stream former-result))
	      (former-result-token (ep:get-succeeded-token former-result)))
;	 (message "seq-x:former-result: %s" former-result)
;	 (message "seq-x:new-token-stream: %s" new-token-stream)
;	 (message "seq-x:former-result-token: %s" former-result-token)
	 (cond ((null former-result)
		nil)
	       (t
		(let* ((latter-result (funcall ,latter-parser new-token-stream))
		       (new-token-stream (ep:get-token-stream latter-result))
		       (latter-result-token (ep:get-succeeded-token latter-result)))
;		  (message "seq-x:latter-result: %s" latter-result)
;		  (message "seq-x:new-token-stream: %s" new-token-stream)
;		  (message "seq-x:latter-result-token: %s" latter-result-token)
		  (cond ((null latter-result)
			 nil)
			(t
			 (let (
			       (result (list (list (append former-result-token nil) new-token-stream))))
;			   (message "seq-x:result: %s" result)
			   result)))))))))


;; 補助関数 supplementary functions


; (defmacro ep:using (parser constructor);  token-stream)
;   `#'(lambda (token-stream)
;        (let* ((parse-result (funcall ,parser token-stream))
; 	      (succeeded-token (ep:get-succeeded-token parse-result))
; 	      (new-token-stream (ep:get-token-stream parse-result))
; 	      )
; 	 (funcall 
; 	  #'(lambda (const)
; 	      (list (ep:construct-parser-message (funcall const succeeded-token)
; 						 new-token-stream)))
; 	  ,constructor))))
	 

;; 構文解析に失敗すれば直ちに nil を返すべき??

(defmacro ep:using (parser constructor); token-stream)
  `#'(lambda (token-stream)
       (let ((parse-result (funcall ,parser token-stream)))
;	 (message "ep:using processing")
;	 (message "ep:using parse-result: %s" parse-result)
	 (if (null parse-result)
	     nil
	   (let* ((succeeded-token (ep:get-succeeded-token parse-result))
		  (new-token-stream (ep:get-token-stream parse-result))
		  (constructed (funcall ,constructor succeeded-token)))
;	     (message "ep:using succeeded-token: %s" succeeded-token)
;	     (message "ep:using new-token-stream: %s" new-token-stream)
;	     (message "ep:using constructed: %s" constructed)
	     (let* ((constructed2 (if (atom (car constructed))
				      (list constructed)
				    constructed))
		    (parser-message (list (ep:construct-parser-message constructed2 new-token-stream))))
;	       (message "ep:using constructed2: %s" constructed2)
;	       (message "ep:using parser-message: %s" parser-message)
	       parser-message))))))
	 

;; parse functions


;; BNF parser


  
;; assertion
; (load-file "/home/emile/develop/elisp/eparser.el")
(defvar ep:testing nil)
(eval-when-compile
  (setq ep:testing t)
  
  (defmacro ep:plus ()
    (function (ep:literal "+")))
  (defmacro ep:plusminus ()
    (function (ep:seq (ep:literal "+") (ep:literal "-"))))
  (defmacro ep:sign ()
    (function (ep:alt (ep:literal "+") (ep:literal "-"))))
  (defmacro ep:plusminus- ()
    (function
     (ep:using (ep:seq (ep:literal "+") (ep:literal "-"))
	       #'(lambda (L) (let ((arg1 (nth 0 L))
				   (arg2 (nth 1 L)))
;			       (message "ep:plusminus- L: %s" L)
;			       (message "ep:plusminus- arg1: %s" arg1)
;			       (message "ep:plusminus- arg2: %s" arg2)
			       (list arg1 arg2))))))
  (defmacro ep:expr ()
    (function
     (ep:using
      (ep:seq (ep:using (SI:integer)
			#'(lambda (L) (let ((num (string-to-number (cdr (nth 0 L)))))
					num)))
	      (ep:expr^))
      #'(lambda (L) (let ((arg1 (nth 0 L))
			  (arg2 (nth 1 L)))
;		      (message "expr:arg1: %s" arg1) ;expr:arg1: 1
;		      (message "expr:arg2: %s" arg2) ;expr:arg2: ((+ 2))
		      (cons (car arg2) (cons arg1 (cdr arg2))))))))
;		      (list (car (car arg2)) arg1 (cdr (car arg2)) (cdr arg2)))))))
;		      (list (car arg2) arg1 (cdr arg2)))))))

  (defmacro ep:expr^ ()
    (function
     (ep:alt
      (ep:using
       (ep:seq
	(ep:using (ep:seq (ep:literal "+") (SI:integer))
		  #'(lambda (L) (let ((arg1 (cdr (nth 0 L)))
				      (arg2 (cdr (nth 1 L))))
;				  (message "expr^(1):arg1: %s" arg1)
;				  (message "expr^(1):arg2: %s" arg2)
				  (list '+ (string-to-number arg2)))))
	(ep:expr^))
       #'(lambda (L) (let ((arg1 (nth 0 L))
			   (arg2 (nth 1 L)))
;		       (message "expr^(2):arg1: %s" arg1)
;		       (message "expr^(2):arg2: %s" arg2)
		       (if (null arg2)
			   arg1
			 (cons arg1 arg2)))))
      (ep:always-succeed))))
  )



(provide 'eparser)

;;; eparser.el ends here


;;; Local Variables:
;;; End:


