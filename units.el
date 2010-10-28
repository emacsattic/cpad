;;; units.el -- a units library in elisp

;;; Licence: 
;;
;;  Copyright (C) 2002 Akimichi Tatsukawa <akimichi@mbox.co.jp>
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
;;  Keywords: units,SI
;;  $Id: units.el,v 1.8 2002/09/27 00:40:51 emile Exp emile $
;;  $Date: 2002/09/27 00:40:51 $
;;  $Log: units.el,v $
;;  Revision 1.8  2002/09/27 00:40:51  emile
;;  *** empty log message ***
;;
;;  Revision 1.7  2002/08/21 12:36:24  emile
;;  *** empty log message ***
;;
;;  Revision 1.6  2002/08/09 17:19:20  emile
;;  Unit table is added.
;;
;;  Revision 1.5  2002/07/21 10:15:01  emile
;;  Before the divide method is modified.
;;
;;  Revision 1.4  2002/07/16 11:23:50  emile
;;  Functions for debug message are all commented out.
;;
;;  Revision 1.3  2002/07/02 08:10:34  emile
;;  Unit standardization provided.
;;
;;  Revision 1.2  2002/05/24 20:18:45  emile
;;  Before changing multiply to *.
;;
;;  Revision 1.13  2002/05/08 09:49:04  emile
;;  *** empty log message ***
;;
;;  Revision 1.12  2002/05/02 10:00:18  emile
;;  Previous tasks are almost completed.
;;
;;  Revision 1.11  2002/05/02 08:44:30  emile
;;  The slot prefix of the class basic-units should be removed.
;;  The class quantity is going to inherit a units and has a prefix.
;;
;;  Revision 1.10  2002/05/02 07:04:13  emile
;;  Several method names are going to be uniformed.
;;
;;  Revision 1.9  2002/05/01 08:51:56  emile
;;  *** empty log message ***
;;
;;  Revision 1.8  2002/05/01 03:53:02  emile
;;  *** empty log message ***
;;
;;  Revision 1.7  2002/05/01 01:48:57  emile
;;  *** empty log message ***
;;
;;  Revision 1.6  2002/04/30 07:48:37  emile
;;  Rename class quantity to dimention.
;;
;;  Revision 1.5  2002/04/30 06:36:49  emile
;;  *** empty log message ***
;;
;;  Revision 1.4  2002/04/18 03:12:17  emile
;;  Class decimal multiples renamed to prefix.
;;  Class unit is inherited from connector in order to mix with constraint library.
;;
;;  Revision 1.3  2002/04/18 02:25:09  emile
;;  *** empty log message ***
;;
;;  Revision 1.1  2002/04/17 06:01:47  emile
;;  Initial revision
;;
;;  Revision 1.2  2002/04/17 00:45:19  emile
;;  *** empty log message ***
;;
;;  Revision 1.1  2002/04/12 02:33:32  emile
;;  Initial revision
;;

;;; Commentary:
;;;   This library handles the international system of units,so-called SI unit system.
;;;   Note that it has nothing to do with a unit testing proposed by eXtreme Programming.

;;; Usage:
;;    (require 'units) ; units.el requires cl,eieio,constraint and eparser packages.
;;    ;; just parse a unit string
;;    (SI:parse-string "(m * s)/(d L)")
;;    ;; parse and build unit object from the given string
;;    (SI:build-unit-string "(m * g)/L")
;;    ;; manipulate the unit object
;;    (get-dimention (SI:build-unit-string "mg / dL"))
;;    (same-dimention (get-dimention (SI:build-unit-string "mg / dL"))
;;                    (get-dimention (SI:build-unit-string "kg / m3")))
;;    (SI:convert SI:cubic-centimeter SI:liter)
;;    (SI:convert (SI:build-unit-string "Torr / dL") (SI:build-unit-string "(k Pa) / m3"))
;;    (add (SI:build-unit-string "mg/L") (SI:build-unit-string "kg/m3"))

;;; TODO:
;;    12{mg} / 11.0{(dL / m3)};
;;    numeric class
;;    complex-units processing
;;      reciprocal calc
;;    simplification of the unit symbol
;;; Code:

(require 'cl)
(require 'eieio)
;(require 'constraint)
(require 'eparser)
(require 'calc)
(require 'calc-ext)
(defvar calc-command-flags)


;;; numeric class
(defclass numeric ()
  ((value   :initarg :value
	    :accessor get-value
	    :type string
	    :documentation "the numeric value as a string"))
  "The numeric class")

(defun SI:make-numeric (val)
  (cond ((stringp val)
	 (numeric "numeric" :value val))
	((numberp val)
	 (numeric "numeric" :value (number-to-string val)))
	(t
	 (error "SI:make-numeric: Argument must be a string or number."))))

(defmethod add ((x numeric) y)
  (cond ((numberp y)
	 (SI:make-numeric (calc-eval (format "(%s) + (%s)" (get-value x) y))))
	((same-class-p y 'numeric)
	 (SI:make-numeric (calc-eval (format "(%s) + (%s)" (get-value x) (get-value y)))))
	(t
	 (error "numeric.add: The 2nd argument must be a number or numeric."))))

(defmethod subtract ((x numeric) y)
  (cond ((numberp y)
	 (SI:make-numeric (calc-eval (format "(%s) - (%s)" (get-value x) y))))
	((same-class-p y 'numeric)
	 (SI:make-numeric (calc-eval (format "(%s) - (%s)" (get-value x) (get-value y)))))
	(t
	 (error "numeric.subtract: The 2nd argument must be a number or numeric."))))

(defmethod multiply ((x numeric) y)
  (cond ((numberp y)
	 (SI:make-numeric (calc-eval (format "(%s) * (%s)" (get-value x) y))))
	((same-class-p y 'numeric)
	 (SI:make-numeric (calc-eval (format "(%s) * (%s)" (get-value x) (get-value y)))))
	(t
	 (error "numeric.multiply: The 2nd argument must be a number or numeric."))))

(defmethod divide ((x numeric) y)
  (cond ((numberp y)
	 (assert (not (zerop y)))
	 (SI:make-numeric (calc-eval (format "(%s) / (%s)" (get-value x) y))))
	((same-class-p y 'numeric)
	 (assert (not (SI:zerop y)))
	 (SI:make-numeric (calc-eval (format "(%s) / (%s)" (get-value x) (get-value y)))))
	(t
	 (error "numeric.divide: The 2nd argument must be a number or numeric."))))

(defmethod SI:zerop ((x numeric))
  (calc-eval "0 == $" 'pred (get-value x)))



;;; unit classes


(defvar SI:whole-units-alist '() "Association list of whole units ,each item has (sym . obj) format.")
(defvar SI:whole-prefixes-alist '() "Association list of whole prefixes ,each item has (sym . obj) format.")
(defvar SI:prefix-str-list '() "List of whole prefix ,each item is a string.")
(defvar SI:units-str-list '() "List of whole units ,each item is a string.")

(defmacro SI:add-to-unit-alist (sym constructor)
  `(let ((obj ,constructor))
     (assert (obj-of-class-p obj 'units))
     (setq ,sym obj)
     (setq SI:whole-units-alist (acons (get-symbol obj) obj SI:whole-units-alist))
     (setq SI:units-str-list (cons (symbol-name (get-symbol obj)) SI:units-str-list))))

(defmacro SI:add-to-prefix-alist (sym constructor)
  `(let ((obj ,constructor))
     (assert (same-class-p obj 'prefix))
     (setq ,sym obj)
     (setq SI:whole-prefixes-alist (acons (get-symbol obj) obj SI:whole-prefixes-alist))
     (setq SI:prefix-str-list (cons (symbol-name (get-symbol obj)) SI:prefix-str-list))))

(defun SI:find-unit (sym)
  "Find unit of the symbol SYM among SI:whole-units-alist. Return its cons cell item if found, nil if not found."
  (let ((result (assoc sym SI:whole-units-alist)))
    (if result
	result
      nil)))

(defun SI:find-prefix (sym)
  "Find a prefix of the symbol SYM among SI:whole-prefixes-alist. Return its cons cell item if found, nil if not found."
  (let ((result (assoc sym SI:whole-prefixes-alist)))
    (if result
	result
      nil)))




;;; dimention classes
(defclass dimention ()
  ((dimention-alist :initarg :dimention
		    :accessor get-dimention-alist
		    :documentation "dimention slot as an association list")
   (dsymbol :initarg :dsymbol
	    :accessor get-symbol
	    :initform nil
	    :documentation "symbol slot")
   (whole-dimention :initform (length mass time electric-current temperature amount luminous-intensity)
		    :type list
		    :allocation class
		    :accessor read-whole-dimention))
  "dimention class")
  
(defun SI:make-dimention (s &rest d)
  (let* ((d-instance (dimention "dimention" :dsymbol s))
	 (dimention nil))
    (dolist (i d)
      (setq dimention (acons (car i) (cdr i) dimention))
      )
    (oset d-instance dimention-alist dimention)
    d-instance))

(defmethod get-dimention-value ((q dimention) dsymbol)
  (let ((dim-pair (assoc dsymbol (get-dimention-alist q))))
    (if dim-pair
	(cdr dim-pair)
      0)))
    
(defmethod multiply ((x dimention) (y dimention))
  (let ((new-dimention nil)
	(all-dimentions (read-whole-dimention x)))
    (dolist (i all-dimentions)
      (let ((x-dimention-value (get-dimention-value x i))
	    (y-dimention-value (get-dimention-value y i)))
	(if (not (and (= x-dimention-value 0) (= y-dimention-value 0)))
	    (setq new-dimention (acons i (+ x-dimention-value y-dimention-value) new-dimention)))))
    (dimention "dimention" :dsymbol `(* ,(get-symbol x) ,(get-symbol y)) :dimention new-dimention)))

(defmethod divide ((x dimention) (y dimention))
  (assert (same-class-p y 'dimention))
  (let ((new-dimention nil)
	(all-dimentions (read-whole-dimention x)))
    (dolist (i all-dimentions)
      (let ((x-dimention-value (get-dimention-value x i))
	    (y-dimention-value (get-dimention-value y i)))
	(if (not (and (= x-dimention-value 0) (= y-dimention-value 0)))
	    (setq new-dimention (acons i (- x-dimention-value y-dimention-value) new-dimention)))))
    (dimention "dimention" :dsymbol `(/ ,(get-symbol x) ,(get-symbol y)) :dimention new-dimention)))

(defmethod reciprocal ((x dimention))
  (let ((new-dimention nil)
	(all-dimentions (read-whole-dimention x)))
    (dolist (i all-dimentions)
      (setq new-dimention (acons i (- (get-dimention-value x i)) new-dimention)))
    (dimention "dimention" :dsymbol `(/ nil ,(get-symbol x)) :dimention new-dimention)))


(defmethod same-dimention ((x dimention) (y dimention))
  (let ((all-dimentions (read-whole-dimention x))
	(rval t))
    (assert (same-class-p y 'dimention))
    (dolist (i all-dimentions)
      (cond  ((not (= (get-dimention-value x i) (get-dimention-value y i)))
	      (setq rval nil))))
    rval))


(setq SI:dimentionless (SI:make-dimention nil nil))
(setq SI:length (SI:make-dimention 'length '(length . 1)))
(setq SI:mass (SI:make-dimention 'mass '(mass . 1)))
(setq SI:time (SI:make-dimention 'time '(time . 1)))
(setq SI:electric-current (SI:make-dimention 'electric-current '(electric-current . 1)))
(setq SI:temperature (SI:make-dimention 'temperature '(temperature . 1)))
(setq SI:amount (SI:make-dimention 'amount '(amount . 1)))
(setq SI:luminous-intensity (SI:make-dimention 'luminous-intensity '(luminous-intensity . 1)))
(setq SI:area (SI:make-dimention 'area '(length . 2)))
(setq SI:volume (SI:make-dimention 'volume '(length . 3)))
(setq SI:velocity (SI:make-dimention 'velocity '(length . 1) '(time . -1)))
(setq SI:acceleration (SI:make-dimention 'acceleration '(length . 1) '(time . -2)))
(setq SI:wave-number (SI:make-dimention 'wave-number '(length . -1)))
(setq SI:mass-density (SI:make-dimention 'mass-density '(length . 3) '(mass . -1)))
(setq SI:frequency (SI:make-dimention 'frequency '(time . -1)))
(setq SI:force (SI:make-dimention 'force '(length . 1) '(mass . 1) '(time . -2) ))
(setq SI:pressure (SI:make-dimention 'pressure '(length . -1) '(mass . 1) '(time . -2) ))
(setq SI:energy (SI:make-dimention 'energy '(length . 2) '(mass . 1) '(time . -2) ))
(setq SI:power (SI:make-dimention 'power '(length . 2) '(mass . 1) '(time . -3)))
(setq SI:moment (SI:make-dimention 'moment '(length . 2) '(mass . 1) '(time . -2)))
(setq SI:electric-charge (SI:make-dimention 'electric-charge '(time . 1) '(electric-current . 1)))
(setq SI:electric-potential (SI:make-dimention 'electric-potential '(length . 2) '(mass . 1) '(time . -3) '(electric-current . -1)))
(setq SI:capacitance (SI:make-dimention 'capacitance '(length . -2) '(mass . -1) '(time . 4) '(electric-current . 2)))
(setq SI:magnetic-flux (SI:make-dimention 'magnetic-flux '(length . 2) '(mass . 1) '(time . -2) '(electric-current . -1)))
(setq SI:magnetic-flux-density (SI:make-dimention 'magnetic-flux-density '(mass . 1) '(time . -2) '(electric-current . -1)))
(setq SI:inductance (SI:make-dimention 'inductance '(length . 2) '(mass . 1) '(time . -2) '(electric-current . -2)))

;;; prefixes
(defclass prefix ()
  ((pexpo   :initarg :pexpo
	    :initform 0
	    :accessor get-pexpo
	    :type integer
	    :documentation "exponential slot")
   (psymbol :initarg :psymbol
	    :initform nil
	    :accessor get-symbol
	    :documentation "prefix symbol slot"))
;;    (pconnector :initarg :pconnector	; Is it necessary ??
;; 	       :accessor get-connector
;; 	       :documentation "prefix connector slot"))
  ()
  "prefixes of SI units")

(defun SI:make-prefix (expo &optional sym)
  (prefix "SI prefix" :pexpo expo :psymbol sym ));:pconnector (make-connector)))

(defmethod multiply ((x prefix) (y prefix))
  (SI:make-prefix (+ (get-pexpo x) (get-pexpo y)) `(* ,(get-symbol x) ,(get-symbol y))))

(defmethod divide ((x prefix) (y prefix))
  (SI:make-prefix (- (get-pexpo x) (get-pexpo y)) `(/ ,(get-symbol x) ,(get-symbol y))))

(defmethod reciprocal ((x prefix))
  (SI:make-prefix (- (get-pexpo x)) `(/ void ,(get-symbol x))))

(SI:add-to-prefix-alist SI:void-prefix (SI:make-prefix 0 'void))
(SI:add-to-prefix-alist SI:yotta (SI:make-prefix 24 'Y))
(SI:add-to-prefix-alist SI:zetta (SI:make-prefix 21 'Z))
(SI:add-to-prefix-alist SI:exa (SI:make-prefix 18 'E))
(SI:add-to-prefix-alist SI:peta (SI:make-prefix 15 'P))
(SI:add-to-prefix-alist SI:tera (SI:make-prefix 12 'T))
(SI:add-to-prefix-alist SI:giga (SI:make-prefix 9 'G))
(SI:add-to-prefix-alist SI:mega (SI:make-prefix 6 'M))
(SI:add-to-prefix-alist SI:kilo (SI:make-prefix 3 'k))
(SI:add-to-prefix-alist SI:hecto (SI:make-prefix 2 'h))
(SI:add-to-prefix-alist SI:deka (SI:make-prefix 1 'da))
(SI:add-to-prefix-alist SI:deci (SI:make-prefix -1 'd))
(SI:add-to-prefix-alist SI:centi (SI:make-prefix -2 'c))
(SI:add-to-prefix-alist SI:milli (SI:make-prefix -3 'm))
(SI:add-to-prefix-alist SI:micro (SI:make-prefix -6 'u))
(SI:add-to-prefix-alist SI:nano (SI:make-prefix -9 'n))
(SI:add-to-prefix-alist SI:pico (SI:make-prefix -12 'p))
(SI:add-to-prefix-alist SI:femto (SI:make-prefix -15 'f))
(SI:add-to-prefix-alist SI:atto (SI:make-prefix -18 'a))
(SI:add-to-prefix-alist SI:zepto (SI:make-prefix -21 'z))
(SI:add-to-prefix-alist SI:yocto (SI:make-prefix -24 'y))


;;; units classes
(defclass units ()
  ((udimention :initarg :udimention
	       :type dimention
	       :accessor get-dimention
	       :documentation "dimention slot")
   (usymbol :initarg :usymbol
	    :accessor get-symbol
	    :documentation "unit symbol slot"))
;;    (uconnector :initarg :uconnector
;; 	       :accessor get-connector
;; 	       :documentation "unit connector slot"))
  "most basic units superclass")


(defmethod standardize ((u units))
  (standardize (SI:make-quantity (get-symbol u) u SI:void-prefix 1.0)))
  
(defclass basic-units (units)
  ()
  "basic units class")

(defun SI:make-basic-units (s d)
  (assert (same-class-p d 'dimention))
  (basic-units "basic units" :usymbol s :udimention d)); :uconnector (make-connector)))

(defmethod multiply ((x units) y)
  (assert (obj-of-class-p y 'units))
  (cond ((numberp y)
;	 (message "units.multiply: y -> %s" y)
	 (SI:make-quantity (get-symbol x) (get-dimention x) SI:void-prefix y))
	((obj-of-class-p y 'units)
	 (SI:make-basic-units `(* ,(get-symbol x) ,(get-symbol y)) (multiply (get-dimention x) (get-dimention y))))
	(t
	 (error "units.multiply: second argument must be a number or a kind of units"))))	
  
(defmethod divide ((x units) y)
  (assert (obj-of-class-p y 'units))
  (let ((x-prefix (if (obj-of-class-p x 'quantity)
		      (get-prefix x)
		    SI:void-prefix))
	(y-prefix (if (obj-of-class-p y 'quantity)
		      (get-prefix y)
		    SI:void-prefix)))
    (quantity "quantity"
	      :qsymbol `(/ ,(get-symbol x) ,(get-symbol y))
	      :udimention (divide (get-dimention x) (get-dimention y))
	      :qprefix (divide x-prefix y-prefix)
	      :qnumber (if (obj-of-class-p y 'quantity)
			   (get-number y)
			 (SI:make-numeric 1.0)))))
;	      :qconnector (make-connector))))

  
  

(defmethod exponential ((x units) y)
  (assert (integerp y))
  (cond ((> y 1)
	 (let ((result (multiply x x)))
	   (dotimes (i (- y 2) result)
	     (setq result (multiply result x)))))
	((= y 1)
	 x)
	((= y 0)
	 (error "0 Exponential of quantity is meaningless."))
	((= y -1)
	 (reciprocal x))
	((< y -1)
	 (let ((result (multiply x x)))
	   (dotimes (i (- y 1) result)
	     (setq result (multiply result x)))
	   (reciprocal result)))))
	


(defmethod reciprocal ((x units))
  (SI:make-basic-units `(/ nil ,(get-symbol x))
		       (reciprocal (get-dimention x))))

(defmethod same-dimention ((x units) y)
  (assert (obj-of-class-p y 'units))
  (same-dimention (get-dimention x) (get-dimention x)))

(defmethod get-dimention-alist ((this units))
  (let ((q (get-dimention this)))
    (get-dimention-alist q)))


(defmethod add-prefix ((this units) (p prefix))
  (assert (same-class-p p 'prefix))
  (SI:make-quantity (get-symbol this) this p))

; (defmethod add-prefix ((this quantity) (p prefix))
;   (let ((q-prefix (get-prefix this)))
;     (assert (same-class-p p 'prefix))
;     (assert (same-class-p q-prefix 'prefix))
;     (SI:make-quantity (get-symbol this) this (multiply q-prefix p))))


(defclass derived-units (units)
  ()
  "The derived units class.")

(defun SI:make-derived-units (s d)
  (SI:make-basic-units s d))


;; supplementary units (angle:radian,steradian)
(defclass supplementary-units (units)
  ()
  "Supplementary units class. Note that this type of units is dimentionless.")

(defun SI:make-supplementary-units (s)
  (supplementary-units "supplementary units" :usymbol s :udimention SI:dimentionless))



;;; quantity(= prefix + unit)

(defclass quantity (units)
  ((qprefix   :initarg :qprefix
	      :accessor get-prefix
	      :type prefix
	      :documentation "quantity prefix slot")
   (qnumber   :initarg :qnumber
;	      :initform 1.0
	      :accessor get-number
	      :type numeric
	      :documentation "The numeric value of the quantity")
   (qsymbol :initarg :qsymbol
	    :initform nil
	    :accessor get-symbol
	    :documentation "A symbol of quantity. Note that this is optional and provided if it is a proper symbol."))
;;    (qconnector :initarg :qconnector
;; 	       :accessor get-connector
;; 	       :documentation "quantity connector slot"))
  "quantity with connector")

(defun SI:make-quantity (s u &optional p n)
  "'S is a symbol,'U is an unit, 'P is a prefix."
  (assert (obj-of-class-p u 'units))
  (if (null p)
      (setq p SI:void-prefix)
    (assert (same-class-p p 'prefix)))
  (let ((dim (get-dimention u))
	(num (cond ((null n)
		    (SI:make-numeric "1.0"))
		   ((numberp n)
		    (SI:make-numeric n))
		   ((stringp n)
		    (SI:make-numeric n))
		   ((same-class-p n 'numeric)
		    n)
		   (t
;		    (message "SI:make-quantity: N -> %s" n)
		    (error "SI:make-quantity: The argument 'N must be a nil, number or string")))))
    (quantity "quantity" :qsymbol s :udimention dim :qprefix p :qnumber num))); :qconnector (make-connector))))


(defmethod add ((x quantity) y)
  "Add two quantities"
  (assert (obj-of-class-p y 'units))
  (assert (same-dimention x y))
  (let ((sym (get-symbol x))
	(dim (get-dimention x))
	(pre (get-prefix x))
	(num (get-number x))
;	(con (get-connector x))
	(converted-y (SI:convert y (SI:make-quantity (get-symbol x) x SI:void-prefix 1.0))))
    (quantity "quantity" :qsymbol sym :udimention dim :qprefix pre
	      :qnumber (add num
			    (multiply
			     (get-number converted-y)
			     (if (< 0 (get-pexpo (get-prefix converted-y)))
				 (SI:make-numeric (calc-eval (format "exp10(%s)" (get-pexpo (get-prefix converted-y)))))
			       (divide (SI:make-numeric "1.0")
				       (SI:make-numeric (calc-eval (format "exp10(%s)" (abs (get-pexpo (get-prefix converted-y)))))))))))))

;	      :qconnector con)))

(defmethod subtract ((x quantity) y)
  "Subtract two quantities"
  (assert (obj-of-class-p y 'units))
  (assert (same-dimention x y))
  (let ((sym (get-symbol x))
	(dim (get-dimention x))
	(pre (get-prefix x))
	(num (get-number x))
;	(con (get-connector x))
	(converted-y (SI:convert y x)))
    (quantity "quantity" :qsymbol sym :udimention dim :qprefix pre
	      :qnumber (subtract num
				 (multiply
				  (SI:make-numeric (calc-eval (format "exp10(%s)" (get-pexpo (get-prefix converted-y)))))
				  (get-number converted-y))) )));:qconnector con)))
    
  
(defmethod multiply ((x quantity) y)
  (cond ((numberp y)
	 (quantity "quantity"
		   :qsymbol (get-symbol x)
		   :udimention (get-dimention x)
		   :qprefix (get-prefix x)
		   :qnumber (multiply (get-number x) y)
;		   :qconnector (make-connector)))
		   ))
	((obj-of-class-p y 'units)
	 (let ((yy
		(cond ((same-class-p y 'quantity)
		       y)
		      ((obj-of-class-p y 'units)
		       (SI:make-quantity (get-symbol y) y SI:void-prefix))
		      (t
		       (error "quantity.multiply: %s should be an instance of units or quantity" y)))))
	   (let ((x-dimention (get-dimention x))
		 (y-dimention (get-dimention yy))
		 (raw-symbol `(* ,(get-symbol x) ,(get-symbol yy))))
	     (quantity "quantity"
		       :qsymbol raw-symbol;(SI:standardize-symbol raw-symbol)
		       :udimention (multiply x-dimention y-dimention)
		       :qprefix (multiply (get-prefix x) (get-prefix yy))
		       :qnumber (multiply (get-number x) (get-number yy))
;		       :qconnector (make-connector)))))
		       ))))
	(t
	 (error "Error in multiply: 2nd argument is not either number or a kind of units"))))
	
	
(defmethod divide ((x quantity) y)
  (cond ((numberp y)
	 (quantity "quantity"
		   :qsymbol (get-symbol x)
		   :udimention (get-dimention x)
		   :qprefix (get-prefix x)
		   :qnumber (divide (get-number x) y)))
	((obj-of-class-p y 'numeric)
	 (quantity "quantity"
		   :qsymbol (get-symbol x)
		   :udimention (get-dimention x)
		   :qprefix (get-prefix x)
		   :qnumber (divide (get-number x) (get-value y))))
	((obj-of-class-p y 'units)
	 (let ((yy
		(cond ((same-class-p y 'quantity)
		       y)
		      ((obj-of-class-p y 'units)
		       (SI:make-quantity (get-symbol y) y SI:void-prefix))
		      (t
		       (error "quantity.divide: %s should be an instance of units or quantity" y)))))
	   (let ((x-dimention (get-dimention x))
		 (y-dimention (get-dimention yy))
		 (raw-symbol `(/ ,(get-symbol x) ,(get-symbol yy))))
	     (quantity "quantity"
		       :qsymbol raw-symbol;(SI:standardize-symbol raw-symbol)
		       :udimention (divide x-dimention y-dimention)
		       :qprefix (divide (get-prefix x) (get-prefix yy))
		       :qnumber (divide (get-number x) (get-number yy))))))
	(t
	 (error "Error in divide: 2nd argument is not either number or a kind of units"))))


(defmethod reciprocal ((x quantity))
  (quantity "quantity"
	    :qsymbol `(/ nil ,(get-symbol x))
	    :udimention (reciprocal (get-dimention x))
	    :qprefix (reciprocal (get-prefix x))
	    :qnumber (divide (SI:make-numeric "1.0") (get-number x))))
;	    :qconnector (make-connector)))
  
(defmethod add-prefix ((this quantity) (p prefix))
  (let ((q-prefix (get-prefix this)))
    (assert (same-class-p p 'prefix))
    (assert (same-class-p q-prefix 'prefix))
    (SI:make-quantity (get-symbol this) this (multiply q-prefix p))))

(defmethod convert2string ((x quantity))
  (let ((num (get-number x))
	(sym (get-symbol x))
	(pre (get-prefix x)))
;    (message "convert2string: %s" x)
    (format "%s{%s}" (get-value num) (SI:convert-symbol sym))))
  
(defmethod construct-unit-symbol ((q quantity))
  (let ((result nil)
	(dim-alist (get-dimention-alist (get-dimention q)))
	(procedure (lambda (basic-unit i)
;		     (message "dimention::construct-unit-symbol result -> %s" result)
;		     (message "dimention::construct-unit-symbol i -> %s" i)
;		     (message "dimention::construct-unit-symbol basic-unit -> %s" basic-unit)
		     (cond ((null (car i)) ; in case of dimensionless
			    (setq result basic-unit))
			   ((eq 1 (cdr i))
			    (if (null result)
				(setq result basic-unit)
			      (if (null (car (cdr result)))
				  (setq result `(/ ,basic-unit ,(car (cdr (cdr result)))))
				(setq result `(* ,basic-unit ,result)))))
			   ((< 1 (cdr i))
			    (dotimes (ii (cdr i) result)
			      (if (eq ii 0)
				  (if (null result)
				      (setq result basic-unit)
				    (setq result `(* ,basic-unit ,result)))
				(if (null result)
				    (setq result basic-unit)
				  (setq result `(* ,basic-unit ,result))))))
			   ((> 0 (cdr i))
			    (dotimes (ii (abs (cdr i)) result)
			      (if (eq ii 0)
				  (if (null result)
				      (setq result `(/ nil ,basic-unit))
				    (if (and (consp result) (null (car (cdr result))))
					(setq result `(/ nil (* ,(car (cdr (cdr result))) ,basic-unit)))
				      (setq result `(/ ,result ,basic-unit))))
				(if (null result)
				    (setq result basic-unit)
				  (if (null (car (cdr result)))
				      (setq result `(/ nil (* ,(car (cdr (cdr result))) ,basic-unit)))
				    (setq result `(/ ,result ,basic-unit))))))))))
	(result nil))
;    (message "dimention::construct-unit-symbol dim-alist -> %s" dim-alist)
    (dolist (i dim-alist result)
      (case (car i)
	(length
	 (funcall procedure 'm i))
	(mass
	 (funcall procedure 'kg i))
	(time
	 (funcall procedure 's i))
	(electric-current
	 (funcall procedure 'A i))
	(temperature
	 (funcall procedure 'K i))
	(amount
	 (funcall procedure 'mol i))
	(luminous-intensity
	 (funcall procedure 'cd i))
	(t
	 (funcall procedure (car i) i))
	))))
  
(defun SI:convert-symbol (sym)
  (cond ((symbolp sym)
	 (format "%s" (symbol-name sym))) 
	((consp sym)			;(/ (/ (km) (hr)) (hr))
	 (let ((stem (SI:convert-symbol (car sym))))
	   (cond ((string= stem "/")
		  (let ((left-leaf (SI:convert-symbol (nth 1 sym)))
			(right-leaf (SI:convert-symbol (nth 2 sym))))
		    (format "(%s %s %s)" left-leaf stem right-leaf)))
		 ((string= stem "*")
		  (let ((first-arg (SI:convert-symbol (nth 1 sym)))
			(rest-args (mapcar #'SI:convert-symbol (cddr sym))))
;		    (message "SI:convert-symbol: first-arg -> %s" first-arg)
;		    (message "SI:convert-symbol: rest-args -> %s" rest-args)
		    (format "(%s %s %s)" first-arg stem
			    (if (eq (length rest-args) 1)
				(car rest-args)
			      (SI:convert-symbol (cons '* rest-args))))))
		 (t
		  (format "%s" sym)))))
	(t
	 (format "%s" sym))))
;	 (error "SI:convert-symbol: unexpected input %s" sym))))


;;; units instances

(SI:add-to-unit-alist
 SI:void-unit (SI:make-basic-units 'void SI:dimentionless))
(SI:add-to-unit-alist
 SI:meter (SI:make-basic-units 'm SI:length))
(SI:add-to-unit-alist
 SI:kilogram (SI:make-basic-units 'kg SI:mass))
(SI:add-to-unit-alist
 SI:second (SI:make-basic-units 's SI:time))
(SI:add-to-unit-alist
 SI:ampere (SI:make-basic-units 'A SI:electric-current))
(SI:add-to-unit-alist
 SI:kelvin (SI:make-basic-units 'K SI:temperature))
(SI:add-to-unit-alist
 SI:mole (SI:make-basic-units 'mol SI:amount))
(SI:add-to-unit-alist
 SI:candela (SI:make-basic-units 'cd SI:luminous-intensity))
(SI:add-to-unit-alist
 SI:hertz (SI:make-basic-units 'Hz SI:frequency))
(SI:add-to-unit-alist
 SI:gray (SI:make-basic-units 'Gy (divide SI:energy SI:mass)))
(SI:add-to-unit-alist
 SI:becqurel (SI:make-basic-units 'Bq SI:frequency))
(SI:add-to-unit-alist
 SI:square-meter (SI:make-derived-units 'm2 SI:area))
(SI:add-to-unit-alist
 SI:cubic-meter (SI:make-derived-units 'm3 SI:volume))
(SI:add-to-unit-alist
 SI:meter-per-second (SI:make-derived-units 'm/s SI:velocity))
(SI:add-to-unit-alist
 SI:meter-per-second-squared (SI:make-derived-units 'm/s2 SI:acceleration))
(SI:add-to-unit-alist
 SI:newton (SI:make-derived-units 'N SI:force))
(SI:add-to-unit-alist
 SI:pascal (SI:make-derived-units 'Pa SI:pressure))
(SI:add-to-unit-alist
 SI:joule (SI:make-derived-units 'J SI:energy))
(SI:add-to-unit-alist
 SI:watt (SI:make-derived-units 'W SI:power))
(SI:add-to-unit-alist
 SI:newton-meter (SI:make-derived-units 'Nm SI:moment))
(SI:add-to-unit-alist
 SI:electric-charge (SI:make-derived-units 'C SI:electric-charge))
(SI:add-to-unit-alist
 SI:electric-potential (SI:make-derived-units 'V SI:electric-potential))
(SI:add-to-unit-alist
 SI:capacitance (SI:make-derived-units 'F SI:capacitance))
(SI:add-to-unit-alist
 SI:magnetic-flux (SI:make-derived-units 'Wb SI:magnetic-flux))
(SI:add-to-unit-alist
 SI:magnetic-flux-density (SI:make-derived-units 'T SI:magnetic-flux-density))
(SI:add-to-unit-alist
 SI:inductance (SI:make-derived-units 'H SI:inductance))

(SI:add-to-unit-alist
 SI:radian (SI:make-supplementary-units 'rad))
(SI:add-to-unit-alist
 SI:steradian (SI:make-supplementary-units 'sr))

(SI:add-to-unit-alist
 SI:void-quantity (SI:make-quantity 'void SI:void-unit))
(SI:add-to-unit-alist
 SI:centi-meter (SI:make-quantity 'cm SI:meter))
(SI:add-to-unit-alist
 SI:milli-meter (SI:make-quantity 'mm SI:meter))
(SI:add-to-unit-alist
 SI:liter (SI:make-quantity 'L SI:cubic-meter))
(SI:add-to-unit-alist
 SI:deci-liter (SI:make-quantity 'dL SI:cubic-meter)) ;  1[L] = 1[dm3] = 10e-3[m3]
(SI:add-to-unit-alist
 SI:cubic-centimeter (SI:make-quantity 'cc SI:cubic-meter))
(SI:add-to-unit-alist
 SI:torr (SI:make-quantity 'Torr SI:pascal))
(SI:add-to-unit-alist
 SI:standard-atmosphere (SI:make-quantity 'atm SI:pascal))
(SI:add-to-unit-alist
 SI:gram (SI:make-quantity 'g SI:kilogram))
(SI:add-to-unit-alist
 SI:milli-gram (SI:make-quantity 'mg SI:kilogram))

(SI:add-to-unit-alist
 SI:minute (SI:make-quantity 'min SI:second))
(SI:add-to-unit-alist
 SI:hour (SI:make-quantity 'hr SI:second))
(SI:add-to-unit-alist
 SI:day (SI:make-quantity 'day SI:second))




;;; parser for the SI unit system

(setq ep:seek-all-alternatives nil)
(ep:define-token 'open-bracket "{")
(ep:define-token 'close-bracket "}")
(ep:define-token 'operator "[/*]")
(ep:define-token 'unit "[a-zA-Z]\\([a-zA-Z]\\|[+-]*[0-9]+\\|[/*]*\\)*")
(ep:define-token 'integer "[+-]*[0-9]+\\(e-?[0-9]+\\)?")
(ep:define-token 'float "[+-]*[0-9]+\\\.")
(ep:define-token 'float "[+-]*[0-9]+\\\.[0-9]+")
(ep:define-token 'float "[+-]*[0-9]+\\\.[0-9]+e-?[0-9]+")


(defun ep:define-subtoken-hook (matched-type matched-string)
  (if (eq matched-type 'unit)
      (let ((sym (intern matched-string)))
	(SI:classify-unit sym))
    matched-type))



(defmacro ep:operator ()
;  (message "ep:operator processing")
  (function (ep:token-type 'operator)))

(defmacro ep:integer ()
;  (message "ep:integer processing")
  (function (ep:token-type 'integer)))
(defmacro ep:float ()
;  (message "ep:float processing")
  (function (ep:token-type 'float)))
(defmacro ep:numeric ()
;  (message "ep:numeric processing")
  (function (ep:alt
	     (ep:float)
	     (ep:integer))))

(defmacro ep:open-paren ()
;  (message "ep:open-paren processing")
  (function (ep:token-type 'open-paren)))

(defmacro ep:close-open ()
;  (message "ep:close-paren processing")
  (function (ep:token-type 'close-paren)))

(defmacro ep:open-bracket ()
;  (message "ep:open-bracket processing")
  (function (ep:token-type 'open-bracket)))

(defmacro ep:close-bracket ()
;  (message "ep:close-bracket processing")
  (function (ep:token-type 'close-bracket)))


(defmacro ep:derived-unit ()
;  (message "ep:derived-unit processing")
  (function (ep:token-type 'derived-units)))

(defmacro ep:basic-unit ()
;  (message "ep:basic-unit processing")
  (function (ep:token-type 'basic-units)))

(defmacro ep:complex-unit ()		;The unit such as 's-3' needs to be analyzed after parsing, 
;  (message "ep:complex-unit processing")
  (function (ep:token-type 'complex-units)))

(defmacro ep:prefix ()
;  (message "ep:prefix processing")
  (function (ep:token-type 'prefix)))

(defmacro ep:unit ()
;  (message "ep:unit processing")
  (function (ep:alt
	     (ep:alt
	      (ep:alt
	       (ep:basic-unit)
	       (ep:derived-unit))
	      (ep:complex-unit))
	     (ep:using (ep:seq (ep:seq (ep:open-paren) (ep:expression))
			       (ep:close-open))
		       #'(lambda (L) (let ((arg1 (nth 0 L))
					   (arg2 (nth 1 L)))
;				       (message "ep:unit:arg1: %s" arg1) ; ((open-paren . "(") ((operator . *) (basic-units . m) (basic-units . kg)))
;				       (message "ep:unit:arg2: %s" arg2) ; (close-paren . ")")
				       (nth 1 arg1)))))))
  
(defmacro ep:term ()
;  (message "ep:term processing")
  (function (ep:alt (ep:seq (ep:prefix) (ep:unit))
		    (ep:unit))))

(defmacro ep:compound-unit ()
;  (message "ep:compound-unit processing")
  (function (ep:alt
	     (ep:alt
	      (ep:using (ep:seq (ep:operator) (ep:compound-unit))
			#'(lambda (L) (let ((arg1 (nth 0 L))
					    (arg2 (nth 1 L)))
;			#'(lambda (L) (let ((arg1 (car L))
;					    (arg2 (cdr L)))
;					(message "ep:compound-unit: L -> %s" L)
;					(message "ep:compound-unit: arg1 -> %s" arg1)
;					(message "ep:compound-unit: arg2 -> %s" arg2)
					(list arg1 arg2))))
	      (ep:term))
	     (ep:always-succeed))))

(defmacro ep:expression ()
;  (message "ep:expression processing")
  (function (ep:using (ep:seq (ep:term) (ep:compound-unit))
		      #'(lambda (L) (let ((arg1 (nth 0 L))
					  (arg2 (nth 1 L)))
;				      (message "ep:expression:arg1: %s" arg1)
;				      (message "ep:expression:arg2: %s" arg2)
				      ;; operator construction
				      (if (null arg2)
					  arg1
					;;((operator . *) (basic-units . kg))
					(if (and (listp (car arg2))
						 (eq (car (car arg2)) 'operator))
					    (list (car arg2) arg1 (car (cdr arg2)))
					  (list arg1 arg2))))))))

(defmacro ep:quantity ()
;  (message "ep:quantity processing")
  (function (ep:using (ep:seq (ep:numeric)
			      (ep:seq (ep:seq (ep:open-bracket)
					      (ep:expression))
				      (ep:close-bracket)))
		      #'(lambda (L) (let ((arg1 (nth 0 L))
					  (arg2 (nth 1 L)))
;				      (message "ep:quantity:arg1: %s" arg1)
;				      (message "ep:quantity:arg2: %s" arg2)
				      (list arg1 (car (cdr (car arg2)))))))))



(defun SI:parse-unit (token-stream)
  (funcall (ep:expression) token-stream))

(defun SI:parse-unit-region (start end)
  (interactive "r")
  (let ((lexer-result (ep:lexer-region start end)))
    (let ((parser-result (SI:parse-unit lexer-result)))
;      (message "lexer result: %s" lexer-result)
;      (message "parser result: %s" parser-result)
      parser-result)))

(defun SI:parse-unit-string (str)
  (let ((token-stream (ep:lexer-string str)))
    (SI:parse-unit token-stream)))
;      (message "lexer result: %s" lexer-result)
;      (message "parser result: %s" parser-result)



(defun SI:build-unit (parsed-result)
  "Build unit objects from parsed result."
  (if (null parsed-result)
      nil)
  (let ((tree (ep:get-succeeded-token parsed-result)))
;    (message "SI:build-unit:tree: %s" tree)
    (SI:evaluate tree)))

(defun SI:build-unit-region (start end)
  "Create the unit instance from the region."
  (interactive "r")
  (SI:build-unit (SI:parse-unit-region start end)))

(defun SI:build-unit-string (str)
  "Create the unit instance from the given STR."
  (SI:build-unit (SI:parse-unit-string str)))

(defun SI:parse-quantity (token-stream)
  (funcall (ep:quantity) token-stream))

(defun SI:parse-quantity-string (str)
  (let ((token-stream (ep:lexer-string str)))
    (SI:parse-quantity token-stream)))
;      (message "lexer result: %s" lexer-result)
;      (message "parser result: %s" parser-result)


(defun SI:build-quantity (parsed-result)
  "Build a quantity object from parsed result."
  (if (null parsed-result)
      nil)
  (let ((tree (ep:get-succeeded-token parsed-result)))
;    (message "SI:build-unit: tree -> %s" tree)
    (let ((num (cdr (car tree)))
	  (unt (car (cdr tree))))
;      (message "SI:build-unit: num -> %s" num)
;      (message "SI:build-unit: unt -> %s" unt)
;      (message "SI:build-unit: (SI:evaluate unt) -> %s" (SI:evaluate unt))
      (let ((built-unit (SI:evaluate unt))
	    (made-numeric (SI:make-numeric num)))
;	(message "SI:build-unit: built-unit -> %s" built-unit)
;	(message "SI:build-unit: made-numeric -> %s" made-numeric)
	(SI:make-quantity (get-symbol built-unit) built-unit nil made-numeric)))))
	      

(defun SI:build-quantity-string (str)
  "Create the quantity instance from the given STR."
  (SI:build-quantity (SI:parse-quantity-string str)))



(defun SI:token-type (token)
  (assert (consp token) nil "SI:token-type:")
  (let ((head (car token)))
    (if (atom head)
	head
      'nested)))				;token is nested
       
(defun SI:token-str (token)
  (assert (consp token) nil "SI:token-str: %s is not cons cell.")
  (cdr token))
  
(defun SI:evaluate (tree)
  "Evaluate the token tree and return an instance."
;  (assert (SI:treep tree))
  (let ((top (car tree))
	(args (cdr tree)))
;    (message "SI:evaluate: tree -> %s" tree)
;    (message "SI:evaluate: top -> %s" top)
;    (message "SI:evaluate: args -> %s" args)
    (let ((top-type (SI:token-type top)))
;      (message "SI:evaluate:top-type %s" top-type)
      (cond ((eq top-type 'operator)
	     (SI:apply top args))
	    ;; prefix
	    ((eq top-type 'prefix)	; prefix is necessarily followed by unit
	     (let ((p (SI:find-prefix (intern (SI:token-str top)))) ;assumes prefix
		   (unit (if (SI:treep (car args))
			     (SI:evaluate (car args))
			   (SI:make-unit-instance-from (car args)))))
	       (assert (consp p) t "The prefix %s is not found at SI:whole-units-alist !!")
;	       (message "SI:evaluate:prefix %s" (cdr p))
;	       (message "SI:evaluate:quantity %s" unit)
	       (add-prefix unit (cdr p))))
	    ((eq top-type 'nested)
	     (SI:evaluate top))
	    ((or (eq top-type 'float) (eq top-type 'integer))
	     (if (null args)		; numeric instance
		 (SI:make-numeric-instance-from top)
	       ;; quantity instance
	       (let ((made-unit-instance (SI:make-unit-instance-from (car args)))
		     (result nil))
		 (setq result (SI:make-quantity (get-symbol made-unit-instance) made-unit-instance) )
		 (oset result qnumber (SI:make-numeric-instance-from top)))))
	    ;; self-evaluating token such as 'basic-token'
	    (t
	     (SI:make-unit-instance-from top))))))

(defun SI:treep (tree-or-token)
  (if (atom (car tree-or-token))
      nil				;must be a token
    t))
  
(defun SI:apply (top rest)
  (assert (eq (length rest) 2))
;  (message "SI:apply:top %s" top)
;  (message "SI:apply:rest %s" rest)
  (let ((op (SI:token-str top))
	(arg1 (if (SI:treep (nth 0 rest))
		  (SI:evaluate (nth 0 rest))
		(SI:make-unit-instance-from (nth 0 rest))))
	(arg2 (if (SI:treep (nth 1 rest))
		  (SI:evaluate (nth 1 rest))
		(SI:make-unit-instance-from (nth 1 rest)))))
;    (message "SI:apply:op %s" op)
;    (message "SI:apply:arg1 %s" arg1)
;    (message "SI:apply:arg2 %s" arg2)
    (cond ((string= op "*")
	   (multiply arg1 arg2))
	  ((string= op "/")
	   (divide arg1 arg2))
	  (t
	   (error "SI:apply: Unknown operator %s" op)))))

(defun SI:make-unit-instance-from (token)
  (let* ((token-type (SI:token-type token))
	 (token-str (SI:token-str token)))
    (let ((instance (or (SI:find-unit (intern token-str))
			(SI:find-prefix (intern token-str)))))
;      (message "SI:make-unit-instance-from:token-str: %s" token-str)
;      (message "SI:make-unit-instance-from:instance: %s" instance)
      (if (consp instance)
	  (cdr instance)
	;; execute the process for the token that is not found on SI:whole-units-alist
	;; for example, s-3,m3,mm,kPa,cm3,cd/m2,N/kg,W/m2*K,m2*K/W,kPa*m
	(assert (eq token-type 'complex-units) t "token-type: %s")
	(cond ((eq (string-match "^\\([^/]*\\)/\\([^/]*\\)" token-str) 0)
	       (let ((numerator (match-string 1 token-str))
		     (denominator (match-string 2 token-str))
		     (num-splitted nil)
		     (deno-splitted nil))
		 (dolist (i (SI:split-token-by-star numerator nil))
		   (let ((found (or (SI:find-unit (intern i))
				    (SI:find-prefix (intern i)))))
		     (if (consp found)
			 (setq num-splitted (cons (cdr found) num-splitted))
		       (setq num-splitted (cons (SI:make-unit-instance-from (cons 'complex-units i)) num-splitted)))))
		 (dolist (i (SI:split-token-by-star denominator nil))
		   (let ((found (or (SI:find-unit (intern i))
				    (SI:find-prefix (intern i)))))
		     (if (consp found)
			 (setq deno-splitted (cons (cdr found) deno-splitted))
		       (setq deno-splitted (cons (SI:make-unit-instance-from (cons 'complex-units i)) deno-splitted)))))
;		 (message "SI:make-unit-instance-from:num-splitted: %s" num-splitted)
;		 (message "SI:make-unit-instance-from:deno-splitted: %s" deno-splitted)
		 (setq num-splitted (reduce #'(lambda (x y) (multiply x y)) num-splitted))
		 (setq deno-splitted (reduce #'(lambda (x y) (multiply x y)) deno-splitted))
;		 (message "SI:make-unit-instance-from:num-splitted: %s" num-splitted)
;		 (message "SI:make-unit-instance-from:deno-splitted: %s" deno-splitted)
		 (divide num-splitted deno-splitted)))
	      ;; m-2,etc
	      ((eq (string-match "^\\([a-zA-Z]+\\)\\(-?[0-9]+\\)" token-str) 0)
	       (let ((sym (match-string 1 token-str))
		     (expo (match-string 2 token-str)))
;		 (message "SI:make-unit-instance-from:sym: %s" sym)
;		 (message "SI:make-unit-instance-from:expo: %s" expo)
		 (exponential (SI:build-unit-string sym) (string-to-number expo))))
	      ((eq (string-match "^\\([a-zA-Z]+\\)" token-str) 0)
	       ;; must be a prefix + unit(quantity) or nil(which means void-unit)
	       (cond ((eq (string-match "^kg$" token-str) 0)
		      SI:kilogram)
		     ((eq (string-match "^nil$" token-str) 0)
		      SI:void-unit)
		     (t
		      (SI:make-instance-prefix-unit token-str))))
	      (t
	       (error "SI:make-unit-instance-from: Does not matched: %s" token-str)))))))

;; (SI:make-instance-prefix-unit "kPa") 
(defun SI:make-instance-prefix-unit (str)
  (let ((possible-list nil)
	(result nil))
    (cond ((< 2 (length str))
	   (dotimes (i 2)
	     (let ((former (substring str 0 (1+ i)))
		   (latter (substring str (1+ i) nil)))
	       (setq possible-list (cons (cons former latter) possible-list)))))
	  ((= 2 (length str))
	   (setq possible-list (cons (cons (substring str 0 1) (substring str 1 nil)) possible-list)))
	  (t
	   (error "Unknown unit %s" str)))
;    (message "SI:make-instance-prefix-unit possible-list -> %s" possible-list)
    (dolist (i possible-list result)
      (let ((prf (car i))
	    (unt (cdr i)))
;	(message "SI:make-instance-prefix-unit prf -> %s" prf)
;	(message "SI:make-instance-prefix-unit unt -> %s" unt)
	(cond ((string= prf "da")
	       (dolist (ii SI:units-str-list nil)
		 (if (string= unt ii)
		     (setq result (SI:make-quantity nil (cdr (SI:find-unit (intern unt)) SI:deka)))))) ;asumes unit
	      (t
	       (if (assoc* (intern prf) SI:prefix-table)
		   (dolist (ii SI:units-str-list nil)
		     (if (string= unt ii)
			 (setq result (SI:make-quantity
				       (intern str)
				       (cdr (SI:find-unit (intern unt))) ;asummes unit
				       (cdr (SI:find-prefix (intern prf)))))))))))) ;asummes prefix
    (if (null result)
	(error "Unknow unit: %s" str)
      result)))
		 

(defun SI:make-numeric-instance-from (top)
  (assert (or (eq (car top) 'float) (eq (car top) 'integer)))
  (SI:make-numeric (cdr top)))

  
(defun SI:split-token-by-expo (str result)
  "Not in use."
  (if (eq (string-match "\\(^[a-zA-Z+-]+[0-9]+\\)*" str) 0)
	(let ((matched-end (match-end 1)))
;	  (message "matched-end: %s" matched-end)
;	  (message "result: %s" result)
	  (if matched-end
	      (let ((result (cons (match-string 1 str) result)))
		(SI:split-token-by-expo (substring str matched-end) result))
	    (if (eq (length str) 0)
		result
	      (cons str result))))
    result))
	
(defun SI:split-token-by-star (str result)
;  (if (eq (string-match "\\(^[a-zA-Z+-]+[0-9]*\\*\\)*" str) 0)
  (if (eq (string-match "\\(^[^*]+\\)\\*" str) 0)
	(let ((matched-end (match-end 1)))
;	  (message "matched-end: %s" matched-end)
	  (if matched-end
	      (let ((result (cons (match-string 1 str) result))
		    (new-str (substring str (1+ matched-end)))
		    )
;		(message "result: %s" result)
;		(message "new-str: %s" new-str)
		(SI:split-token-by-star new-str result))
	    (if (eq (length new-str) 0)
		result
	      (cons new-str result))))
    (if (eq (length str) 0)
	result
      (cons str result))))

	
    

(defun SI:convert (from to)
  "Convert 'FROM quantity object to 'TO quantity object.
Return 'TO quantity object if the conversion succeeded, nil if failed."
  (if (not (same-dimention from to))
      nil
    ;;引数をそれぞれ standardize して変換する。
    (let* ((standardized-from (standardize from))
	   (standardized-to (standardize to))
	   (divided (divide standardized-from standardized-to)))
;      (message "SI:convert: standardized-from -> %s" standardized-from)
;      (message "SI:convert: standardized-to -> %s" standardized-to)
      (SI:make-quantity (get-symbol to)
			to
			SI:void-prefix
			(get-number divided)))))

(defun SI:coerce (from to)
  "Convert 'FROM quantity or numeric object to 'TO quantity object.
Return 'TO quantity object if the conversion succeeded, nil if failed.
Note that the numeric part of 'TO is discarded."
  (cond ((numberp from)
	 (SI:make-quantity (get-symbol to)
			   to
			   SI:void-prefix
			   (SI:make-numeric (number-to-string from))))
	((obj-of-class-p from 'units)
	 (SI:make-quantity (get-symbol to)
			   to
			   SI:void-prefix
			   (get-number from)))
	((same-class-p from 'numeric)  
	 (SI:make-quantity (get-symbol to)
			   to
			   SI:void-prefix
			   from))
	(t
	 (error "Error in SI:coerce"))))

;;; simplification

(defmethod standardize ((q quantity))
  (let ((scale (SI:extract-scale (get-symbol q)))
	(dim-alist (get-dimention-alist q))
	(num (get-number q)))
;    (message "SI:standardize scale -> %s" scale)
;    (message "SI:standardize dim-alist -> %s" dim-alist)
;    (message "SI:standardize num -> %s" num)
    (quantity "quantity" :qsymbol (construct-unit-symbol q) :udimention (dimention "dimention" :dsymbol nil  :dimention dim-alist) :qprefix SI:void-prefix :qnumber (multiply num scale))))

(defun SI:extract-scale (symbol-tree)
  "Return a numeric instance."
  (cond ((listp symbol-tree)
	 (let ((stem (car symbol-tree))
	       (left-leaf (nth 1 symbol-tree))
	       (right-leaf (nth 2 symbol-tree)))
;	   (message "SI:extract-scale stem -> %s" stem)
;	   (message "SI:extract-scale left-leaf -> %s" left-leaf)
;	   (message "SI:extract-scale right-leaf -> %s" right-leaf)
	   (cond ((eq '/  stem)
		  (divide (SI:extract-scale left-leaf) (SI:extract-scale right-leaf)))
		 ((eq '* stem)
		  (multiply (SI:extract-scale left-leaf) (SI:extract-scale right-leaf)))
		 (t
		  (error "SI:extract-scale")))))
	((symbolp symbol-tree)
	 (cond ((SI:basic-unit-p symbol-tree)
		(SI:make-numeric "1.0"))
	       ((SI:defined-unit-p symbol-tree)
		(let* ((reduced-quantity (cdr (assoc* symbol-tree SI:unit-table)))
		       (reduced-quantity-symbol (get-symbol reduced-quantity)))
		  (if (not (SI:basic-unit-p reduced-quantity-symbol))
		      (multiply (SI:make-numeric (get-value (get-number reduced-quantity)))
				(SI:extract-scale reduced-quantity-symbol))
		    (SI:make-numeric (get-value (get-number (cdr (assoc* symbol-tree SI:unit-table))))))))
	       (t			;constructed unit
		(let ((str (symbol-name symbol-tree))
		      (possible-list nil)
		      (result nil))
;		  (message "SI:extract-prefix str -> %s" str)
		  (cond 
		   ((< 2 (length str))
		    (dotimes (i 2)
		      (let ((former (substring str 0 (1+ i)))
			    (latter (substring str (1+ i) nil)))
			(setq possible-list (cons (cons former latter) possible-list)))))
		   ((= 2 (length str))
		    (setq possible-list (cons (cons (substring str 0 1) (substring str 1 nil)) possible-list)))
		   (t
		    (error "Unknown unit %s" str)))
;		  (message "SI:extract-scale possible-list -> %s" possible-list)
		  (dolist (i possible-list result)
		    (let ((prf (car i))
			  (unt (cdr i)))
		      (cond ((string= prf "da")
			     (dolist (ii SI:units-str-list nil)
			       (if (string= unt ii)
				   (setq result (SI:make-numeric (calc-eval (format "exp10(%s)" (cdr (assoc* 'da SI:prefix-table)))))))))
			    ((assoc* (intern prf) SI:prefix-table)
			     (case (SI:classify-unit (intern unt))
			       (basic-units
				(setq result (SI:make-numeric (calc-eval (format "exp10(%s)" (cdr (assoc* (intern prf) SI:prefix-table)))))))
			       (derived-units
;				(message "SI:extract-scale prf -> %s" prf)
;				(message "SI:extract-scale unt -> %s" unt)
;				(message "SI:extract-scale (assoc* unt SI:unit-table) -> %s" (assoc* unt SI:unit-table))
				(setq result
				      (multiply
				       (SI:make-numeric (calc-eval (format "exp10(%s)" (cdr (assoc* (intern prf) SI:prefix-table)))))
				       (get-number (cdr (assoc* (intern unt) SI:unit-table))))))
			       (complex-units
				(error "complex-units: %s" str)
				)
			       (t
				(error "Unknow unit: %s" str)))))))))))))


(defmethod SI:simplify-quantity ((q quantity))
  "Incomplete"
  (let ((standardized-symbol (SI:standardize-symbol (get-symbol q))))
    (if (or (symbolp standardized-symbol) (SI:symbol-standardized-multiples-p standardized-symbol))
	q
      (assert (eq (car standardized-symbol) '/))
      (let ((expo 0)
	    (numerator (nth 1 standardized-symbol))
	    (denominator (nth 2 standardized-symbol)))
	(cond ((symbolp numerator)
	       
	       )
	      ((eq (car numerator) '*)
	       )
	      (t
	       (error "SI:simplify-quantity: The numerator of the quantity %s should be a symbol or multiples, but I got %s" q numerator)))))))

(defun SI:symbol-standardized-p (tree)
  "Check if 'TREE is standardized enough.
Standardized symbol is either a symbol or (* symbol symbol ...) or (/ (* symbol ...) (* symbol ...))."
  (cond ((symbolp tree)
	 t)
	((consp tree)
	 (if (or (SI:symbol-standardized-multiples-p tree)
		 (and (eq '/ (car tree))
		      (or (symbolp (nth 1 tree))
			  (SI:symbol-standardized-multiples-p (nth 1 tree)))
		      (or (symbolp (nth 2 tree))
			  (SI:symbol-standardized-multiples-p (nth 2 tree)))))
	     t
	   nil))
	(t
	 nil)))

(defun SI:symbol-standardized-multiples-p (tree)
  "Return t if the given symbols are (* symbol symbol ...)"
  (if (eq (car tree) '*)
      (let ((result t))
	(dolist (i (cdr tree))
	  (if (not (symbolp i))
	      (setq result nil)))
	result)
    nil))

  
(defun SI:standardize-symbol (tree)
  (let ((standardized-enough nil)
	(once-standardized tree))
    (loop until standardized-enough
	  do
	  (setq once-standardized (SI:standardize-symbol-once once-standardized))
	  (if (SI:symbol-standardized-p once-standardized)
	      (setq standardized-enough t)))
    once-standardized))


(defun SI:standardize-symbol-once (tree)
  (if (SI:symbol-standardized-p tree)
      tree
    (let ((proc-symplify-* (lambda (tree)
			     (if (SI:symbol-standardized-multiples-p tree)
				 tree
			       (let ((first-node (car (cdr tree)))
				     (rest-nodes (cdr (cdr tree))))
;				 (message "SI:standardize-symbol-once first-node -> %s" first-node)
;				 (message "SI:standardize-symbol-once rest-nodes -> %s" rest-nodes)
				 ;;(* (* a b) c) -> (* a b c)
				 ;;(* (/ a b) c) -> (/ (* a c) b)
				 (if (consp first-node)
				     (cond
				      ((eq (car first-node) '*)
				       (cons '* (append (cdr first-node) rest-nodes)))
				      ((eq (car first-node) '/)
				       (cons '/ (cons (cons '* (cons (car (cdr first-node)) rest-nodes))
						      (cdr (cdr first-node)))))
				      (t
				       (error "SI:standardize-symbol-once")))
				   ;;(* c (* a b)) -> (* (* a b) c)
				   ;;(* c (/ a b) ...) -> (/ (* c a ...) b)
				   ;;(* c (/ nil b) ...) -> (/ (* c ...) b)
				   (cond ((consp (car rest-nodes))
					  (let ((item-a (car (cdr (car rest-nodes))))
						(item-c first-node)
						(item-b (car (cddr (car rest-nodes)))))
;					    (message "SI:standardize-symbol-once: item-a -> %s" item-a)
;					    (message "SI:standardize-symbol-once: item-b -> %s" item-b)
;					    (message "SI:standardize-symbol-once: item-c -> %s" item-c)
					    (cond 
					     ((eq (car (car rest-nodes)) '*)
					      (cons '* (cons first-node (cdr (car rest-nodes)))))
					     ((eq (car (car rest-nodes)) '/)
					      (if (null item-a)
						  (if (< 1 (length rest-nodes))
						      (cons '/ (list (cons '*
									   (cons item-c
										 (cdr rest-nodes)))
								     item-b))
						    (list '/ item-c item-b))
						(cons '/ (list (cons '*
								     (cons item-c
									   (cons item-a
										 (cdr rest-nodes))))
							       item-b)))))))
					 ;; (* c d ... (op a b)) -> (* (op a b) c d ...)
					 ((symbolp (car rest-nodes))
					  (cons '* (append rest-nodes (list first-node))))
					 (t
					  tree)))))))
	  (proc-symplify-/ (lambda (tree)
			     ;;(/ (/ a b) c) -> (/ a (* b c))
			     ;;(/ a (/ b c)) -> (/ (* a c) b)
			     ;;(/ (* a b) (/ c d))) -> (/ (* a b d) c)
			     (let ((first-arg (nth 1 tree))
				   (second-arg (nth 2 tree)))
;			       (message "SI:standardize-symbol-once first-arg -> %s" first-arg)
;			       (message "SI:standardize-symbol-once second-arg -> %s" second-arg)
			       (if (consp first-arg)
				   (cond ((eq (car first-arg) '/)
					  (list '/ (car (cdr first-arg)) (list '* (nth 2 first-arg) second-arg)))
					 ((and (eq (car first-arg) '*) (consp second-arg) (eq (car second-arg) '/))
					  (list '/ (append first-arg (list (caddr second-arg)))
						(cadr second-arg)))
					 (t
					  tree))
				 (if (symbolp first-arg)
				     (cond ((eq (car second-arg) '/)
					    (list '/ (list '* first-arg (nth 2 second-arg)) (nth 1 second-arg)))
					   (t
					    tree))))))))
      (cond ((consp tree)
	     (let ((stem (car tree))
		   (children (cdr tree)))
;	       (message "SI:standardize-symbol-once tree -> %s" tree)
;	       (message "SI:standardize-symbol-once stem -> %s" stem)
;	       (message "SI:standardize-symbol-once children -> %s" children)
	       (setq tree (cons stem (mapcar (function SI:standardize-symbol) children)))
;	       (message "SI:standardize-symbol-once tree -> %s" tree)
	       (cond
		((eq stem '*)
		 (funcall proc-symplify-* tree))
		((eq stem '/)
		 (assert (eq (length tree) 3))
		 (funcall proc-symplify-/ tree))
		(t
;		 (message "SI:standardize-symbol stem -> %s" stem)
		 (error "SI:standardize-symbol: operator must be a * or /")))))
	    (t
	     (error "SI:standardize-symbol: argument must be a symbol or list"))))))

;;; unit table for conversion
;; symbol, equivalent unit, base unit or not


(defvar SI:unit-table '() "The table of units")
(defvar SI:prefix-table '() "The table of prefixes")

(defmacro SI:add-to-unit-table (sym &optional equiv-str)
  `(let ((val (if (null ,equiv-str)
		  nil
		(SI:build-quantity-string ,equiv-str))))
     (setq SI:unit-table (acons ,sym val SI:unit-table))))

;; (defmacro SI:add-to-unit-table (sym &optional equiv-str)
;;   `(let ((val (if (null ,equiv-str)
;; 		  nil
;; 		(string-match "\\([^{]+\\){\\([^\}]+\\)}" ,equiv-str 0)
;; 		(list
;; 		 (match-string 1 ,equiv-str)
;; 		 (ep:get-succeeded-token (SI:parse-unit-string (match-string 2 ,equiv-str)))))))
;;      (setq SI:unit-table (acons ,sym val SI:unit-table))))

(defmacro SI:add-to-prefix-table (sym scale)
  `(setq SI:prefix-table (acons ,sym (calc-eval ,scale) SI:prefix-table)))

(defun SI:defined-unit-p (sym)
  (if (assoc* sym SI:unit-table)
      t
    nil))

(defun SI:basic-unit-p (sym)
  (if (not (SI:defined-unit-p sym))
      nil
    (if (cdr (assoc* sym SI:unit-table))
	nil
      t)))

(defun SI:classify-unit (sym)
  (let ((found-as-unit (assoc* sym SI:unit-table)))
    (if (not found-as-unit)
	(if (assoc* sym SI:prefix-table)
	    'prefix
	  'complex-units)
      (if (null (cdr found-as-unit))
	  'basic-units
	'derived-units))))


(SI:add-to-unit-table 'm)
(SI:add-to-unit-table 'kg)
(SI:add-to-unit-table 's)
(SI:add-to-unit-table 'A)
(SI:add-to-unit-table 'K)
(SI:add-to-unit-table 'mol)
(SI:add-to-unit-table 'cd)
(SI:add-to-unit-table 'Hz "1.0{s-1}")
(SI:add-to-unit-table 'Gy "1.0{J / kg}")
(SI:add-to-unit-table 'N "1.0{(m * kg) / s-2}")
(SI:add-to-unit-table 'Pa "1.0{N / m2}")
(SI:add-to-unit-table 'J "1.0{N / m}")
(SI:add-to-unit-table 'W "1.0{J / s}")
(SI:add-to-unit-table 'C "1.0{s * A}")
(SI:add-to-unit-table 'V "1.0{W / A}")
(SI:add-to-unit-table 'F "1.0{C / V}")
(SI:add-to-unit-table 'Wb "1.0{V * s}")
(SI:add-to-unit-table 'T "1.0{Wb / m2}")
(SI:add-to-unit-table 'H "1.0{Wb / A}")
(SI:add-to-unit-table 'rad)		; treated as base unit
(SI:add-to-unit-table 'sr)		; treated as base unit
(SI:add-to-unit-table 'L "1e-3{m3}")
(SI:add-to-unit-table 'cc "1e-3{L}")
(SI:add-to-unit-table 'Torr "0.0013157894736842105{atm}")
(SI:add-to-unit-table 'atm "101325{Pa}")
(SI:add-to-unit-table 'g "1e-3{kg}")
(SI:add-to-unit-table 'min "60{s}")
(SI:add-to-unit-table 'hr "60{min}")
(SI:add-to-unit-table 'day "24{hr}")
(SI:add-to-unit-table 'week "7{day}")
(SI:add-to-unit-table 'mmHg "1.0{Torr}")

;; extra units

(defmacro SI:add-user-unit (sym constructor)
  `(let ((obj ,constructor)
	 (new-symbol-name (intern (concat "SI:" (symbol-name ,sym))))) ;Should I chech whether the new-symbol-name is already predined ??
     (assert (obj-of-class-p obj 'units))
     ;; checking dimention
     (let* ((new-dimention (get-dimention obj))
	    (new-dimention-alist (get-dimention-alist new-dimention))
	    (dimention-list (read-whole-dimention new-dimention)))
       (dolist (i new-dimention-alist dimention-list)
	 (if (not (member (car i) (read-whole-dimention new-dimention)))
	     (setq dimention-list (cons (car i) dimention-list))))
       (oset new-dimention whole-dimention dimention-list))
     ;; adding user defined unit to the database
     (SI:add-to-unit-table ,sym)
     (SI:add-to-unit-alist new-symbol-name obj)))

(SI:add-user-unit 'drop (SI:make-basic-units 'drop (SI:make-dimention 'drop '(drop . 1))))
;(SI:add-to-unit-table 'drop)
;(SI:add-to-unit-alist SI:drop (SI:make-basic-units 'drop (SI:make-dimention 'drop '(drop . 1))))


(SI:add-to-prefix-table 'Y "24")
(SI:add-to-prefix-table 'Z "21")
(SI:add-to-prefix-table 'E "18")
(SI:add-to-prefix-table 'P "15")
(SI:add-to-prefix-table 'T "12")
(SI:add-to-prefix-table 'G "9")
(SI:add-to-prefix-table 'M "6")
(SI:add-to-prefix-table 'k "3")
(SI:add-to-prefix-table 'h "2")
(SI:add-to-prefix-table 'da "1")
(SI:add-to-prefix-table 'd "-1")
(SI:add-to-prefix-table 'c "-2")
(SI:add-to-prefix-table 'm "-3")
(SI:add-to-prefix-table 'u "-6") 
(SI:add-to-prefix-table 'n "-9")
(SI:add-to-prefix-table 'p "-12")
(SI:add-to-prefix-table 'f "-15")
(SI:add-to-prefix-table 'a "-18")
(SI:add-to-prefix-table 'z "-21")
(SI:add-to-prefix-table 'y "-24")


;;; assertion
; (load-file "/home/emile/develop/elisp/units.el")

(defvar SI:testing nil)
(eval-when-compile
  (setq SI:testing t)
  )

(when SI:testing
;  (assert (same-dimention SI:liter SI:cubic-centimeter))
;  (assert (equal (SI:parse-unit '((open-paren . "(") (prefix . "c") (basic-units . "s") (close-paren . ")") (operator . "/") (basic-units . "m"))) '((((operator . "/") ((prefix . "c") (basic-units . "s")) (basic-units . "m")) nil))))
;;    (assert (equal (ep:lexer-string "(m * s)/dL")
;;  		 '((open-paren . "(") (basic-units . "m") (operator . "*") (basic-units . "s") (close-paren . ")") (operator . "/") (quantity . "dL"))))
;;    (assert (same-dimention (get-dimention (SI:build-unit-string "mg / dL"))
;;  			   (get-dimention (SI:build-unit-string "kg / m3"))))
;  (assert (equal (SI:standardize-symbol '(/ (/ kg (* s s)) m)) '(/ kg (* s s m))))
  )



(provide 'units)

;;; units.el ends here


;;; Local Variables:
;;; : ***
;;; End:




