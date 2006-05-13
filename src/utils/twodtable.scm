;;; ----------------------------------------------------------------------
;;; Copyright 2005 Alexey Radul and Rebecca Frankel.
;;; ----------------------------------------------------------------------
;;; This file is part of The Symmetriad.
;;; 
;;; The Symmetriad is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;; 
;;; The Symmetriad is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with The Symmetriad; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
;;; ----------------------------------------------------------------------

; This is a piece of abstraction layer for easily swapping
; implementations of two-d-tables.

(declare (usual-integrations))

;(load "utils/twodtablesimple")
(load "utils/twodtablehash")


;; These printing procedures live outside the swappable abstraction.

;; A simple printing procedure. 
;; x-keys go down the side. y-keys across the top. 
;; print val proc can be used to format the printing 
;; of the value however the caller desires. 
;; Note: the print-val-proc will sometimes be handed 
;; #f if there is no value.

(define (print-table-by-x 
	 the-table x-symbol-list y-symbol-list print-val-proc)
  (assert (two-d-table? the-table))
  (let ((table-char "|"))
	;;print out header
	(format #t "~@9a  " table-char)
	(for-each (lambda (y-sym) (format #t "~7a" y-sym)) 
	         y-symbol-list)
	(format #t "~%~@9a" table-char)
	(for-each (lambda (sym) (format #t "-------" ))
		  y-symbol-list)
        (for-each 
	 (lambda (symbol)
	   (let ((x-alist (two-d-get-alist-x the-table symbol)))
		 (begin (format #t "~% ~6a ~@1a  " symbol table-char)
			(for-each 
			 (lambda (y-sym)
			   (let ((entry (assq y-sym x-alist)))
			     (if entry 
				 (format #t "~7a"
				     (print-val-proc (cdr entry)))
				 (format #t "~7a" 
					 (print-val-proc #f)))))
			 y-symbol-list))))
		  x-symbol-list)))


;; Same but transposed. Now x keys go across the top and 
;; y keys down the side. 

(define (print-table-by-y
	 the-table x-symbol-list y-symbol-list print-val-proc)
  (assert (two-d-table? the-table))
  (let ((table-char "|"))
	;;print out header
	(format #t "~@9a  " table-char)
	(for-each (lambda (y-sym) (format #t "~7a" y-sym)) 
	         x-symbol-list)
	(format #t "~%~@9a" table-char)
	(for-each (lambda (sym) (format #t "-------" ))
		  x-symbol-list)
        (for-each 
	 (lambda (symbol)
	   (let ((x-alist (two-d-get-alist-y the-table symbol)))
		 (begin (format #t "~% ~6a ~@1a  " symbol table-char)
			(for-each 
			 (lambda (y-sym)
			   (let ((entry (assq y-sym x-alist)))
			     (if entry 
				 (format #t "~7a"
				     (print-val-proc (cdr entry)))
				 (format #t "~7a" 
					 (print-val-proc #f)))))
			 x-symbol-list))))
		  y-symbol-list)))