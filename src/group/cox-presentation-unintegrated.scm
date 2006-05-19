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

;;; This file contains the stuff from coxpresent that breaks 
;;; under (declare (usual-integrations))
;;; TODO what can I integrate?

;; cox-symbol is a 2D lookup table (keyed by the symbols for the roots)
;; that gives the cosines of the angles between the roots.
;; Returns a procedure that represents the root that results from reflecting 
;; mirror_(gen/proc) across the mirror given by reflect_(gen/proc)
;; Breaks horribly if the given roots are from different chambers.
(define ((reflect-lambda cox-symbol length-alist) 
	 mirror_gen reflect_gen mirror_proc reflect_proc)
  (let ((mirror_len (cdr (assq mirror_gen length-alist)))
	(reflect_len (cdr (assq reflect_gen length-alist)))
	(dot_cosine (two-d-get cox-symbol mirror_gen reflect_gen))
	(cache-key ()) ; Ah, the beauty of lexical scoping!
	(cache-val ()))
    (lambda (el-list)
      (if (eq? el-list cache-key)
	  cache-val
	  (begin 
	    (set! cache-key el-list)
	    (set! cache-val (unquote (simplify (sqrt-rules (simplify
                  (- (mirror_proc el-list) 
		     (* (reflect_proc el-list)
			2 (/ mirror_len reflect_len) dot_cosine)))
	    ))))
	    ;(pp cache-val) (newline)
	    cache-val)))))

;; symbolic values of cosines and sines
;; note also we want the *obtuse* version of the angle
;; so all the cosines are the negatives 
(define (sym-cox-cos angle-divisor)
  (cond ((= angle-divisor 1) 1)
	((= angle-divisor 2) 0)
	((= angle-divisor 3) (/ -1 2))
	((= angle-divisor 4) (/ -1 'sqrt2))
	;((= angle-divisor 5) (* -1 'cospi/5))
	((= angle-divisor 5) (* -1 (/ (+ 1 'sqrt5) 4)))
	((= angle-divisor 6) (* -1 (/ 'sqrt3 2)))
	(else `(cos (/ (*  pi (- ,angle-divisor 1)) 
		       ,angle-divisor)))))

(define (sym-cox-sin angle-divisor)
  (cond ((= angle-divisor 2) 1)
	((= angle-divisor 3) (/ 'sqrt3 2))
	((= angle-divisor 4) (/ 1 'sqrt2))
	((= angle-divisor 5) 'sinpi/5)
	((= angle-divisor 6) (/ 1 2))
	(else (sin (/ (*  pi (- angle-divisor 1)) 
		      angle-divisor)))))

