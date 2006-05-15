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

(declare (usual-integrations))

;;; This file contains functions that create coxeter matrices and coxeter lengths
;;; to go with them, for the possible coxeter groups

(define (make-cox-matrix-A n)
  (make-coxeter-matrix 
   n
   (lambda (i j)
     (cond ((= i j) 1)
	   ((= (- i j) 1) 3)
	   ((= (- j i) 1) 3)
	   (else 2)))))

(define (make-cox-len-A n)
  (make-list n 1))

#|
(pe (make-cox-matrix-A 3))
(*coxeter-matrix*
 (matrix-by-rows (list 1 3 2)
		 (list 3 1 3)
		 (list 2 3 1)))

(pe (make-cox-matrix-A 4))
(*coxeter-matrix*
 (matrix-by-rows (list 1 3 2 2)
                 (list 3 1 3 2)
                 (list 2 3 1 3)
                 (list 2 2 3 1)))
|#

	 
(define (make-cox-matrix-B/C n)
  (make-coxeter-matrix 
   n
   (lambda (i j)
     (cond ((= i j) 1)
	   ((= (- i j) 1)
	    (if (= i (- n 1))
		4
		3))
	   ((= (- j i) 1)
	    (if (= j (- n 1))
		4
		3))
	   (else 2)))))

;(define (make-cox-len-B n)
;  (append
;   (make-list (- n 1) 1)
;   (list (/ 1 'sqrt2))))

;(define (make-cox-len-C n)
;  (append
;   (make-list (- n 1) 1)
;   (list 'sqrt2)))

#|
(pe (make-cox-matrix-B/C 4))	 
(*coxeter-matrix*
 (matrix-by-rows (list 1 3 2 2)
                 (list 3 1 3 2)
                 (list 2 3 1 4)
                 (list 2 2 4 1)))
;Unspecified return value

(pe (make-cox-matrix-B/C 3))
(*coxeter-matrix*
 (matrix-by-rows (list 1 3 2)
		 (list 3 1 4)
		 (list 2 4 1)))
|#	 

(define (make-cox-matrix-D n)
  (make-coxeter-matrix 
   n
   (lambda (i j)
     (cond ((= i j) 1)
	   ((= i (- n 1))
	    (if (= (- i j) 2) 3 2))
	   ((= j (- n 1))
	    (if (= (- j i) 2) 3 2))
	   ((= (- i j) 1) 3)
	   ((= (- j i) 1) 3)
	   (else 2)))))

(define (make-cox-len-D n)
  (make-list n 1))

#|
(pe (make-cox-matrix-D 2))
(*coxeter-matrix* (matrix-by-rows (list 1 2)
				  (list 2 1)))

(pe (make-cox-matrix-D 3))
(*coxeter-matrix*
 (matrix-by-rows (list 1 3 3)
		 (list 3 1 2)
		 (list 3 2 1)))

(pe (make-cox-matrix-D 4))
(*coxeter-matrix*
 (matrix-by-rows (list 1 3 2 2)
                 (list 3 1 3 3)
                 (list 2 3 1 2)
                 (list 2 3 2 1)))
|#

(define (make-cox-matrix-H n)
  (assert (or (= n 3) (= n 4)) "H must be dimension 3 or 4")
  (make-coxeter-matrix 
   n
   (lambda (i j)
     (cond ((= i j) 1)
	   ((= (- i j) 1)
	    (if (= i (- n 1)) 5 3))
	   ((= (- j i) 1)
	    (if (= j (- n 1)) 5 3))
	   (else 2)))))

; The lengths only actually matter if the group is crystallographic.
; These just need to agree with the actual roots...
(define (make-cox-len-H n)
  (make-list n 1))
  
#|
(pe (make-cox-matrix-H 3))
(*coxeter-matrix* 
 (matrix-by-rows (list 1 3 2) 
		 (list 3 1 5) 
		 (list 2 5 1)))

(pe (make-cox-matrix-H 4))
(*coxeter-matrix* 
 (matrix-by-rows (list 1 3 2 2) 
		 (list 3 1 3 2) 
		 (list 2 3 1 5) 
		 (list 2 2 5 1)))

|#

(define (make-cox-matrix-I2 m)
  (make-coxeter-matrix 
   2
   (lambda (i j)
     (if (= i j) 1 m))))

(define (make-cox-len-I2)
  (make-list 2 1))

#|
(pe (make-cox-matrix-I2 6))
(*coxeter-matrix* 
 (matrix-by-rows (list 1 6) 
		 (list 6 1)))

|#

(define (make-cox-matrix-F n)
  (assert (= 4 n) (string-append "F must be dimension 4, not " 
				 (number->string n) "."))
  (make-coxeter-matrix 
   4
   (lambda (i j)
     (cond ((= i j) 1)
	   ((= (- i j) 1)
	    (if (= i 2) 4 3))
	   ((= (- j i) 1)
	    (if (= j 2) 4 3))
	   (else 2)))))

; The lengths only actually matter if the group is crystallographic and we care
; about its crystallography.
; These just need to agree with the actual roots...
(define (make-cox-len-F4)
  (make-list 4 1))
  
#|
(pe (make-cox-matrix-F4))
(*coxeter-matrix*
 (matrix-by-rows (list 1 3 2 2)
                 (list 3 1 4 2)
                 (list 2 4 1 3)
                 (list 2 2 3 1)))
|#

