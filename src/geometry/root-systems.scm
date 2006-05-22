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

;(declare (usual-integrations)) TODO: why not? generic arithmetic?
; TODO this file may care about load order.

;;;;; Now creating geometries 
;;; These functions create roots
;;; We have chosen roots which will generate a polygon 
;;; with a front face along the z axis and a 
;;; front edge parallel to the x axis.

(define (make-cox-len-A n)
  (make-list n 1))

;; These roots agree with (make-cox-len-A 2)
(define (simple-roots-A2)
  (let* ((cox-mat (make-cox-matrix-A 2))
	 (p (matrix-ref (cm:matrix cox-mat) 0 1))
	 (root-dirs (list (vector 1 0)
			  (vector (sym-cox-cos p)
				  (sym-cox-sin p)))))
    (map * root-dirs (make-cox-len-A 2))))


(define (hacked-roots-A3)
  (list
   (vector 1 0 0)
   (vector -1/2 (/ 1 'sqrt2) -1/2)
   (vector 0 0 1)))

(define (hacked-roots-A4)
  (list
   (vector 1 0 0 0)
   (vector -1/2 (/ 1 'sqrt2) -1/2 0)
   (vector 0 0 1 0)
   (vector 0 (/ -1/2 'sqrt2) -1/2 (/ 'sqrt5 (* 2 'sqrt2)))))

;; Another way of creating geometries; the version that 
;; Richard Kane in "Reflection Groups and Invariant Theory"
;; claims is canonical. 

;; a utility that makes the basis vector (0 0 .. 1 .. 0 0) in n space.
;; note that e-sub-1 is (1 0 0 ...) not (0 1 0 ...) i.e.
;; we do 1-based indexing not 0-based indexing as comp. sci. people do.
(define (e-sub-i i n)
  (let ((answer (make-vector n 0)))
    (vector-set! answer (- i 1) 1)
    answer))
 
;; another slightly stranger utility useful for below procedures
(define (e-sub-i-minus-e-sub-iplusone i n)
  (let ((answer (make-vector n 0)))
    (vector-set! answer (- i 1) 1)
    (vector-set! answer i -1)
    answer))

; Here the angles are right, but the dimensionality is one too high,
; and the lengths are wrong (don't agree with (make-cox-len-A n))
(define (canonical-roots-A n)
  (map 
   (lambda (i)
     (e-sub-i-minus-e-sub-iplusone i (+ n 1)))
   (enumerate-interval 1 n)))
 
; The angles and dimensionality are right.
; The lenghts agree with (canonical-len-B n)
(define (canonical-roots-B n)
  (append 
   (map 
    (lambda (i)
      (e-sub-i-minus-e-sub-iplusone i n))
    (enumerate-interval 1 (- n 1)))
   (list (e-sub-i n n))))

(define (canonical-len-B n)
  (append
   (make-list (- n 1) 'sqrt2)
   (list 1)))

; The angles and dimensionality are right.
; The lenghts agree with (canonical-len-C n)
(define (canonical-roots-C n)
  (append 
   (map 
    (lambda (i)
      (e-sub-i-minus-e-sub-iplusone i n))
    (enumerate-interval 1 (- n 1)))
   (list (* 2 (e-sub-i n n)))))
 
(define (canonical-len-C n)
  (append
   (make-list (- n 1) 'sqrt2)
   (list 2)))

; The angles and dimensionality are right.
; The lenghts agree with (canonical-len-D n)
(define (canonical-roots-D n)
  (append 
   (map 
    (lambda (i)
      (e-sub-i-minus-e-sub-iplusone i n))
    (enumerate-interval 1 (- n 1)))
   (list (+ (e-sub-i (- n 1) n) (e-sub-i n n)))))

(define (canonical-len-D n)
  (make-list n 'sqrt2))

(define (canonical-roots-F n)
  (assert (= 4 n) (string-append "F must be dimension 4, not " 
				 (number->string n) "."))
  (list (e-sub-i-minus-e-sub-iplusone 2 4)
	(e-sub-i-minus-e-sub-iplusone 3 4)
	(e-sub-i 4 4)
	(vector 1/2 -1/2 -1/2 -1/2)))

(define (canonical-len-F n)
  (assert (= 4 n) (string-append "F must be dimension 4, not " 
				 (number->string n) "."))
  (list 'sqrt2 'sqrt2 1 1))
	
; The angles are right.  The lengths agree with
; (make-cox-len-I2)
(define (canonical-roots-I2 m)
  (list (vector 1 0)
	(vector (sym-cox-cos m) (sym-cox-sin m))))

(define (make-cox-len-I2)
  (make-list 2 1))


; Here's yet a third way to create a geometry: The Hack.
(define icosoverts
  (list (up 0 0 1)
	(up (/ 2 (sqrt 5)) 0 (/ 1 (sqrt 5)))
	(up (/ (* 2 (cos (* 2/5 :pi))) (sqrt 5))
	    (/ (* 2 (sin (* 2/5 :pi))) (sqrt 5))
	    (/ 1 (sqrt 5)))
	))


(define H3-omegavects
  (list (car icosoverts)
	(* 1/2 (+ (car icosoverts) (cadr icosoverts)))
	(* 1/3 (+ (car icosoverts) (cadr icosoverts) (caddr icosoverts)))
	))

(define H3-roots
  (let* ((omegamat (matrix-by-row-list (map up-structure->list H3-omegavects)))
	 (invmat (invert omegamat))
	 (unnorm-roots (map (lambda (col)
			      (apply up (map (lambda (row)
					       (matrix-ref invmat row col))
					     '(0 1 2))))
			    '(0 1 2))))
    (map (lambda (vector) (/ vector (sqrt (dot-product vector vector))))
	 unnorm-roots)))

#|
; These roots come out in the order 0---0-5-0
(pp H3-roots)
(#(-.85065080835204 2.3014357274088827e-16 .5257311121191336) 
 #(.5877852522924731 -.8090169943749475 0) 
 #(0 1. 0))
|#

(define H3-lengths
  (map (lambda (vector) (sqrt (dot-product vector vector)))
       H3-roots))

(define H3-matrix
  (make-cox-matrix-H 3)) ; At least this is elegant...

;; And, last but not least, the double-hack:

(define H4-roots 
  (let ((foo -.85065080835204)
	(bar .5257311121191336)
	(baz .5877852522924731)
	(quux -.8090169943749475))
    (let ((coord1-4 (/ -1/2 bar))
	  (coord1-1 (sqrt (- 1 (/ 1/4 (* bar bar))))))
      (list (vector coord1-1 0 0 coord1-4)
	    (vector 0 foo 0 bar)
	    (vector 0 baz quux 0)
	    (vector 0 0 1 0)))))

(define H4-lengths
  (map (lambda (vector) (sqrt (dot-product vector vector)))
       H4-roots))

(define H4-matrix
  (make-cox-matrix-H 4)) ; At least this is elegant...

