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

;;; This file just contains random playing around with 
;;; the procedures defined in drawing.scm (and other files).
;;; Play with specific Coxeter groups is relegated to the play-* 
;;; files.

(load "load")

;; An experiment with a random Coxeter diagram

(define cox-matrix
  (%create-coxeter-matrix
   (matrix-by-row-list (list (list 1 3 4)
			     (list 3 1 3)
			     (list 4 3 1)))))
(pp cox-matrix)
(define cox-lengths '(1 1 1))

(define coxsub (make-group-presentation '(s0) '((s0 s0))))
(define cox-pres (cox-presentation cox-matrix cox-lengths))
(define cox-group-net (group-network cox-pres coxsub "Autogen"))

; Dies with strange errors without informing me as to whether 
; this group ends up being infinite.
(gn:hlt! cox-group-net)

(define (append-zeroes vect n)
  (vector-append vect (make-vector n 0)))

(define (prepend-zeroes vect n)
  (vector-append (make-vector n 0) vect))

(define (root-cross-product root-list1 root-list2)
  (cond ((null? root-list1) root-list2)
	((null? root-list2) root-list1)
	(else
	 (let ((dim1 (vector-length (car root-list1)))
	       (dim2 (vector-length (car root-list2))))
	   (append (map (lambda (vect) (append-zeroes vect dim2)) root-list1)
		   (map (lambda (vect) (prepend-zeroes vect dim1)) root-list2))))))

;(root-cross-product '(#(1)) '(#(1))))
;Value: (#(1 0) #(0 1))
;(root-cross-product (canonical-roots-I2 5) (canonical-roots-I2 5))
;Value: (#(1 0 0 0) #((*number* (expression (* -1/4 (+ 1 sqrt5)))) sinpi/5 0 0) #(0 0 1 0) #(0 0 (*number* (expression (* -1/4 (+ 1 sqrt5)))) sinpi/5))

(define length-cross-product append)

(define (coxeter-matrix-cross-product cm1 cm2)
  (let ((dim1 (cm:dimension cm1))
	(dim2 (cm:dimension cm2)))
    (make-coxeter-matrix
     (+ dim1 dim2)
     (lambda (i j)
       (cond ((and (< i dim1) (< j dim1))
	      (cm:matrix-ref cm1 i j))
	     ((and (>= i dim1) (>= j dim1))
	      (cm:matrix-ref cm2 (- i dim1) (- j dim1)))
	     (else 2))))))

;(pp (coxeter-matrix-cross-product (make-cox-matrix-A 2) (make-cox-matrix-A 2)))
;#[coxeter-matrix 18]
;(matrix (*matrix* (4 . 4) #(#(1 3 2 2) #(3 1 2 2) #(2 2 1 3) #(2 2 3 1))))

;; Reducible group 0-5-0   0-5-0
(let* ((cox-matrix-H2H2
	(%create-coxeter-matrix
	 (matrix-by-row-list '((1 5 2 2)
			       (5 1 2 2)
			       (2 2 1 5)
			       (2 2 5 1)))))
       (cox-roots-H2H2
	(root-cross-product (canonical-roots-I2 5) (canonical-roots-I2 5)))
       (cox-len-H2H2 (length-cross-product (make-cox-len-I2) (make-cox-len-I2)))
       (H2H2-system
	(build-cox-geometry cox-matrix-H2H2 cox-len-H2H2 cox-roots-H2H2)))
  (assert-equal 100 (gn:num-live-cosets (cxg/group-net H2H2-system)))
  (assert-equal 45 (length (gn:dead-cosets (cxg/group-net H2H2-system))))
  (let* ((H2H2-full (magic-spec->symmetric-object H2H2-system '(1 1 1 1))))
    (symo:file-print-gv 
     H2H2-full "playout/H2H2-full.off" 'off-conformal
     (highlight-multigroup-cosets
      H2H2-full
      `(((s0 s1 s2) . ,*purple*) ((s1 s2 s3) . ,*green*))))))

(canonical-roots-I2 7)

;; Reducible group 0-5-0   0-7-0
(let* ((cox-matrix-prism
	(%create-coxeter-matrix
	 (matrix-by-row-list '((1 5 2 2)
			       (5 1 2 2)
			       (2 2 1 7)
			       (2 2 7 1)))))
       (cox-roots-prism
	(root-cross-product (canonical-roots-I2 5) (canonical-roots-I2 7)))
       (cox-len-prism (length-cross-product (make-cox-len-I2) (make-cox-len-I2)))
       (prism-system
	(build-cox-geometry cox-matrix-prism cox-len-prism cox-roots-prism)))
  (assert-equal 140 (gn:num-live-cosets (cxg/group-net prism-system)))
  (assert-equal 73 (length (gn:dead-cosets (cxg/group-net prism-system))))
  (let* ((prism-full (magic-spec->symmetric-object prism-system '(1 1 1 1))))
    (symo:file-print-gv 
     prism-full "playout/prism-full.off" 'off-conformal
     (highlight-multigroup-cosets
      prism-full
      `(((s0 s1 s2) . ,*purple*) ((s1 s2 s3) . ,*blue*))))))
