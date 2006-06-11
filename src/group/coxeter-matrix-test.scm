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

(assert-true (coxeter-matrix? (make-cox-matrix-A 3)))

(assert-equal
 (cm:matrix (make-cox-matrix-A 3))
 (matrix-by-rows (list 1 3 2)
		 (list 3 1 3)
		 (list 2 3 1)))

(assert-true (coxeter-matrix? (make-cox-matrix-A 4)))

(assert-equal
 (cm:matrix (make-cox-matrix-A 4))
 (matrix-by-rows (list 1 3 2 2)
		 (list 3 1 3 2)
		 (list 2 3 1 3)
		 (list 2 2 3 1)))

(assert-true (coxeter-matrix? (make-cox-matrix-B/C 4)))

(assert-equal
 (cm:matrix (make-cox-matrix-B/C 4))
 (matrix-by-rows (list 1 3 2 2)
		 (list 3 1 3 2)
		 (list 2 3 1 4)
		 (list 2 2 4 1)))

(assert-true (coxeter-matrix? (make-cox-matrix-B/C 3)))

(assert-equal
 (cm:matrix (make-cox-matrix-B/C 3))
 (matrix-by-rows (list 1 3 2)
		 (list 3 1 4)
		 (list 2 4 1)))

(assert-true (coxeter-matrix? (make-cox-matrix-D 2)))

(assert-equal
 (cm:matrix (make-cox-matrix-D 2))
 (matrix-by-rows (list 1 2)
		 (list 2 1)))

(assert-true (coxeter-matrix? (make-cox-matrix-D 3)))

(assert-equal
 (cm:matrix (make-cox-matrix-D 3))
 (matrix-by-rows (list 1 3 3)
		 (list 3 1 2)
		 (list 3 2 1)))

(assert-true (coxeter-matrix? (make-cox-matrix-D 4)))

(assert-equal
 (cm:matrix (make-cox-matrix-D 4))
 (matrix-by-rows (list 1 3 2 2)
		 (list 3 1 3 3)
		 (list 2 3 1 2)
		 (list 2 3 2 1)))

(assert-true (coxeter-matrix? (make-cox-matrix-F 4)))

(assert-equal
 (cm:matrix (make-cox-matrix-F 4))
 (matrix-by-rows (list 1 3 2 2)
		 (list 3 1 4 2)
		 (list 2 4 1 3)
		 (list 2 2 3 1)))

(assert-true (coxeter-matrix? (make-cox-matrix-H 3)))

(assert-equal
 (cm:matrix (make-cox-matrix-H 3))
 (matrix-by-rows (list 1 3 2)
		 (list 3 1 5)
		 (list 2 5 1)))

(assert-true (coxeter-matrix? (make-cox-matrix-H 4)))

(assert-equal
 (cm:matrix (make-cox-matrix-H 4))
 (matrix-by-rows (list 1 3 2 2)
		 (list 3 1 3 2)
		 (list 2 3 1 5)
		 (list 2 2 5 1)))

(assert-true (coxeter-matrix? (make-cox-matrix-I2 6)))

(assert-equal
 (cm:matrix (make-cox-matrix-I2 6))
 (matrix-by-rows (list 1 6)
		 (list 6 1)))

(let ((product (cm:cross-product (make-cox-matrix-A 2) (make-cox-matrix-A 2))))
  (assert-true (coxeter-matrix? product))
  (assert-equal
   (cm:matrix product)
   (matrix-by-rows (list 1 3 2 2)
		   (list 3 1 2 2)
		   (list 2 2 1 3)
		   (list 2 2 3 1))))
