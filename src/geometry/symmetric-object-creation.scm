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

;;; This file contains procedures for creating symmetric objects from
;;; user data.

;; Returns a list of the directions of the omega vectors of the
;; identity chamber of the given cox-geometry.  They are given in an
;; order that corresponds to the order in which the roots are stored
;; in the cox-geometry root table.  They are just directions, their
;; lengths are unspecified!
(define (omega-dirs cox-geo)
  (let* ((e-root-list (cxg:chamber-root-list cox-geo 'e))
	 (root-matrix (matrix-by-row-list
                       (map up-structure->list e-root-list)))
	 (root-mat-inv (invert root-matrix))) ; Will not fail, b/c roots form a basis.
    (map (lambda (col)
	   (apply up (map (lambda (row)
                            (matrix-ref root-mat-inv row col))
			  (enumerate-interval
                           0 (- (m:num-rows root-mat-inv) 1)))))
	 (enumerate-interval 0 (- (m:num-cols root-mat-inv) 1)))))
  
;; Relengths a set of omega directions so that the one given as the
;; base is length 1, and acts like the (only) vertex of a solid.  The
;; rest are lengthed such that they lie on the surface of that solid.
;; I hope.  Again, purely numeric.
(define (omega-vectors directions base-index)
  (let ((normalized 
	 (map (lambda (vector) 
		(vector/scalar
                 vector (sqrt (dot-product vector vector))))
	      directions)))
    (map (lambda (vector) 
	   (vector*scalar
            vector 
            (dot-product vector (list-ref normalized base-index))))
	 normalized)))

;; Returns the magic direction corresponding to the given spec.  The
;; spec needs to be a list parallel to the roots of the geometry.
;; Each number in the spec is the dot product that the point needs to
;; have with the corresponding (normalized) root.
;; Currently only numeric.
;; Returns a direction --- length unspecified.
(define ((magic-point cox-geo) root-in-spec)
  (assert (= (length root-in-spec) 
	     (length (cxg/gen-list cox-geo))) 
	  "Spec wrong length.")
  (define (normalize vect)
    (vector/scalar vect (sqrt (dot-product vect vect))))
  (let* ((e-root-list (cxg:chamber-root-list cox-geo 'e))
	 (norm-root-list (map normalize e-root-list))
	 (root-matrix (matrix-by-row-list
                       (map up-structure->list norm-root-list)))
	 (root-mat-inv (invert root-matrix)) ; Will not fail, b/c roots form a basis.
	 (targ-vec (list->up-structure root-in-spec)))
    (matrix*vector root-mat-inv targ-vec)))

(define ((normal-magic-point cox-geo) root-in-spec)
  (let ((magic ((magic-point cox-geo) root-in-spec)))
    (vector/scalar magic (sqrt (dot-product magic magic)))))
  
;; Returns a procedure suitable for tabulate-point
(define ((cartesian-point cox-geo) pt-vec)
  (let* ((e-root-list (cxg:chamber-root-list cox-geo 'e))
	 (root-matrix (matrix-by-col-list
                       (map up-structure->list e-root-list)))
	 (root-mat-inv (invert root-matrix)) ; Will not fail, b/c roots form a basis
	 (coords-rel-roots (matrix*vector root-mat-inv pt-vec)))
    (lambda (#!rest roots-list)
      ;(pp root-mat-inv) (newline)
      ;(pp coords-rel-roots) (newline)
      (let* ((given-matrix 
	      (matrix-by-col-list
               (map up-structure->list roots-list))))
	;(pp given-matrix) (newline)
	(matrix*vector given-matrix coords-rel-roots)))))

(define (cartesian-point->symmetric-object cox-g point
                                           #!optional mild-testing)
  (make-symmetric-object
   cox-g ((cartesian-point cox-g) point) mild-testing))

(define (compute-magic-spec->symmetric-object cox-g magic-spec)
  (let ((mild-testing #f))
    (for-each (lambda (spec-item)
		(if (not (or (= spec-item 1) (= spec-item 0)))
		    (set! mild-testing #t)))
	      magic-spec)
;    (pp (list "Testing mildly:" mild-testing))
    (let ((answer
           (cartesian-point->symmetric-object 
            cox-g 
            ((normal-magic-point cox-g) magic-spec) mild-testing))
	  (vertex-group (coxg-gen-subgroup cox-g magic-spec)))
      (set-symo/vertex-group! answer vertex-group)
      (if *check-correct-collapse?*
	  (assert-collapse-matches-vertex-group answer))
      answer)))

(define magic-spec->symmetric-object 
  (cache-wrapper
   compute-magic-spec->symmetric-object cxg/done-cache))

(define (symmetric-object family spec)
  (magic-spec->symmetric-object 
   (geom-family->cox-geometry family (length spec)) spec))
