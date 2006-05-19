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

;; This file defines a data structure for detecting clusters among 
;; points in n-space, assuming the following property: 
;; There exist constants c1 and c2 such that c2 >> c1 and for any points x, y, either 
;; d(x, y) < c1 or d(x, y) > c2.


(define-structure (point-clusterer
		   (constructor %create-point-clusterer
				(dimensionality max-near-dist min-far-dist cube-edge-size
						point-hash rep-map unique-list unique-coords))
		   (conc-name pocl/))
  (dimensionality #f read-only #t)
  (max-near-dist #f read-only #t)
  (min-far-dist #f read-only #t)
  (cube-edge-size #f read-only #t)
  (point-hash #f read-only #t)
  (rep-map #f read-only #t)
  (unique-list '())
  (unique-coords #f read-only #t)
  )
  
(define (make-point-clusterer dimensionality max-near-dist min-far-dist)
  (if (> (* 2 max-near-dist) (/ min-far-dist (* (sqrt dimensionality) 2)))
      (error "Insufficient clustering assumed by (max-near-dist min-far-dist dimensionality):"
	     (list max-near-sit min-far-dist dimensionality)))
  (%create-point-clusterer
   dimensionality
   max-near-dist
   min-far-dist
   (/ min-far-dist (* (sqrt dimensionality) 2))
   (make-equal-hash-table)
   (make-eq-hash-table)
   '()
   (make-eq-hash-table)
   ))

(define (pocl:add! clusterer point-symb point)
  (let* ((point-coords (if (up? point) (up-structure->list point) point))
	 (near-cube-specs (pocl-int:near-cube-specs clusterer point-coords))
	 (near-cube-reps 
	  (lin-rem-dup-eq
	   (filter (lambda (x) x) 
		   (map (lambda (spec) (hash-table/get (pocl/point-hash clusterer) spec #f))
			near-cube-specs)))))
    (if (not (= (pocl/dimensionality clusterer) (length point-coords)))
	(error "Point has the wrong dimensionality." point-list))
    (cond ((> (length near-cube-reps) 1)
	   (error "Multiple cubes near this point have representatives." (cons point-symb point-up)))
	  ((= (length near-cube-reps) 1)
	   (pocl-int:assert-point-near clusterer (car near-cube-reps) point-symb point-coords)
	   (hash-table/put! (pocl/rep-map clusterer) point-symb (car near-cube-reps)))
	  (else
	   (hash-table/put! (pocl/rep-map clusterer) point-symb point-symb)
	   (for-each (lambda (spec)
		       (hash-table/put! (pocl/point-hash clusterer) spec point-symb))
		     near-cube-specs)
	   (set-pocl/unique-list! clusterer (cons point-symb (pocl/unique-list clusterer)))
	   (hash-table/put! (pocl/unique-coords clusterer) point-symb point-coords))
	  )))

(define (pocl-int:cube-spec clusterer point-list)
  (map (lambda (coord)
	 (floor->exact (/ coord (pocl/cube-edge-size clusterer))))
       point-list))

; Quadratic in the number of cube specs returned.
; Might return at most 2^dimensionality cube specs (for a point in the very corner)
(define (pocl-int:near-cube-specs clusterer point-list)
  (define (prepend-all item list-of-lists)
    (map (lambda (lst) (cons item lst)) list-of-lists))
  (let ((max-near-dist (pocl/max-near-dist clusterer))
	(min-far-dist (pocl/min-far-dist clusterer))
	(cube-edge-size (pocl/cube-edge-size clusterer)))
    (let loop ((cube-spec-left (pocl-int:cube-spec clusterer point-list))
	       (point-list-left point-list))
      (cond ((and (null? cube-spec-left) (null? point-list-left))
	     '(()))
	    ((or (null? cube-spec-left) (null? point-list-left))
	     (error "Impossible: Point list not the same length as cube-spec"))
	    (else
	     (let ((close-low (> max-near-dist (- (car point-list-left) (* cube-edge-size 
									   (car cube-spec-left)))))
		   (close-high (> max-near-dist (- (* cube-edge-size (+ 1 (car cube-spec-left))) 
						   (car point-list-left))))
		   (recur (loop (cdr cube-spec-left)
				(cdr point-list-left))))
	       (cond ((and close-high close-low)
		      (error "Inexplicable internal error."))
		     (close-low
		      (append (prepend-all (car cube-spec-left) recur)
			      (prepend-all (- (car cube-spec-left) 1) recur)))
		     (close-high
		      (append (prepend-all (car cube-spec-left) recur)
			      (prepend-all (+ (car cube-spec-left) 1) recur)))
		     (else (prepend-all (car cube-spec-left) recur)))))))))

(define (pocl-int:assert-point-near clusterer ref pt pt-coords)
  (let* ((ref-coords (hash-table/get (pocl/unique-coords clusterer) ref #f)))
    (if (not ref-coords)
	(error "Internal error: Representative without coordinates." ref))
    (if (not (pocl-int:points-near? clusterer ref-coords pt-coords))
	(if (pocl-int:points-far? clusterer ref-coords pt-coords)
	    (error "Internal error: Far points in near cubes."
		   (list pt pt-coords ref ref-coords))
	    (error "Assumption violation: Points neither near nor far."
		   (list pt pt-coords ref ref-coords))))))

(define (pocl-int:points-near? clusterer p1-coords p2-coords)
  (reduce (lambda (a b) (and a b)) #t 
	  (map (lambda (c1 c2) (> (pocl/max-near-dist clusterer) (abs (- c1 c2))))
	       p1-coords p2-coords)))

(define (pocl-int:points-far? clusterer p1-coords p2-coords)
  (< (pocl/min-far-dist clusterer)
     (sqrt (reduce + 0 (map (lambda (c1 c2) (* (- c1 c2) (- c1 c2)))
			    p1-coords p2-coords)))))


; Returns a point clusterer representing the clustering of the given points
(define (pocl:cluster-points point-table max-near-dist min-far-dist)
  (let* ((pt-symb-list (hash-table/key-list point-table))
	 (first-point (hash-table/get point-table (car pt-symb-list) #f))
	 (dimensionality (if (up? first-point) (length (up-structure->list first-point))
			     (length first-point)))
	 (clusterer (make-point-clusterer dimensionality max-near-dist min-far-dist)))
    (for-each (lambda (pt-symb)
		(pocl:add! clusterer pt-symb (hash-table/get point-table pt-symb #f)))
	      pt-symb-list)
    clusterer))
