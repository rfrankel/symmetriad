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

;;; This is a file that defines a structure to store 
;;; the attributes of a projection
;;; and creates an associated projection matrix.
;;; This follows the construction in Foley and Van Dam

;; The default specifies a 3d orthonormal projection
(define-structure (projection
		   (print-procedure 
		    (standard-unparser-method 
		     'PROJECTION
		     (lambda (pj port)
		       (write-string " " port)
		       (write `(named ,(proj/name pj)) port))))
		   (constructor %projection
				(name dimension))
		   (conc-name proj/))		
  (name #f read-only #t)
  (dimension #f read-only #t)
  (zp 0)
  (cap-q 'infinity)
  (dw 'n/a)
  (dx 0)
  (dy 0)
  (dz -1)
  (viewing-transform (matrix-by-rows (list 1 0 0 0)
				     (list 0 1 0 0)
				     (list 0 0 0 0)
				     (list 0 0 0 1))))

;; For transpose; I'm not sure which way it goes
(define (make-projection-T name dim zp cap-q dx dy dz . dw)
  (if (not (eq? cap-q 'infinity))
      (assert (not (= cap-q 0))
	      "MAKE-PROJECTION: divide by zero"))
  (let ((proj (%projection name dim))
	(cap-q-inv (if (eq? cap-q 'infinity)
		   0
		   (/ 1 cap-q)))
	(dz-inv (/ 1 dz)))
    (set-proj/zp! proj zp)
    (set-proj/cap-q! proj cap-q)
    (set-proj/dx! proj dx)
    (set-proj/dy! proj dy)
    (set-proj/dz! proj dz)
    (set-proj/viewing-transform! 
     proj 
     (down
      (down 1 0 (* -1 dx dz-inv) (* zp dx dz-inv))
      (down 0 1 (* -1 dy dz-inv) (* zp dy dz-inv))
      (down 0 0 (* -1 zp cap-q-inv dz-inv) (+ (* zp zp cap-q-inv dz-inv) zp))
      (down 0 0 (* -1 cap-q-inv dz-inv) (+ (* zp cap-q-inv dz-inv) 1))))
    proj))
 
(define (make-projection name dim zp cap-q dx dy dz . dw)
  (if (not (eq? cap-q 'infinity))
      (assert (not (= cap-q 0))
	      "MAKE-PROJECTION: divide by zero"))
  (let ((proj (%projection name dim))
	(cap-q-inv (if (eq? cap-q 'infinity)
		   0
		   (/ 1 cap-q)))
	(dz-inv (/ 1 dz)))
    (set-proj/zp! proj zp)
    (set-proj/cap-q! proj cap-q)
    (set-proj/dx! proj dx)
    (set-proj/dy! proj dy)
    (set-proj/dz! proj dz)
    (set-proj/viewing-transform! 
     proj 
     (down
      (down 1 0 0 0)
      (down 0 1 0 0)
      (down (* -1 dx dz-inv) (* -1 dy dz-inv)
	    (* -1 zp cap-q-inv dz-inv) (* -1 cap-q-inv dz-inv))
      (down (* zp dx dz-inv) (* zp dy dz-inv)
	     (+ (* zp zp cap-q-inv dz-inv) zp) (+ (* zp cap-q-inv dz-inv) 1))))
    proj))

(define (make-simple-orthogonal name dim)
  (if (= dim 3)
      (make-projection name 3 0 'infinity 0 0 -1)
      (display "non-three-dimensional projections not yet implemented -- Sorry")))

;; This projects on a plane at z=d from the origin
(define (make-simple-perspective name dim d)
  (if (= dim 3)
      (make-projection name 3 d d 0 0 -1)
      (display "non-three-dimensional projections not yet implemented -- Sorry")))

(define (make-z-perspective name dim zp cap-q)
  (if (= dim 3)
      (make-projection name 3 zp cap-q 0 0 -1)
      (display "non-three-dimensional projections not yet implemented -- Sorry")))

;; General -- specify desired x and y of vanishing point
(define (make-gen-perspective name dim zp cap-q van-x van-y)
  (assert (not (or (eq? cap-q 'infinity) (= cap-q 0))) "Improper cap-q")
  (let ((dx (/ van-x cap-q))
	(dy (/ van-y cap-q)))
    (assert (< 0 (- 1 (* dx dx) (* dy dy))) "improper vanishing point")
    (let ((dz (sqrt (- 1 (* dx dx) (* dy dy)))))
      (if (= dim 3)
	  (make-projection name 3 zp cap-q dx dy dz)
	  (display "non-three-dimensional projections not yet implemented -- Sorry")))))

(define (make-cavalier name dim alpha)
  (let ((sina (sin alpha))
	(cosa (cos alpha)))
    (if (= dim 3)
	(make-projection name 3 0 'infinity cosa sina -1)
	(display "non-three-dimensional projections not yet implemented -- Sorry"))))

(define (make-cabinet name dim alpha)
  (let ((sina (/ (sin alpha) 2))
	(cosa (/ (cos alpha) 2)))
    (if (= dim 3)
	(make-projection name 3 0 'infinity cosa sina -1)
	(display "non-three-dimensional projections not yet implemented -- Sorry"))))

	      



