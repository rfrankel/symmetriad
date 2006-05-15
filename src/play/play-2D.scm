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

(load "load")

;; Example One: Group of type "A" dimension 2
;; type A groups are simplices (triangle, tetrahedron, etc)

(define A2-play (geom-family->cox-geometry A-family 2))

(define foo1 (make-symmetric-object A2-play (lambda (x y) (+ x (/ y 2)))))
(symo:draw foo1 (frame -2 2 -2 2))

(define foo2 (cartesian-point->symmetric-object A2-play (up 0 1)))
(symo:draw foo2 (frame -2 2 -2 2))

(pp (omega-dirs test))

(symo:draw (cartesian-point->symmetric-object 
	     A2-play 
	     (cadr (omega-dirs A2-play)))
	    (frame -2 2 -2 2))

(symo:draw (magic-spec->symmetric-object A2-play '(1 1))
	    (frame -2 2 -2 2))


;; Example Two: Group of type "B" dimension 2
;; type B groups are squares, cubes, etc

(define B2-system (geom-family->cox-geometry B-family 2))

(symo:draw (make-symmetric-object B2-system 
				   (lambda (x y) (+ x (/ y 1))))
	    (frame -2 2 -2 2))

(symo:draw (cartesian-point->symmetric-object B2-system (up 1 1))
	    (frame -2 2 -2 2))

(pp (omega-dirs B2-system))

;;; Example 5: The 2D polygons

(define pentagon-system (geom-family->cox-geometry (I-family 5) 2))
(define (point-proc x y) (+ x (* (cos (/ pi 5)) y)))
(define (point-proc x y) y) ; This does not lead to even-length edges
(symo:draw (make-symmetric-object pentagon-system point-proc) (frame -2 2 -2 2))

(define hexagon-system (geom-family->cox-geometry (I-family 6) 2))
(define (point-proc x y) (+ x (* (cos (/ pi 6)) y)))
(define (point-proc x y) y) ; This does not lead to even-length edges
(symo:draw (make-symmetric-object hexagon-system point-proc) (frame -2 2 -2 2))

(define heptagon-system (geom-family->cox-geometry (I-family 7) 2))
(define (point-proc x y) (+ x (* (cos (/ pi 7)) y)))
; Broken, probably because of bugs in angle handling
(symo:draw (make-symmetric-object heptagon-system point-proc) (frame -2 2 -2 2))

