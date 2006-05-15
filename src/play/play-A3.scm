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

;; Example Three: Group of type "A" dimension 3

(define A3-system (geom-family->cox-geometry A-family 3))

(define (point-proc x y z)
  x)

(define (point-proc x y z)
  (+ (* 2 x) y))

(define (point-proc x y z)
  (+ y (* 1/2 x) (* 1/2 z)))

(symo:draw (make-symmetric-object A3-system point-proc #t)
	   (frame -2 2 -2 2) cab)

; These are the midpoints of the sides of this tetrahedron
(symo:draw (cartesian-point->symmetric-object A3-system (up 0 1 0)) (frame -2 2 -2 2) cab)

(symo:draw (cartesian-point->symmetric-object A3-system (up 0.2 1 .1) #t) 
	   (frame -2 2 -2 2) cab)

(symo:file-print-gv-skel (cartesian-point->symmetric-object A3-system (up 0.2 1 .1) #t) 
			 "playout/A3.skel" (merge-by-first (list) '(1.0 1.0 1.0 1.0)))

; Something strange
(symo:draw (cartesian-point->symmetric-object A3-system (up 0 1 -.47) #t) 
	   (frame -2 2 -2 2) cab)

(symo:draw (cartesian-point->symmetric-object A3-system (up 0 1 1) #t) 
	   (frame -2 2 -2 2) cab)

; This shows the tetrahedron
(let ((win (frame -2 2 -2 2)))
  (symo:draw (cartesian-point->symmetric-object A3-system (up 0 1 (sqrt 2))) 
	     win cab)
  (add-three-d-axes win cab))

; Here's a point outside the chamber of the identity
(let ((win (frame -2 2 -2 2)))
  (symo:draw (cartesian-point->symmetric-object A3-system (up (sqrt 2) 0 1) #t) 
	     win cab)
  (add-three-d-axes win cab)
  )

(pp (omega-dirs A3-system))

(symo:draw (magic-spec->symmetric-object A3-system '(1 0 0)) (frame -2 2 -2 2) cab)
(symo:draw (magic-spec->symmetric-object A3-system '(0 1 0)) (frame -2 2 -2 2) cab)
(symo:draw (magic-spec->symmetric-object A3-system '(0 0 1)) (frame -2 2 -2 2) cab)

(let ((tetrahedron (magic-spec->symmetric-object A3-system '(1 0 0))))
  (with-output-to-file-ensuring-path
      "playout/tetra.wrl" 
    (lambda ()
      (symo:print-vrml tetrahedron))))

