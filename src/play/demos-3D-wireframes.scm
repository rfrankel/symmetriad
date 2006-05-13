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

;; This file contains pictures of cool things in 3D.
; Attend ye to the ratios.  They are very interesting, and desreve to 
; be (symbolically!) computed instead of being given by hand.

(load "graphics/drawing.scm")

;; Tetrahedral (A3) symmetry:

(define A3-demo-system (geom-family->cox-geometry A-family 3))

(pp (omega-dirs A3-demo-system))

; Tetrahedron (Platonic)
(symo:draw (magic-spec->symmetric-object A3-demo-system '(1 0 0))
	   (frame -2 2 -2 2) cab)
(symo:draw (magic-spec->symmetric-object A3-demo-system '(0 0 1))
	   (frame -2 2 -2 2) cab)

; Octahedron (Platonic, repeated)
(symo:draw (magic-spec->symmetric-object A3-demo-system '(0 1 0))
	   (frame -2 2 -2 2) cab)

; Truncated tetrahedron (Archimedean)
(symo:draw (magic-spec->symmetric-object A3-demo-system '(1 1 0))
	   (frame -2 2 -2 2) cab)


;; Cubic (B3) symmetry:

(define B3-demo-system (geom-family->cox-geometry B-family 3))

; The cube (Platonic)
(symo:draw (magic-spec->symmetric-object B3-demo-system '(0 0 1))
	   (frame -2 2 -2 2) cab)

; The cuboctahedron (Archimedean)
(symo:draw (magic-spec->symmetric-object B3-demo-system '(0 1 0))
	   (frame -2 2 -2 2) cab)
(symo:draw (magic-spec->symmetric-object A3-demo-system '(1 0 1))
	   (frame -2 2 -2 2) cab)

; The octahedron (Platonic)
(symo:draw (magic-spec->symmetric-object B3-demo-system '(1 0 0))
	   (frame -2 2 -2 2) cab)

; The truncated cube (Archimedean)
(symo:draw (magic-spec->symmetric-object B3-demo-system '(0 1 1))
	   (frame -2 2 -2 2) cab)

; The truncated ocahedron (Archimedean)
(symo:draw (magic-spec->symmetric-object B3-demo-system '(1 1 0))
	   (frame -2 2 -2 2) cab)

; The small rhombicuboctahedron (Archimedean)
(symo:draw (magic-spec->symmetric-object B3-demo-system '(1 0 1))
	   (frame -2 2 -2 2) cab)

; The great rhombicuboctahedron (Archimedean)
(symo:draw (magic-spec->symmetric-object B3-demo-system '(1 1 1))
	   (frame -2 2 -2 2) cab)
