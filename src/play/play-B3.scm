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

(load "graphics/drawing")

;; Example Four: Group of type "B" dimension 3

(define B3-play (geom-family->cox-geometry B-family 3))		      

; The cube
(symo:draw (cartesian-point->symmetric-object B3-play (up 1 1 1)) 
	   (frame -2 2 -2 2) cab)

; The cuboctahedron
(symo:draw (cartesian-point->symmetric-object B3-play (up 1 1 0)) 
	   (frame -2 2 -2 2) cab)

; The octahedron
(symo:draw (cartesian-point->symmetric-object B3-play (up 1 0 0)) 
	   (frame -2 2 -2 2) cab)

; Another vaguely regular thing
; Small Rhombicuboctahedron?
(symo:draw (cartesian-point->symmetric-object B3-play (up (* .5 (+ 1 (sqrt 2))) .5 .5)) 
	   (frame -2 2 -2 2) cab)

(symo:draw (cartesian-point->symmetric-object B3-play (up -1.5 .5 .5) #t) 
	   (frame -2 2 -2 2) cab)

(symo:file-print-vrml 
 (cartesian-point->symmetric-object 
  B3-play
  (up (* .5 (+ 1 (sqrt 2))) .5 .5))
 "playout/fooz.wrl")
	    
(symo:draw (symmetric-object B-family '(6 0 1))
	   (frame -2 2 -2 2) cab)

(symo:draw (symmetric-object H-family '(6 0 1))
	   (frame -2 2 -2 2) cab)

