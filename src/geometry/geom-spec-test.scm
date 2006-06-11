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

(let ((B2xB2-spec (gspec:cross-product
		   (geom-family->geom-spec B-family 2)
		   (geom-family->geom-spec B-family 2))))
  (assert-equal
   (cm:matrix (gspec/matrix B2xB2-spec))
   (matrix-by-rows (list 1 4 2 2)
		   (list 4 1 2 2)
		   (list 2 2 1 4)
		   (list 2 2 4 1)))
  (assert-equal
   (gspec/lengths B2xB2-spec)
   (list 'sqrt2 1 'sqrt2 1))
  (assert-equal
   (gspec/roots B2xB2-spec)
   '(#(1 -1 0 0) #(0 1 0 0) #(0 0 1 -1) #(0 0 0 1))))

