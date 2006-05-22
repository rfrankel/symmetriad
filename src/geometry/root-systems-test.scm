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

(assert-equal 
 (canonical-roots-A 3)
 (list #(1 -1 0 0) #(0 1 -1 0) #(0 0 1 -1)))

(assert-equal
 (canonical-roots-B 3)
 (list #(1 -1 0) #(0 1 -1) #(0 0 1)))

(assert-equal
 (canonical-roots-C 3)
 (list #(1 -1 0) #(0 1 -1) #(0 0 2)))

(assert-equal
 (canonical-roots-D 3)
 (list #(1 -1 0) #(0 1 -1) #(0 1 1)))

(assert-equal
 (canonical-roots-I2 5)
 (list #(1 0) #((*number* (expression (* -1/4 (+ 1 sqrt5)))) sinpi/5)))

(assert-equal
 (canonical-roots-I2 6)
 (list #(1 0) #((*number* (expression (* -1/2 sqrt3))) 1/2)))
