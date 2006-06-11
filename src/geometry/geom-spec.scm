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

;; A specification for a geometry.  Contains a coxeter-matrix, a set
;; of roots for it, and a set of lengths for those roots.
(define-structure (geom-spec
		   (constructor %create-geom-spec (matrix lengths roots))
		   (conc-name gspec/))
  (matrix #f read-only #t)
  (lengths #f read-only #t)
  (roots #f read-only #t))

(define (make-geom-spec matrix lengths roots)
  (assert-roots-match matrix lengths roots)
  (%create-geom-spec matrix lengths roots))
