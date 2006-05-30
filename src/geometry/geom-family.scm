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

;;; This file provides definitions of objects that capture geometry family specs.
;;; Each member of such a structure is a procedure that, when handed a dimension, 
;;; returns an appropriate piece of the spec for a coxeter system, in the right 
;;; dimension

(define-structure (geom-family
		   (constructor %create-geom-family (matrix len roots))
		   (conc-name gfam/))
  (matrix #f read-only #t)
  (len #f read-only #t)
  (roots #f read-only #t)
  (done-cache (make-eq-hash-table) read-only #t))

(define A-family
  (%create-geom-family 
   make-cox-matrix-A 
   make-cox-len-A 
   (lambda (dimension)
     (cond ((= dimension 2) (simple-roots-A2))
	   ((= dimension 3) (hacked-roots-A3))
	   ((= dimension 4) (hacked-roots-A4))
	   (else (error (string-append 
			 "A-family does not currently support dimension "
			 dimension ".")))))))

(define B-family 
  (%create-geom-family make-cox-matrix-B/C canonical-len-B canonical-roots-B))

(define C-family 
  (%create-geom-family make-cox-matrix-B/C canonical-len-C canonical-roots-C))

(define D-family 
  (%create-geom-family make-cox-matrix-D canonical-len-D canonical-roots-D))

(define F-family 
  (%create-geom-family make-cox-matrix-F canonical-len-F canonical-roots-F))

(define H-family
  (%create-geom-family
   make-cox-matrix-H
   (lambda (dimension)
     (make-list dimension 1))
   (lambda (dimension)
     (cond ((= dimension 3) 
	    H3-roots)
	   ((= dimension 4) 
	    H4-roots)
	   (else
	    (error "H-family only supports dimensions 3 or 4."))))
   ))

(define (I-family number)
  (%create-geom-family
   (lambda (dimension)
     (if (= dimension 2) 
	 (make-cox-matrix-I2 number)
	 (error "I-family only supports dimension 2.")))
   (lambda (dimension)
     (if (= dimension 2) 
	 (make-cox-len-I2)
	 (error "I-family only supports dimension 2.")))
   (lambda (dimension)
     (if (= dimension 2) 
	 (canonical-roots-I2 number)
	 (error "I-family only supports dimension 2.")))
   ))

(define (gfam:clear-cache! geom-family-or-list)
  (cond ((geom-family? geom-family-or-list)
	 (hash-table/clear! (gfam/done-cache geom-family-or-list)))
	((list? geom-family-or-list)
	 (map gfam:clear-cache! geom-family-or-list))
	(else
	 (error "Argument has incorrect type, expected a geom-family or list structure of them:"
		geom-family-or-list)))
  'done)

(define (gfam:all-families)
  ; The constant I-family is not exactly a family, in that it is a
  ; parametrized family of families, and thus is not homogenous with
  ; the others.
  (list A-family B-family C-family D-family F-family H-family))
