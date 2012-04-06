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

;;; This file defines a structure for representing the symmetric
;;; objects that occur when one reflects a point through a coxeter 
;;; geometry.

(define-structure (symmetric-object
		   (constructor %create-symmetric-object
				(geometry point-proc point-table face-list
					  unique-vertices representative-map rep-index-map))
		   (conc-name symo/))
  (geometry #f read-only #t)
  (point-proc #f read-only #t)
  (point-table #f read-only #t) ;; Should be private, hash of symbol to up-structure
  (face-list '() read-only #t)
  (unique-vertices '() read-only #t) ; Symbols of vertices that are unique
  (representative-map #f read-only #t) ; Maps coset to symbol of representative
  (rep-index-map #f read-only #t) ; Maps unique vertex its index in unique-vertices list
  (vertex-group #f)
  )

(define (make-symmetric-object cox-g point-proc #!optional mild-testing)
  (if (default-object? mild-testing) (set! mild-testing #t))
  (define (hashtablify point-table)
    ((list->eq-hash-table car cadr) point-table))
  (let* ((point-assoc-table (tabulate-point cox-g point-proc))
	 (point-table (hashtablify point-assoc-table))
	 (clusters (pocl:cluster-points point-table 10e-10 0.01))
	 (unique-vertices (pocl/unique-list clusters))
	 (rep-map (pocl/rep-map clusters))
	 (rep-index-map (index-map unique-vertices))
	 (face-list (build-unique-face-list cox-g rep-map)))
    (pp "Components finished.")
    (let ((answer (%create-symmetric-object cox-g point-proc point-table face-list
					    unique-vertices rep-map rep-index-map)))
      (assert-verts-on-sphere answer)
      (assert-correct-face-vertex-incidence-stats answer)
      (if (not mild-testing) (assert-same-edge-lengths answer))
      answer)))

(define (build-face-list cox-g)
  (pp "- Building face list.")
  (let ((group-net (cxg/group-net cox-g))
	(done-cache (make-eq-hash-table))
	(chambers (cxg/chamber-list cox-g)))
    (for-each (lambda (chamber)
		(hash-table/put! done-cache chamber (make-eq-hash-table)))
	      chambers)
    (define (process-face chamber relation)
      (let ((my-done-cache (hash-table/get done-cache chamber #f)))
	(if (hash-table/get my-done-cache relation #f)
	    #f
	    (let ((face (gr:follow-relation 
			 relation
			 (lambda (elt gen) (gn:product group-net elt gen))
			 chamber)))
	      (for-each (lambda (face-chamber)
			  (hash-table/put! 
			   (hash-table/get done-cache face-chamber #f)
			   relation #t))
			face)
	      face))))
    (filter (lambda (x) x)
	    (flatten (map (lambda (chamber)
			    (map (lambda (relation)
				   (process-face chamber relation))
				 (cxg:relations cox-g)))
			  chambers)))))

;; Index proc takes a chamber symbol and returns its index 
;; in the list of chambers of the relevant coxeter geometry.
(define (normalize-face face representative-map index-proc)
  (let ((resymboled-face 
	 (lin-rem-dup-eq ; TODO Technically should be consecutive duplicates...
	  (map (lambda (chamber-symb)
		 (hash-table/get representative-map chamber-symb #f))
	       face))))
    (let ((min-index (apply min (map index-proc resymboled-face))))
      (rotate-to-front (lambda (symb)
                         (= min-index (index-proc symb)))
                       resymboled-face))))

(define (build-unique-face-list cox-g representative-map)
  (pp "Building list of unique faces.")
  (define cox-indexer (coset->index cox-g))
  (define (normalize face)
    (normalize-face face representative-map cox-indexer))
  (let ((face-list (build-face-list cox-g)))
    (pp "- Removing spurious faces.")
    (remove-duplicate-faces
     (filter (lambda (face) (> (length face) 2))
	     (map normalize face-list))
     normalize)))

(define (remove-duplicate-faces face-list normalizer)
  (pp "- Running duplicate face removal.")
  (let ((unique-faces (make-equal-hash-table)))
    (for-each 
     (lambda (face)
       (if (not (or (hash-table/get unique-faces face #f)
		    (hash-table/get unique-faces (normalizer (reverse face)) #f)))
	   (hash-table/put! unique-faces face #t)))
     face-list)
    (filter (lambda (face)
	      (if (hash-table/get unique-faces face #f)
		  (begin (hash-table/remove! unique-faces face)
			 face)
		  #f))
	    face-list)))

(define (symo:dimension sym-obj)
  (length (cxg/gen-list (symo/geometry sym-obj))))

(define (symo:representative sym-obj chamber-symb)
  (hash-table/get (symo/representative-map sym-obj)
		  chamber-symb #f))  

(define (symo:rep-index sym-obj chamber-symb)
  (hash-table/get (symo/rep-index-map sym-obj)
		  (symo:representative sym-obj chamber-symb)
		  #f))

(define (symo:get-vertex sym-obj chamber-symb)
  (hash-table/get 
   (symo/point-table sym-obj)
   (symo:representative sym-obj chamber-symb)
   #f))

(define (symo:face-stats sym-obj)
  (let ((answer (make-multi-set)))
    (for-each (lambda (face)
		(mset:add! answer (length face)))
	      (symo/face-list sym-obj))
    answer))

;; Contains duplicates
(define (symo:edge-list sym-obj)
  (map (lambda (cell) 
	 (list (car cell) (cdr cell)))
       (flatten (map conseq-pairs-list (symo/face-list sym-obj)))))

(define (symo:unique-edge-list sym-obj)
  ((lin-rem-dup equal-hash-mod equal?) 
   (map (lambda (edge)
	  (if (< (symo:rep-index sym-obj (car edge)) 
		 (symo:rep-index sym-obj (cadr edge)))
	      edge
	      (list (cadr edge) (car edge))))
	(symo:edge-list sym-obj))))

;; The point proc is a procedure capturing an 
;; expression for a point as a *linear* function of
;; the two (for two dimensions) roots of the appropriate 
;; chamber.
(define (tabulate-point cox-g point-proc)
  (map 
   (lambda (chamber)
     (list chamber
	   (apply point-proc 
		  (cxg:chamber-root-list cox-g chamber))))
   (cxg/chamber-list cox-g)))
