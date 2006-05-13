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

;; This file defines the tools needed to associate arbitrary data to
;; sets of vertices of symmetric objects (usually edges or faces).  An
;; association of data to a symmetric object is represented as a
;; procedure that takes a set of vertices thereof and returns a data
;; object for it.  The procedure is expected to already know what
;; symmetric object it is associating data with.

(declare (usual-integrations))

(load "graphics/object-creation")
(load "group/subgroup")

(define *data:pass* (string->uninterned-symbol "Passive data object"))

(define (data:passive? data)
  (or (not data) (eq? data *data:pass*)))

(define (data:default) *data:pass*)

;; Associated the given data object with every set of vertices that
;; intersects the given one.  If supplied, associates the given
;; default data object with all other sets.
(define (data:associate-touching sym-obj vert-list data #!optional default-data) 
  (if (default-object? default-data) (set! default-data (data:default)))
  (let ((vert-set (list->eq-hash-set 
		   (map (lambda (vert) (symo:rep-index sym-obj vert)) vert-list))))
    (lambda (given-verts)
      ;(pp given-verts)
      (if (reduce 
	   (lambda (a b) (or a b))
	   #f
	   (map (lambda (vert)
		  (hash-table/get vert-set (symo:rep-index sym-obj vert) #f))
		given-verts))
	  data
	  default-data))))

;; Associated the given data object with every set of vertices
;; contained in the given one.  If supplied, associates the given
;; default data object with all other sets.
(define (data:associate-contained sym-obj vert-list data #!optional default-data) 
  (if (default-object? default-data) (set! default-data (data:default)))
  (let ((vert-set (list->eq-hash-set 
		   (map (lambda (vert) (symo:rep-index sym-obj vert)) vert-list))))
    (lambda (given-verts)
      ;(pp given-verts)
      (if (reduce 
	   (lambda (a b) (and a b))
	   #t
	   (map (lambda (vert)
		  (hash-table/get vert-set (symo:rep-index sym-obj vert) #f))
		given-verts))
	  data
	  default-data))))

;; Associated the given data object with every set of vertices that is
;; contained in one of the given ones.  If supplied, associates the
;; given default data object with all other sets.
(define (data:associate-partition sym-obj vert-lists data #!optional default-data)
  (if (default-object? default-data) (set! default-data (data:default)))
  (let ((vert-sets (lists->set-map
		    (map (lambda (lst)
			   (map (lambda (vert) (symo:rep-index sym-obj vert)) lst))
			 vert-lists))))
    (lambda (given-verts)
      (let ((index-msets (map (lambda (vert)
				(hash-table/get vert-sets (symo:rep-index sym-obj vert) #f))
			      given-verts)))
	(let ((results (reduce
			(lambda (set1 set2)
			  (and set1 set2 (mset:intersection set1 set2)))
			(car index-msets)  
			index-msets)))
	  (if (and results (not (mset:empty? results)))
	      data
	      default-data))))))

;; Associates those vertex sets that are entirely contained in one of
;; the given lists of vertices with the corresponding data object.  If
;; a thing is contained entirely in multiple lists, uses an
;; unspecified one of the corresponding data objects.  If supplied,
;; associates all other sets with the default data object.
(define (data:associate-partition-alist sym-obj vert-list-data-alist #!optional default-data)
  (if (default-object? default-data) (set! default-data (data:default)))
  (let ((vert-sets (lists->set-map
		    (map (lambda (lst)
			   (map (lambda (vert) (symo:rep-index sym-obj vert)) lst))
			 (map car vert-list-data-alist))))
	(data-map (list->vector (map cdr vert-list-data-alist))))
    (lambda (given-verts)
      (let ((index-msets (map (lambda (vert)
				(hash-table/get vert-sets (symo:rep-index sym-obj vert) #f))
			      given-verts)))
	(let ((results (reduce
			(lambda (set1 set2)
			  (and set1 set2 (mset:intersection set1 set2)))
			(car index-msets)  
			index-msets)))
	  (if (and results (not (mset:empty? results)))
	      (vector-ref data-map (car (mset:elt-list results)))
	      default-data))))))

(define (merge-by-first proc-list #!optional default-data)
  (if (default-object? default-data) (set! default-data (data:default)))
  (lambda (given-verts)
    ;(pp given-verts)
    (if (null? proc-list)
	default-data
	(let ((data ((car proc-list) given-verts)))
	  (if (not (data:passive? data))
	      data
	      ((merge-by-first (cdr proc-list) default-data) given-verts))))))

