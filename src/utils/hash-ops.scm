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

;; This file defines utilities actively using, working on or producing hash tables

(define ((lin-rem-dup key-hash key=?) lst)
  (let ((unique-elts ((strong-hash-table/constructor key-hash key=? #t))))
    (for-each (lambda (elt)
		(if (not (hash-table/get unique-elts elt #f))
		    (hash-table/put! unique-elts elt #t)))
	      lst)
    (filter (lambda (elt)
	      (if (hash-table/get unique-elts elt #f)
		  (begin (hash-table/remove! unique-elts elt)
			 elt)
		  #f))
	    lst)))

(define lin-rem-dup-eq (lin-rem-dup eq-hash-mod eq?))

(define (index-map lst)
  (let ((answer (make-eq-hash-table))
	(cur-num 0))
    (for-each (lambda (elt)
		(hash-table/put! answer elt cur-num)
		(set! cur-num (+ 1 cur-num)))
	      lst)
    answer))

(define (((list->hash-table key-hash key=?) get-key get-val) lst)
  (let ((answer ((strong-hash-table/constructor key-hash key=? #t))))
    (for-each (lambda (elt)
		(hash-table/put! answer (get-key elt) (get-val elt)))
	      lst)
    answer))

(define list->eq-hash-table (list->hash-table eq-hash-mod eq?))

(define list->equal-hash-table (list->hash-table equal-hash-mod equal?))

(define list->eq-hash-set 
  (list->eq-hash-table (lambda (elt) elt) (lambda (elt) #t)))

;; The lists given must be disjoint.  The result is a hash table from 
;; elements in the lists to the index of the list in the input (i.e. a 
;; unique identifier)
(define (disjoint-lists->set-map list-of-lists)
  (let ((elt-sets (make-eq-hash-table))
	(list-num 0))
    (for-each 
     (lambda (lst)
       (for-each 
	(lambda (elt)
	  (let ((val (hash-table/get elt-sets elt #f)))
	    (if (and val (not (= val list-num)))
		(error (string-append "Element occurs in lists " (number->string list-num) 
				      " and " (number->string val) ":")
		       elt))
	    (hash-table/put! elt-sets elt list-num)))
	lst)
       (set! list-num (+ 1 list-num)))
     list-of-lists)
    elt-sets))

;; The result is a hash table from elements to multi-sets of the 
;; indecies in the input of the lists that contain those elements
(define (lists->set-map list-of-lists)
  (let ((elt-sets (make-eq-hash-table))
	(list-num 0))
    (for-each 
     (lambda (lst)
       (for-each 
	(lambda (elt)
	  (let ((val (hash-table/get elt-sets elt #f)))
	    (if (not val) (set! val (make-multi-set)))
	    (mset:add! val list-num)
	    (hash-table/put! elt-sets elt val)))
	lst)
       (set! list-num (+ 1 list-num)))
     list-of-lists)
    elt-sets))