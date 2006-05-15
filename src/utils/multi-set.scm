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

;; This file defines a multi-set abstraction.
;; If a set contains zero of some element, by definition, it does not contain
;; that element.  Therefore, no multi-set will ever register an element as being
;; contained in quantity zero.

(define-structure (multi-set
		   (conc-name mset/)
		   (constructor %create-multi-set (hash)))
  (hash #f read-only #t))

(define (make-multi-set)
  (%create-multi-set (make-eq-hash-table)))

(define (mset:number multi-set thing)
  (hash-table/get (mset/hash multi-set) thing #f))

(define (mset:add! multi-set thing #!optional num)
  (if (default-object? num) (set! num 1))
  (cond ((< num 0) (mset:remove! multi-set thing (- 0 num)))
	((= num 0) #f)
	(else
	 (let ((number (mset:number multi-set thing)))
	   (if number
	       (hash-table/put! (mset/hash multi-set) thing (+ num number))
	       (hash-table/put! (mset/hash multi-set) thing num))))))
	
(define (mset:remove! multi-set thing #!optional num)
  (if (default-object? num) (set! num 1))
  (cond ((< num 0) (mset:add! multi-set thing (- 0 num)))
	((= num 0) #f)
	(else
	 (let ((number (mset:number multi-set thing)))
	   (if (and number (> number num))
	       (hash-table/put! (mset/hash multi-set) thing (- number num))
	       (hash-table/remove! (mset/hash multi-set) thing))))))

(define (mset:same-set mset1 mset2)
  (and (mset:contained-in mset1 mset2)
       (mset:contained-in mset2 mset1)))
       
(define (mset:contained-in mset1 mset2)
  (reduce (lambda (one two) (and one two))
	  #t
	  (map (lambda (elt)
		 (let ((num1 (mset:number mset1 elt))
		       (num2 (mset:number mset2 elt)))
		   (or (not num1)
		       (and num2 (>= num2 num1)))))
	       (hash-table/key-list (mset/hash mset1)))))

(define (mset:elt-list mset)
  (hash-table/key-list (mset/hash mset)))

(define (mset:empty? mset)
  (= 0 (length (mset:elt-list mset))))

(define (mset:intersection mset1 mset2)
  (let ((result (make-multi-set)))
    (for-each (lambda (key)
		(let ((num1 (mset:number mset1 key))
		      (num2 (mset:number mset2 key)))
		  (if (and num1 num2)
		      (mset:add! result key (min num1 num2)))))
	      (mset:elt-list mset1))
    result))

(define (alist->mset alist)
  (let ((multi-set (make-multi-set)))
    (for-each (lambda (pair)
		(mset:add! multi-set (car pair) (cdr pair)))
	      alist)
    multi-set))

(define (mset->alist mset)
  (map (lambda (elt)
	 (cons elt (mset:number mset elt)))
       (mset:elt-list mset)))