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

(define-structure (group-table
		   (print-procedure
		    (standard-unparser-method
		     'GROUP-TABLE
		     (lambda (cg port)
		       (write-string " " port)
		       ;(write `(named ,(cxg/name cg)) port)
		       )))
		   (constructor %create-group-table
				(mult-table #!optional gens-list num-cosets num-entries))
		   (conc-name gt/))
  (mult-table '() read-only #t)
  (gens-list '())
  (num-cosets 0)
  (num-entries 0))


(define (make-empty-group-table) 
  (%create-group-table (make-eq-hash-table)))

(define (gt:put-gen-coset! table gen coset value)
  (if (not (memq gen (gt/gens-list table)))
      (set-gt/gens-list! table (cons gen (gt/gens-list table))))
  (let* ((hash (gt/mult-table table))
	 (row (hash-table/get hash coset '()))
	 (entry (assq gen row)))
    (if (null? row)
	(set-gt/num-cosets! table (+ 1 (gt/num-cosets table))))
    (if entry
	(set-cdr! entry value)
	(begin
	  (set-gt/num-entries! table (+ 1 (gt/num-entries table)))
	  (hash-table/put! (gt/mult-table table) coset (cons (cons gen value) row))))))

(define (gt:get-gen-coset table gen coset)
  (let ((row (hash-table/get (gt/mult-table table) coset #f)))
    (and row
	 (let ((entry (assq gen row)))
	   (and entry (cdr entry))))))

(define (gt:remove-gen-coset! table gen coset)
  (let* ((hash (gt/mult-table table))
	 (row (hash-table/get hash coset '()))
	 (new-row (del-assq gen row)))
    (set-gt/num-entries! table (- (gt/num-entries table) 1))
    (if (> (length new-row) 0)
	(hash-table/put! hash coset new-row)
	(begin
	  (set-gt/num-cosets! table (- (gt/num-cosets table) 1))
	  (hash-table/remove! hash coset)))))

;; Given a generator, returns an association list of
;; coset to data
(define (gt:get-gen-alist table gen)
  (map (lambda (row)
	 (assq gen row))
       (hash-table/datum-list (gt/mult-table table))))

;; Given a coset, returns an association list of 
;; generator to data
(define (gt:get-coset-alist table coset)
  (hash-table/get (gt/mult-table table) coset '()))

(define (gt:coset-full? table coset)
;  (pp (list "Generators" (gt/gens-list table)
;	    "Gen-to-vals" (gt:get-coset-alist table coset)))
  (= (length (gt/gens-list table))
     (length (gt:get-coset-alist table coset))))

(define (gt:table-full? table)
;  (pp (list "Number of entries" (gt/num-entries table)
;	    "Number of cosets" (gt/num-cosets table)
;	    "Generators" (gt/gens-list table)))
  (= (gt/num-entries table)
     (* (gt/num-cosets table)
	(length (gt/gens-list table)))))

;; Beware transpositions
(define (group-table->two-d-table table)
  (hash-table->two-d-table (gt/mult-table table)))

;; Returns a two-d-table
;(define (gt:map table proc)
;  (two-d-table-map (group-table->two-d-table table) proc))

(define (gt:print-by-gen table gen-list coset-list print-val-proc)
  (print-table-by-y (group-table->two-d-table table) 
		    coset-list gen-list print-val-proc))

(define (gt:print-by-coset table gen-list coset-list print-val-proc)
  (print-table-by-x (group-table->two-d-table table) 
		    coset-list gen-list print-val-proc))

;; Given {a group table}, {a procedure that takes a group table, a coset, 
;; and a generator, and returns the coset product}, and {a list of
;; generators}, returns the subgroup that those generators 
;; define.
(define (gt:generator-subgroup group-table mult-proc gen-list)
  (%create-subgroup (lambda (elt gen)
		      (mult-proc group-table elt gen))
		    (map list gen-list)))