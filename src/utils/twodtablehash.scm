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

;;;; Local Two Dimensional Property Tables
;; These are restricted to be keyed by symbols
;; This variant is implemented as a hash table on 
;; the first coordinate and an association list on the second

(load-option 'format)
(declare (usual-integrations))

(define two-d-table-type-tag '*two-d-table-hash*)

(define (two-d-table? td)
  (eq? (car td) two-d-table-type-tag))

(define (td:type td) two-d-table-type-tag)

(define (td:type-predicate v) two-d-table?)

(define (make-empty-two-d-table)
  (cons two-d-table-type-tag (make-eq-hash-table)))

(define (td-int:make-two-d-table data)
  (cons two-d-table-type-tag data))

(define (td-int:table-data td)
  (cdr td))

; Assumes that the table is a hash-table of association lists
(define (hash-table->two-d-table table)
  (td-int:make-two-d-table table))

(define (two-d-put! the-table x y value)
  (assert (two-d-table? the-table))
  (let ((table-data (td-int:table-data the-table)))
    (let ((bucket (hash-table/get table-data x #f)))
      (if bucket
	  (let ((entry (assq y bucket)))
	    (if entry
		(set-cdr! entry value)
		(hash-table/put! table-data x (cons (cons y value) bucket))))
	  (hash-table/put! table-data x
			   (cons (cons y value) '()))))))

(define (two-d-get the-table x y)
  (assert (two-d-table? the-table))
  (let ((table-data (td-int:table-data the-table)))
    (let ((bucket (hash-table/get table-data x #f)))
      (and bucket
	   (let ((entry (assq y bucket)))
	     (and entry
		  (cdr entry)))))))

;;; Removes the bucket if the entry removed was the only entry.

(define (two-d-remove! the-table x y)
  (assert (two-d-table? the-table))
  (let ((table-data (td-int:table-data the-table)))
    (let ((bucket (hash-table/get table-data x #f)))
      (and bucket
	   (begin
	     (del-assq! y bucket)
	     (if (pair? bucket)
		 (hash-table/put! table-data x bucket)
		 (hash-table/remove! table-data x)))))))

(define (two-d-get-alist-x the-table x)
  (assert (two-d-table? the-table))
  (let ((table-data (td-int:table-data the-table)))
    (hash-table/get table-data x #f)))

(define (two-d-get-alist-y the-table y)
  (assert (two-d-table? the-table))
  (let ((table-data (td-int:table-data the-table)))
    (let loop ((rest (hash-table/key-list table-data)))
      (cond ((null? rest) '())
	    ((symbol? (car rest))
	     (let ((entry (assq y (hash-table/get table-data (car rest) #f))))
	       (if entry
		   (cons (cons (car rest)
			       (cdr entry))
			 (loop (cdr rest)))
		   (loop (cdr rest)))))
	    (else (loop (cdr rest)))))))

;; This returns a new two-d table with 
;; the procedure applied to arguments
(define (two-d-table-map the-table proc)
  (assert (two-d-table? the-table))
  (let ((table-data (td-int:table-data the-table))
	(new-data (make-eq-hash-table))
	(counter 0))
    (hash-table/for-each 
     table-data 
     (lambda (key datum)
       (let ((new-datum (map (lambda (buck-pair)
			       (set! counter (+ 1 counter))
			       (if (= 0 (remainder counter 500)) (pp counter))
			       (cons (car buck-pair) (proc (cdr buck-pair))))
			     datum)))
	 (hash-table/put! new-data key new-datum))))
    (td-int:make-two-d-table new-data)))

