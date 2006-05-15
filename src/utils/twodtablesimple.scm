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

#| -*-Scheme-*-

$Id: twodtablesimple.scm,v 1.3 2005/06/25 06:20:39 axch Exp $

Copyright (c) 1988, 1999 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;;; Local Two Dimensional Property Tables
;;; These are restricted to be keyed by symbols

(load-option 'format)
(declare (usual-integrations))

;; Stuff from the original for garbage collection. 
;; Completely obsolete -- if we end up caring about gc 
;; we should use weak pointers, Chris advises. See 1d table. 
;;(define delete-invalid-hash-numbers! (list-deletor! filter-bucket!))
;;(define delete-invalid-y! (list-deletor! filter-entry!))
;;(add-secondary-gc-daemon! (gc-the-table! <fill in the-table here>))

(define two-d-table-type-tag '*two-d-table*)

(define (two-d-table? td)
  (eq? (car td) two-d-table-type-tag))

(define (td:type td) two-d-table-type-tag)

(define (td:type-predicate v) two-d-table?)

(define (make-empty-two-d-table)
  (cons two-d-table-type-tag '()))

(define (td-int:make-two-d-table data)
  (cons two-d-table-type-tag data))

(define (td-int:table-data td)
  (cdr td))

; Assumes that the table is a hash-table of association lists
(define (hash-table->two-d-table table)
  (td-int:make-two-d-table (hash-table->alist table)))

(define (two-d-put! the-table x y value)
  (assert (two-d-table? the-table))
  (let ((table-data (td-int:table-data the-table)))
    (let ((bucket (assq x table-data)))
      (if bucket
	  (let ((entry (assq y (cdr bucket))))
	    (if entry
		(set-cdr! entry value)
		(set-cdr! bucket
			  (cons (cons y value)
				(cdr bucket)))))
	  (set-cdr! the-table
		(cons (cons x
			    (cons (cons y value)
				  '()))
		      table-data))))))

(define (two-d-get the-table x y)
  (assert (two-d-table? the-table))
  (let ((table-data (td-int:table-data the-table)))
    (let ((bucket (assq x table-data)))
      (and bucket
	   (let ((entry (assq y (cdr bucket))))
	     (and entry
		  (cdr entry)))))))

;;; Returns TRUE iff an entry was removed.
;;; Removes the bucket if the entry removed was the only entry.

(define (two-d-remove! the-table x y)
  (assert (two-d-table? the-table))
  (let ((table-data (td-int:table-data the-table)))
    (let ((bucket (assq x table-data)))
      (and bucket
	   (begin (set-cdr! bucket
			    (del-assq! y
				       (cdr bucket)))
		  (if (null? (cdr bucket))
		      (set-cdr! the-table
				(del-assq! x
					   table-data)))
		true)))))

;;; This clever piece of code removes all invalid entries and buckets,
;;; and also removes any buckets which [subsequently] have no entries.

;(define (gc-the-table! the-table)
;  (set! the-table (delete-invalid-hash-numbers! the-table)))

;(define (filter-bucket! the-table bucket)
;  (or (not (valid-hash-number? (car bucket)))
;      (begin (set-cdr! bucket (delete-invalid-y! (cdr bucket)))
;	     (null? (cdr bucket)))))

;(define (filter-entry! the-table entry)
;  (not (valid-hash-number? (car entry))))

(define (two-d-get-alist-x the-table x)
  (assert (two-d-table? the-table))
  (let ((table-data (td-int:table-data the-table)))
    (let ((bucket (assq x table-data)))
      (if bucket
	  (let loop ((rest (cdr bucket)))
	    (cond ((null? rest) '())
		  ((symbol? (caar rest))
		   (cons (cons (caar rest)
			       (cdar rest))
			 (loop (cdr rest))))
		  (else (loop (cdr rest)))))
	  '()))))

(define (two-d-get-alist-y the-table y)
  (assert (two-d-table? the-table))
  (let ((table-data (td-int:table-data the-table)))
    (let loop ((rest table-data))
      (cond ((null? rest) '())
	    ((symbol? (caar rest))
	     (let ((entry (assq y (cdar rest))))
	       (if entry
		   (cons (cons (caar rest)
			       (cdr entry))
			 (loop (cdr rest)))
		   (loop (cdr rest)))))
	    (else (loop (cdr rest)))))))

;; This returns a new two-d table with 
;; the procedure applied to arguments
(define (two-d-table-map the-table proc)
  (assert (two-d-table? the-table))
  (let ((table-data (td-int:table-data the-table))
	(counter 0))
    (append 
     (list two-d-table-type-tag)
     (map 
      (lambda (pair)
	(let ((key-one (car pair))
	      (bucket (cdr pair)))
	  (cons key-one
		(map (lambda (buck-pair)
		       (let ((key-two (car buck-pair))
			     (value (cdr buck-pair)))
			 (set! counter (+ 1 counter))
			 (if (= 0 (remainder counter 500)) (pp counter))
			 (cons key-two (proc value))))
		     bucket))))
      table-data))))

