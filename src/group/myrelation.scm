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

;;;;   Group Relations
;;; 18 March 2001 -- rfrankel

;; This is the presentation of a group in terms
;; of generators and relations

(load "utils/general")
;(load-option 'format)
(declare (usual-integrations))

(define group-relation-type-tag '*group-relation*)

(define (group-relation? gr)
  (eq? (car gr) group-relation-type-tag))

(define (gr:type a-relation) group-relation-type-tag)

(define (gr:type-predicate v) group-relation?)

;; Relation-list should be a list of at least two symbols (maybe more)
;; (Change from previous version: used to be integers).

(define (make-group-relation relation-list)
  (assert (> (length relation-list) 1) "MAKE-RELATION: list too short" relation-list)
  (for-each (lambda (alleged-symbol)
	     (assert (symbol? alleged-symbol)
		     "MAKE-RELATION: not an symbol" alleged-symbol))
	   relation-list)
  (list group-relation-type-tag relation-list))

(define (group-relation relation-list)
  (make-group-relation relation-list))

(define (gr:relation-list relation)
  (list-ref relation 1))

(define (gr:length relation)
  (length (gr:relation-list relation)))

(define (gr:relation-element relation position)
  (list-ref (gr:relation-list relation) position))

;;multiply-proc should take an element of the type of start-el or a null
;;and an integer from the relation and return either
;;another element of the type of start-el
;;or a null. This returns the result of the 
;;trace as a list of all the partial results up to failure 
;;and thereafter nulls. For example, the result 
;;might look like: (2 3 1 4 '() '())
;;The returned list is one element longer than the relation.

;; Not anymore: takes a group constraint network and a name of a connector 
;; to start at. Traces from left along the relation finding new connectors 
;; Returns a list of names of connectors and then nulls after it can go no
;; further. Actually there's only one big change: we don't turn the relation
;; elements into numbers anymore -- they might (should?) be a list of symbols. 
;; The multiply proc is defined for this purpose as:
;; (define (group-mult el gen)
;;	 ((trace-multiplication-name el gen) group))

;; Example: in a little network where 
;; 'e times 'x = 'one and 'one times 'x = 'two. 
;; (gr:trace-from-left (make-group-relation (list 'x 'x)) 'e group-mult)
;Value 90: (e one two)
;(define xcubed (make-group-relation (list 'x 'x 'x)))
;Value: xcubed
;;(gr:trace-from-left xcubed 'e group-mult)
;Value 91: (e one two ())

(define (gr:trace-from-left relation start-el multiply-proc)
  (let ((relation-list (gr:relation-list relation)))
    (define (trace-from-left element-to-multiply partial-relation)
      (let ((answer (multiply-proc element-to-multiply (car partial-relation))))
	(if (not (pair? (cdr partial-relation)))
	    (list element-to-multiply answer)
	    (cons element-to-multiply
		  (trace-from-left answer (cdr partial-relation))))))
    (trace-from-left start-el relation-list)))
	    
;; Same deal as trace-from-left except in reverse.
;; Now we define mult-proc this way: 
;; (define (group-mult-inverse el gen)
;;	 ((trace-multiplication-inverse-name el gen) group))
;; Example:
;(gr:trace-from-right xcubed 'e group-mult-inverse)
;Value 94: (() () () e)

(define (gr:trace-from-right relation start-el multiply-proc)
  (let ((relation-list (reverse (gr:relation-list relation))))
    (define (trace-from-left element-to-multiply partial-relation)
      (let ((answer (multiply-proc element-to-multiply (car partial-relation))))
	(if (not (pair? (cdr partial-relation)))
	    (list element-to-multiply answer)
	    (cons element-to-multiply
		  (trace-from-left answer (cdr partial-relation))))))
    (reverse (trace-from-left start-el relation-list))))


;; OK, we are going to step across the right and left lists.
;; we are looking for two things: the point when we hit the first null
;; in the from-left list, and the point when we hit the first non-null
;; in the from-right list.  

;; Example: 
;(gr:trace-both-ways xcubed 'e 'e group-mult group-mult-inverse)
;Value 103: (2 3 (() () () e) (e one two ()))

(define (gr:trace-both-ways relation start-el-left start-el-right 
			    mult-proc-l mult-proc-r)
  (let ((result-from-right (gr:trace-from-right 
			    relation start-el-right mult-proc-r))
	(result-from-left (gr:trace-from-left 
			   relation start-el-left mult-proc-l))
	(relation-length (gr:length relation))
	(found-left-false #f)
	(found-right-non-false #f)
	(last-right-false-pos #f)
	(first-left-false-pos #f)
	(result '()))
    (for-each 
     (lambda (el-from-left el-from-right position)
       (if (and (not el-from-left) (not found-left-false)) 
           (begin (set! found-left-false #t)
		  (set! first-left-false-pos position)))
       (if (and el-from-right (not found-right-non-false))
           (begin (set! found-right-non-false #t)
		  (set! last-right-false-pos (- position 1)))))
     result-from-left
     result-from-right 
    (enumerate-interval 0 relation-length))
    (list last-right-false-pos
	  first-left-false-pos
	  result-from-right 
	  result-from-left)))

;;sjca? means "success, join, coincidence, or add new coset?"
;;if success (left trace went all the way across) -- return list: 'success
;;if the traces just meet (difference of null-pos is one step) 
;;return a list: 'join, the right value, the left value, and the generator symbool
;;if the traces don't meet (difference of null-pos is more than one step) 
;;return a list: 'add-coset, the left value, and the generator symbol between them
;;if the traces overlap (difference of null-pos is less than one step) 
;;return a list: 'coincidence, the left value, and the right value at the same
;; position (note that this might be different than the first-right-non-false)

;;Example:
;(gr:sjca? xcubed 'e 'e group-mult group-mult-inverse)
;Value 106: (join two e x)

;;New example -- we have the network where 'e times 'x = 'one 
;;but 'one times 'x is not defined yet --
;(gr:sjca? xcubed 'e 'e group-mult group-mult-inverse)
;;Value 113: (add-coset one x)

(define (gr:sjca? relation start-el-left start-el-right mult-proc-l mult-proc-r)
  (let ((trace-result (gr:trace-both-ways relation 
					  start-el-left start-el-right
					  mult-proc-l mult-proc-r)))
    ;(pp trace-result)
    (let ((last-r-false (list-ref trace-result 0))
	  (first-l-false (list-ref trace-result 1))
	  (result-from-right (list-ref trace-result 2))
	  (result-from-left (list-ref trace-result 3)))
      (if (not first-l-false) (list 'success)
	  (let ((gen-num (gr:relation-element relation (- first-l-false 1)))
		(first-right-non-false 
		 (list-ref result-from-right (+ last-r-false 1)))
		(last-left-non-false 
		 (list-ref result-from-left (- first-l-false 1)))
		(r-val-at-last-l-pos 
		 (list-ref result-from-right (- first-l-false 1))))
	    (cond ((= (-  first-l-false last-r-false) 1)
		   (list 'join last-left-non-false first-right-non-false gen-num))
		  ((< (-  first-l-false last-r-false) 1)
		   (list 'add-coset last-left-non-false gen-num))
		  ((> (-  first-l-false last-r-false) 1)	     
		   (list 'coincidence last-left-non-false r-val-at-last-l-pos))))))))


;(define my-rel (make-group-relation (list 0 2 1 3)))
;Value: my-rel

;(gr:trace-from-right my-rel 4 (lambda (el rel-el) (+ el rel-el)))
;Value 20: (10 10 8 7 4)

;(gr:trace-from-left my-rel 4 (lambda (el rel-el) (+ el rel-el)))
;Value 17: (4 4 6 7 10)

;; Given a relation, a multiplication procedure, and a 
;; starting element, returns the list of elements that 
;; would be encountered as one follows the relation.
;; The multiplication procedure has to take an 
;; element and a component of the relation and return 
;; their product.
(define (gr:follow-relation relation mult-proc start)
  (let ((cur-elt start))
    (map (lambda (rel-elt) ;; Relies on the order in which map invokes the proc
	   (let ((result (mult-proc cur-elt rel-elt)))
	     (set! cur-elt result)
	     result))
	 (gr:relation-list relation))))