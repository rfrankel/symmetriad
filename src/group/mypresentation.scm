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

(load-option 'format)
(declare (usual-integrations))

;;;;   Group Presentation
;;; 13 March 2001 -- rfrankel

;; This is the presentation of a group in terms
;; of generators and relations

(define group-presentation-type-tag '*group-presentation*)

(define (group-presentation? gp)
  (eq? (car gp) group-presentation-type-tag))

(define (gp:type a-group) group-presentation-type-tag)

(define (gp:type-predicate v) group-presentation?)

(define (make-group-presentation generators relations-list)
   (let ((num-gens (length generators))
	 (gen-inverse-table (make-1d-table))
	 (symbolic-reflect-proc (lambda (v) v))) ;;dummy proc

     ;;Initialize gen-inverse-table -- association between generators 
     ;; and inverses. 
     (for-each (lambda (gen-symbol)
		   (1d-table/put! gen-inverse-table 
				  gen-symbol 
				  (symbol-append '- gen-symbol))
		   (1d-table/put! gen-inverse-table 
				  (symbol-append '- gen-symbol) 
				  gen-symbol))
	       generators)

     ;; Check that the relations are properly expressed in terms 
     ;; of generator symbols
     (for-each (lambda (relation)
		 (for-each (lambda (letter)
			     (assert (1d-table/get gen-inverse-table letter #f)
				     "MAKE-PRESENTATION: not a generator --"
				     letter))
		       relation))
	       relations-list)

     ;; Make the relation objects
     (let ((relation-objs (map (lambda (relation-list)
				 (make-group-relation relation-list))
				relations-list)))
       (list group-presentation-type-tag
	     num-gens
	     gen-inverse-table
	     relation-objs
	     (list)
	     generators
	     symbolic-reflect-proc))))

(define (group-presentation generators . relations)
  (make-group-presentation generators relations))

(define (group-presentation-and-proc gens relations reflect-proc)
  (gp:set-reflect-proc! 
   (make-group-presentation gens relations) reflect-proc))
       
(define (gp:num-gens gp)
  (list-ref gp 1))

(define (gp:gen-num-table gp)
  (list-ref gp 2))

(define (gp:relations-list gp)
  (list-ref gp 3))

(define (gp:inv-generators gp)
  (list-ref gp 4))

(define (gp:generator-symbols gp)
  (list-ref gp 5))

(define (gp:reflect-proc gp)
  (list-ref gp 6))

(define (gp:find-gen-num gp letter)
  (1d-table/get (gp:gen-num-table gp) letter #f))

(define (gp:inv-gen gp gen-symbol)
  (let ((invtable (gp:gen-num-table gp)))
    (1d-table/get invtable gen-symbol #f)))

(define (gp:set-reflect-proc! gp mult-proc)
  (set-car! (cddr (cddddr gp)) mult-proc)
  gp)
