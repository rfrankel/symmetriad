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

;;; This file defines a subgroup of a group.
;;; Subgroups are represented by a list of 
;;; words made of generators, and know the 
;;; multiplication table of their group.
;;; These words are the generators of the subgroup.
;;; From this, they can compute their own cosets
;;; starting from an arbitrary element.

(define-structure (subgroup
		   (print-procedure
		    (standard-unparser-method
		     'SUBGROUP
		     (lambda (subg port)
		       ())))
		   (constructor %create-subgroup
				(mult-proc word-list))
		   (conc-name subg/))
  (mult-proc #f read-only #t) 
  (word-list '() read-only #t)
  (coset-cache (make-eq-hash-table) read-only #t))

; mult-proc must take a supergroup element and a supergroup 
; generator and return their supergroup product

(define (coxg-subgroup cox-g word-list)
  (%create-subgroup
   (lambda (elt gen) (gn:product (cxg/group-net cox-g) elt gen))
   (map as-list word-list)))

; gen-spec is a list of 1's and 0's.  The 0's indicate which 
; generators go into the subgroup.
(define (coxg-gen-subgroup cox-g gen-spec)
  (coxg-subgroup cox-g (gens-and-spec->words 
			(cxg/gen-list cox-g)
			gen-spec)))

(define (symo-subgroup sym-obj word-list)
  (coxg-subgroup (symo/geometry sym-obj) word-list))

;; May be computing the wrong handedness of coset...
(define (subg:compute-coset subg start)
  ;(pp (list "Computing coset for" subg "at" start))
  (define (traverse-word mult-proc start word)
    (define (loop cur-elt word-left)
      (if (null? word-left)
	  cur-elt
	  (loop (mult-proc cur-elt (car word-left))
                (cdr word-left))))
    ;(pp (loop start word))
    (loop start word))
  (let ((done-elts
         ((strong-hash-table/constructor eq-hash-mod eq? #t))))
    (define (loop todo-elts)
      (if (null? todo-elts)
	  (hash-table/key-list done-elts)
	  (let ((cur-elt (car todo-elts))
		(left (cdr todo-elts)))
	    (if (hash-table/get done-elts cur-elt #f)
		(loop left) ; This was a repeat, do nothing
		(begin
                  (hash-table/put! done-elts cur-elt #t)
                  (for-each 
                   (lambda (word)
                     (let ((result
                            (traverse-word (subg/mult-proc subg) 
                                           cur-elt word)))
                       (set! left (cons result left))))
                   (subg/word-list subg))
		       (loop left))))))
    (loop (list start))
    (hash-table/key-list done-elts)))

(define (subg:get-coset subgroup elt #!optional use-cache)
  (if (default-object? use-cache) (set! use-cache #t))
  (if use-cache
      (let ((cached
             (hash-table/get (subg/coset-cache subgroup) elt #f)))
	(if cached
	    cached
	    (let ((coset (subg:compute-coset subgroup elt)))
	      (for-each
               (lambda (key)
                 (hash-table/put!
                  (subg/coset-cache subgroup) key coset))
               coset)
	      coset)))
      (subg:compute-coset subgroup elt)))

(define (subg:same-coset? subg elt1 elt2)
  (eq? (subg:get-coset subg elt1)
       (subg:get-coset subg elt2)))
       
(define (subg:coset-list subg elt-list)
  (if (symmetric-object? elt-list)
      (subg:coset-list
       subg (cxg/chamber-list (symo/geometry elt-list)))
      (lin-rem-dup-eq
       (map (lambda (elt)
	      (subg:get-coset subg elt))
	    elt-list))))
