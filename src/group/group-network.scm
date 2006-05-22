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

;;;;;  Group Multiplication Table found using Todd Coxeter Algorithm 
;;; Adapted for mirror reflection groups
;;;; October 2003 -- rfrankel

(define group-network-type-tag '*mirror-group*)

(define (group-network? gn)
  (eq? (car gn) group-network-type-tag))

(define (gn:type tc) group-network-type-tag)

(define (gn:type-predicate v) group-network?)

(define (make-group-network group-presentation
			  subgroup-presentation
                          multiplication-table
			  group-constraint-system
			  geometry-table
			  num-cosets
			  dead-cosets
			  use-tms?)
    (list group-network-type-tag
	group-presentation
        subgroup-presentation
        multiplication-table 
	group-constraint-system
        geometry-table
	num-cosets
	dead-cosets
	use-tms?))

;; utility for initializing the geometry
(define ((selector_n n) el_list)
  (assert (< n (length el_list)) "SELECTOR_N: selector out of range")
  (list-ref el_list n))

(define (group-network group-present subgroup-present name)
  (define use-tms? *use-tms?*)
  (let* ((group-net (if use-tms? (create-constraint-network name) #f))
	 (mult-table (make-empty-group-table))
	 (geometry-table (make-empty-two-d-table))
	 (gen-symbols (gp:generator-symbols group-present)))

    ;; Add appropriate geometry procedures for the identity
    ;; The procedures representing the roots of the chamber corresponding
    ;; to the identitiy element of the group are *really* trivial, as
    ;; the roots of said chamber are the parameters in whose terms all the
    ;; others are expressed.
    (for-each 
     (lambda (gen)
       (let ((index (- (length gen-symbols)
		       (length (memq gen gen-symbols)))))
	 (let ((sroot (symbol-append gen '_root))
	       (sel-proc (selector_n index)))
	   (two-d-put! geometry-table 'e sroot sel-proc))))
     gen-symbols)
      
    (make-group-network
     group-present
     subgroup-present
     mult-table
     group-net
     geometry-table
     0
     '()
     use-tms?
     )))

(define (gn:presentation group-network)
  (list-ref group-network 1))

(define (gn:subgroup group-network)
  (list-ref group-network 2))

(define (gn:mult-table group-network)
  (list-ref group-network 3))

(define (gn:constraint-network group-network)
  (list-ref group-network 4))

(define (gn:geometry group-network)
  (list-ref group-network 5))

(define (gn:set-geometry gn element tag geom)
  (let ((geometry-table (gn:geometry gn)))
    (two-d-put! geometry-table element tag geom)))

(define (gn:get-geometry gn element tag)
  (let ((geometry-table (gn:geometry gn)))
    (two-d-get geometry-table element tag)))

(define (gn:num-cosets group-network)
  (list-ref group-network 6))

(define (gn:set-num-cosets! group-network new-cos-num)
  (set-cdr! (cdr (cddddr group-network)) 
	    (list new-cos-num 
		  (gn:dead-cosets group-network)
		  (gn:use-tms? group-network)
		  )))

(define (gn:dead-cosets group-network)
  (list-ref group-network 7))

(define (gn:add-dead-coset! group-network coset)
  (set-cdr! (cddr (cddddr group-network)) 
	    (list (cons coset (gn:dead-cosets group-network))
		  (gn:use-tms? group-network)
		  )))

(define (gn:dead-coset? group-network coset)
  (not (not (memq coset (gn:dead-cosets group-network)))))

(define (gn:use-tms? group-network)
  (list-ref group-network 8))

(define (coset-num->coset coset-num)
  (if (eq? coset-num 0) 
      'e 
      (string->symbol (string-append
		       "c"
		       (number->string coset-num)))))
	  
(define (coset-list num-cosets)
  (map coset-num->coset (enumerate-interval 0 num-cosets)))

(define (gn:coset-list group-net)
  (filter (lambda (coset-symb)
	    (not (gn:dead-coset? group-net coset-symb)))
	  (coset-list (gn:num-cosets group-net))))

;(define (gn:print-table gn generator-list)
;  (let ((at (gn:constraint-network gn)))
;    (at:print-table at generator-list)))

(define (root-symbols gen-symbols)
  (map 
   (lambda (gen)
     (symbol-append  gen '_root))
   gen-symbols))

       
(define (handle-add gn coset gen)
  (let* ((gp (gn:presentation gn))
         (inv-gen (gp:inv-gen gp gen))
	 (num-cosets (gn:num-cosets gn))
	 (new-coset-num (+ 1 num-cosets))
	 (new-coset-symbol (coset-num->coset new-coset-num))
	 )

    (define (add-new-geometry)
      (let* ((gens (gp:generator-symbols gp))
	     (reflect-proc (gp:reflect-proc gp)))
	(for-each 
	 (lambda (mirror-sym mirror-gen)
	   (let ((mirror-proc (gn:get-geometry gn coset mirror-sym))
		 (gen-proc (gn:get-geometry gn coset      
					    (symbol-append  gen '_root))))
	     (if mirror-proc
		 ; Here reflect-proc reflects mirror-(gen/proc) across
		 ; gen/gen-proc.
		 (gn:set-geometry gn new-coset-symbol mirror-sym
				  (reflect-proc mirror-gen gen mirror-proc gen-proc))
		 (begin
		   (pp (list "Problem: can't retrieve root at" coset mirror-sym))))))
	 (root-symbols gens) gens)))

    ;; Bump the coset number
    (gn:set-num-cosets! gn new-coset-num)    

    ;; Record the fact of the product
    (if (gn:use-tms? gn)
        ;; Build the new structures in the constraint system
        ;; add the new (bidirectional) constraint
	(let ((new-mult-symbol
	       (symbol-append gen (string->symbol "*") coset))
	      (new-inv-mult-symbol
	       (symbol-append inv-gen (string->symbol "*") new-coset-symbol)))
	  (record-product! gn gen coset new-coset-symbol new-mult-symbol)
	  (record-product! gn gen new-coset-symbol coset new-inv-mult-symbol))
	(begin
	  (record-product! gn gen coset new-coset-symbol #f)
	  (record-product! gn gen new-coset-symbol coset #f)))

    (add-new-geometry)))

(define (handle-join gn cos-start cos-end gen)
  (let* ((gp (gn:presentation gn))
	 (inv-gen (gp:inv-gen gp gen))
	 )
    (if (gn:use-tms? gn)
	(let ((new-mult-symbol
	       (symbol-append gen (string->symbol "*") cos-start))
	      (new-inv-mult-symbol
	       (symbol-append inv-gen (string->symbol "*") cos-start))
	      )
	  (record-product! gn gen cos-start cos-end new-mult-symbol)
	  (record-product! gn gen cos-end cos-start new-inv-mult-symbol))
	(begin (record-product! gn gen cos-start cos-end #f)
	       (record-product! gn gen cos-end cos-start #f)))))
	

(define (record-product! gn gen factor product symb)
  (let* ((grp-table (gn:mult-table gn))
	 )
    (if (gn:dead-coset? gn factor)
	(error "Given a dead factor" factor))
    (if (gn:dead-coset? gn product)
	(error "Given a dead product" product))
    ;; Add the new coset rule      
    (if (gn:use-tms? gn)
	(let* ((grp-net (gn:constraint-network gn))
	       (grp-tms (cn/tms grp-net))
	       (product-node (tms-create-node-with-name
			      grp-tms symb product)))
	  (gt:put-gen-coset! grp-table gen factor product-node))
	(gt:put-gen-coset! grp-table gen factor product))
    ))

(define (handle-coincidence gn left-bad right-bad)
  ;(pp (list "Coincidence detected" left-bad right-bad))
  (define (remove-link grp-table victim gen)
    (let* ((inv-node (gt:get-gen-coset grp-table gen victim))
	   (prev-coset (if (gn:use-tms? gn)
			   (node-datum inv-node)
			   inv-node))
	   ; TODO this appears unused.  What's it for?
	   (forw-node (gt:get-gen-coset grp-table gen prev-coset)))
      ;(pp (list "Unlinking node" victim ", generator" gen)) 
      ;(pp inv-node)
      (gt:remove-gen-coset! grp-table gen victim)
      (gt:remove-gen-coset! grp-table gen prev-coset)
      ;(pp (list "Unlinked node" victim ", generator" gen)) 
      ))
  (define (merge-nodes loser winner)
    ;(pp (list "Merging node" loser "into node" winner))
    (if (eq? loser winner)
	'()
	(let* ((grp-table (gn:mult-table gn))
	       (loser-attachments (gt:get-coset-alist grp-table loser))
	       (winner-attachments (gt:get-coset-alist grp-table winner))
	       (recursion-list '()))
	  ;(pp loser-attachments)
	  ;(pp winner-attachments)
	  (for-each
	   (lambda (loser-link)
	     (let* ((gen (car loser-link))
		    (other-coset (if (gn:use-tms? gn)
				     (node-datum (cdr loser-link))
				     (cdr loser-link)))
		    )
	       (if (not (assq gen winner-attachments))
		   (begin
		     (remove-link grp-table loser gen)
		     (handle-join gn winner other-coset gen))
		   (let ((other-winner-coset (lookup-mult grp-table winner gen)))
		     (remove-link grp-table loser gen)
		     (set! recursion-list (cons (cons other-coset other-winner-coset)
						recursion-list))))))
	   loser-attachments)
	  (gn:add-dead-coset! gn loser)
	  recursion-list)))
  (define (recursive-merge loser winner)
    (let ((answer (merge-nodes loser winner)))
      (map recursive-merge (map car answer) (map cdr answer))))
  (recursive-merge left-bad right-bad)
  )
	

(define (lookup-mult grp-mult coset generator)
  (if (not (null? coset))
      (let ((node
	     (gt:get-gen-coset grp-mult generator coset)))
	(if node
	    (if *use-tms?* (tms-node/datum node) node)
	    #f))
      #f))

(define (print-group gn)
  (gt:print-by-coset
   (gn:mult-table gn)
   (gen-and-inv-list (gn:presentation gn))
   (gn:coset-list gn)
   (lambda (node-or-nil) 
     (if (not node-or-nil)
	 "-"
	 (if (gn:use-tms? gn)
	     (tms-node/datum node-or-nil)
	     node-or-nil)
	 )))
  (newline) (display (length (gn:dead-cosets gn))) 
  (display " dead cosets: ") (pp (gn:dead-cosets gn)))

;; Note: multiplication and inverse multiplication are 
;; the same in a mirror group.
(define (gn:trace-relation gn left-symbol right-symbol relation)
  (let ((grp-mult (gn:mult-table gn)))
    (let ((trace-result (gr:sjca? relation left-symbol right-symbol
			    (lambda (coset generator)
			     (lookup-mult grp-mult coset generator))
			    (lambda (coset generator)
			     (lookup-mult grp-mult coset generator)))))
      (let ((switch (list-ref trace-result 0)))
	;;(pp switch)
	;;(newline)
	(cond ((eq? switch 'success) 'success)
	      ((eq? switch 'add-coset) 
		(let ((coset (list-ref trace-result 1))
		      (gen (list-ref trace-result 2)))
		  (handle-add gn coset gen)
		  'not-done-yet))
	      ((eq? switch 'join)
	       (let ((cos-start (list-ref trace-result 1))
		     (cos-end (list-ref trace-result 2))
		     (gen (list-ref trace-result 3)))
		  (handle-join gn cos-start cos-end gen)
		  'not-done-yet))
	      ((eq? switch 'coincidence)
	       (let ((left-bad (list-ref trace-result 1))
		     (right-bad (list-ref trace-result 2)))
		 (handle-coincidence gn left-bad right-bad)
		 )))))))
                
;; Now I only want to produce the generators
;; inverses are the same
(define (gen-and-inv-list gp)
 (let ((gen-list (gp:generator-symbols gp)))
   gen-list))

(define (gn:trace-across gn left-cos right-cos relation)
  (let ((trace-result 
	 (gn:trace-relation gn left-cos right-cos relation)))
    (cond ((eq? trace-result 'coindicence)      ;; we had a coincidence
	   (display "Coincidence") (newline) (print-group gn)
	   gn)
	  ((eq? trace-result 'not-done-yet) 
	   ;(print-group gn)
	   ;(newline) (display "Not done") (newline) (newline)
	   (gn:trace-across gn left-cos right-cos relation))
	  ((eq? trace-result 'success) 
	   ;(print-group gn)
	   ;(newline) (display "Success") (newline) (newline)
	   gn))))

;; Messeuirs H, L, and T published a paper detailing an algorithm for 
;; figuring out the multiplication table of a group from its generators
;; and relations.  See the Simms book.
;; Builds the root procedures along with the group multiplication table.
(define (gn:hlt gn)
  (let ((gp (gn:presentation gn))
	(grp-mult (gn:mult-table gn)))
    (let ((relations (gp:relations-list gp))
	  (gen-list (gp:generator-symbols gp))
	  )
      ;; OK, now for the action.
      (define (done?)
	(gt:table-full? grp-mult))
      (define (report-done coset)
	(display "Finally done at ") (pp coset)
	;(display "Resulting group table:") (newline)
	;(print-group gn) (newline)
	)
      (define (skip-dead-coset coset-num)
	;(display "Skipping dead coset ") (pp (coset-num->coset coset-num))
	(hlt-helper (+ coset-num 1)))
      (define (skip-coset coset-num)
	;(display "Skipping coset ") (pp (coset-num->coset coset-num))
	(hlt-helper (+ coset-num 1)))
      (define (internal-trace-relation coset relation)
	;(display "Tracing ") (pp relation)
	(gn:trace-across gn coset coset relation))
      (define (process-coset coset-num)
	(let ((coset (coset-num->coset coset-num)))
	  (if (= 0 (remainder coset-num 200))
	      (begin (display "Processing coset ") (pp coset))
	      )
	  (for-each 
	   (lambda (relation) 
	     (internal-trace-relation coset relation))
	   relations)
	  (if (done?) 
	      (report-done coset)
	      (hlt-helper (+ coset-num 1)))))
      (define (hlt-helper coset-num)
	(let ((coset (coset-num->coset coset-num)))
	  (cond ((gn:dead-coset? gn coset)
		 (skip-dead-coset coset-num))
		; Hack, becuase coset skipping causes bad coincidences in A4 and D4
		; (but more or less safely improves performance in < 4 dimensions)
		((and (< (length gen-list) 4) 
		      (not (eq? coset 'e))
		      (gt:coset-full? grp-mult coset))
		 (skip-coset coset-num))
		(else 
		 (process-coset coset-num)))))
      (newline) (display "Running gn:hlt") (newline)
      (hlt-helper 0)
      'done)))

;(define (gn:print gn)
;  (let ((at (gn:constraint-network gn))
;	(gp (gn:presentation gn))
;	(sp (gn:subgroup gn))
;	(cq (gn:coincidence-queue gn))
;	(dfq (gn:definition-queue gn))
;	(dq (gn:deduction-queue gn)))
;    (let ((generator-list (gp:generator-symbols gp)))
;      (at:print-table at generator-list)
;      (fq:print cq)
;      (fq:print dfq)
;      (fq:print dq))))
	  
;(define (gn:schreier-vectors gn)
;  (let ((dfq (gn:definition-queue gn))
;	(num-states (at:num-states (gn:constraint-network gn))))
;    (let ((def-list (fq:front-ptr dfq))
;	  (schreier-vec (make-vector num-states))
;	  (backlink-vec (make-vector num-states)))
;      (foreach (lambda (triple)
;		 (let ((coset (list-ref triple 0))
;		       (gen (list-ref triple 1))
;		       (entry (list-ref triple 2)))
;		   (vector-set! schreier-vec entry gen)
;		   (vector-set! backlink-vec entry coset)))
;	       def-list)
;      (list screier-vec backlink-vec))))
