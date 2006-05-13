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

;;;;     Elementary Truth-Maintenance System -- GJS

;;; TMS nodes represent propositions manipulated by a deduction engine. 
;;; A node is "in" if it is an assumed premise or if it has support from
;;; premises that are "in".  Otherwise the node is said to be "out".

(declare (usual-integrations))

;;; This code assumes that we are running with mechanics system.  
;;; In particular, this code depends on
;;; /usr/local/scmutils/src/general/logic-utils.  Other incidental
;;; utilities from the mechanics system that are needed are pulled
;;; into tms/extra.scm.  To run it outside of the mechanics system,
;;; load those files.  Links are in the tms directory.

;;; The LISP property list mechanism, accessed with eq-put! and
;;; eq-get, is defined in tms/eq-properties.scm.  This must be loaded.

#|
                  Interface procedures

A TMS is created with a character-string name and procedures that are
called when a node changes state from "out" to "in" or from "in" to
"out".  These procedures are typically used to enque actions to be
taken by the deduction engine.

(define (create-tms name note-in note-out) ...)

(define (tms-name tms) ...)

A node is created within a particular TMS.  It carries a datum that
describes the proposition it represents.  The datum is an arbitrary
piece of structure attached by the deduction engine.

(define (tms-create-node tms datum) ...)

(define (node-tms node) ...)

(define (node-datum node) ...)

node-in? returns the supporting justification for a node that is "in"
or #f for a node that is "out".

(define (node-in? node) ...)

A node may be marked as a contradiction.

(define (node-contradiction! node) ...)

(define (node-contradiction? node) ...)

A unjustified node (a premise) may be made "in", or it may be
retracted and made "out".

(define (node-assume! node) ...)

(define (node-retract! node) ...)

A node may be justified.  A justification has a "reason", which may be
an arbitrary structure, and a list of antecedents that this justification
depends on.  A node may have more than one justification.

(define (node-add-justification! node justification) ...)

(define (node-justifications node) ...)

(define (make-justification reason antecedents) ...)

(define (justification-reason justification) ...)

(define (justification-antecedents justification) ...)
|#


;;; The simplest TMS is just a headed list of nodes.
;;; No, actually ---
;;; trying to make a structure definition
;;; Alternate tms structure

(define-structure (tms
		   (print-procedure 
		    (standard-unparser-method 
		     'TMS
		     (lambda (tms port)
		       (write-string " " port)
		       (write `(named ,(tms/name tms)) port)
		       (write-string " " port)
		       (write `(has ,(tms/count tms) nodes)
			       port)
		       (write-string " with data:" port)
		       (for-each (lambda (node) 
				   (write-string " " port)
				   (write (tms-node/datum node) port))
				 (tms/nodes tms)))))
		   (constructor %create-tms
				(name note-in note-out))
		   (conc-name tms/))		
  (name #f read-only #t)
  (note-in #f read-only #t)
  (note-out #f read-only #t)
  (count '0)
  (nodes '()))

;;; The simplest TMS WAS just a headed list of nodes.
;(define (create-tms name note-in note-out)
;  (list '*TMS* (cons name 0) (cons note-in note-out)))

(define (create-tms name note-in note-out)
  (%create-tms name note-in note-out))

; Already defined automatically
;(define (tms? obj)
;  (and (pair? obj)
;       (eq? (car obj) '*TMS*)))

;(define (tms-name tms)
;  (caadr tms))

(define (tms-name tms)
  (tms/name tms))

;(define (tms-count tms)
;  (cdadr tms))

(define (tms-count tms)
  (tms/count tms))

;(define (tms-bump-count! tms)
;  (set-cdr! (cadr tms) (fix:+ (tms-count tms) 1)))

(define (tms-bump-count! tms)
  (set-tms/count! tms (fix:+ (tms/count tms) 1)))

;(define (tms-note-in tms)
;  (caaddr tms))

(define (tms-note-in tms)
  (tms/note-in tms))

;(define (tms-note-out tms)
; (cdaddr tms))

(define (tms-note-out tms)
  (tms/note-out tms))

;(define (tms-nodes tms)
;  (cdddr tms))

(define (tms-nodes tms)
  (tms/nodes tms))

;(define (tms-set-nodes! tms nodes)
;  (set-cdr! (cddr tms) nodes))

(define (tms-set-nodes! tms nodes)
  (set-tms/nodes! tms nodes))

;(display ";")
;(pp `(,node ,(node-datum node) ,just))

;;; trying to make a node structure definition
;; Note that a node may be associated with some object
;; (defined elsewhere) so there is a slot provided for 
;; that contigency, and also a slot to label the type
;; of associated object. In fact, in the constraint system
;; nodes are associated with either connectors or constraints.

(define-structure (tms-node 
		   (print-procedure 
		    (standard-unparser-method 
		     'TMS-NODE
		     (lambda (tms-node port)
		       (write-string " " port)
		       (write `(named ,(tms-node/id tms-node)) port)
		       (write-string " " port)
		       (write `(has datum ,(tms-node/datum tms-node)) port))))
		   (constructor %tms-create-node (id tms datum))
		   (conc-name tms-node/))		
  (id #f read-only #t)
  (tms #f read-only #t)
  (datum)
  (in #f)
  (contradiction #f)
  (justifications '())
  (consequents '())
  (premise-node '())
  (associated-object 'none-yet)
  (associated-object-type 'none-yet))  
  		   
(define (tms-create-node tms datum)
  (assert (tms? tms))
  (tms-bump-count! tms)
  (let ((id
	 (string->symbol
	  (string-append (tms-name tms)
			 (number->string (tms/count tms))))))
    (let ((new-node (%tms-create-node id tms datum)))
      (tms-set-nodes! tms (cons new-node (tms/nodes tms)))
      new-node)))

(define (tms-create-node-with-name tms symbol-name datum)
  (assert (tms? tms))
  (assert (symbol? symbol-name))
  (tms-bump-count! tms)
  (let ((new-node (%tms-create-node symbol-name tms datum)))
      (tms-set-nodes! tms (cons new-node (tms/nodes tms)))
      new-node))

(define (of-tms name tms)
  (let ((node-list (tms/nodes tms)))
    (filter 
     (lambda (node)
       (eq? (tms-node/id node) name))
     node-list)))

;;; A node is represented by a generated symbol.

;(define (tms-create-node tms datum)
;  (assert (tms? tms))
;  (tms-bump-count! tms)
;  (let ((node
;	 (string->uninterned-symbol
;	  (string-append (tms-name tms)
;			 (number->string (tms-count tms))))))
;    (eq-put! node 'datum datum)
;    (eq-put! node 'tms tms)
;    (eq-put! node 'in #f)
;    (eq-put! node 'contradiction #f)
;    (eq-put! node 'justifications '())
;    (eq-put! node 'consequents '())
;    (tms-set-nodes! tms (cons node (tms-nodes tms)))
;    node))


;;; not needed because it is automatically created 
;; by the define-structure macro
;(define (tms-node? obj)
;  (and (symbol? obj)
;       (eq-get obj 'tms)))

;(define (node-tms node)
;  (eq-get node 'tms))

(define (node-tms node)
  (tms-node/tms node))

;(define (node-datum node)
;  (assert (tms-node? node))
;  (eq-get node 'datum))

(define (node-datum node)
  (assert (tms-node? node))
  (tms-node/datum node))

;(define (node-in! node justification)
;  (assert (tms-node? node))
;  (eq-put! node 'in justification))

(define (node-in! node justification)
  (assert (tms-node? node))
  (set-tms-node/in! node justification))

;(define (node-out! node)
;  (assert (tms-node? node))
;  (eq-put! node 'in #f))

(define (node-out! node)
  (assert (tms-node? node))
  (set-tms-node/in! node #f))

;(define (node-in? node)
;  (assert (tms-node? node))
;  (eq-get node 'in))

(define (node-in? node)
  (assert (tms-node? node))
  (tms-node/in node))

;(define (node-out? node)
;  (assert (tms-node? node))
;  (not (eq-get node 'in)))

(define (node-out? node)
  (assert (tms-node? node))
  (not (tms-node/in node)))

;(define (node-contradiction! node)
;  (assert (tms-node? node))
;  (eq-put! node 'contradiction #t))

(define (node-contradiction! node)
  (assert (tms-node? node))
  (set-tms-node/contradiction! node #t))

;(define (node-contradiction? node)
;  (assert (tms-node? node))
;  (eq-get node 'contradiction))

(define (node-contradiction? node)
  (assert (tms-node? node))
  (tms-node/contradiction node))

;(define (node-justifications node)
;  (assert (tms-node? node))
;  (eq-get node 'justifications))

(define (node-justifications node)
  (assert (tms-node? node))
  (tms-node/justifications node))

;(define (node-set-justifications! node justifications)
;  (assert (tms-node? node))
;  (eq-put! node 'justifications justifications))

(define (node-set-justifications! node justifications)
  (assert (tms-node? node))
  (set-tms-node/justifications! node justifications))

;(define (node-consequents node)
;  (assert (tms-node? node))
;  (eq-get node 'consequents))

(define (node-consequents node)
  (assert (tms-node? node))
  (tms-node/consequents node))

;(define (node-set-consequents! node consequents)
;  (assert (tms-node? node))
;  (eq-put! node 'consequents consequents))

(define (node-set-consequents! node consequents)
  (assert (tms-node? node))
  (set-tms-node/consequents! node consequents))

(define (node-set-object! node associated-object)
  (assert (tms-node? node))
  (set-tms-node/associated-object! node associated-object))

(define (node-set-object-type! node associated-object-type)
  (assert (tms-node? node))
  (set-tms-node/associated-object-type! node associated-object-type))



(define (node-assume! node)
  (assert (tms-node? node))
  ;; Can only assume a premise node.
  (if (null? (node-justifications node))
      (begin (bring-node-in! node 'premise) #t)
      #f))

(define (bring-node-in! node justification)
  (node-in! node justification)
  (note-in! node)
  (for-each try-to-bring-node-in! (node-consequents node)))

(define (try-to-bring-node-in! node)
  (if (not (node-in? node))
      (for-the-first
       (lambda (justification)
	 (if (&and (map node-in? (justification-antecedents justification)))
	     (begin (bring-node-in! node justification) #t)
	     #f))
       (node-justifications node))))

(define (note-in! node)
  ((tms-note-in (node-tms node)) node))

(define (for-the-first proc lst)
  (cond ((null? lst) #f)
	((proc (car lst)) #t)
	(else (for-the-first proc (cdr lst)))))


(define (node-retract! node)
  (assert (tms-node? node))
  ;; Can only retract a premise node.
  (if (null? (node-justifications node))
      (begin
	(node-out! node)
	(note-out! node)
	(let ((outs (propagate-retractions node)))
	  (for-each try-to-bring-node-back-in! outs)
	  (for-each note-out! (filter node-out? outs)))
	#t)
      #f))

(define (propagate-retractions node)
  (let ((ins (filter node-in? (node-consequents node))))
    (for-each node-out! ins)
    (append ins
	    (apply append
		   (map propagate-retractions ins)))))


;;; The following two procedures are the same as the ones above,
;;; except for the note-in!  This is not pretty.

(define (bring-node-back-in! node justification)
  (node-in! node justification)
  (for-each try-to-bring-node-back-in! (node-consequents node)))

(define (try-to-bring-node-back-in! node)
  (if (not (node-in? node))
      (for-the-first
       (lambda (justification)
	 (if (&and (map node-in? (justification-antecedents justification)))
	     (begin (bring-node-back-in! node justification) #t)
	     #f))
       (node-justifications node))))

(define (note-out! node)
  ((tms-note-out (node-tms node)) node))

(define (node-add-justification! node justification)
  (assert (tms-node? node))
  (node-set-justifications! node
			    (cons justification
				  (node-justifications node)))
  (for-each (lambda (a)
	      (node-set-consequents! a
				     (cons node
					   (node-consequents a))))
	    (justification-antecedents justification))
  (try-to-bring-node-in! node)
  (node-in? node))


(define (make-justification reason antecedents)
  (cons reason antecedents))

(define (justification-reason justification)
  (car justification))

(define (justification-antecedents justification)
  (cdr justification))


;;; Since only unjustified nodes may be assumed and retracted it is 
;;; necessary to make such extra nodes to control contentful propositions. 

;(define (tms-justify-with-premise a-node premise-name)
;  (if (not (eq-get a-node 'premise-node))
;      (let ((p (tms-create-node (node-tms a-node) premise-name)))
;	(eq-put! a-node 'premise-node p)
;	(eq-put! p 'controlled a-node)
;	(node-add-justification! a-node
;				 (make-justification 'assumption (list p))))))

;Ignoring the "controlled" feature because I don't 
; see it used anywhere.
(define (tms-justify-with-premise a-node premise-name)
   (if (not (tms-node/premise-node a-node))
       (let ((p (tms-create-node (node-tms a-node) premise-name)))
	(set-tms-node/premise-node! a-node p)
;	(eq-put! p 'controlled a-node)
	(node-add-justification! a-node
				 (make-justification 'assumption (list p))))))

(define (tex-print-node tms-node name)
  (let ((file-name 
	 (string-append 
	  "/home/rfrankel/pc-home/context/tms" name ".tex"))
	(slsh (ascii->char 92)))
    (let ((port (open-output-file file-name))
	  (tms-id (tms-node/id tms-node))
	  (justifications (node-justifications tms-node))
	  (consequents (node-consequents tms-node)))
      (format port "~Asetupoutput[pdftex]~%" slsh)
      (format port "~Astarttext~%" slsh)
      (format port "~Aframed" slsh)
      (format port "[width=3cm, offset=5pt, align=middle]~%")
      (format port "{Node named ~A ~Ahairline~%" tms-id slsh)
      (format port "Datum: ~A ~Ahairline~%" (pp datum) slsh)
      (format port "Justifications: ~%")
      (for-each 
       (lambda (just) 
	 (format port "~A~%" (tms-node/id just)))
       justifications)
      (format port " ~Ahairline ~%" slsh)
      (format port "Consequents: ~%")
      (for-each 
       (lambda (consq) 
	 (format port "~A~%" (tms-node/id consq)))
       consequents)
      (format port "}%")
      (format port "~Astoptext~%" slsh))))

;; returns string
(define (tex-print-string tms-node)
    (let ((slsh (ascii->char 92))
	  (tms-id (tms-node/id tms-node))
	  (justifications (node-justifications tms-node))
	  (consequents (node-consequents tms-node)))
      (string-append
       (format #f "~Asetupoutput[pdftex]~%" slsh)
       (format #f "~Astarttext~%" slsh)
       (format #f "~Aframed" slsh)
       (format #f "[width=3cm, offset=5pt, align=middle]~%")
       (format #f "{Node named ~A ~Ahairline~%" tms-id slsh)
       (format #f "Datum: ~A ~Ahairline~%" (pp datum) slsh)
       (format #f "Justifications: ~%")
       (map
	(lambda (just) 
	  (format #f "~A~%" (tms-node/id just)))
	justifications)
       (format #f " ~Ahairline ~%" slsh)
       (format #f "Consequents: ~%")
       (map 
	(lambda (consq) 
	  (format #f "~A~%" (tms-node/id consq)))
	consequents)
       (format #f "}%")
       (format #f "~Astoptext~%" slsh))))


;;;; To print a proof.

(define (why? node)
  (if (node-in? node)
      (let plp ((to-print (list node)) (done '()))
	(if (null? to-print)
	    'QED
	    (let ((node (car to-print)))
	      (if (not (memq node done))
		  (let ((just (node-in? node)))
		    (cond ((not just) 'HMMMM!)
			  ((eq? just 'premise)
			   (display ";")
			   (pp `(,node ,(node-datum node) PREMISE))
			   (plp (cdr to-print)
				(cons node done)))
			  (else
			   (display ";")
			   (pp `(,node ,(node-datum node) ,just))
			   (plp (append (cdr to-print)
					(justification-antecedents just))
				(cons node done)))))
		  (plp (cdr to-print) done)))))
      #f))

#|
(define tms
  (create-tms "n"
	      (lambda (new-in)
		(display ";") (pp `(in ,new-in ,(node-datum new-in))))
	      (lambda (new-out)
		(display ";") (pp `(out ,new-out ,(node-datum new-out))))))

(define a (tms-create-node-with-name tms 'a 'A))
(define b (tms-create-node tms 'B))
(define c (tms-create-node tms 'C))
(define d (tms-create-node tms 'D))
(define e (tms-create-node tms 'E))
(define f (tms-create-node tms 'F))
(define g (tms-create-node tms 'G))
(define h (tms-create-node tms 'H))
(define i (tms-create-node tms 'I))

(node-add-justification! e (make-justification 'MP (list a b)))
(node-add-justification! e (make-justification 'X (list c)))
(node-add-justification! g (make-justification 'Y (list e)))
(node-add-justification! h (make-justification 'Z (list e f)))
(node-add-justification! f (make-justification 'W (list c d)))

(node-assume! c)
;(in n3 C)
;(in n5 E)
;(in n7 G)
;Value: #t

(node-in? c)
;Value: premise

(node-in? e)
;Value: (X #[uninterned-symbol n3])

(why? g)
;(n7 G (Y n5))
;(n5 E (X n3))
;(n3 C PREMISE)
;Value: QED

(node-retract! c)
;(out n3 C)
;(out n5 E)
;(out n7 G)
;Value: #t

(node-assume! a)
;(in n1 A)
;Value: #t

(node-assume! b)
;(in n2 B)
;(in n5 E)
;(in n7 G)
;Value: #t

(node-assume! c)
;(in n3 C)
;Value: #t

(node-assume! d)
;(in n4 D)
;(in n6 F)
;(in n8 H)
;Value: #t

(node-retract! a)
;(out n1 A)
;Value: #t

(why? h)
;(n8 H (Z n5 n6))
;(n5 E (X n3))
;(n6 F (W n3 n4))
;(n3 C PREMISE)
;(n4 D PREMISE)
;Value: QED

(node-retract! d)
;(out n4 D)
;(out n6 F)
;(out n8 H)
;Value: #t

(node-add-justification! i (make-justification 'U (list f)))

(node-add-justification! i (make-justification 'Q (list g)))
;(in n9 I)
;Value: (Q #[uninterned-symbol n7])

(node-add-justification! d (make-justification 'R (list i)))
;(in n4 D)
;(in n6 F)
;(in n8 H)
;Value: (R #[uninterned-symbol n9])

(why? h)
;(n8 H (Z n5 n6))
;(n5 E (X n3))
;(n6 F (W n3 n4))
;(n3 C PREMISE)
;(n4 D (R n9))
;(n9 I (Q n7))
;(n7 G (Y n5))
;Value: QED

(node-retract! c)
;(out n3 C)
;(out n6 F)
;(out n5 E)
;(out n9 I)
;(out n8 H)
;(out n4 D)
;(out n7 G)
;Value: #t

(node-assume! a)
;(in n1 A)
;(in n5 E)
;(in n7 G)
;(in n9 I)
;(in n4 D)
;Value: #t

(why? d)
;(n4 D (R n9))
;(n9 I (Q n7))
;(n7 G (Y n5))
;(n5 E (MP n1 n2))
;(n1 A PREMISE)
;(n2 B PREMISE)
;Value: QED

(node-assume! c)
;(in n3 C)
;(in n6 F)
;(in n8 H)
;Value: #t

(node-retract! c)
;(out n3 C)
;(out n6 F)
;(out n8 H)
;Value: #t
|#
