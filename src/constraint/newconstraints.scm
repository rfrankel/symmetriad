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

;;;;            Simple Constraint Propagator -- GJS
;;; This constraint propagator is a simple deduction engine intended
;;; to be used with the Elementary TMS. 

(declare (usual-integrations))

#|
A constraint network is a set of connectors, interconnected by
constraints.  We say that the connectors participate in the
constraints. A connector may or may not hold a value.  When a
connector is given a value the constraints it participates in are
awakened.  A constraint may use this information to deduce values
for other connectors that participate in it, or it may notice a
contradiction.       

That a connector holds a particular value is represented by a node in
the TMS.  If the node is "in" then the connector is actually believed
to hold that value.  The datum of the TMS node is the statement that
the connector holds the value.  The justification of the TMS node is
the constraint that assigned it (the "reason") and the TMS nodes
asserting the values of the connectors that were used to deduce the
assignment (the "antecedents").


                     Interface procedures

The name of the constraint network is a character string used to
prefix its TMS nodes.

(define (create-constraint-network name) ...)


Each connector in the constraint network has a name in that network,
by which it can be referenced.

(define (create-connector constraint-network connector-name) ...)


A constraint has a name, by which it can be referenced.  It has
participant connectors, such that if any participant is given a value
the handler procedure is called with its name and the list of all its
participants. 

(define (create-constraint constraint-network
			   constraint-name
			   handler-procedure
			   list-of-participants) 
  ...)

The procedure of is used to access a connector or a constraint in a
constraint network.

(define (of name constraint-network) ...)
|#


;;; Implementation

;; The "{connector|constraint}-names" part of 
;; this structure is an association list  connecting
;; names of connector or constraint to the object.

(define-structure (constraint-network
		   (print-procedure 
		    (standard-unparser-method 
		     'CONSTRAINT-NETWORK
		     (lambda (cn port)
		       (write-string " " port)
		       (write `(named ,(cn/name cn)) port))))
		   (constructor %create-constraint-network
				(name tms))
		   (conc-name cn/))		
  (name #f read-only #t)
  (tms #f read-only #t)
  (to-be-awakened '())
  (interesting-variables '())
  (connectors '())
  (connector-names '())  ; this will be an association list
  (constraints '())
  (constraint-names '()) ; this will also be an association list
  (probes '())           ; probes for debugging purposes
  (contradictions '()))

(define (create-constraint-network name)
  (let ((tms (create-tms (string-append name "P")
			 node-asserted
			 node-retracted))
	(sname (string->symbol name)))
    (%create-constraint-network sname tms)))
     
;;; Important properties

(define (cn-tms cn)
  (cn/tms cn))

(define (cn-to-be-awakened cn)
  (cn/to-be-awakened cn))

(define (cn-set-to-be-awakened! cn tba)
  (set-cn/to-be-awakened! cn tba))

(define (cn-connectors cn)
  (cn/connectors cn))

(define (cn-set-connectors! cn conns)
  (set-cn/connectors! cn conns))

;; New proc of mine (RF) which I think is easier to use
(define (cn-add-connector! cn conn)
  (assert (connector? conn))
  (set-cn/connectors! cn (eq-set/adjoin conn (cn/connectors cn))))

;; New procedures for the connector-names slot
(define (cn-connector-names cn)
  (cn/connector-names cn))

(define (cn-set-connector-names! cn conns-alist)
  (assert (alist? conns-alist))
  (set-cn/connector-names! cn conns-alist))

(define (cn-add-connector-name-pair! cn pair)
  (assert (and (symbol? (car pair)) (connector? (cdr pair))))
  (set-cn/connector-names! cn (eq-set/adjoin pair 
				    (cn/connector-names cn))))

(define (cn-constraints cn)
  (cn/constraints cn))

(define (cn-set-constraints! cn consts)
  (set-cn/constraints! cn consts))


;; New utility procs analogous to the ones above
(define (cn-add-constraint! cn const)
  (assert (constraint? const))
  (set-cn/constraints! cn (eq-set/adjoin const (cn/constraints cn))))

;; New procedures for the constraint-names slot
(define (cn-constraint-names cn)
  (cn/constraint-names cn))

(define (cn-set-constraint-names! cn const-alist)
  (assert (alist? const-alist))
  (set-cn/constraint-names! cn const-alist))

(define (cn-add-constraint-name-pair! cn pair)
  (assert (and (symbol? (car pair)) (constraint? (cdr pair))))
  (set-cn/constraint-names! cn (eq-set/adjoin pair 
				    (cn/constraint-names cn))))


(define (cn-contradictions cn)
  (cn/contradictions cn))

(define (cn-set-contradictions! cn contrs)
  (set-cn/contradictions! cn contrs))

(define-structure (connector
		   (print-procedure 
		    (standard-unparser-method 
		     'CONNECTOR
		     (lambda (conn port)
		       (write-string " " port)
		       (write `(named ,(connector/name conn)) port)
		       (write-string " connects" port)
		       (if (eq? (length (connector/constraints conn)) 0) 
			   (write-string " nothing yet." port)
			   (for-each (lambda (constraint) 
				       (write-string " " port)
				       (write (constraint/name constraint) port))
				     (connector/constraints conn))))))
		   (constructor %create-connector
				(name parent constraint-network))
		   (conc-name connector/))		
  (name #f read-only #t)
  (parent #f read-only #t)
  (constraint-network #f read-only #t)
  (all-assignments '())  ; associated tms nodes   
  (constraints '()))    ; constraints particpated in

(define (create-connector cn name)
  (assert (constraint-network? cn))
  (let ((me (%create-connector name cn cn)))
     (cn-add-connector! cn me)
     (cn-add-connector-name-pair! cn (cons name me))
;    (set-connector/name! cn name)      ;don't do this because name read-only
;    (eq-put! me 'constraints '())	;don't do this because default already set
     (if *debugging-constraints* (add-probe me))
    me))

(define (connector-add-constraint! connector constraint)
  (set-connector/constraints! connector
			      (eq-set/adjoin constraint (connector/constraints connector))))

(define *debugging-constraints* #f)

(define-structure (constraint
		   (print-procedure 
		    (standard-unparser-method 
		     'CONSTRAINT
		     (lambda (const port)
		       (write-string " " port)
		       (write `(named ,(constraint/name const)) port)
		       (write-string " connects" port)
		       (for-each (lambda (participant) 
				   (write-string " " port)
				   (write (connector/name participant) port))
				 (constraint/participants const)))))
		   (constructor %create-constraint
				(name parent constraint-network 
				      participants registrar))
		   (conc-name constraint/))		
  (name #f read-only #t)
  (parent #f read-only #t)
  (constraint-network #f read-only #t)
  (participants #f read-only #t)
  (registrar #f read-only #t)
  (awaken (lambda () 'NOTHING-TO-DO))
  (constraint-type 'none-yet)
  (conditional #f)
  (conditional-node '())
  (participant-names '()) ;keys for the alist below
  (participant-alist '()))

;; Note deleted deta entry for conditional node linking to constraints
(define (create-constraint cn name registrar participants #!optional conditional)
  (assert (constraint-network? cn))
  (assert (for-all? participants connector?))
  (if (default-object? conditional)
      (set! conditional #f)
      (assert (tms-node? conditional)))
  (let ((me (%create-constraint name cn cn participants registrar)))
      (if conditional
	  (begin
	    (set-constraint/conditional-node! me conditional)
	    (set-tms-node/associated-object! conditional me)
	    (set-tms-node/associated-object-type! conditional 'constraint)))
      (cn-add-constraint! cn me)
      (cn-add-constraint-name-pair! cn (cons name me))
      (for-each (lambda (participant)
		  (connector-add-constraint! participant me))
		participants)
    (let ((notifier (apply (registrar me conditional) participants)))
      (set-constraint/awaken! me notifier)
      (notifier))
    me))

(define (of name cn)
  (let ((possible-connector (find-connector-by-name name cn))
	(possible-constraint (find-constraint-by-name name cn)))
    (if (eq? possible-connector 'could-not-find-it)
	possible-constraint
        possible-connector)))

(define (find-constraint-by-name constraint-name cn)
  (let ((name-alist (cn/constraint-names cn)))
    (let ((the-pair (assq constraint-name name-alist)))
      (if (and the-pair (pair? the-pair))  
          (cdr the-pair)
          'could-not-find-name))))

(define (find-connector-by-name connector-name cn)
  (let ((name-alist (cn/connector-names cn)))
    (let ((the-pair (assq connector-name name-alist)))
      (if (and the-pair (pair? the-pair))  
	  (cdr the-pair)
          'could-not-find-name)))) 


(define (signature me others conditional-node)
  (assert (constraint? me))
  (if conditional-node
      (append (cons me others) (list conditional-node))
      (cons me others)))

;;; Elementary constraint-registration procedures

;;; A constraint-registration procedure defines the behavior of a
;;; primitive constraint object.  It takes the constraint object "me"
;;; and produces a procedure that takes connectors and an optional
;;; conditional tms node.  The constraint is imposed on the given
;;; connectors.  If the optional conditional is given then the
;;; constraint is only active iff the conditional is "in".  Thus the
;;; conditional node can be considered a condition on the constraint.

(define (((constant-constraint constant) me conditional) c)
  (define (awaken-constant)
    (let ((ac (connector-assignment c)))
      (cond ((value-assigned? ac)
	     (check-coincidence (lambda () (assignment-value ac))
				(lambda () constant)
				me (list ac) conditional))
	    ((not conditional)
	     (connector-set-value! c constant
	       (signature me '() conditional)))
	    ((node-in? conditional)
	     (connector-set-value! c constant
	       (signature me '() conditional)))
	    (else
	     'NOTHING-TO-DO))))
  (assert (constraint? me))
  (assert (connector? c))
  (set-constraint/constraint-type! me 'constant)
  (set-constraint/participant-names! me '(c))
  (set-constraint/participant-alist! me (list (cons 'c c)))
  (if conditional (set-constraint/conditional! me conditional))
  awaken-constant)


(define ((exp-constraint me conditional) a1 a2)
  (define (awaken-exp)
    (let ((aa1  (connector-assignment a1))
	  (aa2  (connector-assignment a2)))
      (cond ((and (value-assigned? aa1) (value-assigned? aa2))
	     (check-coincidence (lambda () (assignment-value aa1))
				(lambda () (exp (assignment-value aa2)))
				me (list aa1 aa2) conditional))
	    ((or (not conditional) (node-in? conditional))
	     (cond ((value-assigned? aa2)
		    (connector-set-value! a1 (exp (assignment-value aa2))
		      (signature me (list aa2) conditional)))
		   ((value-assigned? aa1)
		    (connector-set-value! a2 (log (assignment-value aa1))
		      (signature me (list aa1) conditional)))
		   (else 'NOTHING-TO-DO)))
	    (else 'NOTHING-TO-DO))))
  (assert (constraint? me))
  (assert (connector? a1))
  (assert (connector? a2))
  (set-constraint/constraint-type! me 'exp)
  (set-constraint/participant-names! me '(a1 a2))
  (set-constraint/participant-alist! me (list (cons 'a1 a1)
					      (cons 'a2 a2)))
  (if conditional (set-constraint/conditional! me conditional))
  awaken-exp)



(define ((equality-constraint me conditional) a1 a2)
  (define (awaken-equality)
    (let ((aa1  (connector-assignment a1))
	  (aa2  (connector-assignment a2)))
      (cond ((and (value-assigned? aa1) (value-assigned? aa2))
	     (check-coincidence (lambda () (assignment-value aa1))
				(lambda () (assignment-value aa2))
				me (list aa1 aa2) conditional))
	    ((or (not conditional) (node-in? conditional))
	     (cond ((value-assigned? aa1)
		    (connector-set-value! a2 (assignment-value aa1)
		      (signature me (list aa1) conditional)))
		   ((value-assigned? aa2)
		    (connector-set-value! a1 (assignment-value aa2)
		      (signature me (list aa2) conditional)))
		   (else 'NOTHING-TO-DO)))
	    (else 'NOTHING-TO-DO))))
  (assert (constraint? me))
  (assert (connector? a1))
  (assert (connector? a2))
  (set-constraint/constraint-type! me 'equality)
  (set-constraint/participant-names! me '(a1 a2))
  (set-constraint/participant-alist! me (list (cons 'a1 a1)
					      (cons 'a2 a2)))
  (if conditional (set-constraint/conditional! me conditional))
  awaken-equality)


(define ((negate-constraint me conditional) a1 a2)
  (define (awaken-negate)
    (let ((aa1  (connector-assignment a1))
	  (aa2  (connector-assignment a2)))
      (cond ((and (value-assigned? aa1) (value-assigned? aa2))
	     (check-coincidence (lambda () (assignment-value aa1))
				(lambda () (- (assignment-value aa2)))
				me (list aa1 aa2) conditional))
	    ((or (not conditional) (node-in? conditional))
	     (cond ((value-assigned? aa1)
		    (connector-set-value! a2 (- (assignment-value aa1))
		      (signature me (list aa1) conditional)))
		   ((value-assigned? aa2)
		    (connector-set-value! a1 (- (assignment-value aa2))
		      (signature me (list aa2) conditional)))
		   (else 'NOTHING-TO-DO)))
	    (else 'NOTHING-TO-DO))))
  (assert (constraint? me))
  (assert (connector? a1))
  (assert (connector? a2))
  (set-constraint/constraint-type! me 'negate)
  (set-constraint/participant-names! me '(a1 a2))
  (set-constraint/participant-alist! me (list (cons 'a1 a1)
					      (cons 'a2 a2)))
  (if conditional (set-constraint/conditional! me conditional))
  awaken-negate)

(define ((adder-constraint me conditional) sum a1 a2)
  (define (awaken-adder)
    (let ((aa1  (connector-assignment a1))
	  (aa2  (connector-assignment a2))
	  (asum (connector-assignment sum)))
      (if (value-assigned? asum)
	  (if (value-assigned? aa1)
	      (if (value-assigned? aa2)
		  (check-coincidence (lambda ()
				       (assignment-value asum))
				     (lambda ()
				       (+ (assignment-value aa1)
					  (assignment-value aa2)))
				     me (list asum aa1 aa2) conditional)
		  (if (or (not conditional) (node-in? conditional))
		      (connector-set-value! a2
			(- (assignment-value asum) (assignment-value aa1))
			(signature me (list asum aa1) conditional))
		      'NOTHING-TO-DO))
	      (if (value-assigned? aa2)
		  (if (or (not conditional) (node-in? conditional))
		      (connector-set-value! a1
		        (- (assignment-value asum) (assignment-value aa2))
			(signature me (list asum aa2) conditional))
		      'NOTHING-TO-DO)))
	  (if (and (value-assigned? aa1) (value-assigned? aa2))
	      (if (or (not conditional) (node-in? conditional))
		  (connector-set-value! sum
		    (+ (assignment-value aa1) (assignment-value aa2))
		    (signature me (list aa1 aa2) conditional))
		  'NOTHING-TO-DO)))))
  (assert (constraint? me))
  (assert (connector? sum))
  (assert (connector? a1))
  (assert (connector? a2))
  (set-constraint/constraint-type! me 'adder)
  (set-constraint/participant-names! me '(sum a1 a2))
  (set-constraint/participant-alist! me (list (cons 'sum sum)
					      (cons 'a1 a1)
					      (cons 'a2 a2)))
  (if conditional (set-constraint/conditional! me conditional))
  awaken-adder)

(define ((multiplier-constraint me conditional) product m1 m2)
  (define (awaken-multiplier)
    (let ((am1  (connector-assignment m1))
	  (am2  (connector-assignment m2))
	  (aproduct (connector-assignment product)))
      (cond ((value-assigned? aproduct)
	     (cond ((value-assigned? am1)
		    (cond ((value-assigned? am2)
			   (check-coincidence
			    (lambda () (assignment-value aproduct))
			    (lambda () (* (assignment-value am1)
					  (assignment-value am2)))
			    me (list aproduct am1 am2) conditional))
			  ((or (not conditional) (node-in? conditional))
			   (if (= (assignment-value am1) 0)
			       (if (= (assignment-value aproduct) 0)
				   'NOTHING-TO-DO
				   (signal-contradiction me
							 (list am1 aproduct)))
			       (connector-set-value! m2
				 (/ (assignment-value aproduct)
				    (assignment-value am1))
				 (signature me (list aproduct am1)
					    conditional))))
			  (else 'NOTHING-TO-DO)))
		   ((value-assigned? am2)
		    (if (or (not conditional) (node-in? conditional))
			(if (= (assignment-value am2) 0)
			    (if (= (assignment-value aproduct) 0)
				'NOTHING-TO-DO
				(signal-contradiction me (list am2 aproduct)))
			    (connector-set-value! m1
			      (/ (assignment-value aproduct)
				 (assignment-value am2))
			      (signature me (list aproduct am2) conditional)))
			'NOTHING-TO-DO))
		   (else 'NOTHING-TO-DO)))
	  ((and conditional (not (node-in? conditional)))
	   'NOTHING-TO-DO)
	  ((and (value-assigned? am1) (value-assigned? am2))
	   (connector-set-value! product
	     (* (assignment-value am1) (assignment-value am2))
	     (signature me (list am1 am2) conditional)))
	  ((and (value-assigned? am1) (= (assignment-value am1) 0))
	   (connector-set-value! product 0
	     (signature me (list am1) conditional)))
	  ((and (value-assigned? am2) (= (assignment-value am2) 0))
	   (connector-set-value! product 0
	     (signature me (list am2) conditional)))
	  (else 'NOTHING-TO-DO))))
  (assert (constraint? me))
  (assert (connector? product))
  (assert (connector? m1))
  (assert (connector? m2))
  (set-constraint/constraint-type! me 'multiplier)
  (set-constraint/participant-names! me '(product m1 m2))
  (set-constraint/participant-alist! me (list (cons 'product product)
					      (cons 'm1 m1)
					      (cons 'm2 m2)))
  (if conditional (set-constraint/conditional! me conditional))
  awaken-multiplier)


;;; This constraint is satisfied iff (value(a1) R value(a2)).
;; Avoiding this because I don't want to deal with "plunks"
#|
(define (((predicate-constraint R) me conditional) a1 a2)
  (define (awaken-predicate)
    (let ((aa1  (connector-assignment a1))
	  (aa2  (connector-assignment a2)))
      (cond ((and (value-assigned? aa1) (value-assigned? aa2))
	     (let ((val (R (assignment-value aa1) (assignment-value aa2))))
	       (cond ((eq? val #t)
		      (if conditional
			  (if (node-in? conditional)
			      'NOTHING-TO-DO
			      (careful-justify! conditional me (list aa1 aa2)))
			  'NOTHING-TO-DO))
		     ((eq? val #f)
		      (if conditional
			  (if (node-in? conditional)
			      (signal-contradiction me
						    (cons conditional
							  (list aa1 aa2)))
			      'NOTHING-TO-DO)
			  (signal-contradiction me (list aa1 aa2))))
		     ((or conditional (node-in? conditional))
		      (display "; ")
		      (write (name-of me))
		      (display "? ")
		      (pp (list (assignment-value aa1)
				(assignment-value aa2))))
		     (else 'NOTHING-TO-DO))))
	    ((and (or (value-assigned? aa1) (value-assigned? aa2))
		  (eq-get (eq-get me 'constraint-network) 'aggressive-mode))
	     ;; Post this.
	     (if (value-assigned? aa1)
		 (connector-plunk! a2 me)
		 (connector-plunk! a1 me)))
	    (else
	     'NOTHING-TO-DO))))
  (assert (constraint? me))
  (assert (connector? a1))
  (assert (connector? a2))
  (eq-put! me 'constraint-type 'predicate)
  (let ((cn (eq-get me 'constraint-network)))
    (eq-put! cn 'predicates
	     (cons me (eq-get cn 'predicates))))
  (eq-put! me 'participant-names '(a1 a2))
  (eq-put! me 'a1 a1)
  (eq-put! me 'a2 a2)
  (if conditional (eq-put! me 'conditional conditional))
  awaken-predicate)

(define ((safe-predicate R) x y)
  (if (and (number? x) (number? y))
      (R x y)
      'Do-not-know))

(define (connector-plunk! connector me)
  (let ((old-plunk (eq-get connector 'plunk-variable)))
    (if old-plunk
	(connector-assume-value! connector old-plunk)
	(let ((plunk (generate-uninterned-symbol 'x)))
	  (eq-put! connector 'plunk-variable plunk)
	  (eq-put! plunk 'plunk-connector connector)
	  (connector-assume-value! connector plunk))))
  (let ((cn (eq-get me 'constraint-network)))
    (eq-put! cn 'plunked-connectors
	     (cons connector (eq-get cn 'plunked-connectors)))))

|#

(define ((derivative-constraint me conditional) a1 a2)
  (define (awaken-derivative)
    (let ((aa1  (connector-assignment a1))
	  (aa2  (connector-assignment a2)))
      (cond ((and (value-assigned? aa1) (value-assigned? aa2))
	     (check-coincidence (lambda () (assignment-value aa1))
				(lambda () (derivative (assignment-value aa2)))
				me (list aa1 aa2) conditional))
	    ((or (not conditional) (node-in? conditional))
	     (cond ((value-assigned? aa2)
		    (connector-set-value! a1 (derivative (assignment-value aa2))
		      (signature me (list aa2) conditional)))
		   #|;;To be added later
		   ((value-assigned? aa1)
		    (connector-set-value! a2 (integral (assignment-value aa1))
		      (signature me (list aa1) conditional)))
		   |#
		   (else 'NOTHING-TO-DO)))
	    (else 'NOTHING-TO-DO))))
  (assert (constraint? me))
  (assert (connector? a1))
  (assert (connector? a2))
  (eq-put! me 'constraint-type 'derivative)
  (eq-put! me 'participant-names '(a1 a2))
  (eq-put! me 'a1 a1)
  (eq-put! me 'a2 a2)
  (if conditional (eq-put! me 'conditional conditional))
  awaken-derivative)

(define (create-logical-constraint cn name registrar participants)
  (assert (constraint-network? cn))
  (assert (for-all? participants connector?))
  (let ((me (%create-constraint name cn cn participants registrar)))
      (cn-add-constraint! cn me)
      (cn-add-constraint-name-pair! cn (cons name me))
      (for-each (lambda (participant)
		  (connector-add-constraint! participant me))
		participants)
    (let ((notifier (apply (registrar me conditional) participants)))
      (set-constraint/awaken! me notifier)
      (notifier))
    me))

(define ((not-both me) tms-node1 tms-node2)
  (define (awaken)
    (if (and (node-in? tms-node1) (node-in? tms-node2))
	(signal-contradiction me (list tms-node1 tms-node2))))
  (assert (constraint? me))
  (assert (tms-node? tms-node1))
  (assert (tms-node? tms-node2))
  (set-constraint/constraint-type! me 'not-both)
  (set-constraint/participant-names! me '(node1 node2))
  (set-constraint/participant-alist! me
   (list (cons 'node1 tms-node1)
	 (cons 'node2 tms-node2)))
  awaken)

(define ((and-constraint me) tms-result-node tms-node1 tms-node2)
  (define (awaken)
    (cond ((and (node-in? tms-node1)
		(node-in? tms-node2)
		(not (node-in? tms-result-node)))
	   (node-add-justification! tms-result-node
				    (list me tms-node1 tms-node2)))
	  ((node-in? tms-result-node)
	   (if (not (node-in? tms-node1))
	       (node-add-justification! tms-node1 (list me tms-result-node)))
	   (if (not (node-in? tms-node2))
	       (node-add-justification! tms-node2 (list me tms-result-node))))
	  (else 'NOTHING-TO-DO)))
  (assert (constraint? me))
  (assert (tms-node? tms-result-node))
  (assert (tms-node? tms-node1))
  (assert (tms-node? tms-node2))
  (set-constraint/constraint-type! me 'and)
  (set-constraint/participant-names! me '(result-node node1 node2))
  (set-constraint/participant-alist! me
   (list (cons 'result-node tms-result-node)
         (cons 'node1 tms-node1)
	 (cons 'node2 tms-node2)))
  awaken)


;;; If a connector holds a value then there is a TMS node representing
;;; that proposition.  This TMS node is associated with the connector
;;; as an assignment of the connector that is IN.

(define (connector-all-assignments connector)
   (connector/all-assignments connector))

(define (connector-set-all-assignments! connector assignments)
   (set-connector/all-assignments! connector assignments))

(define (connector-assignment connector)
  (let ((in-assign
	 (keep-matching-items (connector-all-assignments connector)
	   node-in?)))
    ;(assert (or (null? inas) (null? (cdr inas)))) ; should be at most one
    (if (null? in-assign) #f (car in-assign))))

(define (value-assigned? assignment)
  assignment)

(define (assignment-value assignment)
  (node-datum assignment))

(define (connector-set-value! connector value support)
  (define (doit a)
    (if (symbol? support)		;premise
	(tms-justify-with-premise a support)
	(careful-justify! a (car support) (cdr support)))
    a)
  (let ((a (connector-assignment connector)))
    (if (value-assigned? a)
	(if (values-equal? (assignment-value a) value)
	    (doit a)
	    #f)
	(let* ((aas (connector-all-assignments connector))
	       (as
		(keep-matching-items aas
		  (lambda (a)
		    (values-equal? (assignment-value a) value)))))
	  (assert (or (null? as) (null? (cdr as)))) ; should be at most one
	  (if (null? as)		; no assignments to value
	      (let* ((tms (cn-tms (connector/constraint-network connector)))
		     (new-node (tms-create-node tms value)))
		(set-tms-node/associated-object! new-node connector)
		(set-tms-node/associated-object-type! new-node 'connector)
		(connector-set-all-assignments! connector (cons new-node aas))
		(doit new-node))
	      (doit (car as)))))))

(define (values-equal? v1 v2)
  (and (number? v1) (number? v2)
       (close-enough? v1 v2 *setting-tolerance* *setting-scale*)))

(define *setting-tolerance* 1.0e-14)
(define *setting-scale* 1.0e-22)

(define (check-coincidence lhs rhs me participants conditional)
  (if (not (I-am-responsible? me participants conditional))
      (check-equal? (lhs) (rhs)
		    (lambda ()		; tautology 
		      (if (and conditional (not (node-in? conditional)))
			  (careful-justify! conditional me participants)
			  'OK))
		    (lambda ()		; contradiction
		      (if (or (not conditional) (node-in? conditional))
			  (signal-contradiction me
						(if conditional
						    (cons conditional
							  participants)
						    participants))
			  'OK))
		    ;; not determined
		    (post-equation me participants conditional))
      'OK))

; This is confusing: to understand it, node that node-in?
; is actually a selector, which pull out the contents 
; of the node-in slot from each support. The contents
; will have a justification with the form 
; the reason: a constraint 
; the supports: various collections of tms nodes.
; Also note that "me" is a constraint. 
; so the keep-matching-items is looking for participants
; (that is, tms nodes) which have the given constraint 
; as justification. So my-justs is any justification of 
; any participant which happens to mention that particular
; constraint as its reason. 

(define (I-am-responsible? me participants conditional)
  (let ((supports
	 (if conditional (cons conditional participants) participants)))
    (let ((my-justs
	   (keep-matching-items (map node-in? supports)
	     (lambda (j)
	       (and j (pair? j)
		    (eq? (justification-reason j) me))))))
      (and (not (null? my-justs))
	   (forall (lambda (j)
		     (eq-set/subset? (justification-antecedents j) supports))
		   my-justs)))))

(define (check-equal? v1 v2 tautology contradiction I-dunno)
  (if (and (number? v1) (number? v2))
      (if (close-enough? v1 v2 *contradiction-tolerance* *contradiction-scale*)
	  (tautology)
	  (contradiction))
      (I-dunno v1 v2)))

(define *contradiction-tolerance* 1.0e-10)
(define *contradiction-scale* 1.0e-20)


(define (careful-justify! a me participants)
  (let ((just (make-justification me participants)))
    (if (not (member just (node-justifications a)))
	(node-add-justification! a just)
	(error "node justification already here"))))

;; "me" is a constraint
(define (signal-contradiction me participants)
  (let ((candidates
	 (filter node-contradiction?
		 (reduce eq-set/intersection '()
			 (map node-consequents participants))))
	(just (make-justification me participants)))
    (cond ((null? candidates)
	   (let* ((cn (constraint/constraint-network me))
		  (c (tms-create-node (cn-tms cn) 'contradiction)))
	     (set-tms-node/associated-object! c me)
	     (set-tms-node/associated-object-type! c 'constraint)
	     ;(tms-node-set-constraint-network! c cn)
	     (node-contradiction! c)
	     (node-add-justification! c just)))
	  ((null? (cdr candidates))
	   (assert (member just (node-justifications (car candidates))))
	   'NOTHING-TO-DO)
	  (else
	   (error "Multiple contradictions?" me participants)))))

; Well, actually, there is no constraint-network slot for tms nodes 
; anymore. So we have to find it using the "associate object"
; We don't need to set the constraint network if we have an associated 
; object that carries that information. 
;(define (tms-node-set-constraint-network! node constraint-network)
;  (eq-put! node 'constraint-network constraint-network))

;(define (tms-node-constraint-network node)
;  (eq-get node 'constraint-network))

(define (tms-node-constraint-network node)
  (let ((ass-obj (tms-node/associated-object node)))
    (cond ((symbol? ass-obj) #f)
          ((constraint? ass-obj)
	   (constraint/constraint-network ass-obj))
	  ((connector? ass-obj)
	   (connector/constraint-network ass-obj))
	  (else #f))))



(define ((post-equation me participants conditional) lhs rhs)
  (if (or (not conditional) (node-in? conditional))
      (begin (display "; ")
	     (write (name-of me))
	     (display ":  Equation: 0=")
	     ;; The following line requires the entire mechanics system.
	     (pe (- lhs rhs)))))

;;; tolerance is for relative-error region; scale is for absolute error limit.

(define (close-enough? h1 h2 tolerance scale)
  (<= (magnitude (- h1 h2))
      (+ (* tolerance .5 (+ (magnitude h1) (magnitude h2)))
	 scale)))

(define (connector-assume-value! connector value)
   (let ((assignment-node
	 (connector-set-value! connector value 'assumption)))
     (if assignment-node
	(let ((premise-node (tms-node/premise-node assignment-node)))
	  (assert premise-node)
	  (node-assume! premise-node)
	  'done)
	#f)))

(define (connector-retract-value-assumption! connector)
  (let ((assignment-node (connector-assignment connector)))
    (if assignment-node
	(let ((premise-node (tms-node/premise-node assignment-node)))
	  (assert premise-node)
	  (node-retract! premise-node)
	  'done)
	#f)))

;; This seems to have a bug: It assumes the node's associated object is a 
;; list, whereas in reality it is just a single (tms?) node
(define (awaken-my-constraints node)
  (tms-node? node)
  (let ((ass-object (tms-node/associated-object node)))
    (if (and ass-object (constraint? ass-object))
        ;we node know node is a participant
	(let ((constraints ass-object))
	  (for-each (lambda (constraint)
		      (let ((cn (constraint/constraint-network constraint)))
			(cn-set-to-be-awakened! cn
			 (eq-set/adjoin 
			  (constraint/awaken constraint)
			  (cn-to-be-awakened cn)))))
		    constraints)))
    (if (and ass-object (connector? ass-object))
	; we now know node asserts connector value
	(let* ((connector ass-object)
	       (cn (connector/constraint-network connector)))
	  (cn-set-to-be-awakened! cn
	    (eq-set/union (cn-to-be-awakened cn)
			  (map (lambda (constraint)
				 (constraint/awaken constraint)) 
			       (connector/constraints connector))))))))




(define (node-asserted node)
  (if (node-contradiction? node)
      (let ((cn (tms-node-constraint-network node)))
	(cn-set-contradictions! cn (cons node (cn-contradictions cn))))
      (awaken-my-constraints node))
  'done)

(define (node-retracted node)
  (if (node-contradiction? node)
      (let ((cn (tms-node-constraint-network node)))
	(cn-set-contradictions! cn (delq node (cn-contradictions cn))))
      ;(awaken-my-constraints node)
      )
  'done)

(define (one-step cn)
  (assert (constraint-network? cn))
  (let ((todo (cn-to-be-awakened cn)))
    (cn-set-to-be-awakened! cn '())
    (for-each (lambda (awake) (awake)) todo))
  (for-each (lambda (probe-spec) ((cdr probe-spec)))
	    ;(eq-get cn 'probes)
	    (cn/probes cn))
  (for-each
   (lambda (contradiction)
     (display "; ") 
     (write-line `(Contradiction! ,contradiction)))
   (cn-contradictions cn))
  'done)

(define (propagate cn)
  (assert (constraint-network? cn))
  (let lp ((todo (cn-to-be-awakened cn)))
    (if (not (null? todo))
	(begin (cn-set-to-be-awakened! cn '())
	       (for-each (lambda (awake) (awake)) todo)
	       (lp (cn-to-be-awakened cn)))))
  (for-each (lambda (probe-spec) ((cdr probe-spec)))
	    ;(eq-get cn 'probes)
            (cn/probes cn))
  (for-each
   (lambda (contradiction)
     (display "; ") 
     (write-line `(Contradiction! ,contradiction)))
   (cn-contradictions cn))
  'done)

(define (add-probe connector)
  (define (the-probe)
    (if *printing-probes*
	(begin 
	  (display "; Probe ")
	  (write (path-name connector))
	  (let ((a (connector-assignment connector)))
	    (if (value-assigned? a)
		(begin
		  (display " = ")
		  (write (assignment-value a)))
		(display " is not assigned."))
	    (newline)))))
  (assert (connector? connector))
  (let ((cn (connector/constraint-network connector)))
     (set-cn/probes! cn 
	     (cons (cons connector the-probe)
		   (cn/probes cn))))
  (the-probe)
  'done)

(define *printing-probes* #t)

(define (remove-probe connector)
  (assert (connector? connector))
  (let ((cn (connector/constraint-network connector)))
    (set-cn/probes
	     ((list-deletor (lambda (x) (eq? (car x) connector)))
	      (cn/probes cn))))
  'done)


;(define (path-name thing)
;  (if thing
;      (let ((given (eq-get thing 'name)))
;	(if given
;	    (cons given
;		  (path-name (eq-get thing 'parent)))
;	    '()))
;      '()))

;;; OOPS -- really can't be as elegant here
(define (name-of thing)
  (get-name-of-thing thing))

(define (get-name-of-thing thing)
  (cond ((tms? thing) (tms/name thing))
	 ((tms-node? thing) (tms-node/name thing))
	 ((constraint-network? thing) (cn/name thing))
	 ((connector? thing) (connector/name thing))
	 ((constraint? thing) (constraint/name thing))))

(define (get-parent-of-thing thing)
  (cond  ((tms? thing) '())
	 ((tms-node? thing) (tms-node/parent thing))
	 ((constraint-network? thing) '())
	 ((connector? thing) (connector/parent thing))
	 ((constraint? thing) (constraint/parent thing))))


(define (path-name thing)
  (if thing
      (let ((given (get-name-of-thing thing)))
	(if given
	    (cons given
		  (path-name (get-parent-of-thing thing)))
	    '()))
      '()))



(define (declare-interesting-variables cn vars)
  ;(eq-put! cn 'interesting-variables vars)
  (set-cn/interesting-variables! cn vars)
  'done)

(define (propagate-until-interesting cn)
  (assert (constraint-network? cn))
  (let ((do-not-have-values
	 (filter (lambda (c)
		   (let ((a (connector-assignment c)))
		     (not (value-assigned? a))))
		 ;(eq-get cn 'interesting-variables)
                 (cn/interesting-variables cn))))
    (let lp ((todo (cn-to-be-awakened cn)))
      (if (not (null? todo))
	  (begin (cn-set-to-be-awakened! cn '())
		 (for-each (lambda (awake) (awake)) todo)
		 (let ((have-new-values
			(filter (lambda (c)
				  (let ((a (connector-assignment c)))
				    (value-assigned? a)))
				do-not-have-values)))
		   (if (null? have-new-values)
		       (lp (cn-to-be-awakened cn))
		       (for-each
			(lambda (connector)
			  (display "; ")
			  (write (path-name connector))
			  (let ((a (connector-assignment connector)))
			    (display " = ")
			    (write (assignment-value a)))
			  (newline))
			have-new-values)))))))
  (for-each (lambda (probe-spec) ((cdr probe-spec)))
	    ;(eq-get cn 'probes)
	    (cn/probes cn))
  (for-each
   (lambda (contradiction)
     (display "; ") 
     (write-line `(Contradiction! ,contradiction)))
   (cn-contradictions cn))
  'done)


#|
;;; For example, cf is a constraint network implementing 
;;;    9 * C = 5 * (F - 32)   --  Figure 3.28 SICP

(define cf (create-constraint-network "CF"))

(create-connector cf 'C)
(create-connector cf 'F)

(create-connector cf 'X)
(create-connector cf 'Y)
(create-connector cf 'U)
(create-connector cf 'V)
(create-connector cf 'W)

(create-constraint cf 'M1 multiplier-constraint
		   (list (of 'U cf)	;product
			 (of 'C cf)	;factor
			 (of 'W cf)))	;factor

(create-constraint cf 'M2 multiplier-constraint
		   (list (of 'U cf)
			 (of 'V cf)
			 (of 'X cf)))

(create-constraint cf 'A1 adder-constraint
		   (list (of 'F cf)	;sum
			 (of 'V cf)	;addend
			 (of 'Y cf)))	;addend

(create-constraint cf 'C1 (constant-constraint 9)
		   (list (of 'W cf)))

(create-constraint cf 'C2 (constant-constraint 5)
		   (list (of 'X cf)))

(create-constraint cf 'C3 (constant-constraint 32)
		   (list (of 'Y cf)))

(add-probe (of 'C cf))
; Probe (C CF) is not assigned.
;Value: done

(add-probe (of 'F cf))
; Probe (F CF) is not assigned.
;Value: done

(connector-assume-value! (of 'C cf) 37.0)
;Value: done

(one-step cf)
; Probe (F CF) is not assigned.
; Probe (C CF) = 37.
;Value: done

(one-step cf)
; Probe (F CF) = 98.6
; Probe (C CF) = 37.
;Value: done

(connector-retract-value-assumption! (of 'C cf))
;Value: done

(one-step cf)
; Probe (F CF) is not assigned.
; Probe (C CF) is not assigned.
;Value: done

(connector-assume-value! (of 'F cf) 32.0)
;Value: done

(one-step cf)
; Probe (F CF) = 32.
; Probe (C CF) is not assigned.
;Value: done

(one-step cf)
; Probe (F CF) = 32.
; Probe (C CF) is not assigned.
;Value: done

;(one-step cf)
; Probe (F CF) = 32.
; Probe (C CF) = 0.
;Value: done

(connector-assume-value! (of 'C cf) 25.)
;Value: done

(one-step cf)
; My values:
; Probe (F CF) = 32.
; Probe (C CF) = 25.
; (Contradiction! #[TMS-NODE 40 (named CFP15) (has datum contradiction)])
;Value: done

; Test values:
; Probe (F CF) = 32.
; Probe (C CF) = 25.
; (Contradiction! #[uninterned-symbol 87 CFP16])
;Value: done

(pp (cn-contradictions cf))
;My value:
(#[TMS-NODE 40 (named CFP15) (has datum contradiction)])

;Test value:
;(CFP16)

(explain (car (cn-contradictions cf)))
;(CFP16 contradiction found by (M1 CF) (CFP12 CFP14 CFP1))
;(CFP12 (U CF) = 0. set by (M2 CF) (CFP11 CFP2))
;(CFP14 (C CF) = 25. set by (()) (CFP15))
;(CFP1 (W CF) = 9 set by (C1 CF) ())
;(CFP11 (V CF) = 0. set by (A1 CF) (CFP9 CFP3))
;(CFP2 (X CF) = 5 set by (C2 CF) ())
;(CFP15 assumption PREMISE)
;(CFP9 (F CF) = 32. set by (()) (CFP10))
;(CFP3 (Y CF) = 32 set by (C3 CF) ())
;(CFP10 assumption PREMISE)
;Value: QED

(connector-retract-value-assumption! (of 'F cf))
;Value: done

(one-step cf)
; Probe (F CF) is not assigned.
; Probe (C CF) = 25.
;Value: done

(one-step cf)
; Probe (F CF) is not assigned.
; Probe (C CF) = 25.
;Value: done

(one-step cf)
; Probe (F CF) = 77.
; Probe (C CF) = 25.
;Value: done
|#





