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

;;; Structure to hold information about coxeter graphics. 

(define-structure (cox-geometry
		   (print-procedure 
		    (standard-unparser-method 
		     'COX-GEOMETRY
		     (lambda (cg port)
		       (write-string " " port)
		       (write `(named ,(cxg/name cg)) port))))
		   (constructor %create-cox-geometry
				(name gen-list chamber-list 
				      mult-table roots-proc group-net))
		   (conc-name cxg/))		
  (name #f read-only #t)
  (gen-list '() read-only #t)   ;list of symbols for generators
  (chamber-list '() read-only #t)  ;list of chambers in group
  (mult-table #f read-only #t) ;; two-d-table
  (roots-proc #f read-only #t) ;; two-d-table
  (group-net #f read-only #t)
  (gen-coords '()) ;; alist with root generators keyed to values
  (roots-symb '()) ;; same as roots-proc but with symbols
  (roots-exact '()) ;; same as roots-symb but with exact numbers
  (roots-inexact '()) ;; same as roots-symb but with inexact numbers
  (done-cache (make-equal-hash-table) read-only #t) ;; Cache of sym-objs built
  ) 


;; a utility to list the root symbols.
(define (root-symbols gen-symbols)
  (map 
   (lambda (gen)
     (symbol-append  gen '_root))
   gen-symbols))

(define (create-cox-geometry name grp-net)
  (let ((mult-table (gn:mult-table grp-net))
	(gen-list (gp:generator-symbols
		   (gn:presentation grp-net)))
	(chamber-list (gn:coset-list grp-net))
	(geom (gn:geometry grp-net))
	(sname (string->symbol name)))
    (%create-cox-geometry sname gen-list chamber-list mult-table geom grp-net)))

(define (cxg:relations cox-g)
  (gp:relations-list (gn:presentation (cxg/group-net cox-g))))

; The idea is to preserve the order wrt the order of the root symbols
(define (cxg:chamber-root-list cox-g chamber)
  ;(map cdr (two-d-get-alist-x (cxg/roots-inexact cox-g) chamber))
  (map (lambda (gen-symb)
	 (two-d-get (cxg/roots-inexact cox-g) chamber gen-symb))
       (root-symbols (cxg/gen-list cox-g)))
  )

(define (cxg:clear-cache! cox-geometry)
  (hash-table/clear! (cxg/done-cache cox-geometry)))

(define (initialize-root-geometry cox-g geometry-list)
  (let ((gen-list (cxg/gen-list cox-g)))
    (set-cxg/gen-coords! 
     cox-g
     (map
      (lambda (gen-symbol geom)
	(cons gen-symbol geom))
      gen-list geometry-list))))

(define (coset->index cox-g)
  (let ((chamber-number-hash (index-map (cxg/chamber-list cox-g))))
    (lambda (chamber-symbol)
      (hash-table/get chamber-number-hash chamber-symbol #f))))

;;utility for fill-in-roots
;two-d-table-map is linear in the size of the table
(define (apply-table proc-table coord-list) 
  (two-d-table-map 
   proc-table 
   (lambda (entry) 
     (unquote (simplify 
	       (sqrt-rules 
		(simplify (entry coord-list))))))))

;; another utility for inexact answers
(define (apply-inexact proc-table coord-list) 
  (two-d-table-map 
   proc-table 
   (lambda (entry) 
     (unquote (simplify 
	       (substitute-multiple 
		(simplify (entry coord-list)) symbol-dict))))))

(define (fill-in-symb-roots! cox-g)
  (display "Filling in symbolic roots... ") (newline)
  (let ((gen-list (cxg/gen-list cox-g))
	(roots-procs (cxg/roots-proc cox-g)))
    (let ((symb-roots (apply-table roots-procs gen-list)))
      (set-cxg/roots-symb! cox-g symb-roots)))
  (display "done") (newline))

(define (fill-in-exact-roots! cox-g)
  (display "Filling in exact roots... ") (newline)
  (let ((gen-coords (cxg/gen-coords cox-g))
	(roots-procs (cxg/roots-proc cox-g)))
    (if (not gen-coords)
	(error "You must fill in a geometry first")
	(let ((coords-list (map cdr gen-coords)))
	  (let ((exact-roots (apply-table roots-procs coords-list)))
	    (set-cxg/roots-exact! cox-g exact-roots)))))
  (display "done") (newline))

(define (fill-in-inexact-roots! cox-g)
  (display "Filling in inexact roots... ") (newline)
  (let ((gen-coords (cxg/gen-coords cox-g))
	(roots-procs (cxg/roots-proc cox-g)))
    (if (not gen-coords)
	(error "You must fill in a geometry first")
	(let ((exact-coords-list 
	       (map (lambda (pair) (substitute-multiple (cdr pair) symbol-dict))
		    gen-coords)))
	  (let ((inexact-roots (apply-inexact roots-procs exact-coords-list)))
	    (set-cxg/roots-inexact! cox-g inexact-roots)))))
  (display "done") (newline))

(define (fill-in-root-table cox-g)
  ;(fill-in-symb-roots! cox-g)
  ;(fill-in-exact-roots! cox-g)
  (fill-in-inexact-roots! cox-g))
  
; The arguments better agree!!
(define (build-cox-geometry matrix lengths roots)
  (assert-roots-match matrix lengths roots)
  (let* ((coxsub (make-group-presentation '(s0) '((s0 s0))))
	 (cox-pres (cox-presentation matrix lengths))
	 (cox-group-net (group-network cox-pres coxsub "Autogen")))
    (gn:hlt cox-group-net)
    (assert-valid-group-net cox-group-net)
    (display "Group net appears valid.") (newline)
    (let ((cox-geom (create-cox-geometry "Autogen again" cox-group-net)))
      (initialize-root-geometry cox-geom roots)
      (fill-in-root-table cox-geom)
      cox-geom)))

(define (compute-geom-family->cox-geometry family dimension)
  (assert (geom-family? family)
	  "Object fails to be a geom-family."
	  family)
  (build-cox-geometry ((gfam/matrix family) dimension)
		      ((gfam/len family) dimension) 
		      ((gfam/roots family) dimension)))

(define geom-family->cox-geometry
  (cache-wrapper compute-geom-family->cox-geometry gfam/done-cache))
