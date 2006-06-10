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
(define-structure
  (group-presentation
   (constructor %create-group-presentation)
   (conc-name gp:))
  (gen-num-table #f read-only #t)
  (relations-list #f read-only #t)
  (generator-symbols #f read-only #t)
  (reflect-proc #f read-only #t))

(define (make-group-presentation generators relations-list #!optional reflect-proc)
  (define (initialize-inverse-table! gen-inverse-table)
    (for-each 
     (lambda (gen-symbol)
       (1d-table/put! gen-inverse-table gen-symbol (inverse-symbol gen-symbol))
       (1d-table/put! gen-inverse-table (inverse-symbol gen-symbol) gen-symbol))
     generators))
  (define (check-relations relations-list gen-inverse-table)
    (for-each
     (lambda (relation)
       (for-each
	(lambda (letter)
	  (assert (1d-table/get gen-inverse-table letter #f)
		  "MAKE-PRESENTATION: not a generator --"
		  letter))
	relation))
     relations-list))
  (if (default-object? reflect-proc) (set! reflect-proc (lambda v v))) ; dummy proc
  (let ((gen-inverse-table (make-1d-table)))
    (initialize-inverse-table! gen-inverse-table)
    (check-relations relations-list gen-inverse-table)
    (let ((relation-objs (map make-group-relation relations-list)))
      (%create-group-presentation
       gen-inverse-table
       relation-objs
       generators
       reflect-proc))))

(define (gp:find-gen-num gp letter)
  (1d-table/get (gp:gen-num-table gp) letter #f))

(define (gp:inv-gen gp gen-symbol)
  (let ((invtable (gp:gen-num-table gp)))
    (1d-table/get invtable gen-symbol #f)))
