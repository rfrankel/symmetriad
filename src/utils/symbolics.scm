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

;; TODO Is this correct?  Why wasn't it here before?
(declare (usual-integrations))

;;; Utilities for dealing with symbolic computation

;; A utility for substituting values in: 
(define (substitute-multiple expression dictionary)
  (define (walk e)
    (if (pair? e)
	(cons (walk (car e)) (walk (cdr e)))
	(if (vector? e) 
	    ((vector-elementwise walk) e)
	    (let ((v (assoc e dictionary)))
	      (if v
		  (cdr v)
		  e)))))
  (walk expression))

;; Custom environments for evaluating 
;; our symbolic expressions:
;; Doesn't actually work at the moment, but is elegant and fixable in theory.
(define (symbol-environment-maker)
  (let ((e (extend-ic-environment generic-environment)))
    (let ((d (lambda (name value)
	       (local-assignment e name value))))
      (d 'sqrt2 '(sqrt 2))
      (d 'sqrt3 '(sqrt 3))
      (d 'sqrt5 '(sqrt 5))
      (d 'cospi/5 'cospi/5)
      (d 'sinpi/5 'sinpi/5)
      (d ':pi ':pi))
    e))

(define symbol-env (symbol-environment-maker))

(define (answer-environment-maker)
  (let ((e (extend-ic-environment generic-environment)))
    (let ((d (lambda (name value)
	       (local-assignment e name value))))
      (d 'sqrt2 (sqrt 2))
      (d 'sqrt3 (sqrt 3))
      (d 'sqrt5 (sqrt 5))
      (d 'cospi/5 (/ (+ (sqrt 5) 1) 4))
      (d 'sinpi/5 (* (/ 1 2) (sqrt (/ (- 5 (sqrt 5)) 2))))
      (d ':pi :pi))
    e))

(define answer-env (answer-environment-maker))

(define symbol-dict
  (list (cons 'sqrt2 (sqrt 2))
	(cons 'sqrt3 (sqrt 3))
	(cons 'sqrt5 (sqrt 5))
	(cons 'cospi/5 (/ (+ (sqrt 5) 1) 4))
	(cons 'sinpi/5 (* (/ 1 2) (sqrt (/ (- 5 (sqrt 5)) 2))))
	))

(define sqrt-rules
  (rule-system
   ((expt sqrt2 (? b even?))
    none
    (expt 2 (/ (: b) 2)))
   ((expt sqrt2 (? b odd?))
    none
    (* sqrt2 (expt 2 (/ (- (: b) 1) 2))))
   ((expt sqrt3 (? b even?))
    none
    (expt 3 (/ (: b) 2)))
   ((expt sqrt3 (? b odd?))
    none
    (* sqrt3 (expt 3 (/ (- (: b) 1) 2))))
   ((expt sqrt5 (? b even?))
    none
    (expt 5 (/ (: b) 2)))
   ((expt sqrt5 (? b odd?))
    none
    (* sqrt5 (expt 5 (/ (- (: b) 1) 2))))
   ))
