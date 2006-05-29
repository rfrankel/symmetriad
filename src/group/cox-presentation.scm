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

;;;;; Coxeter Groups presentations
;;;; 28 Feb 2002

(define (number->generator-symbol num)
  (symbol-append 's (string->symbol (number->string num))))

;; Takes a matrix that gives the numbers m in (s_i s_j)^m_{ij} = 1 and
;; returns a lookup table of the cosines of the angles between s_i and
;; s_j.
(define (compute-cosine-table cox-cosine-proc cox-matrix)
  (let ((the-table (make-empty-two-d-table))
	(dimension (cm:dimension cox-matrix)))
    (for-each
     (lambda (col)
       (for-each
	(lambda (row)
	  (let ((rowkey (number->generator-symbol row))
		(colkey (number->generator-symbol col))
		(val (cm:matrix-ref cox-matrix row col)))
	    (two-d-put! the-table rowkey colkey
			(cox-cosine-proc val))))
	(enumerate-interval 0 (- dimension 1))))
     (enumerate-interval 0 (- dimension 1)))
    the-table))

;; Computes, and returns as a list, the relations present in a Coxeter
;; group with the given Coxeter matrix.
(define (compute-cox-relations cox-matrix)
  (define dim (cm:dimension cox-matrix))
  (define (make-cox-relation gen-one gen-two n)
    (append-map (lambda (num)
	          (list
		   (number->generator-symbol gen-one)
		   (number->generator-symbol gen-two)))
		(enumerate-interval 1 n)))
  (append-map
   (lambda (gen-one)
     (map
      (lambda (gen-two)
	(let ((m (cm:matrix-ref cox-matrix gen-one gen-two)))
	  (make-cox-relation gen-one gen-two m)))
      (enumerate-interval (+ gen-one 1) (- dim 1))))
   (enumerate-interval 0 (- dim 1))))

(define ((numeric-reflect-lambda cox-symbol length-alist)
	 mirror_gen reflect_gen mirror_proc reflect_proc)
  (let ((mirror_len (cdr (assq mirror_gen length-alist)))
	(reflect_len (cdr (assq reflect_gen length-alist)))
	(dot_cosine (two-d-get cox-symbol mirror_gen reflect_gen)))
    (let ((proj-factor (* 2 (/ mirror_len reflect_len) dot_cosine))
	  (cache-key ()) ; Ah, the beauty of lexical scoping!
	  (cache-val ()))
      (lambda (el-list)
	(if (eq? el-list cache-key)
	    cache-val
	    (begin
	      (set! cache-key el-list)
	      (set! cache-val
		    (vector-vector
		     (mirror_proc el-list)
		     (vector*scalar (reflect_proc el-list)
				    proj-factor)))
	      ;(pp cache-val) (newline)
	      cache-val))))))

;; numeric values of cosines and sines
;; note also we want the *obtuse* version of the angle
;; so all the cosines are the negatives
(define (num-cox-cos angle-divisor)
  (cond ((= angle-divisor 1) 1)
	((= angle-divisor 2) 0)
	((= angle-divisor 3) (/ -1 2))
	((= angle-divisor 4) (/ -1 (sqrt 2)))
	((= angle-divisor 5) (* -1 (/ (+ 1 (sqrt 5)) 4)))
	((= angle-divisor 6) (* -1 (/ (sqrt 3) 2)))
	(else `(cos (/ (*  pi (- ,angle-divisor 1))
		       ,angle-divisor)))))

;; TODO: Check this comment with Rebecca
;; Returns the group presentation of a Coxeter group with the given
;; Coxeter matrix and root lengths.  The root lengths are necessary to
;; construct the procedure that reflects about a generator.
(define (cox-presentation cox-matrix root-lengths)
  (define compute-numeric? *cox-presentation/compute-numeric?*)
  (if compute-numeric?
      (set! root-lengths (substitute-multiple root-lengths symbol-dict)))
  (define n (length root-lengths))
  (assert (= (cm:dimension cox-matrix) n)
	  "Matrix and lengths incompatible")
  (let* ((generator-list (map number->generator-symbol
			      (enumerate-interval 0 (- n 1))))
	 (cox-relations (compute-cox-relations cox-matrix))
	 (cosine-table (compute-cosine-table
			(if compute-numeric? num-cox-cos sym-cox-cos)
			cox-matrix))
	 (length-alist (map cons generator-list root-lengths))
	 (reflect-proc
	  ((if compute-numeric? numeric-reflect-lambda reflect-lambda)
	   cosine-table length-alist)))
    (make-group-presentation
     generator-list
     cox-relations
     reflect-proc)))
