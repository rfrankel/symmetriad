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

;; utilities for making a presentation
(define (make-cox-relation gen-one gen-two n)
  (append-map (lambda (num) 
	          (list
		   (number->gen-symbol gen-one)
		   (number->gen-symbol gen-two)))
	      (enumerate-interval 1 n)))

(define (number->gen-symbol num)
  (symbol-append 's (string->symbol (number->string num))))
 
; Takes a matrix that gives the numbers m in (s_i s_j)^m_{ij} = 1 and turns 
; it into a lookup table of the cosines of the angles between s_i and s_j.
(define ((cox-matrix->symbol-table cox-cos-proc) cox-mat)
  (let ((the-table (make-empty-two-d-table))
	(matrix (cm:matrix cox-mat)))
    (assert (= (m:num-rows matrix) (m:num-cols matrix))
	    "Non-square coxeter matrix!"
	    cox-mat)
    (let ((dimension (m:num-rows matrix)))
      (for-each
       (lambda (col)
	 (for-each
	  (lambda (row)
	    (let ((rowkey (number->gen-symbol row))
		  (colkey (number->gen-symbol col))
		  (val (matrix-ref matrix row col)))
	      (two-d-put! the-table rowkey colkey 
			  (cox-cos-proc val))))
	  (enumerate-interval 0 (- dimension 1))))
       (enumerate-interval 0 (- dimension 1)))
      the-table)))

(define (cox-lengths->alist cox-len)
  (map 
   (lambda (len index)
     (cons (number->gen-symbol index) len))
   cox-len
   (enumerate-interval 0 (- (length cox-len) 1))))

(define (cox-presentation cox-matrix cox-len)
  (define compute-numeric? *cox-presentation/compute-numeric?*)
  (if compute-numeric?
      (set! cox-len (substitute-multiple cox-len symbol-dict)))
  (define n (length cox-len))
  (assert (= (m:num-rows (cm:matrix cox-matrix)) n)
	  "Matrix and lengths incompatible")
  (let ((gen-list (map number->gen-symbol
		       (enumerate-interval 0 (- n 1))))
	(symbol-table ((cox-matrix->symbol-table 
			(if compute-numeric? num-cox-cos sym-cox-cos))
		       cox-matrix))
	(len-alist (cox-lengths->alist cox-len)))
    (let ((cox-relations 
	   (append-map 
	    (lambda (gen-one)
	      (map 
	       (lambda (gen-two) 
		 (let ((m (matrix-ref (cm:matrix cox-matrix)
				      gen-one
				      gen-two)))
		   (make-cox-relation gen-one gen-two m)))
	       (enumerate-interval (+ gen-one 1) (- n 1))))
	    (enumerate-interval 0 (- n 1))))
	  (reflect-proc
	   ((if compute-numeric? numeric-reflect-lambda reflect-lambda)
	    symbol-table len-alist)))
      (group-presentation-and-proc 
       gen-list 
       cox-relations 
       reflect-proc))))

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

