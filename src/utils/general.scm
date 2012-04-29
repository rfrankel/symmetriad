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

;; This file contains general utilities to be used by the various other files

(declare (usual-integrations))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

;; This version is still a hack, but a somewhat better one.
(define (unquote thing)
  (if (list? thing)
      (if (memq (car thing) '(up down + - / *))
	  (apply (eval (car thing) generic-environment) (map unquote (cdr thing)))
	  thing)
      thing))
	 
(define (close-numbers num1 num2)
  (< (abs (- num1 num2)) 1e-10))

(define (gens-and-spec->words gen-list magic-spec)
  (filter 
   (lambda (x) x)
   (map (lambda (gen spec-elt) (if (= spec-elt 0) (list gen) #f))
	gen-list
	magic-spec)))

(define ((cache-wrapper compute-proc cache-get-proc) 
	 cache-keeper cache-key #!optional use-cache)
  (if (default-object? use-cache) (set! use-cache #t))
  (if use-cache
      (if (hash-table/get (cache-get-proc cache-keeper) cache-key #f)
	  (hash-table/get (cache-get-proc cache-keeper) cache-key #f)
	  (let ((answer (compute-proc cache-keeper cache-key)))
	    (hash-table/put! (cache-get-proc cache-keeper) cache-key answer)
	    answer))
      (compute-proc cache-keeper cache-key)))

(define (count-interned-symbols)
  (let ((v (fixed-objects-item 'obarray))
	(n 0))
    (for-each-vector-element v (lambda (l)
				 (set! n (+ n (weak-length l)))))
    n))

(define (ensure-path-exists dir-pathname)
  (set! dir-pathname (pathname-as-directory dir-pathname))
  (if (file-exists? (directory-namestring dir-pathname))
      (if (file-directory? (directory-namestring dir-pathname))
	  #t
	  (error "File exists and is not a directory." (directory-namestring dir-pathname)))
      (begin (ensure-path-exists (crop-last-directory dir-pathname))
	     (make-directory (directory-namestring dir-pathname)))))

(define (crop-last-directory dir-pathname)
  (directory-pathname (directory-pathname-as-file dir-pathname)))

(define (with-output-to-file-ensuring-path filename proc)
  (ensure-path-exists (directory-pathname filename))
  (with-output-to-file filename proc))
