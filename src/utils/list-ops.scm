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

;;; This file defines utilities working on lists (without hashing)

(define (filter pred lst)
  (keep-matching-items lst pred))

(define (last-cons-cell lst)
  (if (null? (cdr lst))
      lst
      (last-cons-cell (cdr lst))))

(define (flatten list-of-lists)
  (define (loop prev-cons-cell remaining-lists)
    (if (null? remaining-lists)
	'done
	(let* ((new-list (list-copy (car remaining-lists)))
	       (new-last (last-cons-cell new-list)))
	  (set-cdr! prev-cons-cell new-list)
	  (loop new-last (cdr remaining-lists)))))
  (let ((result (cons 1 2)))
    (loop result list-of-lists)
    (cdr result)))

;; Treats the input list as circular, and returns all consecutive 
;; pairs (as a list of cons cells)
(define (conseq-pairs-list lst)
  (map cons lst (append (cdr lst) (list (car lst)))))

;; Returns an association list mapping each key to the corresponding
;; value.  If the value list is too short, extends it cyclically.
(define (cyclic-associate key-list value-list)
  (define (loop keys-left cur-values-left)
    (if (null? keys-left)
	'()
	(if (null? cur-values-left)
	    (loop keys-left value-list)
	    (cons (cons (car keys-left) (car cur-values-left))
		  (loop (cdr keys-left) (cdr cur-values-left))))))
  (loop key-list value-list))
