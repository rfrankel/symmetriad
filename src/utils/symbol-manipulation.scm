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

(define (number->generator-symbol num)
  (symbol-append 's (string->symbol (number->string num))))

(define (generator-symbol->root-symbol gen)
  (symbol-append gen '_root))

(define (root-symbols gen-symbols)
  (map generator-symbol->root-symbol gen-symbols))

(define (coset-num->coset-symbol coset-num)
  (if (eq? coset-num 0)
      'e
      (string->symbol (string-append
		       "c"
		       (number->string coset-num)))))

(define (product-symbol factor1 factor2)
  (symbol-append factor1 (string->symbol "*") factor2))

(define (inverse-symbol generator)
  (symbol-append '- generator))
