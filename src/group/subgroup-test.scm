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

(load "group/subgroup")

(let ()
  (define A2-test (geom-family->cox-geometry A-family 2))
  
  (define subg (coxg-subgroup A2-test '((s0))))
  
  (assert (equal? '(c1 e) (sort (subg:compute-coset subg 'e) symbol<?)))
  (assert (equal? '(c1 e) (sort (subg:compute-coset subg 'c1) symbol<?)))
  (assert (equal? '(c2 c3) (sort (subg:compute-coset subg 'c2) symbol<?)))
  (assert (equal? '(c2 c3) (sort (subg:compute-coset subg 'c3) symbol<?)))
  (assert (equal? '(c4 c5) (sort (subg:compute-coset subg 'c4) symbol<?)))
  (assert (equal? '(c4 c5) (sort (subg:compute-coset subg 'c5) symbol<?)))
)

(let ()
  (define A2-test (geom-family->cox-geometry A-family 2))
  
  (define subg (coxg-subgroup A2-test '((s1))))
  
  (assert (equal? '(c5 e) (sort (subg:compute-coset subg 'e) symbol<?)))
  (assert (equal? '(c1 c2) (sort (subg:compute-coset subg 'c1) symbol<?)))
  (assert (equal? '(c1 c2) (sort (subg:compute-coset subg 'c2) symbol<?)))
  (assert (equal? '(c3 c4) (sort (subg:compute-coset subg 'c3) symbol<?)))
  (assert (equal? '(c3 c4) (sort (subg:compute-coset subg 'c4) symbol<?)))
  (assert (equal? '(c5 e) (sort (subg:compute-coset subg 'c5) symbol<?)))
)

(let ()
  (define A2-test (geom-family->cox-geometry A-family 2))
  
  (define subg (coxg-subgroup A2-test '((s0 s1))))
  
  (assert (equal? '(c2 c4 e) (sort (subg:compute-coset subg 'e) symbol<?)))
  (assert (equal? '(c1 c3 c5) (sort (subg:compute-coset subg 'c1) symbol<?)))
  (assert (eq? (subg:get-coset subg 'e) (subg:get-coset subg 'e)))
  (assert (eq? (subg:get-coset subg 'e) (subg:get-coset subg 'c2)))
  (assert (eq? (subg:get-coset subg 'e) (subg:get-coset subg 'c4)))
  (assert (eq? (subg:get-coset subg 'c1) (subg:get-coset subg 'c1)))
  (assert (eq? (subg:get-coset subg 'c1) (subg:get-coset subg 'c3)))
  (assert (eq? (subg:get-coset subg 'c1) (subg:get-coset subg 'c5)))
  (let ((recomputed-coset (subg:get-coset subg 'c1 #f)))
    (assert (not (eq? (subg:get-coset subg 'c1) recomputed-coset)))
    (assert (equal? (sort recomputed-coset symbol<?)
		    (sort (subg:get-coset subg 'c1) symbol<?))))
)
