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

;;; This file just contains random playing around with 
;;; the procedures defined in drawing.scm (and other files).
;;; Play with specific Coxeter groups is relegated to the play-* 
;;; files.

(load "load")

;; An experiment with a random Coxeter diagram

(define cox-matrix
  (%create-coxeter-matrix
   (matrix-by-row-list (list (list 1 3 4)
			     (list 3 1 3)
			     (list 4 3 1)))))
(pp cox-matrix)
(define cox-lengths '(1 1 1))

(define coxsub (make-group-presentation '(s0) '((s0 s0))))
(define cox-pres (cox-presentation cox-matrix cox-lengths))
(define cox-group-net (group-network cox-pres coxsub "Autogen"))

; Dies with strange errors without informing me as to whether 
; this group ends up being infinite.
(gn:hlt! cox-group-net)

;; Reducible group 0-4-0   0-4-0
(define cox-matrix-B2B2
  (%create-coxeter-matrix
   (matrix-by-row-list '((1 4 2 2)
			 (4 1 2 2)
			 (2 2 1 4)
			 (2 2 4 1)))))

(define cox-roots-B2B2
  (list #(1 -1 0 0) #(0 1 0 0) #(0 0 1 -1) #(0 0 0 1)))

(define cox-len-B2B2 '(sqrt2 1 sqrt2 1))

(define B2B2-system
  (build-cox-geometry cox-matrix-B2B2 cox-len-B2B2 cox-roots-B2B2))

;; Fails with ";Given a dead factor c37".  This is due to an
;; unfortunate choice in coincidence resolution, where the coset that
;; gets resolved away is chosen arbitrarily.  In this case,
;; coincidence resolution slays the coset currently being processed,
;; so the algorithm dies on that coset's next relation.
