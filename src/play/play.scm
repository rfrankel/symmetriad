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

;; Reducible group 0-5-0   0-5-0
(let* ((H2H2-system
	(geom-spec->cox-geometry 
	 (gspec:cross-product
          (geom-family->geom-spec (I-family 5) 2)
          (geom-family->geom-spec (I-family 5) 2)))))
  (assert-equal 100 (gn:num-live-cosets (cxg/group-net H2H2-system)))
  (assert-equal 45 (length (gn:dead-cosets
                            (cxg/group-net H2H2-system))))
  (let* ((H2H2-full
          (magic-spec->symmetric-object H2H2-system '(1 1 1 1))))
    (symo:file-print-gv 
     H2H2-full "playout/H2H2-full.off" 'off-conformal
     (highlight-multigroup-cosets
      H2H2-full
      `(((s0 s1 s2) . ,*purple*) ((s1 s2 s3) . ,*green*))))))

;; Reducible group 0-5-0   0-7-0
(let* ((prism-system
	(geom-spec->cox-geometry 
	 (gspec:cross-product
          (geom-family->geom-spec (I-family 5) 2)
          (geom-family->geom-spec (I-family 7) 2)))))
  (assert-equal 140 (gn:num-live-cosets
                     (cxg/group-net prism-system)))
  (assert-equal 73 (length (gn:dead-cosets
                            (cxg/group-net prism-system))))
  (let* ((prism-full
          (magic-spec->symmetric-object prism-system '(1 1 1 1))))
    (symo:file-print-gv 
     prism-full "playout/prism-full.off" 'off-conformal
     (highlight-multigroup-cosets
      prism-full
      `(((s0 s1 s2) . ,*purple*) ((s1 s2 s3) . ,*blue*))))))
