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

(load "load")

;;; Example 9: D4

(define D4-play (geom-family->cox-geometry D-family 4))

(define D4-full (magic-spec->symmetric-object D4-play '(1 1 1 1)))

(define D4-part (magic-spec->symmetric-object D4-play '(0 1 0 0)))

(let* ((object (symmetric-object D-family '(0 1 0 0)))
       (a-subgroup (symo-subgroup object '(s1 s2 s0))))
  (symo:file-print-gv-skel
   object "playout/D4-0100.skel"
   (highlight-all-cosets object a-subgroup *red* *green*)
   ))

(let* ((object (symmetric-object D-family '(1 1 0 0)))
       (a-subgroup (symo-subgroup object '(s1 s2 s3))))
  (symo:file-print-gv-skel
   object "playout/D4-1100.skel"
   (highlight-all-cosets object a-subgroup *red* *blue*)
   ))

(let* ((object (symmetric-object D-family '(1 0 1 1)))
       (cube (symo-subgroup object '(s0 s2 s3)))
       (cuboctahedron (symo-subgroup object '(s0 s1 s2))))
  (symo:file-print-gv-skel
   object "playout/D4-1011.skel"
   (highlight-all-cosets object cuboctahedron '(1.0 0.0 0.0 1.0)
			 '(0.0 0.0 1.0 1.0))
   ))

(let* ((object (symmetric-object D-family '(1 0 0 0))))
;  (pp (subg:coset-list (symo-subgroup object '(s0 s1 s2))
;		       (cxg/chamber-list (symo/geometry object))))
  (symo:file-print-gv-skel
   object "playout/D4-1000-skel"
   ;(highlight-listed-cosets object '(s0 s1 s2) '(e) *red* (param-grey 0.6))
   (lambda (foo) (param-grey 0.6))
   ))

(let* ((object (symmetric-object D-family '(1 0 1 5))))
  (symo:file-print-gv
   object "playout/D4-1015.off" 'off-conformal
   (highlight-all-cosets object '(s0 s1 s2) '(1.0 0.0 0.0 1.0) #f)
   ))

(let* ((object (symmetric-object D-family '(1 0 5 1))))
  (symo:file-print-gv
   object "playout/D4-1051.off" 'off-conformal
   (highlight-all-cosets object '(s0 s1 s3) '(0.0 1.0 0.0 1.0) #f)
   ))

(let* ((object (symmetric-object D-family '(5 0 1 1))))
  (symo:file-print-gv
   object "playout/D4-5011.off" 'off-conformal
   (highlight-all-cosets object '(s3 s1 s2) '(0.0 0.0 1.0 1.0) #f)
   ))
