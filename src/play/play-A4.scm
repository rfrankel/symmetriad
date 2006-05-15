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

;;; Example 8: 4D simplex A4

; 120 live cosets, 2 dead
(define A4-play (geom-family->cox-geometry A-family 4))

(define simplex (magic-spec->symmetric-object A4-play '(1 0 0 0)))

(let* ((object (symmetric-object A-family '(1 0 0 0)))
       (a-subgroup (symo-subgroup object '(s1 s2 s0))))
  (symo:file-print-gv-skel
   object "playout/simplex.skel"
   (merge-by-first 
    (list (highlight-coset object a-subgroup 'e
			   '(1.0 0.0 1.0 1.0)))
    *grey*)))

(let* ((object (symmetric-object A-family '(0 1 1 0)))
       (a-subgroup (symo-subgroup object '(s1 s2 s0))))
  (symo:file-print-gv-skel
   object "playout/A4-0110.skel"
   (merge-by-first 
    (list (highlight-coset object a-subgroup 'e
			   '(1.0 0.0 1.0 1.0)))
    *grey*)
;   (color-cycle-all-cosets object a-subgroup (list *red* *red* *blue* *blue* *green*)
;			   (param-grey 0.6))
   ))

(let* ((object (symmetric-object A-family '(1 0 0 1)))
       (a-subgroup (symo-subgroup object '(s1 s2 s0))))
  (symo:file-print-gv-skel
   object "playout/A4-1001.skel"
   (merge-by-first 
    (list (highlight-coset object a-subgroup 'e
			   '(1.0 0.0 1.0 1.0)))
    *grey*)
;   (color-cycle-all-cosets object a-subgroup (list *red* *red* *blue* *blue* *green*)
;			   (param-grey 0.6))
   ))

(define other-thing (magic-spec->symmetric-object A4-play '(0 1 0 0)))
(assert-correct-stats other-thing 10 30)

(let* ((object (symmetric-object A-family '(1 1 1 1)))
       )
  (symo:file-print-gv-skel
   object "playout/A3-play.skel"
   (multicolor-listed-cosets object (symo-subgroup object '(s0 s1 s2))
			     `((c3 . ,*red*) (c81 . ,*blue*) (c109 . ,*green*) 
					     (c26 . ,*magenta*) (c91 . ,*yellow*))
			     (param-grey 0.6))
   ))

(let* ((object (symmetric-object A-family '(1 1 1 1)))
       )
  (symo:file-print-gv-skel
   object "playout/A3-play-2.skel"
   (color-cycle-all-cosets object '(s3 s1 s2)
			   (list *red* *blue* *green* *magenta* *yellow*)
			   (param-grey 0.6))
   ))

(let* ((object (symmetric-object A-family '(1 1 1 1)))
       (subg (symo-subgroup object '(s0 s1 s2))))
  (pp (subg:coset-list subg (symo/unique-vertices object))))

(let* ((object (symmetric-object A-family '(1 0 0 1))))
  (symo:file-print-gv
   object "playout/A4-1001.off" 'off-conformal
   (merge-by-first
    (list (highlight-all-cosets object '(s0 s1 s2) '(147 112 219 255))
	  (highlight-all-cosets object '(s3 s1 s2) '(187 12 119 255))
	  ))))
	  
