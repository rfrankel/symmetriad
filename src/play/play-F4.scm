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

;;; Example 10: F4

; Works, 1152 live cosets, 48 dead, 1152 = 2^7 * 3^2
; fill-in takes half a minute on tumunzahar,
; maybe 15 seconds compiled.
(define F4-play (geom-family->cox-geometry F-family 4))

;; I think this is a 24-cell, aka hyperdiamond, aka
;; icositetrachoron
(define F4-minimal (magic-spec->symmetric-object F4-play '(1 0 0 0)))

(let ((object F4-minimal)
      (subg (coxg-subgroup F4-play '(s1 s2 s0))))
  (symo:file-print-gv-skel
   object "playout/F4-min.skel"
   (merge-by-first (list
		    (highlight-coset object subg 'e '(1.0 0.0 0.0 1.0))
		    (highlight-coset object subg 'c1199 '(0.0 1.0 0.0 1.0)))
		   '(0.0 1.0 1.0 1.0))))

; Two minutes to build, 5 seconds if compiled. (Tumunzahar).
(define F4-full (magic-spec->symmetric-object F4-play '(1 1 1 1)))

(let* ((object (symmetric-object F-family '(12 1 1 1)))
       (fam-cubo1 (symo-subgroup object '(s1 s2 s3)))
       (fam-cubo2 (symo-subgroup object '(s1 s2 s0)))
       (fam-prism1 (symo-subgroup object '(s0 s2 s3)))
       (fam-prism2 (symo-subgroup object '(s1 s0 s3)))
       )
  (symo:file-print-gv-skel
   object "playout/F4-1111.skel"
   (highlight-all-cosets object fam-cubo1 *dblue* #f)
   ))

(let* ((object (symmetric-object F-family '(12 1 1 1)))
       (fam-cubo1 (symo-subgroup object '(s1 s2 s3)))
       (fam-cubo2 (symo-subgroup object '(s1 s2 s0)))
       (fam-prism1 (symo-subgroup object '(s0 s2 s3)))
       (fam-prism2 (symo-subgroup object '(s1 s0 s3)))
       )
  (symo:file-print-gv
   object "playout/F4-1111.off"
   'off-conformal
   (highlight-all-cosets object fam-cubo1 *dblue* #f)
   ))

(let* ((object (symmetric-object F-family '(1 1 1 1)))
       (fam-cubo1 '(s1 s2 s3))
       (fam-cubo2 '(s1 s2 s0)))
  (symo:file-print-gv-skel
   object "playout/F4-play-struct.skel"
   (highlight-cell-and-neighbors
    object `((,fam-cubo1 . ,*red*) (,fam-cubo2 . ,*blue*))
    '(0.6 0.6 0.6 1.0))))

(let* ((object (symmetric-object F-family '(0 1 0 0)))
       (subg-lead (symo-subgroup object '(s1 s2 s3)))
       (subg-follow (symo-subgroup object '(s1 s2 s0))))
  (pp (symmetric-object? object))
  (pp (map car (subg:coset-list subg-lead object)))
  (symo:file-print-gv-skel
   object "playout/F4-0100.skel"
   (merge-by-first
    (list (multicolor-listed-cosets
	   object subg-lead `((e . ,*red*) (c1046 . ,*red*)))
	  (highlight-listed-cosets
	   object subg-follow (subg:get-coset subg-lead 'e) *blue*)
	  (highlight-listed-cosets
	   object subg-follow (subg:get-coset subg-lead 'c1046) *blue*)
	  )
    *dred*)
   ;(color-cycle-all-cosets object subg-lead (list *red* *green* *blue* *dred* *yellow*) *white*)
;   (multicolor-listed-cosets 
;    object subg-lead 
;    `(( c206 . ,*red*) ( c358 . ,*green* ) ( c479 . ,*blue*) ( c688 . ,*dred*) ( c988 . ,*yellow*)
;		       ( c1046 . ,*magenta*))
;    *white*)
   ))

(let* ((object (symmetric-object F-family '(0 1 1 0)))
       (subg-lead (symo-subgroup object '(s1 s2 s3)))
       (subg-follow (symo-subgroup object '(s1 s2 s0))))
  ;(pp (map car (subg:coset-list subg-lead object)))
  (symo:file-print-gv-skel
   object "playout/F4-0110.skel"
;   (color-cycle-all-cosets 
;    object subg-lead 
;    (list *red* *green* *blue* *dred* *yellow* *dgreen* *dblue* *magenta* *purple* *cyan*) *white*)
;   (lambda (foo) *dred*)
   (multicolor-listed-cosets 
    object subg-lead 
    `(( c220 . ,*red*) ( c709 . ,*blue* ) ( c919 . ,*red*) ( c1101 . ,*blue*) ( c998 . ,*red*) 
		       ( c777 . ,*blue*) (c236 . ,*red*) (c422 . ,*blue*))
    (param-grey 0.8))
   ))

(let* ((object (symmetric-object F-family '(1 0 0 1)))
       (fam-cubo1 (symo-subgroup object '(s1 s2 s3)))
       (fam-cubo2 (symo-subgroup object '(s1 s2 s0))))
  (symo:file-print-gv-skel
   object "playout/F4-1001.skel"
   (lambda (foo) *dred*)
   ))

(let ((object (symmetric-object F-family '(1.0 0.5 0.25 0.125))))
  (symo:file-print-gv-skel
   object
   "playout/F4-play-irreg.skel"
   (merge-by-first
    (list 
    (highlight-all-cosets object '(s0) *red*)
    (highlight-all-cosets object '(s1) *green*)
    (highlight-all-cosets object '(s2) *blue*)
    (highlight-all-cosets object '(s3) '(1.0 1.0 0.0 1.0)))
    *white*)))

(let ((object (symmetric-object F-family '(0.0 0.2 0.0 1.0))))
  (symo:file-print-gv-skel
   object
   "playout/F4-play-irreg.skel"
   (merge-by-first
    (list 
    (highlight-all-cosets object '(s0 s1 s2) *green*))
    *blue*)))
    
(let ((object (symmetric-object F-family '(1 1 3 3))))
  (symo:file-print-gv
   object
   "playout/F4-1133.off"
   'off-conformal
   (merge-by-first 
    (list
     (highlight-all-cosets object '(s0 s1 s2) '(0.5 0.0 0.5 1.0))
     (highlight-all-cosets object '(s0 s1 s3) '(0.0 0.5 0.5 1.0))))
   ;(color-cycle-all-cosets 
   ; object '(s0 s1 s3)
   ; (list *red* *green* *blue* *yellow*))
   ))

(let ((object (symmetric-object F-family '(9 1 1 9))))
  (symo:file-print-gv
   object
   "playout/F4-9119.off"
   'off-conformal
   (merge-by-first 
    (list
     (highlight-all-cosets object '(s0 s1 s2) '(0.5 0.0 0.5 1.0))
     (highlight-all-cosets object '(s2 s1 s3) '(0.0 0.5 0.5 1.0))))
   ;(color-cycle-all-cosets 
   ; object '(s0 s1 s3)
   ; (list *red* *green* *blue* *yellow*))
   ))

(let ((object (symmetric-object F-family '(1 1 1 5))))
  (symo:file-print-gv
   object
   "playout/F4-1115.off"
   'off-conformal
   (merge-by-first 
    (list
     (highlight-all-cosets object '(s0 s1 s2) '(0.9 0.7 0.5 1.0))
     (highlight-all-cosets object '(s0 s1 s3) '(0.5 0.3 0.1 1.0))))
   ))

(let ((object (symmetric-object F-family '(5 1 1 1))))
  (symo:file-print-gv
   object
   "playout/F4-5111.off"
   'off-conformal
   (merge-by-first 
    (list
     (highlight-all-cosets object '(s3 s1 s2) '(0.5 0.7 0.9 1.0))
     (highlight-all-cosets object '(s0 s2 s3) '(0.1 0.3 0.5 1.0))))
   ))

(let ((object (symmetric-object F-family '(1 1 1 9))))
  (symo:file-print-gv
   object
   "playout/F4-1119.off"
   'off-conformal
   (merge-by-first 
    (list
     (highlight-all-cosets object '(s0 s1 s2) '(0.9 0.7 0.5 1.0))
     (highlight-all-cosets object '(s0 s1 s3) '(0.5 0.3 0.1 1.0))))
   ))

(let ((object (symmetric-object F-family '(9 1 1 1))))
  (symo:file-print-gv
   object
   "playout/F4-9111.off"
   'off-conformal
   (merge-by-first 
    (list
     (highlight-all-cosets object '(s3 s1 s2) '(0.5 0.7 1.0 1.0))
     (highlight-all-cosets object '(s0 s2 s3) '(0.1 0.3 0.5 1.0))))
   ))

(let ((object (symmetric-object F-family '(1 1 1 9))))
  (symo:file-print-gv
   object
   "playout/F4-1119.skel"
   'skel
   (merge-by-first 
    (list
     (highlight-all-cosets object '(s0 s1 s2) '(0.9 0.7 0.5 1.0))
     (highlight-all-cosets object '(s0 s1 s3) '(0.5 0.3 0.1 1.0))))
   ))

(let ((object (symmetric-object F-family '(9 1 1 1))))
  (symo:file-print-gv
   object
   "playout/F4-9111.skel"
   'skel
   (merge-by-first 
    (list
     (highlight-all-cosets object '(s3 s1 s2) '(0.5 0.7 1.0 1.0))
     (highlight-all-cosets object '(s0 s2 s3) '(0.1 0.3 0.5 1.0))))
   ))

(let* ((object (symmetric-object F-family '(1 0 0 1))))
  (symo:file-print-gv
   object "playout/F4-1001.off" 'off-conformal
   (merge-by-first
    (list (highlight-all-cosets object '(s0 s1 s2) '(147 112 219 255))
	  (highlight-all-cosets object '(s3 s1 s2) '(187 12 119 255))
	  ))))
