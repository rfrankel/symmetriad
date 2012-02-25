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

;;; Example 7: The Tesseract B4

; Works, 384 live cosets, as expected
(define B4-play (geom-family->cox-geometry B-family 4))

(define tesseract (make-symmetric-object
		   B4-play ((cartesian-point B4-play) (up 1 1 1 1))))

(define not-tesseract (magic-spec->symmetric-object B4-play '(1 0 0 0)))

(define tesseract (magic-spec->symmetric-object B4-play '(0 0 0 1)))

(assert-correct-stats tesseract 16 24)

(symo:file-print-oogl-off tesseract "playout/tesseract.off")

(symo:file-print-oogl-off 
 tesseract
 "playout/tesseract-2.off"
 (lambda (face)
   (list 0.10 0.0 0.5)))

; This looks weird... It may be wrong...
(symo:file-print-oogl-off 
 (magic-spec->symmetric-object B4-play '(1 1 1 1))
 "playout/B4-full.off")

(symo:file-print-gv-skel 
 tesseract "playout/tess.skel"
 (merge-by-first (list) '(1.0 1.0 1.0 1.0)))

(symo:file-print-gv-skel 
 tesseract "playout/tess.skel"
 (merge-by-first 
  (list (highlight-touching tesseract '(c402) '(1.0 0.0 0.0 1.0))
	(highlight-contained tesseract 
			     (map (lambda (int) 
				    (ref (symo/unique-vertices tesseract) int))
				  '(0 1 2 3))
			     '(0.0 0.0 1.0 1.0)))
  '(0.0 1.0 0.0 1.0)))


(define a-cube (%create-subgroup (lambda (elt gen)
				   (gn:product (cxg/group-net B4-play) elt gen))
				 '((s1) (s2) (s3))))

(subg:get-coset a-cube 'e)
(map (lambda (elt) (symo:get-vertex tesseract elt)) 
     (subg:get-coset a-cube 'e))

(list-intersection (subg:get-coset a-cube 'e)
		   (subg:get-coset a-cube 'c402))
(lin-rem-dup-eq
 (list-intersection (map (lambda (elt) (symo:get-vertex tesseract elt)) 
			 (subg:get-coset a-cube 'e))
		    (map (lambda (elt) (symo:get-vertex tesseract elt)) 
			 (subg:get-coset a-cube 'c402))))

(gn:product (cxg/group-net B4-play) 'e 's2)
(subg/word-list a-cube)

(pp (lin-rem-dup-eq (map (lambda (elt)
			   (car (subg:get-coset a-cube elt)))
			 (cxg/chamber-list B4-play))))

(symo:file-print-gv-skel 
 tesseract "playout/tess-2.skel"
 (merge-by-first 
  (list ;(highlight-touching tesseract '(e) '(0.0 1.0 1.0 1.0))
   (highlight-coset tesseract a-cube 'e '(0.0 0.0 1.0 1.0))
   (highlight-coset tesseract a-cube 'c111 '(1.0 0.0 0.0 1.0)))
  '(0.0 1.0 0.0 1.0)))

(define B4-full (magic-spec->symmetric-object B4-play '(1 1 1 1)))

(symo:file-print-gv-skel
 B4-full "playout/B4-full.skel"
 (merge-by-first 
  (list
   (highlight-coset B4-full a-cube 'e '(0.0 0.0 1.0 1.0))
   (highlight-coset B4-full a-cube 'c402 '(1.0 0.0 0.0 1.0))) 
  '(1.0 1.0 1.0 1.0)))

(let ((a-subgroup (coxg-subgroup B4-play '(s3 s0 s1))))
  (symo:file-print-gv-skel
   B4-full "playout/B4-full-2.skel"
   (merge-by-first 
    (list
     (highlight-coset B4-full a-subgroup 'e '(0.0 0.0 1.0 1.0))
     (highlight-coset B4-full a-subgroup 'c402 '(1.0 0.0 0.0 1.0)))
    '(1.0 1.0 1.0 1.0))))

(define B4-part (magic-spec->symmetric-object B4-play '(1 0 0 1)))

(length (symo/unique-vertices B4-part))

(let ((a-subgroup (coxg-subgroup B4-play '(s0 s2 s3))))
  (symo:file-print-gv-skel
   B4-part "playout/B4-part.skel"
   (merge-by-first 
    (list 
     (highlight-coset B4-part a-subgroup 'e '(0.0 0.0 1.0 1.0))
     (highlight-coset B4-part a-subgroup 'c402 '(1.0 0.0 0.0 1.0)))
    '(1.0 1.0 1.0 1.0))))

(let ((object B4-part)
      (a-subgroup (coxg-subgroup B4-play '(s1 s2 s3))))
  (symo:file-print-gv-skel
   object "playout/B4-part-2.skel"
   (merge-by-first 
    (map (lambda (elt)
	   (highlight-coset object a-subgroup elt '(0.0 0.0 1.0 1.0)))
	 (cxg/chamber-list B4-play))
    '(1.0 1.0 1.0 1.0))))

(let* ((object (symmetric-object B-family '(0 0 0 1))))
  (symo:file-print-gv-skel
   object "playout/B4-0001.skel"
   (multicolor-listed-cosets
    object '(s1 s2 s3) `((e . ,*red*) (c111 . ,*blue*)) (param-grey 0.6))))

(let* ((object (symmetric-object B-family '(1 1 1 1)))
       (a-subgroup '(s1 s2 s3))
       (b-subgroup '(s1 s2 s0))
       (intersect '(s1 s2)))
  (let ((elt-list (subg:get-coset (symo-subgroup object a-subgroup) 'e)))
    (symo:file-print-gv-skel
     object "playout/B4-play.skel"
     (highlight-listed-multigroup-cosets
      object `((,intersect . ,*blue*) 
	       (,a-subgroup . ,*red*) 
	       (,b-subgroup . ,*green*))
      elt-list *grey*))))

(let ((object (symmetric-object B-family '(1 1 1 1))))
  (symo:file-print-gv-skel
   object "playout/B4-play.skel"
   (highlight-cell-and-neighbors
    object `(((s1 s2 s3) . ,*red*) 
	     ((s0 s1 s2) . ,*blue*))
    #f)))

(let ((object (symmetric-object B-family '(1 1 1 6))))
  (symo:file-print-gv-skel
   object "playout/B4-1116.skel"
   (highlight-all-cosets
    object '(s0 s1 s2) *blue* (param-grey 0.6))))

(let ((object (symmetric-object B-family '(1 1 6 1))))
  (symo:file-print-gv-skel
   object "playout/B4-1161.skel"
   (highlight-all-cosets
    object '(s0 s1 s3) *green* (param-grey 0.6))))

(let ((object (symmetric-object B-family '(1 6 1 1))))
  (symo:file-print-gv-skel
   object "playout/B4-1611.skel"
   (highlight-all-cosets
    object '(s0 s2 s3) *magenta* (param-grey 0.6))))

(let ((object (symmetric-object B-family '(1 1 1 1))))
  (symo:file-print-gv-skel
   object "playout/B4-b3-oct-struct.skel"
   (highlight-cell-and-neighbors
    object `(((s1 s2 s3) . ,*red*) ((s0 s3 s2) . ,*magenta*))
    (param-grey 0.7))))

(let ((object (symmetric-object B-family '(1 1 1 1))))
  (symo:file-print-gv-skel
   object "playout/B4-hex-oct-struct.skel"
   (highlight-cell-and-neighbors
    object `(((s1 s3 s0) . ,*dgreen*) ((s0 s3 s2) . ,*magenta*))
    #f)
   ))

(let ((object (symmetric-object B-family '(1 1 1 1))))
  (symo:file-print-gv-skel
   object "playout/B4-vert-struct.skel"
   (highlight-listed-multigroup-cosets
    object `(((s1 s2 s3) . ,*red*) 
	     ((s0 s1 s2) . ,*blue*) 
	     ((s1 s3 s0) . ,*dgreen*) 
	     ((s0 s3 s2) . ,*magenta*))
    (list 'e)
    #f)
   ))

(let* ((object (symmetric-object B-family '(0 1 1 0)))
       (subg-lead (symo-subgroup object '(s1 s2 s3)))
       (subg-follow (symo-subgroup object '(s1 s2 s0)))
       )
  (symo:file-print-gv-skel
   object "playout/B4-0110.skel"
   (merge-by-first
    (list (multicolor-listed-cosets
	   object subg-lead `((e . ,*red*) (c348 . ,*red*)))
	  (highlight-listed-cosets
	   object subg-follow (subg:get-coset subg-lead 'e) *blue*)
	  (highlight-listed-cosets
	   object subg-follow (subg:get-coset subg-lead 'c348) *dgreen*)
	  )
    (param-grey 0.6))
   ))

(let ((object (symmetric-object B-family '(1.0 0.5 0.25 0.125))))
  (symo:file-print-gv-skel
   object
   "playout/B4-play-irreg.skel"
   (merge-by-first
    (list 
    (highlight-all-cosets object '(s0) *red*)
    (highlight-all-cosets object '(s1) *green*)
    (highlight-all-cosets object '(s2) *blue*)
    (highlight-all-cosets object '(s3) '(1.0 1.0 0.0 1.0)))
    *white*)))
    
(let ((object (symmetric-object B-family '(1.5 0.3 0.3 0.3))))
  (symo:file-print-gv-skel
   object
   "playout/B4-full-shrunk.skel"
   (highlight-multigroup-cosets object `(((s1 s2 s3) . ,*red*))
				'(0.5 0.5 0.5 1.0))))

(let ((object (symmetric-object B-family '(0.3 0.3 0.3 1.5))))
  (symo:file-print-gv-skel
   object
   "playout/B4-full-shrunk-2.skel"
   (highlight-multigroup-cosets object `(((s1 s2 s0) . ,*blue*))
				'(0.5 0.5 0.5 1.0))))

(let ((object (symmetric-object B-family '(1 1 0 1))))
  (symo:file-print-gv-skel
   object
   "playout/B4-1101-play.skel"
   (multicolor-listed-cosets object '(s0 s1 s3) `((c5 . ,*red*) (c120 . ,*green*)) 
			     (param-grey 0.6))
   ;(color-cycle-all-cosets 
   ; object '(s0 s1 s3)
   ; (list *red* *green* *blue* *yellow*))
   ))

(let ((object (symmetric-object B-family '(1 1 0 1))))
  (symo:file-print-gv-skel
   object
   "playout/B4-1101-play-2.skel"
   (highlight-multigroup-cosets
    object `(((s0) . ,*red*) ((s1) . ,*green*) ((s3) . ,*blue*)))
   ))

(let ((object (symmetric-object B-family '(1 0 0 1))))
  (symo:file-print-oogl-off
   object
   "playout/B4-1001-1.off"
   (highlight-all-cosets object '(s1 s2 s3) '(0.0 0.5 0.5 1.0))
   'off-conformal))

(let ((object (symmetric-object B-family '(1 0 0 1))))
  (symo:file-print-gv
   object
   "playout/B4-1001-2.off"
   'off-conformal
   (highlight-all-cosets object '(s1 s2 s0) '(0.0 0.0 0.5 1.0))
   ))

(let ((object (symmetric-object B-family '(1 0 0 1))))
  (symo:file-print-gv
   object
   "playout/B4-1001-3.off"
   'off-conformal
   (highlight-all-cosets object '(s1 s3 s0) '(0.5 0.0 0.0 1.0))
   ))

(let ((object (symmetric-object B-family '(1 0 0 1))))
  (symo:file-print-gv
   object
   "playout/B4-1001-4.off"
   'off-conformal
   (highlight-all-cosets object '(s2 s3 s0) '(0.0 0.5 0.0 1.0))
   ))

(let ((object (symmetric-object B-family '(1 1 3 3))))
  (symo:file-print-gv
   object
   "playout/B4-1133-2.off"
   'off-conformal
   (merge-by-first 
    (list
     (highlight-all-cosets object '(s0 s1 s2) '(0.5 0.0 0.5 1.0))
     (highlight-all-cosets object '(s0 s1 s3) '(0.0 0.5 0.5 1.0))))
   ))

(let ((object (symmetric-object B-family '(1 1 3 3))))
  (symo:file-print-gv
   object
   "playout/B4-1133-3.off"
   'off-conformal
   (merge-by-first 
    (list
     (highlight-coset object '(s0 s1 s2) 'e *color:do-not-draw*)
     (highlight-all-cosets object '(s0 s1 s2) '(0.5 0.0 0.5 1.0))
     (highlight-all-cosets object '(s0 s1 s3) '(0.0 0.5 0.5 1.0))))
   ))

(let ((object (symmetric-object B-family '(3 3 1 1))))
  (symo:file-print-gv
   object
   "playout/B4-3311.off"
   'off-conformal
   (merge-by-first 
    (list
     (highlight-all-cosets object '(s3 s1 s2) '(0.5 0.0 0.5 1.0))
     (highlight-all-cosets object '(s0 s2 s3) '(0.0 0.5 0.5 1.0))))
   ))

(let ((object (symmetric-object B-family '(1 1 5 5))))
  (symo:file-print-gv
   object
   "playout/B4-1155-3.off"
   'off-conformal
   (merge-by-first 
    (list
     (highlight-all-cosets object '(s0 s1 s2) '(0.5 0.0 0.5 1.0))
     (highlight-all-cosets object '(s0 s1 s3) '(0.0 0.5 0.5 1.0))))
   ))

(let ((object (symmetric-object B-family '(5 5 1 1))))
  (symo:file-print-gv
   object
   "playout/B4-5511.off"
   'off-conformal
   (merge-by-first 
    (list
     (highlight-all-cosets object '(s3 s1 s2) '(255 102 153 255))
     (highlight-all-cosets object '(s0 s2 s3) '(0 0 102 255))))
   ))

(let ((object (symmetric-object B-family '(1 1 3 3))))
  (symo:file-print-gv
   object
   "playout/B4-1133-3.off"
   'off-conformal
    (list
     (highlight-all-cosets object '(s0 s1 s2) '(0.5 0.0 0.5 1.0))
     (highlight-all-cosets object '(s0 s1 s3) '(0.0 0.5 0.5 1.0)))
   ))

(let ((object (symmetric-object B-family '(1 1 3 3))))
  (symo:file-print-gv
   object
   "playout/B4-1133-5.off"
   'off-conformal
  (let* ((elt-list (cxg/chamber-list (symo/geometry object)))
	 (subg1 (symo-subgroup object '(s0 s1 s2)))
	 (subg2 (symo-subgroup object '(s0 s1 s3)))
	 (cosets1 (lin-rem-dup-eq
		   (map (lambda (elt)
			  (subg:get-coset subg1 elt))
			elt-list)))
	 (cosets2 (lin-rem-dup-eq
		   (map (lambda (elt)
			  (subg:get-coset subg2 elt))
			elt-list))))
    (list (map (lambda (coset)
		 (highlight-coset object subg1 (car coset) '(0.5 0.0 0.5 1.0)))
	       cosets1)
	  (map (lambda (coset)
		 (highlight-coset object subg2 (car coset) '(0.0 0.5 0.5 1.0)))
	       cosets2)))))

(let ((object (symmetric-object B-family '(1 0 0 45))))
  (symo:file-print-gv
   object
   "playout/B4-1009-fun.skel"
   'skel
   (color-cycle-all-cosets object '(s1 s2 s3)
			   (list *red* *green* *blue* *purple* *dred* *yellow* *dblue* *cyan*))))

(let ((object (symmetric-object B-family '(0 1 0 0))))
  (symo:file-print-gv
   object
   "playout/B4-0100.off"
   'off-conformal
   (merge-by-first 
    (list
     (highlight-all-cosets object '(s3 s1 s2) '(255 102 153 255))
     (highlight-all-cosets object '(s0 s1 s2) '(0 0 102 255))))
   ))

(let ((object (symmetric-object B-family '(0 1 0 0))))
  (symo:file-print-gv
   object
   "playout/B4-0100.skel"
   'skel
   (merge-by-first 
    (list
     (highlight-all-cosets object '(s3 s1 s2) '(255 102 153 255))
     (highlight-all-cosets object '(s0 s1 s2) '(0 0 102 255))))
   ))

(let ((object (symmetric-object B-family '(1 0 0 1))))
  (symo:file-print-polyhedra
   object
   "playout/B4-1001-1.poly"
   (map (lambda (c) (cons c '(0.0 0.5 0.5 1.0))) (all-cosets object '(s1 s2 s3)))
   ))
