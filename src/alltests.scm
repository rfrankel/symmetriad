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
(load "compile")
(load "load")

(let ()
  (load "utils/multi-set-test")
  (load "utils/symbolics-test")
  (load "group/coxeter-matrix-test")
  (load "group/group-network-test")
  (load "group/subgroup-test")
  (load "geometry/root-systems-test"))
  
(let ()
  (define A3-system (geom-family->cox-geometry A-family 3 #f))
  (assert-equal 24 (gn:num-live-cosets (cxg/group-net A3-system)))
  (assert-equal 0 (length (gn:dead-cosets (cxg/group-net A3-system))))
  
  (define tetrahedron 
    (make-symmetric-object A3-system ((cartesian-point A3-system) 
				      (car (omega-dirs A3-system)))))
  (assert-correct-stats tetrahedron 4 4)
  (assert-face-shape-stats tetrahedron '((3 . 4)))
  ;(assert-vrml-prints-correctly tetrahedron "tetrahedron.wrl")
  (symo:file-print-vrml tetrahedron "test-output/tetrahedron.wrl")
  
  (define octahedron
    (cartesian-point->symmetric-object A3-system (cadr (omega-dirs A3-system))))
  (assert-correct-stats octahedron 6 8)
  (assert-face-shape-stats octahedron '((3 . 8)))
  (let ((win (frame -2 2 -2 2)))
    (symo:draw octahedron win)
    (graphics-close win))
  
  (define trunc-tetrahedron
    (magic-spec->symmetric-object A3-system '(1 1 0)))
  (assert-correct-stats trunc-tetrahedron 12 8)
  (assert-face-shape-stats trunc-tetrahedron '((3 . 4) (6 . 4)))
  
  (define cuboctahedron-A
    (magic-spec->symmetric-object A3-system '(1 0 1)))
  (assert-correct-stats cuboctahedron-A 12 14)
  (assert-face-shape-stats cuboctahedron-A '((3 . 8) (4 . 6)))
  (let ((win (frame -2 2 -2 2)))
    (symo:draw octahedron win cab)
    (graphics-close win))
  
  (define trunc-octahedron-A
    (magic-spec->symmetric-object A3-system '(1 1 1)))
  (assert-correct-stats trunc-octahedron-A 24 14)
  (assert-face-shape-stats trunc-octahedron-A '((4 . 6) (6 . 8)))
  
  'pass)
  
(let ()
  (define B3-system (geom-family->cox-geometry B-family 3 #f))
  (assert-equal 48 (gn:num-live-cosets (cxg/group-net B3-system)))
  (assert-equal 0 (length (gn:dead-cosets (cxg/group-net B3-system))))
  
  (define cube
    (cartesian-point->symmetric-object B3-system (up 1 1 1)))
  (assert-correct-stats cube 8 6)
  (assert-face-shape-stats cube '((4 . 6)))
  (let ((win (frame -2 2 -2 2)))
    (symo:draw cube win cav)
    (add-three-d-axes win cav)
    (graphics-close win))
  
  (define cuboctahedron
    (cartesian-point->symmetric-object B3-system (up 1 1 0)))
  (assert-correct-stats cuboctahedron 12 14)
  (assert-face-shape-stats cuboctahedron '((3 . 8) (4 . 6)))
  ;(assert-vrml-prints-correctly cuboctahedron "cuboctahedron.wrl")
  (symo:file-print-vrml cuboctahedron "test-output/cuboctahedron.wrl")

  (define octahedron-B
    (cartesian-point->symmetric-object B3-system (up 1 0 0)))
  (assert-correct-stats octahedron-B 6 8)
  (assert-face-shape-stats octahedron-B '((3 . 8)))
  
  (define trunc-cube
    (magic-spec->symmetric-object B3-system '(0 1 1))) ; X---0-4-0
  (assert-correct-stats trunc-cube 24 14)
  (assert-face-shape-stats trunc-cube '((3 . 8) (8 . 6)))
  
  (define trunc-octahedron
    (magic-spec->symmetric-object B3-system '(1 1 0)))
  (assert-correct-stats trunc-octahedron 24 14)
  (assert-face-shape-stats trunc-octahedron '((6 . 8) (4 . 6)))
  
  (define small-rhombicuboctahedron
    (magic-spec->symmetric-object B3-system '(1 0 1)))
  (assert-correct-stats small-rhombicuboctahedron 24 26)
  (assert-face-shape-stats small-rhombicuboctahedron '((3 . 8) (4 . 18)))
  
  (define great-rhombicuboctahedron
    (magic-spec->symmetric-object B3-system '(1 1 1)))
  (assert-correct-stats great-rhombicuboctahedron 48 26)
  (assert-face-shape-stats great-rhombicuboctahedron '((6 . 8) (4 . 12) (8 . 6)))
  
  'pass)

(let ()
  (define H3-system (geom-family->cox-geometry H-family 3 #f))
  (assert-equal 120 (gn:num-live-cosets (cxg/group-net H3-system)))
  (assert-equal 0 (length (gn:dead-cosets (cxg/group-net H3-system))))
  
  (define dodecahedron
    (cartesian-point->symmetric-object H3-system (caddr (omega-dirs H3-system))))
  (assert-correct-stats dodecahedron 20 12)
  (assert-face-shape-stats dodecahedron '((5 . 12)))
  
  (define icosohedron
    (cartesian-point->symmetric-object H3-system (car (omega-dirs H3-system))))
  (assert-correct-stats icosohedron 12 20)
  (assert-face-shape-stats icosohedron '((3 . 20)))
  ;(assert-off-prints-correctly icosohedron "icosohedron.off")
  (symo:file-print-oogl-off icosohedron "test-output/icosohedron.off")

  (define icosidodecahedron
    (cartesian-point->symmetric-object H3-system (cadr (omega-dirs H3-system))))
  (assert-correct-stats icosidodecahedron 30 32)
  (assert-face-shape-stats icosidodecahedron '((3 . 20) (5 . 12)))
  
  (define trunc-icosohedron
    (magic-spec->symmetric-object H3-system '(1 1 0)))
  (assert-correct-stats trunc-icosohedron 60 32)
  (assert-face-shape-stats trunc-icosohedron '((5 . 12) (6 . 20)))
  
  (define trunc-dodecahedron
    (magic-spec->symmetric-object H3-system '(0 1 1)))
  (assert-correct-stats trunc-dodecahedron 60 32)
  (assert-face-shape-stats trunc-dodecahedron '((3 . 20) (10 . 12)))
  
  (define small-rhombicosidodecahedron
    (magic-spec->symmetric-object H3-system '(1 0 1)))
  (assert-correct-stats small-rhombicosidodecahedron 60 62)
  (assert-face-shape-stats small-rhombicosidodecahedron 
			   '((3 . 20) (4 . 30) (5 . 12)))
  (symo:file-print-gv-skel
   small-rhombicosidodecahedron "test-output/sm-rhombicosidodecahedron.skel"
   (highlight-all-cosets 
    small-rhombicosidodecahedron '(s1 s2) *dblue*))
  (symo:file-print-oogl-off
   small-rhombicosidodecahedron "test-output/sm-rhombicosidodecahedron.off"
   (highlight-all-cosets 
    small-rhombicosidodecahedron '(s1 s2) *dblue*))
  
  (define great-rhombicosidodecahedron
    (magic-spec->symmetric-object H3-system '(1 1 1)))
  (assert-correct-stats great-rhombicosidodecahedron 120 62)
  (assert-face-shape-stats great-rhombicosidodecahedron 
			   '((4 . 30) (6 . 20) (10 . 12)))
  ;(assert-vrml-prints-correctly great-rhombicosidodecahedron
  ;			      "great-rhombicosidodecahedron.wrl")
  (symo:file-print-vrml great-rhombicosidodecahedron
			"test-output/great-rhombicosidodecahedron.wrl")

  'pass)

(let ()  
  (define A4-system (geom-family->cox-geometry A-family 4 #f))
  (assert-equal 120 (gn:num-live-cosets (cxg/group-net A4-system)))
  (assert-equal 2 (length (gn:dead-cosets (cxg/group-net A4-system))))
  
  (define simplex (magic-spec->symmetric-object A4-system '(1 0 0 0)))
  
  (assert-correct-stats simplex 5 10)
  (assert-face-shape-stats simplex '((3 . 10)))
  'pass)

(let ()  
  (define B4-system (geom-family->cox-geometry B-family 4 #f))
  (assert-equal 384 (gn:num-live-cosets (cxg/group-net B4-system)))
  (assert-equal 18 (length (gn:dead-cosets (cxg/group-net B4-system))))
  
  (define tesseract 
    (cartesian-point->symmetric-object B4-system (up 1 1 1 1)))
  (assert-correct-stats tesseract 16 24)
  (assert-face-shape-stats tesseract '((4 . 24)))
  ;(assert-off-prints-correctly tesseract "tesseract.off")
  (symo:file-print-oogl-off tesseract "test-output/tesseract.off")
  'pass)

(let ()  
  (define D4-system (geom-family->cox-geometry D-family 4 #f))
  (assert-equal 192 (gn:num-live-cosets (cxg/group-net D4-system)))
  (assert-equal 0 (length (gn:dead-cosets (cxg/group-net D4-system))))
  
  (define weird-thing
    (cartesian-point->symmetric-object D4-system (up 1 0 0 0)))
  'pass)

; Check that the color system doesn't throw exceptions
(let()
  (let* ((object (symmetric-object A-family '(1 0 0 1)))
	 (a-subgroup (symo-subgroup object '(s1 s2 s0))))
    (symo:file-print-gv-skel
     object "test-output/A4-1001.skel"
     (color-cycle-all-cosets object a-subgroup (list *red* *red* *blue* *blue* *green*)
			     (param-grey 0.6))
     ))
  
  (let ((object (symmetric-object B-family '(1 1 1 1))))
    (symo:file-print-gv-skel
     object "test-output/B4-b3-oct-struct.skel"
     (highlight-cell-and-neighbors
      object `(((s1 s2 s3) . ,*red*) ((s0 s3 s2) . ,*magenta*))
      (param-grey 0.7)))
    (symo:file-print-oogl-off
     object "test-output/B4-b3-oct-struct.off"
     (highlight-cell-and-neighbors
      object `(((s1 s2 s3) . ,*red*) ((s0 s3 s2) . ,*magenta*))
      (param-grey 0.7)))
    )
  
  (let ((tesseract (symmetric-object B-family '(0 0 0 1))))
    (symo:file-print-gv-skel 
     tesseract "test-output/tess-2.skel"
     (merge-by-first 
      (list ;(highlight-touching tesseract '(e) '(0.0 1.0 1.0 1.0))
       (highlight-coset tesseract '(s1 s2 s3) 'e '(0.0 0.0 1.0 1.0))
       (highlight-coset tesseract '(s1 s2 s3) 'c111 '(1.0 0.0 0.0 1.0)))
      '(0.0 1.0 0.0 1.0))))
  
  (let ((object (symmetric-object B-family (quote (0 0 0 1))))) 
    (symo:file-print-gv-skel 
     object "test-output/tess.skel" 
     (multicolor-listed-cosets 
      object (quote (s1 s2 s3)) `((e . ,*red*) (c111 . ,*blue*)) *grey*)))

  (let ((object (symmetric-object B-family '(1.0 0.5 0.25 0.125))))
    (symo:file-print-gv-skel
     object
     "test-output/B4-play-irreg.skel"
     (merge-by-first
      (list 
       (highlight-all-cosets object '(s0) *red*)
       (highlight-all-cosets object '(s1) *green*)
       (highlight-all-cosets object '(s2) *blue*)
       (highlight-all-cosets object '(s3) '(1.0 1.0 0.0 1.0)))
      *white*)))
  
  (let ((object (symmetric-object B-family '(1 1 3 3))))
    (symo:file-print-gv
     object
     "test-output/B4-1133.off"
     'off-conformal
     (merge-by-first 
      (list
       (highlight-coset object '(s0 s1 s2) 'e *color:do-not-draw*)
       (highlight-all-cosets object '(s0 s1 s2) '(0.5 0.0 0.5 1.0))
       (highlight-all-cosets object '(s0 s1 s3) '(0.0 0.5 0.5 1.0))))
     ))

  (let* ((object (symmetric-object D-family '(1 2 4 8)))
	 (subg1 '(s0 s1 s2))
	 (subg2 '(s1 s2 s3)))
    (symo:file-print-gv-skel
     object "test-output/D4-1248.skel"
     (highlight-cell-and-neighbors
      object `((,subg1 . ,*red*) (,subg2 . ,*green*)) *blue*)))

  (let* ((object (symmetric-object F-family '(12 1 1 1)))
	 (fam-cubo1 (symo-subgroup object '(s1 s2 s3)))
	 (fam-cubo2 (symo-subgroup object '(s1 s2 s0)))
	 (fam-prism1 (symo-subgroup object '(s0 s2 s3)))
	 (fam-prism2 (symo-subgroup object '(s1 s0 s3)))
	 )
    (assert-equal 1152 (gn:num-live-cosets (cxg/group-net (symo/geometry object))))
    (assert-equal 48 (length (gn:dead-cosets (cxg/group-net (symo/geometry object)))))
    (symo:file-print-gv-skel
     object "test-output/F4-12111.skel"
     (highlight-all-cosets object fam-cubo1 *dblue* #f)
     ))
  
  (let* ((cox-matrix-B2B2
	  (%create-coxeter-matrix
	   (matrix-by-row-list '((1 4 2 2)
				 (4 1 2 2)
				 (2 2 1 4)
				 (2 2 4 1)))))
	 (cox-roots-B2B2
	  (list #(1 -1 0 0) #(0 1 0 0) #(0 0 1 -1) #(0 0 0 1)))
	 (cox-len-B2B2 '(sqrt2 1 sqrt2 1))
	 (B2B2-system
	  (build-cox-geometry cox-matrix-B2B2 cox-len-B2B2 cox-roots-B2B2)))
    (assert-equal 64 (gn:num-live-cosets (cxg/group-net B2B2-system)))
    (assert-equal 21 (length (gn:dead-cosets (cxg/group-net B2B2-system))))
    (let* ((B2B2-full (magic-spec->symmetric-object B2B2-system '(1 1 1 1))))
      (symo:file-print-gv 
       B2B2-full "test-output/B2B2-full.off" 'off-conformal
       (highlight-multigroup-cosets
	B2B2-full
	`(((s0 s1 s2) . ,*purple*) ((s1 s2 s3) . ,*green*))))))

  'pass)
