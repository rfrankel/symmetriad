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

(load "graphics/drawing")

;;; Example 6: The icosohedral group H3
(define H3-system (geom-family->cox-geometry H-family 3))

(pp (omega-dirs H3-system))

(symo:draw (cartesian-point->symmetric-object 
	    H3-system 
	    (caddr (omega-vectors (omega-dirs H3-system) 0)))
	   (frame -2 2 -2 2) cab)

(symo:draw (cartesian-point->symmetric-object 
	    H3-system 
	    (car (omega-vectors (omega-dirs H3-system) 2)))
	   (frame -2 2 -2 2) cab)

;; Booyah!
;; Great rhombicosidodecahdron!
(symo:draw (magic-spec->symmetric-object H3-system '(1 1 1))
	   (frame -2 2 -2 2) cab)

(symo:file-print-vrml (magic-spec->symmetric-object H3-system '(0 0 1))
		      "playout/dodecahedron.wrl")
(symo:file-print-vrml (magic-spec->symmetric-object H3-system '(1 0 0))
		      "playout/icosohedron.wrl")
(symo:file-print-vrml (magic-spec->symmetric-object H3-system '(1 1 1))
		      "playout/great-rhombicosidodecahedron.wrl")
(symo:file-print-oogl-off 
 (magic-spec->symmetric-object H3-system '(1 0 0))
 "playout/icosohedron.off" 
 (lambda (face) (list .5 .5 .5)))

(let ((object (symmetric-object H-family '(1.0 0.3 0.1))))
  (symo:file-print-gv-skel 
   object
   "playout/H3-play-2.skel" 
   (merge-by-first (list) *white*)))

(let ((object (symmetric-object H-family '(1.0 0.3 0.1))))
  (symo:file-print-gv-skel 
   object
   "playout/H3-play-2.skel" 
   (merge-by-first (list
		    (highlight-all-cosets object '(s0) '(1.0 0.0 0.0))
		    (highlight-all-cosets object '(s1) '(0.0 1.0 0.0))
		    (highlight-all-cosets object '(s2) '(0.0 0.0 1.0))
		    ) '(1.0 1.0 1.0 1.0))))

(let ((object (symmetric-object H-family '(1.0 0.3 0.1))))
  (symo:file-print-gv-skel 
   object
   "playout/H3-play-3.skel" 
   (highlight-multigroup-cosets object `(((s0) . ,*red*) 
					 ((s1) . ,*blue*) 
					 ((s2) . ,*green*))
				*white*)))

(let ((object (symmetric-object H-family '(1 1 1))))
  (symo:file-print-gv-skel 
   object
   "playout/H3-play.skel" 
   (merge-by-first (list
		    (highlight-all-cosets object '(s0)
					  '(1.0 0.0 0.0))
		    (highlight-all-cosets object '(s1)
					  '(0.0 1.0 0.0))
		    (highlight-all-cosets object '(s2)
					  '(0.0 1.0 1.0))
		    ) '(1.0 1.0 1.0 1.0))))

(let ((object (symmetric-object H-family '(1 0 0))))
  (symo:file-print-gv-skel 
   object
   "playout/H3-play-4.skel" 
   (merge-by-first (list
		    (highlight-all-cosets object '(s0)
					  '(1.0 0.0 0.0))
		    (highlight-all-cosets object '(s1)
					  '(0.0 1.0 0.0))
		    (highlight-all-cosets object '(s2)
					  '(0.0 1.0 1.0))
		    ) '(1.0 1.0 1.0 1.0))))

