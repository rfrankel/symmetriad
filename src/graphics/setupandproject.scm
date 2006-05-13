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

;; This will take a list of vectors 
;; augment them to create homogenous vectors
;; and then project them. 

(declare (usual-integrations *))
(load "group/coxpresent")
(load "graphics/projection")
(load-option 'format)

;; this makes a list of vectors 
;; into a list of homogenous column vectors

(define (augment-to-homogenous list-of-vects)
  (map 
   (lambda (vect)
     (let ((vect-len (vector-length vect)))
       (s:generate 
	(+ vect-len 1) 'up
	(lambda (i)
	  (if (= i vect-len)
	      1
	      (vector-ref vect i))))))
   list-of-vects))

(define (project-list projection three-vects)
  (let ((hom-vects (augment-to-homogenous three-vects))
	(proj-matrix (proj/viewing-transform projection)))
    (map 
     (lambda (hom-vect)
       (* proj-matrix hom-vect))
      hom-vects)))

(define (project-three-d projection three-vector)
  (let ((homog-vect (s:generate 4 'up
		     (lambda (i) (if (= i 3) 1
				     (vector-ref three-vector i)))))
    	(proj-matrix (proj/viewing-transform projection)))
    (let ((homog-answer (* proj-matrix homog-vect)))
      (s:generate 
       2 'up
       (lambda (i)
	 (if (not (= 0 (ref homog-answer 3)))
	     (/ (ref homog-answer i) (ref homog-answer 3))
	     (ref homog-answer i)))))))


(define (de-augment ups-list)
  (map 
   (lambda (up)
     (let ((length (size up)))
       (let ((w (ref up (- length 1))))
	 (s:generate 
	  (- length 1) 'up
	  (lambda (i)
	    (if (not (= 0 w))
		(/ (ref up i) w)
		(ref up i)))))))
   ups-list))
		    
(define (plot-vects projected-list)
  ;(define win (frame -1.5 1.5 -1.5 1.5))
   
  (for-each 
   (lambda (down)
     (let ((x (ref down 0))
	   (y (ref down 1)))
       (plot-line win 0 0 x y)))
   projected-list))

(define cube-list
  (list (vector 1 1 1)
	(vector 1 -1 1)
	(vector -1 -1 1)
	(vector -1 1 1)
	(vector 1 1 -1)
	(vector 1 -1 -1)
	(vector -1 -1 -1)
	(vector -1 1 -1)))
	

(define orth (make-simple-orthogonal 'orth 3))

(define farpersp (make-simple-perspective 'persp 3 2))

(define nearpersp (make-simple-perspective 'persp 3 1))

(define cav (make-cavalier 'cav 3 (/ pi 6)))

(define cab (make-cabinet 'cab 3 (/ pi 6)))

    
#|
(augment-to-homogenous
 (cox-simple-roots (make-cox-matrix-B/C 3) 3))

(define proj (make-simple-orthogonal 'orth 3))

(pp
(project-list proj (cox-simple-roots (make-cox-matrix-B/C 3) 3)))

(plot-vects 
(project-list cab (cox-simple-roots (make-cox-matrix-B/C 3) 3)))
 
(plot-vects 
(project-list cab (cox-simple-roots (make-cox-matrix-A 3) 3)))

(plot-vects
(project-list cab (cox-simple-roots (make-cox-matrix-D 3) 3)))



(define orth (make-simple-orthogonal 'orth 3))

(define farpersp (make-simple-perspective 'persp 3 2))

(define nearpersp (make-simple-perspective 'persp 3 1))

(define cav (make-cavalier 'cav 3 (/ pi 6)))

(define cab (make-cabinet 'cab 3 (+ (/ pi 6) .2)))

(define win (frame -2 2 -2 2))

(plot-cube (project-list orth cube-list))
(plot-cube (project-list cav cube-list))
(plot-cube (project-list cab cube-list))
(plot-mp-cube (project-list cab cube-list) "cab")
(plot-mp-cube (project-list cav cube-list) "cav")

(plot-cube (de-augment (project-list farpersp cube-list)))
(plot-cube (de-augment (project-list nearpersp cube-list)))

(define farpersp (make-z-perspective 'persp 3 2 4))
(define nearpersp (make-z-perspective 'persp 3 1 3))

(define newpersp (make-gen-perspective 'persp 3 1 3 2 2.2))

(plot-cube (de-augment (project-list newpersp cube-list)))

|#


(define (plot-cube projected-list)
  (let ((furx (ref (list-ref projected-list 0) 0))
	(fury (ref (list-ref projected-list 0) 1))
	(flrx (ref (list-ref projected-list 1) 0))
	(flry (ref (list-ref projected-list 1) 1))
	(fllx (ref (list-ref projected-list 2) 0))
	(flly (ref (list-ref projected-list 2) 1))
	(fulx (ref (list-ref projected-list 3) 0))
	(fuly (ref (list-ref projected-list 3) 1))
	(burx (ref (list-ref projected-list 4) 0))
	(bury (ref (list-ref projected-list 4) 1))
	(blrx (ref (list-ref projected-list 5) 0))
	(blry (ref (list-ref projected-list 5) 1))
	(bllx (ref (list-ref projected-list 6) 0))
	(blly (ref (list-ref projected-list 6) 1))
	(bulx (ref (list-ref projected-list 7) 0))
	(buly (ref (list-ref projected-list 7) 1)))

    ;(define win (frame -4 4 -4 4))
    (graphics-clear win)
    ;; front of cube
    (plot-line win furx fury flrx flry)
    (plot-line win flrx flry fllx flly)
    (plot-line win fllx flly fulx fuly)
    (plot-line win fulx fuly furx fury)
    ;; back of cube
    (plot-line win burx bury blrx blry)
    (plot-line win blrx blry bllx blly)
    (plot-line win bllx blly bulx buly)
    (plot-line win bulx buly burx bury)
    ;; right-side 
    ;(plot-line win furx fury flrx flry  bllx blly bulx buly)
    (plot-line win furx fury flrx flry)
    (plot-line win flrx flry blrx blry)
    (plot-line win blrx blry burx bury)
    (plot-line win burx bury furx fury)
    ;; left-side 
    (plot-line win fulx fuly fllx flly)
    (plot-line win fllx flly bllx blly)
    (plot-line win bllx blly bulx buly)
    (plot-line win bulx buly fulx fuly)))

;; We need a number after the z for metapost to be happy
(define (plot-mp-cube projected-list name)
  (let ((file-name (string-append 
		     "/home/rfrankel/pc-home/context/mp" name ".tex"))
	(backslash (ascii->char 92)))
    (let ((furx (ref (list-ref projected-list 0) 0))
	  (fury (ref (list-ref projected-list 0) 1))
	  (flrx (ref (list-ref projected-list 1) 0))
	  (flry (ref (list-ref projected-list 1) 1))
	  (fllx (ref (list-ref projected-list 2) 0))
	  (flly (ref (list-ref projected-list 2) 1))
	  (fulx (ref (list-ref projected-list 3) 0))
	  (fuly (ref (list-ref projected-list 3) 1))
	  (burx (ref (list-ref projected-list 4) 0))
	  (bury (ref (list-ref projected-list 4) 1))
	  (blrx (ref (list-ref projected-list 5) 0))
	  (blry (ref (list-ref projected-list 5) 1))
	  (bllx (ref (list-ref projected-list 6) 0))
	  (blly (ref (list-ref projected-list 6) 1))
	  (bulx (ref (list-ref projected-list 7) 0))
	  (buly (ref (list-ref projected-list 7) 1))
	  (port (open-output-file file-name)))
   (format port "~Asetupoutput[pdftex]~%" backslash)
   (format port "~Astarttext~%" backslash)
   (format port "~AstartreusableMPgraphic{~Acube}~%" backslash name)
   (format port "z0fur = (~Acm, ~Acm);~%" furx fury)
   (format port "z0flr = (~Acm, ~Acm);~%" flrx flry)
   (format port "z0fll = (~Acm, ~Acm);~%" fllx flly)
   (format port "z0ful = (~Acm, ~Acm);~%" fulx fuly)
   (format port "z0bur = (~Acm, ~Acm);~%" burx bury)
   (format port "z0blr = (~Acm, ~Acm);~%" blrx blry)
   (format port "z0bll = (~Acm, ~Acm);~%" bllx blly)
   (format port "z0bul = (~Acm, ~Acm);~%" bulx buly)
   ;; front
   (format port "draw z0fur--z0flr--z0fll--z0ful--cycle;~%")
   ;; back
   (format port "draw z0bur--z0blr--z0bll--z0bul--cycle;~%")
   ;; left
   (format port "draw z0ful--z0fll--z0bll--z0bul--cycle;~%")
   ;; right
   (format port "draw z0fur--z0flr--z0blr--z0bur--cycle;~%")
   ;; labels
   (format port "label(\"fur\", z0fur);~%")
   (format port "label(\"flr\", z0flr);~%")
   (format port "label(\"fll\", z0fll);~%")
   (format port "label(\"ful\", z0ful);~%")
   (format port "~AstopreusableMPgraphic~%" backslash)
   (format port "~AuseMPgraphic{~Acube}~%" backslash name)
   (format port "~Astoptext~%" backslash)
   (flush-output port)
   (close-all-open-files))))
   
