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

;;; Procedures for actually drawing stuff

;; Default projection is orthographic
;; This function interacts with scheme graphics
;; Does not handle 4D objects
(define (symo:draw sym-obj win #!optional projection)
  (let* ((dim (symo:dimension sym-obj))
	 (project-proc 
	  (cond ((or (default-object? projection) (= dim 2))
		 (lambda (vect) (up (ref vect 0) (ref vect 1))))
		((= dim 3)
		 (lambda (vect) (project-three-d projection vect)))
		(else
		 (error "Given projection does not match dimension.")))))
    (for-each (lambda (edge)
		(let* ((pt1 (project-proc (symo:get-vertex sym-obj (car edge))))
		       (pt2 (project-proc (symo:get-vertex sym-obj (cadr edge)))))
		  ;(pp (list pt1 pt2))
		  (plot-line win (ref pt1 0) (ref pt1 1)
			     (ref pt2 0) (ref pt2 1))))
	      (symo:unique-edge-list sym-obj))))

(define (add-three-d-axes win projection)
  (for-each (lambda (axis)
	      (let ((proj-zero (project-three-d projection (up 0 0 0)))
		    (proj-axis (project-three-d projection axis)))
		(plot-line win (ref proj-zero 0) (ref proj-zero 1)
			   (ref proj-axis 0) (ref proj-axis 1))))
	    (list (up 1 0 0) (up 0 1 0) (up 0 0 1))))


; At the moment, only works for 3D objects.  Breaks silently if the 
; dimensions are off.
(define (symo:print-vrml sym-obj)
  (define (hack-map chamber-symb)
    (symo:rep-index sym-obj chamber-symb))

  (define (print-VRML-header)
    (display "#VRML V2.0 utf8") (newline) (newline)
    (display "Shape {") (newline)
    (display "  appearance Appearance {") (newline)
    (display "    material Material {emissiveColor 0 1 0} }") (newline)
    (display "  geometry IndexedLineSet {") (newline)
    (display "    coord Coordinate { point [") (newline))
  (define (print-vertices sym-obj)
    (for-each (lambda (vertex-symb)
		(display "      ")
		(let ((vertex-coords (symo:get-vertex sym-obj vertex-symb)))
		  (for-each (lambda (coord) (display coord) (display ", "))
			    (up-structure->list vertex-coords)))
		(newline))
	      (symo/unique-vertices sym-obj)))
  (define (print-vert-edge-transition)
    (display "    ] }") (newline)
    (display "    coordIndex [") (newline))
  (define (print-edges sym-obj)
    (for-each (lambda (edge)
		(let* ((ind1 (hack-map (car edge)))
		       (ind2 (hack-map (cadr edge))))
		  (display "      ") (display ind1) (display ", ")
		  (display ind2) (display ", -1,") (newline)))
	      (symo:unique-edge-list sym-obj)))
  (define (print-VRML-footer)
    (display "    ]") (newline)
    (display "  }") (newline)
    (display "}") (newline))    

  (print-VRML-header)
  (print-vertices sym-obj)
  (print-vert-edge-transition)
  (print-edges sym-obj)
  (print-VRML-footer))

(define (symo:file-print-vrml sym-obj filename)
  (with-output-to-file-ensuring-path
      filename
    (lambda ()
      (symo:print-vrml sym-obj))))


(define (symo:print-gv sym-obj format #!optional color-proc)
  (if (default-object? color-proc) 
      (set! color-proc (lambda (edge) (list 0.0 0.0 0.0 0.0))))
  (if (list? color-proc)
      (begin 
	(display "LIST") (newline)
	(map (lambda (color-sub-proc)
	       (display "{") (newline)
	       (symo:print-gv sym-obj format color-sub-proc)
	       (display "}") (newline))
	     color-proc))
      (symo:print-gv-help sym-obj format color-proc)))

(define (symo:print-gv-help sym-obj format color-proc)
  (define (hack-map chamber-symb)
    (symo:rep-index sym-obj chamber-symb))
  (define drawee-selector 
    (if (eq? format 'skel)
	symo:unique-edge-list
	symo/face-list))
  (define drawee-color-list 
    (filter (lambda (x) x)
	    (map (lambda (drawee)
		   (let ((color (color-proc drawee)))
		     (if (color:drawable? color) 
			 (cons drawee color) #f)))
		 (drawee-selector sym-obj))))
  
  (define (print-header sym-obj format)
    (cond ((eq? format 'off-virtual)
	   (display "nOFF") (newline)
	   (display (symo:dimension sym-obj)) (newline))
	  ((eq? format 'off-conformal)
	   ; Set Geomview into spherical mode, conformal view *before* 
	   ; loading objects made like this 
	   (if (not (= (symo:dimension sym-obj) 4))
	       (error "Do not draw non-4D objects in 4OFF mode."))
	   (display "4OFF") (newline))
	  ((eq? format 'skel)
	   (display "nSKEL") (newline)
	   (display (symo:dimension sym-obj)) (newline))
	  (else (error "Illegal format" format)))
    (let* ((nverts (length (symo/unique-vertices sym-obj)))
	   (ndrawees (length drawee-color-list)))
      (display nverts) (display " ")
      (display ndrawees)
      (if (not (eq? format 'skel))
	  (display " 0")) ; OFF formats demand but do not use edges
      (newline)))
  (define (print-drawee drawee color)
    (display (length drawee)) (display " ")
    (for-each (lambda (drawee-elt)
		(display (hack-map drawee-elt))
		(display " "))
	      drawee)
    (display (color->string color))
    (newline))
  (define (print-drawees drawee-color-list)
    (for-each 
     (lambda (drawee)
       (print-drawee (car drawee) (cdr drawee)))
     drawee-color-list))

  (print-header sym-obj format)
  (display "Printing vertices... " (notification-output-port))
  (print-vertices sym-obj)
  (if (eq? format 'skel)
      (display "Printing edges... " (notification-output-port))
      (display "Printing faces... " (notification-output-port)))
  (print-drawees drawee-color-list)
  (display "Done." (notification-output-port))
  (newline (notification-output-port))

)

(define (print-vertices sym-obj)
  (for-each (lambda (vertex-symb)
	      (let ((vertex-coords (symo:get-vertex sym-obj vertex-symb)))
		(for-each (lambda (coord) 
			    (display (exact->inexact coord)) (display " "))
			  (up-structure->list vertex-coords)))
	      (newline))
	    (symo/unique-vertices sym-obj)))

(define (symo:print-oogl-off sym-obj #!optional color-proc format)
  (if (default-object? color-proc)
      (symo:print-gv sym-obj 'off-virtual)
      (if (default-object? format)
	  (symo:print-gv sym-obj 'off-virtual color-proc)
	  (symo:print-gv sym-obj format color-proc))))

(define (symo:file-print-oogl-off sym-obj filename #!optional color-proc format)
  (with-output-to-file-ensuring-path
      filename
    (lambda ()
      (if (default-object? color-proc)
	  (symo:print-oogl-off sym-obj)
	  (if (default-object? format)
	      (symo:print-oogl-off sym-obj color-proc)
	      (symo:print-oogl-off sym-obj color-proc format)
	      )))))

(define (symo:print-gv-skel sym-obj #!optional color-proc)
  (if (default-object? color-proc)
      (symo:print-gv sym-obj 'skel)
      (symo:print-gv sym-obj 'skel color-proc)))

(define (symo:file-print-gv-skel sym-obj filename #!optional color-proc)
  (with-output-to-file-ensuring-path
      filename
    (lambda ()
      (if (default-object? color-proc)
	  (symo:print-gv-skel sym-obj)
	  (symo:print-gv-skel sym-obj color-proc)))))

(define (symo:file-print-gv sym-obj filename format #!optional color-proc)
  (with-output-to-file-ensuring-path
      filename
    (lambda ()
      (if (default-object? color-proc)
	  (symo:print-gv sym-obj format)
	  (symo:print-gv sym-obj format color-proc)
	  ))))

(define (symo:file-print-both sym-obj file-base #!optional color-spec)
  (symo:file-print-gv
   sym-obj
   (string-append file-base ".off")
   'off-conformal
   color-spec)
  (symo:file-print-gv
   sym-obj
   (string-append file-base ".skel")
   'skel
   color-spec))

;;; Print out the selected polyhedra in a format of my own devising, loosely
;;; based on Geomview's OFF format.  The polyhedra are selected by an alist of
;;; vertex-list to color.  Each vertex-list is expected to be one polyhedron.  The
;;; purpose of the exercise is to provide the polyhedra explicitly to downstream
;;; processors, along with the faces in each polyhedron.
;;;
;;; The output format is as follows:
;;; First, there is a header, consisting of the word "Polyhedra" on one line,
;;; and the number of vertices and the number of polyhedra on the next line.
;;; After this comes the list of vertices, just like in OFF.
;;; After this come the polyhedra, one by one.
;;; Each polyhedron is:
;;; - A line giving the number of vertices in the polyhedron, the number of
;;;   faces in the polyhedron, and then the vertices in the polyhedron by reference
;;;   into the vertex list (zero-indexed).
;;; - Lines giving faces (as many as promised).  Each face is formatted the same
;;;   way as a polygon in OFF format.
(define (symo:print-polyhedra sym-obj hedron-specs)
  (define (print-header)
    (display "Polyhedra") (newline)
    (display (length (symo/unique-vertices sym-obj))) (display " ")
    (display (length hedron-specs)) (newline))
  (define (print-hedron spec)
    ;; TODO Abstract commonalities with symo:print-gv-help
    (define (hack-map chamber-symb)
      (symo:rep-index sym-obj chamber-symb))
    (define vertices (car spec))
    (define color (cdr spec))
    (define color-spec (highlight-contained sym-obj vertices color (color:default)))
    (define face-color-list 
      (filter (lambda (x) x)
	      (map (lambda (face)
		     (let ((color (color-spec face)))
		       (if (color:drawable? color) 
			   (cons face color) #f)))
		   (symo/face-list sym-obj))))
    (define (print-face face color)
      (display (length face)) (display " ")
      (for-each (lambda (face-elt)
		  (display (hack-map face-elt))
		  (display " "))
		face)
      (display (color->string color))
      (newline))
    (define (print-faces face-color-list)
      (for-each 
       (lambda (face)
	 (print-face (car face) (cdr face)))
       face-color-list))
    ;; TODO Why are there non-eq vertices in the input list that map to the same vertex index?
    (define vert-indexes (delete-duplicates (map hack-map vertices) =))
    (display (length vert-indexes)) (display " ")
    (display (length face-color-list)) (display " ")
    (for-each (lambda (index)
		(display index)
		(display " "))
	      vert-indexes)
    (newline)
    (print-faces face-color-list))
  (print-header)
  (print-vertices sym-obj)
  (for-each print-hedron hedron-specs))

(define (symo:file-print-polyhedra sym-obj filename hedron-specs)
  (with-output-to-file-ensuring-path
      filename
    (lambda ()
      (symo:print-polyhedra sym-obj hedron-specs))))