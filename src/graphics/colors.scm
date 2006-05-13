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

;; This file defines the tools needed to create colorings of symmetric
;; objects.  An edge (face) coloring of a symmetric object is represented
;; as a procedure that takes an edge (face) thereof
;; and returns a color (i.e. a list of rgba values) for it.
;; The procedure is expected to already know what symmetric object it is coloring

(declare (usual-integrations))

(load "graphics/object-creation")
(load "group/subgroup")
(load "graphics/color")

(define highlight-touching data:associate-touching)

(define highlight-contained data:associate-contained)

(define highlight-partition data:associate-partition)

(define multihighlight-partition data:associate-partition-alist)

(define (highlight-coset sym-obj subg elt color #!optional default-color)
  (if (default-object? default-color) (set! default-color (color:default)))
  (if (not (subgroup? subg))
      (if (list? subg)
	  (set! subg (symo-subgroup sym-obj subg))
	  (error "Inalid subgroup." subg)))		 
  (highlight-contained sym-obj
		       (subg:get-coset subg elt)
		       color
		       default-color))

; Need a better interface to the colors.
(define (highlight-all-cosets sym-obj subg color
			      #!optional default-color)
  (if (default-object? default-color) (set! default-color (color:default)))
  (highlight-listed-cosets sym-obj subg 
			   (cxg/chamber-list (symo/geometry sym-obj))
			   color
			   default-color))

(define (highlight-listed-cosets sym-obj subg elt-list color 
				 #!optional default-color)
  (if (default-object? default-color) (set! default-color (color:default)))
  (if (not (subgroup? subg))
      (if (list? subg)
	  (set! subg (symo-subgroup sym-obj subg))
	  (error "Inalid subgroup." subg)))		 
  (let ((cosets (lin-rem-dup-eq
		 (map (lambda (elt)
			(subg:get-coset subg elt))
		      elt-list))))
    (display "Highlighting ") (display (length cosets)) 
    (display " cosets") (newline)
    (highlight-partition sym-obj cosets color default-color)))

(define (highlight-multigroup-cosets 
	 sym-obj group-color-alist #!optional default-color)
  (if (default-object? default-color) (set! default-color (color:default)))
  (highlight-listed-multigroup-cosets
   sym-obj group-color-alist (cxg/chamber-list (symo/geometry sym-obj))
   default-color))

(define (highlight-listed-multigroup-cosets 
	 sym-obj group-color-alist elt-list #!optional default-color)
  (if (default-object? default-color) (set! default-color (color:default)))
  (merge-by-first (map (lambda (group-color-pair)
			 (display "Highlighting ")
			 (pp group-color-pair)
			 (highlight-listed-cosets 
			  sym-obj 
			  (symo-subgroup sym-obj (car group-color-pair))
			  elt-list
			  (cdr group-color-pair)))
		       group-color-alist)
		  default-color))

(define (multicolor-listed-cosets 
	 sym-obj subg coset-color-alist #!optional default-color)
  (if (default-object? default-color) (set! default-color (color:default)))
  (if (not (subgroup? subg))
      (if (list? subg)
	  (set! subg (symo-subgroup sym-obj subg))
	  (error "Inalid subgroup." subg)))		 
  (multihighlight-partition
   sym-obj (map (lambda (coset-color-pair)
		  (cons (subg:get-coset subg (car coset-color-pair))
			(cdr coset-color-pair)))
		coset-color-alist)
   default-color))

(define (color-cycle-all-cosets 
	 sym-obj subg color-list #!optional default-color)
  (if (default-object? default-color) (set! default-color (color:default)))
  (if (not (subgroup? subg))
      (if (list? subg)
	  (set! subg (symo-subgroup sym-obj subg))
	  (error "Inalid subgroup." subg)))		 
  (multicolor-listed-cosets 
   sym-obj subg 
   (cyclic-associate (map car (subg:coset-list subg (cxg/chamber-list 
						     (symo/geometry sym-obj))))
		     color-list)
   default-color))

;; Highlights one cell (at identity) from the first subgroup in the list,
;; and its neighboring cells from the others.
(define (highlight-cell-and-neighbors 
	 sym-obj group-color-alist #!optional default-color)
  (if (default-object? default-color) (set! default-color (color:default)))
  (highlight-listed-multigroup-cosets
   sym-obj group-color-alist 
   (subg:get-coset (symo-subgroup sym-obj (caar group-color-alist)) 'e)
   default-color))

