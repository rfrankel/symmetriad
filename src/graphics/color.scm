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

;;; This file defines an abstraction for colors.

(define-structure (color
		   (constructor %create-color
				(color-list color-string))
		   (conc-name color/))
  (color-list #f read-only #t)
  (color-string #f read-only #t))

(define (color-int:list->string color-list)
  (string-append (number->string (car color-list)) " "
		 (number->string (cadr color-list)) " "
		 (number->string (caddr color-list)) " "
		 (number->string (cadddr color-list)) " "))

(define (list->color lst)
  (%create-color lst (color-int:list->string lst)))

(define (color->string color)
  (cond ((color? color) (color/color-string color))
	((string? color) color)
	((list? color) (color-int:list->string color))
	(else (error "Not a drawable color:" color))))

(define *color:do-not-draw* (string->uninterned-symbol "Non-drawing color"))

(define (color:valid? color)
  (or (not color)
      (color? color)
      (string? color)
      (list? color)
      (eq? color *color:do-not-draw*)
      (eq? color *data:pass*)))

(define (color:drawable? color)
  (if (not (color:valid? color))
      (error "Invalid color:" color))
  (and color
       (not (eq? color *color:do-not-draw*))
       (not (eq? color *data:pass*))))

(define (color:default) *data:pass*)

(define *white* (list->color '(1.0 1.0 1.0 1.0)))
(define *red* (list->color '(1.0 0.0 0.0 1.0)))
(define *dred* (list->color '(0.5 0.0 0.0 1.0)))
(define *lred* (list->color '(1.0 0.5 0.5 1.0)))
(define *green* (list->color '(0.0 1.0 0.0 1.0)))
(define *dgreen* (list->color '(0.0 5.0 0.0 1.0)))
(define *blue* (list->color '(0.0 0.0 1.0 1.0)))
(define *dblue* (list->color '(0.0 0.0 0.5 1.0)))
(define *lblue* (list->color '(0.5 0.5 1.0 1.0)))
(define *yellow* (list->color '(1.0 1.0 0.0 1.0)))
(define *magenta* (list->color '(1.0 0.0 1.0 1.0)))
(define *purple* (list->color '(0.5 0.0 0.5 1.0)))
(define *cyan* (list->color '(0.0 1.0 1.0 1.0)))
(define (param-grey level) (list->color `(,level ,level ,level 1.0)))
(define *grey* (list->color '(0.5 0.5 0.5 1.0)))


