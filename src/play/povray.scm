;;; ----------------------------------------------------------------------
;;; Copyright 2012 Alexey Radul and Rebecca Frankel.
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

(let ((object (symmetric-object A-family '(1 0 0 1))))
  (symo->povray
   object
   "playout/A4-1001-1.pov"
   (map cons (all-cosets object '(s1 s2 s3))
        '((0.0 0.5 0.5 1.0) (1.0 0.0 0.0 1.0) (0.0 1.0 0.0 1.0)
          (0.0 0.0 1.0 0.0) (0.5 0.5 0.0 1.0)))
   ))

(define ((colorize color) thing)
  (cons thing color))

;; This one didn't work so well, because in the default projection
;; one of the cells ends up being (almost) space-filling.
(let ((object (symmetric-object A-family '(1 0 0 1))))
  (symo->povray
   object
   "playout/A4-1001-2.pov"
   (append
    (map (colorize '(0.0 0.5 0.5 1.0))
         (all-cosets object '(s1 s2 s3)))
    (map (colorize '(0.5 0.5 0.0 1.0))
         (all-cosets object '(s0 s1 s2))))
   ))

(let* ((object (symmetric-object A-family '(1 0 0 1))))
  (symo:file-print-gv
   object "playout/A4-1001-1.off" 'off-conformal
   #;
   (merge-by-first
    (list (highlight-all-cosets
           object '(s0 s1 s2) '(147 112 219 255))
          (highlight-all-cosets
           object '(s3 s1 s2) '(187 12 119 255))
          ))
   (highlight-coset object '(s1 s2 s3) 'e '(0.0 0.5 0.5 1.0))))

(let ((object (symmetric-object A-family '(1 0 0 1))))
  (symo:file-print-polyhedra
   object
   "playout/A4-1001-1.poly"
   (map cons (all-cosets object '(s1 s2 s3))
        '((0.0 0.5 0.5 1.0) (1.0 0.0 0.0 1.0) (0.0 1.0 0.0 1.0)
          (0.0 0.0 1.0 0.0) (0.5 0.5 0.0 1.0)))
   ))

(let ((object (symmetric-object B-family '(1 0 0 1))))
  (symo->povray
   object
   "playout/B4-1001-1.pov"
   (map (colorize '(0.0 0.5 0.5 1.0))
        (all-cosets object '(s1 s2 s0)))
   ))

(let ((object (symmetric-object B-family '(3 0 0 1))))
  (symo->povray
   object
   "playout/shadows-of-the-tesseract.pov"
   (map (colorize '(0.0 0.5 0.5 1.0))
        (all-cosets object '(s1 s2 s3)))
   "../tools/header-2.pov"))

(let ((object (symmetric-object F-family '(1 1 1 9))))
  (symo->povray
   object
   "playout/F4-1119.pov"
   (append
    (map (colorize '(0.9 0.7 0.5 1.0))
         (all-cosets object '(s0 s1 s2)))
    (map (colorize '(0.5 0.3 0.1 1.0))
         (all-cosets object '(s0 s1 s3))))
   "../tools/header-2.pov"))
