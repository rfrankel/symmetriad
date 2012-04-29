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

;;; Example 11: H4

#|
 (define coxsub (make-group-presentation '(s0) '((s0 s0))))
 (define cox-pres
   (cox-presentation (make-cox-matrix-H 4) H4-lengths))
 (define cox-group-net (group-network cox-pres coxsub "Autogen"))

 ; Works!  Takes its sweet time, but works!
 ; 1367 dead cosets.
 (gn:hlt! cox-group-net)
 (assert-valid-group-net cox-group-net)
 (gc-flip)

 (count-interned-symbols)
|#

; With numeric computation, (using usual-integrations), 
; and without the tms, the build takes
; about a minute and a half on averdon and almost 3 on tumunzahar. 
; ~63000 elts in the root-table
(define H4-play (geom-family->cox-geometry H-family 4))

; It builds!  About 7 seconds on averdon
; And only takes about 0.5 megawords...
(define 600-cell (magic-spec->symmetric-object H4-play '(1 0 0 0)))
; (define 600-cell #f)
; (cxg:clear-cache! H4-play)
; (gfam:clear-cache! F-family)

; Fast for simple functions, but slows down with complexity
(symo:file-print-gv-skel
 600-cell
 "playout/600-cell.skel"
 (color-cycle-all-cosets
  600-cell '(s0) `(,*green* ,*blue*) *red*)
 ;(lambda (edge) *red*)
 )

; It builds!  About 10 seconds on averdon, dominated by
; post-component stuff And only takes about 0.6 megawords...
(define H4-1001 (magic-spec->symmetric-object H4-play '(1 0 0 1)))
; (define H4-1001 #f)
; (cxg:clear-cache! H4-play)

; Coloring takes no more than about 10 seconds.
(symo:file-print-gv-skel
 H4-1001
 "playout/H4-1001.skel"
 (highlight-multigroup-cosets
  H4-1001 `(((s0) . ,*red*) ((s3) . ,*purple*)) *grey*)
 ; (color-cycle-all-cosets H4-1001 '(s3 s1 s2) (list *red* *blue* *green* *purple* *magenta*))
 ; (highlight-all-cosets H4-1001 '(s0) *red* *blue*)
 )

(let ((object (magic-spec->symmetric-object H4-play '(1 0 0 3))))
  (symo:file-print-gv-skel
   object
   "playout/H4-1003.skel"
   (highlight-all-cosets object '(s0) '(0.0 0.5 0.0 1.0))))
 
; Set Geomview into spherical mode, conformal view *before* loading 
; objects made like this 
(let ((object (magic-spec->symmetric-object H4-play '(1 0 0 3))))
  (symo:file-print-oogl-off
   object
   "playout/H4-1003.off"
   (highlight-all-cosets object '(s0 s1 s2) '(0.0 0.5 0.0 1.0))
   'off-conformal))
 
(let ((object (magic-spec->symmetric-object H4-play '(6 0 0 1))))
  (symo:file-print-gv-skel
   object
   "playout/H4-6001.skel"
   (highlight-all-cosets object '(s3) '(0.0 0.0 0.5 1.0))))
 
; Set Geomview into spherical mode, conformal view *before* loading 
; objects made like this 
(let ((object (magic-spec->symmetric-object H4-play '(6 0 0 1))))
  (symo:file-print-oogl-off
   object
   "playout/H4-6001.off"
   (highlight-all-cosets object '(s1 s2 s3) '(0.0 0.0 0.5 1.0))
   'off-conformal))

; Set Geomview into spherical mode, conformal view *before* loading 
; objects made like this 
(let ((object (magic-spec->symmetric-object H4-play '(6 0 1 1))))
  (symo:file-print-oogl-off
   object
   "playout/H4-6011.off"
   (highlight-all-cosets object '(s1 s2 s3) '(0.5 0.0 0.0 1.0))
   'off-conformal))

(let ((object (magic-spec->symmetric-object H4-play '(6 0 1 1))))
  (symo:file-print-gv
   object
   "playout/H4-6011.off"
   'off-virtual
   (highlight-all-cosets object '(s1 s2 s3) '(0.5 0.0 0.0 1.0))
   ))
 

; about 12 seconds on averdon, if
; assert-collapse-matches-vertex-group is disabled in
; compute-magic-spec->symmetric-object 0.8 megawords
(define H4-full (magic-spec->symmetric-object H4-play '(1 1 1 1)))
; (define H4-full #f)
; (cxg:clear-cache! H4-play)

(symo:file-print-gv-skel
 H4-full
 "playout/H4-full.skel"
 ; This is "fast" -- about 25 seconds on tumunzahar, dominated by the
 ; vertices
 (lambda (edge) *purple*)
 )

(let ((object (symmetric-object H-family '(1 1 1 4))))
  (symo:file-print-gv
   object
   "playout/H4-1114.off"
   'off-conformal
   (highlight-all-cosets object '(s0 s1 s2) '(147 112 219 255))))

(gfam:clear-cache! H-family)

(let ((object (symmetric-object H-family '(1 0 0 3))))
  (symo:file-print-gv
   object
   "playout/H4-1003.off"
   'off-conformal
   (highlight-all-cosets object '(s0 s1 s2) '(51 0 204 255))))

(let ((object (symmetric-object H-family '(3 0 0 1))))
  (symo:file-print-gv
   object
   "playout/H4-3001.off"
   'off-conformal
   (highlight-all-cosets object '(s3 s1 s2) '(147 112 219 255))))

(let ((object (symmetric-object H-family '(9 0 0 1))))
  (symo:file-print-gv
   object
   "playout/H4-9001.off"
   'off-conformal
   (highlight-all-cosets object '(s3 s1 s2) '(147 112 219 255))))

(let ((object (symmetric-object H-family '(1 0 0 1))))
  (symo:file-print-gv
   object
   "playout/H4-1001.off"
   'off-conformal
   (highlight-all-cosets object '(s0 s1 s3) '(51 0 204 255))))

(let ((object (symmetric-object H-family '(3 0 0 1))))
  (symo:file-print-gv
   object
   "playout/H4-3001-prisms.off"
   'off-conformal
   (highlight-all-cosets object '(s0 s2 s3) '(51 0 204 255))))

(let ((object (symmetric-object H-family '(0 1 0 0))))
  (symo:file-print-gv
   object
   "playout/H4-0100.off"
   'off-conformal
   (merge-by-first 
    (list
     (highlight-all-cosets object '(s0 s1 s2) '(0 0 102 255))))
     (highlight-all-cosets object '(s3 s1 s2) '(255 102 153 255))
   ))

