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

;;; This is a file that defines a three-d camera structure
;;; and creates an associate rotation matrix.

(define-structure (camera 
		   (print-procedure 
		    (standard-unparser-method 
		     'CAMERA
		     (lambda (cn port)
		       (write-string " " port)
		       (write `(named ,(cn/name cn)) port))))
		   (constructor %camera
				(name dimension))
		   (conc-name cam/))		
  (name #f read-only #t)
  (dimension 3 read-only #t)
  (eye-point #f)
  (lookat-point #f)
  (lookat-vector #f)
  (up-vector #f)
  (right-vector #f)
  (viewing-transform #f))

;; Makes a simple camera placed at -1 on the z-axis looking 
;; at the origin. 
;; Remember, points have last homogenous coordinate "1", vectors "0".

(define (simple-3d-camera)
  (let ((cam (%CAMERA "Simple-3d-camera" 3))
	(eye-point (vector 0 0 -1 1))
	(lookat-point (vector 0 0 0 1))
	(lookat-vector (vector 0 0 1 0))
	(up-vector (vector 0 1 0 0))
	(right-vector (vector 1 0 0 0)))
    (set-cam/eye-point! cam eye-point)
    (set-cam/lookat-point! cam lookat-point)
    (set-cam/lookat-vector! cam lookat-vector)
    (set-cam/up-vector! cam up-vector)
    (set-cam/right-vector! cam right-vector)))
  

(define (three-d-camera eye-point lookat-point up-vector)
  (let ((cam (%CAMERA "Simple-3d-camera" 3))
	(lookat-vector )
	(up-vector (vector 0 1 0 0))
	(right-vector (vector 1 0 0 0)))
    (set-cam/eye-point! cam eye-point)
    (set-cam/lookat-point! cam lookat-point)
    (set-cam/lookat-vector! cam lookat-vector)
    (set-cam/up-vector! cam up-vector)
    (set-cam/right-vector! cam right-vector)))
  
