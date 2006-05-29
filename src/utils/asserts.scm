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

(define (assert-true boolean)
  (if boolean
      'pass
      (error "Should have been true, was false.")))

(define (assert-equal thing1 thing2)
  (if (equal? thing1 thing2)
      'pass
      (error "Things differ" (cons thing1 thing2))))

(define (assert-same-file filename1 filename2)
  (let ((file-contents1 
	 (with-input-from-file filename1
	   (lambda () (read-string (chars->char-set ())))))
	(file-contents2 
	 (with-input-from-file filename2
	   (lambda () (read-string (chars->char-set ())))))
	)
    (if (equal? file-contents1 file-contents2)
	'pass
	(error (string-append "Files differ, " filename1 "."))
	)))

(define (assert-vrml-prints-correctly sym-obj filename)
  (symo:file-print-vrml sym-obj (string-append "testout/" filename))
  (assert-same-file (string-append "testdata/" filename)
		    (string-append "testout/" filename)))

(define (assert-off-prints-correctly sym-obj filename)
  (symo:file-print-oogl-off sym-obj (string-append "testout/" filename))
  (assert-same-file (string-append "testdata/" filename)
		    (string-append "testout/" filename)))

(define (assert-roots-match-lengths lengths roots)
  (for-each (lambda (len root)
	      (set! root (unquote (simplify root)))
	      (set! len (unquote (simplify len)))
	      (assert (close-numbers len (sqrt (dot-product root root)))
		      "Root does not agree with length."
		      (list root len)))
	    (substitute-multiple lengths symbol-dict)
	    (substitute-multiple roots symbol-dict)))

(define (assert-roots-match cox-matrix lengths roots)
  (assert-roots-match-lengths lengths roots)
  (define (normalized-root index)
    (unquote (simplify (substitute-multiple 
			(vector/scalar (list-ref roots index) 
				       (list-ref lengths index))
			symbol-dict))))    
  (let ((n (length roots)))
    (assert (= n (length lengths)))
    (assert (= n (cm:dimension cox-matrix)))
    (for-each 
     (lambda (ind1)
       (for-each
	(lambda (ind2)
	  (let ((cosine (simplify 
			 (substitute-multiple
			  (sym-cox-cos (cm:matrix-ref cox-matrix ind1 ind2))
			  symbol-dict)))
		(root1 (normalized-root ind1))
		(root2 (normalized-root ind2)))
	    (assert (close-numbers cosine (dot-product root1 root2))
		    "Roots fail to match matrix."
		    (list cox-matrix roots))))
	(enumerate-interval 0 (- n 1))))
     (enumerate-interval 0 (- n 1)))))

(define (assert-relation-satisfied group-table coset relation)
  (define (relation-walk cur-coset relation-left)
    (if (= 1 (length relation-left))
	(lookup-mult group-table cur-coset (car relation-left))
	(let ((result (lookup-mult group-table cur-coset (car relation-left))))
	  (assert (not (eq? result coset))
		  "Relation redundant."
		  (list coset relation))
	  (relation-walk result (cdr relation-left)))))
  (let ((end (relation-walk coset (gr:relation-list relation))))
    (assert (eq? end coset)
	    "Relation unsatisfied."
	    (list  coset relation end))))

(define (assert-relations-satisfied group-table coset relations)
  (for-each (lambda (relation)
	      (assert-relation-satisfied group-table coset relation))
	    relations))

(define (assert-valid-group-net gn)
  (let ((cosets (gn:coset-list gn))
	(mult-table (gn:multiplication-table gn))
	(relations (gp:relations-list (gn:presentation gn))))
    (for-each (lambda (coset)
		(assert-relations-satisfied mult-table coset relations))
	      cosets)))

(define (assert-correct-stats sym-obj #!optional num-verts num-faces)
  (if (not (default-object? num-verts)) 
      (assert (= num-verts (length (symo/unique-vertices sym-obj)))
	      "Wrong number of unique vertices."
	      sym-obj))
  (if (not (default-object? num-faces)) 
      (assert (= num-faces (length (symo/face-list sym-obj)))
	      "Wrong number of unique faces."
	      sym-obj))
  'pass)

(define (assert-verts-on-sphere sym-obj #!optional radius)
  (define (helper sym-obj radius)
    (for-each (lambda (vert-symbol)
		(let* ((vertex (symo:get-vertex sym-obj vert-symbol))
		       (vert-radius (sqrt (dot-product vertex vertex))))
		  (assert (close-numbers radius vert-radius)
			  "Vertex radius does not match expectation."
			  (list radius vert-radius))))
	      (symo/unique-vertices sym-obj))
    'pass)
  (display "Checking that the vertices are all on a sphere... ")
  (if (default-object? radius)
      (let* ((e-vert (symo:get-vertex sym-obj 'e))
	     (e-rad (sqrt (dot-product e-vert e-vert))))
	(helper sym-obj e-rad))
      (helper sym-obj radius))
  (display "done.") (newline))

(define (assert-face-shape-stats sym-obj stat-spec)
  (let ((multi-set (alist->mset stat-spec)))
    (assert (mset:same-set multi-set (symo:face-stats sym-obj))
	    "Object fails to satisfy desired stats."
	    (list sym-obj stat-spec))
    'pass))

; Linear in number of vertex-face pairs where the face contains the vertex
(define (assert-correct-face-vertex-incidence-stats sym-obj)
  (display "Checking that the face and vertex incidence statistics are correct... ")
  (let ((vert-face-set-hash (make-eq-hash-table)))
    (for-each (lambda (vert)
		(hash-table/put! vert-face-set-hash vert (make-multi-set)))
	      (symo/unique-vertices sym-obj))
    (for-each 
     (lambda (face)
       (let ((len (length face)))
	 (for-each (lambda (face-vert)
		     (mset:add! (hash-table/get vert-face-set-hash face-vert #f)
				len))
		   face)))
     (symo/face-list sym-obj))
    (let ((num-verts (length (symo/unique-vertices sym-obj)))
	  (face-stats (symo:face-stats sym-obj))
	  (answer (make-multi-set)))
      (for-each 
       (lambda (elt)
	 (mset:add! answer elt (/ (* elt (mset:number face-stats elt)) num-verts)))
       (mset:elt-list face-stats))
      (for-each 
       (lambda (vertex)
	 (assert (mset:same-set 
		  answer (hash-table/get vert-face-set-hash vertex #f))
		 "Vertex has the wrong face counts."
		 vertex))
       (symo/unique-vertices sym-obj))
      (display "done.") (newline)
      'pass)))

(define (assert-same-edge-lengths sym-obj #!optional length)
  (define (edge-length edge)
    (let* ((vert1 (symo:get-vertex sym-obj (car edge)))
	   (vert2 (symo:get-vertex sym-obj (cadr edge)))
	   (edge-vect (vector-vector vert1 vert2))
	   (len (sqrt (dot-product edge-vect edge-vect))))
      len))
  (define (helper sym-obj length)
    (define (assert-right-length edge)
      (assert (close-numbers length (edge-length edge))
	      "Edge has the wrong length."
	      (list length edge (edge-length edge))))
    (for-each assert-right-length 
	      (symo:unique-edge-list sym-obj))
    'pass)
  (display "Checking that the edges are all the same length... ")
  (if (default-object? length)
      (let* ((face (car (symo/face-list sym-obj)))
	     (edge (list (car face) (cadr face))))
	(helper sym-obj (edge-length edge)))
      (helper sym-obj length))
  (display "done.") (newline))

; Quadratic in the number of unique vertices
(define (assert-collapse-matches-vertex-group sym-obj)
  (display "Checking that the vertex collapse pattern matches expectations from spec... ")
  (let ((group (symo/vertex-group sym-obj)))
    (for-each (lambda (elt)
		(assert (subg:same-coset? group elt 
					  (symo:representative sym-obj elt))))
	      (cxg/chamber-list (symo/geometry sym-obj)))
    (for-each 
     (lambda (uniq1)
       (for-each 
	(lambda (uniq2)
	  (if (not (eq? uniq1 uniq2))
	      (assert (not (subg:same-coset? group uniq1 uniq2)))))
	(symo/unique-vertices sym-obj)))
     (symo/unique-vertices sym-obj)))
  (display "done.") (newline))
