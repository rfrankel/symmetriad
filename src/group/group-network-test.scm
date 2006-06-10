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

;; test-trivial-addition
(let* ((toy-presentation (make-group-presentation '(g0 g1 g2) '()))
       (toy-network (group-network toy-presentation #f "No name")))
  (assert-equal 1 (gn:num-cosets toy-network))
  (assert-equal '(e) (gn:coset-list toy-network))
  (assert-equal #f (gn:product toy-network 'e 'g0))

  (handle-add! toy-network 'e 'g0)
  (assert-equal 2 (gn:num-cosets toy-network))
  (assert-equal '(e c1) (gn:coset-list toy-network))
  (assert-equal 'c1 (gn:product toy-network 'e 'g0))
  (assert-equal 'e (gn:product toy-network 'c1 'g0))
  (assert-equal #f (gn:product toy-network 'e 'g1))

  ; TODO Add consistency checking to handle-add! (etc) to either forbid
  ; this, or define reasonable semantics for it.
  ; As it stands, this will produce an inconsistent link structure
  ; where 'c1 x 'g0 will still be 'e, but 'e x 'g0 will be 'c2.
  ; (handle-add! toy-network 'e 'g0)
  'pass)

;; test-trivial-link
(let* ((toy-presentation (make-group-presentation '(g0 g1 g2) '()))
       (toy-network (group-network toy-presentation #f "No name")))
  (handle-add! toy-network 'e 'g0)
  (handle-add! toy-network 'e 'g1)
  (assert-equal 3 (gn:num-cosets toy-network))
  (assert-equal '(e c1 c2) (gn:coset-list toy-network))
  (assert-equal 'c1 (gn:product toy-network 'e 'g0))
  (assert-equal 'e (gn:product toy-network 'c1 'g0))
  (assert-equal 'c2 (gn:product toy-network 'e 'g1))
  (assert-equal 'e (gn:product toy-network 'c2 'g1))
  (assert-equal #f (gn:product toy-network 'c1 'g1))
  (assert-equal #f (gn:product toy-network 'c2 'g0))
  (assert-equal #f (gn:product toy-network 'c1 'g2))
  (assert-equal #f (gn:product toy-network 'c2 'g2))

  (add-link! toy-network 'c1 'c2 'g2)
  (assert-equal 3 (gn:num-cosets toy-network))
  (assert-equal '(e c1 c2) (gn:coset-list toy-network))
  (assert-equal #f (gn:product toy-network 'c1 'g1))
  (assert-equal #f (gn:product toy-network 'c2 'g0))
  (assert-equal 'c2 (gn:product toy-network 'c1 'g2))
  (assert-equal 'c1 (gn:product toy-network 'c2 'g2)))

;; test-simple-coincidence
(let* ((toy-presentation (make-group-presentation '(g0 g1 g2) '()))
       (toy-network (group-network toy-presentation #f "No name")))
  (handle-add! toy-network 'e 'g0)
  (handle-add! toy-network 'e 'g1)
  (assert-equal 'c1 (gn:product toy-network 'e 'g0))
  (assert-equal 'c2 (gn:product toy-network 'e 'g1))
  (handle-add! toy-network 'c2 'g2)
  (assert-equal 'c3 (gn:product toy-network 'c2 'g2))
  (assert-equal #f (gn:product toy-network 'c1 'g2))

  (assert-equal 4 (gn:num-cosets toy-network))
  (assert-equal '() (gn:dead-cosets toy-network))
  (assert-equal '(e c1 c2 c3) (gn:coset-list toy-network))
  (assert-equal 4 (gn:num-live-cosets toy-network))

  ; Now suppose we decide that 'c2 x 'g2 should really be 'c1.
  ; we must merge 'c3 and 'c1 together.
  ; At revision 28, handle-coincidence merges the first of the
  ; two cosets it is given into the second.
  (handle-coincidence! toy-network 'c3 'c1)
  (assert-equal 4 (gn:num-cosets toy-network))
  (assert-equal '(c3) (gn:dead-cosets toy-network))
  (assert-equal '(e c1 c2) (gn:coset-list toy-network))
  (assert-equal 3 (gn:num-live-cosets toy-network))
  (assert-equal 'c1 (gn:product toy-network 'c2 'g2))
  (assert-equal 'c2 (gn:product toy-network 'c1 'g2))
  (assert-equal #f (gn:product toy-network 'c3 'g2))
  'pass)

;; test-chain-coincidence
(let* ((toy-presentation (make-group-presentation '(g0 g1 g2 g3 g4) '()))
       (toy-network (group-network toy-presentation #f "No name")))
  (handle-add! toy-network 'e 'g0)
  (handle-add! toy-network 'e 'g1)
  (assert-equal 'c1 (gn:product toy-network 'e 'g0))
  (assert-equal 'c2 (gn:product toy-network 'e 'g1))
  (handle-add! toy-network 'c2 'g2)

  ; Suppose 'c3 should really be 'c1 but we fail to notice
  ; until after tacking on a few nodes in a trail to each.
  (handle-add! toy-network 'c1 'g3)
  (handle-add! toy-network 'c3 'g3)
  (handle-add! toy-network 'c4 'g4)
  (handle-add! toy-network 'c5 'g4)
  (handle-add! toy-network 'c6 'g3)
  (handle-add! toy-network 'c7 'g3)
  (assert-equal 'c4 (gn:product toy-network 'c1 'g3))
  (assert-equal 'c6 (gn:product toy-network 'c4 'g4))
  (assert-equal 'c8 (gn:product toy-network 'c6 'g3))
  (assert-equal 'c5 (gn:product toy-network 'c3 'g3))
  (assert-equal 'c7 (gn:product toy-network 'c5 'g4))
  (assert-equal 'c9 (gn:product toy-network 'c7 'g3))
  (assert-equal 10 (gn:num-cosets toy-network))
  (assert-equal '() (gn:dead-cosets toy-network))
  (assert-equal '(e c1 c2 c3 c4 c5 c6 c7 c8 c9) (gn:coset-list toy-network))
  ; Just for fun, let's also hang some innocent nodes 
  ; off of the doomed chain
  (handle-add! toy-network 'c4 'g2)
  (handle-add! toy-network 'c8 'g0)
  (handle-add! toy-network 'c8 'g1)
  (handle-add! toy-network 'c8 'g2)
  (assert-equal 'c4 (gn:product toy-network 'c10 'g2))
  (assert-equal 'c8 (gn:product toy-network 'c11 'g0))
  (assert-equal 'c8 (gn:product toy-network 'c12 'g1))
  (assert-equal 'c8 (gn:product toy-network 'c13 'g2))
  (assert-equal 14 (gn:num-cosets toy-network))
  (assert-equal '() (gn:dead-cosets toy-network))
  (assert-equal '(e c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13) (gn:coset-list toy-network))
  (assert-equal 14 (gn:num-live-cosets toy-network))

  (handle-coincidence! toy-network 'c1 'c3)
  (assert-equal 14 (gn:num-cosets toy-network))
  ; The merging chains
  (assert-equal '(c8 c6 c4 c1) (gn:dead-cosets toy-network))
  (assert-equal '(e c2 c3 c5 c7 c9 c10 c11 c12 c13) (gn:coset-list toy-network))
  (assert-equal 10 (gn:num-live-cosets toy-network))
  ; All the merged nodes lose their connections
  (assert-equal #f (gn:product toy-network 'c1 'g3))
  (assert-equal #f (gn:product toy-network 'c4 'g4))
  (assert-equal #f (gn:product toy-network 'c6 'g3))
  (assert-equal #f (gn:product toy-network 'c8 'g0))
  (assert-equal #f (gn:product toy-network 'c8 'g1))
  (assert-equal #f (gn:product toy-network 'c8 'g2))
  (assert-equal #f (gn:product toy-network 'c8 'g3))
  ; The mergees survive unscathed
  (assert-equal 'c5 (gn:product toy-network 'c3 'g3))
  (assert-equal 'c7 (gn:product toy-network 'c5 'g4))
  (assert-equal 'c9 (gn:product toy-network 'c7 'g3))
  ; And legitimate nodes that were connected to the 
  ; chain just merged are now connected to the corresponding
  ; places on the mergees.
  (assert-equal 'c5 (gn:product toy-network 'c10 'g2))
  (assert-equal 'c9 (gn:product toy-network 'c11 'g0))
  (assert-equal 'c9 (gn:product toy-network 'c12 'g1))
  (assert-equal 'c9 (gn:product toy-network 'c13 'g2))
  'pass)

;; test-trivial-hlt
(let* ((cyclic-2-presentation (make-group-presentation '(s0) '((s0 s0))))
       (cyclic-2-network (group-network cyclic-2-presentation #f "No name")))
  (assert-equal 1 (gn:num-cosets cyclic-2-network))
  (assert-equal '() (gn:dead-cosets cyclic-2-network))
  (assert-equal '(e) (gn:coset-list cyclic-2-network))
  (assert-equal #f (gn:product cyclic-2-network 'e 's0))
  (assert-equal #f (gn:product cyclic-2-network 'c1 's0))

  (gn:hlt! cyclic-2-network)
  (assert-equal 2 (gn:num-cosets cyclic-2-network))
  (assert-equal '() (gn:dead-cosets cyclic-2-network))
  (assert-equal '(e c1) (gn:coset-list cyclic-2-network))
  (assert-equal 'c1 (gn:product cyclic-2-network 'e 's0))
  (assert-equal 'e (gn:product cyclic-2-network 'c1 's0)))
