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

(load "utils/multi-set")

(let ()
  (define set1 (make-multi-set))
  (define set2 (make-multi-set))
  (assert (mset:same-set set1 set2))

  (mset:add! set2 'a)
  (assert (mset:contained-in set1 set2))
  (assert (not (mset:contained-in set2 set1)))

  (mset:add! set1 'a)
  (assert (mset:same-set set1 set2))
  
  (mset:add! set1 'a)
  (assert (mset:contained-in set2 set1))
  (assert (not (mset:contained-in set1 set2)))

  (mset:add! set2 'b)
  (assert (not (mset:contained-in set1 set2)))
  (assert (not (mset:contained-in set2 set1)))

  (mset:remove! set1 'a)
  (assert (mset:contained-in set1 set2))
  (assert (not (mset:contained-in set2 set1)))

  (mset:remove! set2 'b)
  (assert (mset:same-set set1 set2))

  'pass)

(let ()
  (define mset (alist->mset '((2 . 4) (3 . 1))))
  (assert (= 4 (mset:number mset 2)))
  (assert (= 1 (mset:number mset 3)))

  (mset:remove! mset 2 2)
  (assert (= 2 (mset:number mset 2)))
  (assert (= 1 (mset:number mset 3)))
  (assert (or (equal? '((2 . 2) (3 . 1)) (mset->alist mset))
	      (equal? '((3 . 1) (2 . 2)) (mset->alist mset))))	      

  (mset:add! mset 7 17)
  (assert (= 2 (mset:number mset 2)))
  (assert (= 1 (mset:number mset 3)))
  (assert (= 17 (mset:number mset 7)))

  (mset:remove! mset 3)
  (assert (= 2 (mset:number mset 2)))
  (assert (= 17 (mset:number mset 7)))
  (assert (or (equal? '((2 . 2) (7 . 17)) (mset->alist mset))
	      (equal? '((7 . 17) (2 . 2)) (mset->alist mset))))	      

  'pass)

(let ()
  (define mset1 (alist->mset '((a . 3) (b . 2))))
  (define mset2 (alist->mset '((c . 4) (b . 1))))

  (assert (not (mset:empty? mset1)))
  (assert (not (mset:empty? mset2)))

  (define mset-int (mset:intersection mset1 mset2))
  (assert (equal? '((b . 1)) (mset->alist mset-int)))

  (mset:remove! mset-int 'b)
  (assert (mset:empty? mset-int))

  (mset:add! mset1 'a -3)
  (assert (not (mset:empty? mset1)))

  (mset:add! mset1 'b -2)
  (assert (mset:empty? mset1))

  'pass)