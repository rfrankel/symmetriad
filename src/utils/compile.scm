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

(let ((fn "matchsyn"))
  (cf-conditionally fn)
  (let ((environment (nearest-repl/environment)))
    (load fn environment)
    (for-each (lambda (name)
		(link-variables system-global-environment name
				environment name))
	      '(rule-system
		matcher-procedure
		matches
		matches-one-of
		match-assign
		:
		::))))


(for-each (lambda (file)
	    (cf-conditionally file))
	  '("asserts"
	    "general"
	    "global-flags"
	    "hash-ops"
	    "list-ops"
	    "multi-set"
	    "multi-set-test"
	    "point-clusterer"
	    "symbolics"
	    "symbolics-test"
	    "symbol-manipulation"
	    "twodtable"
	    "twodtablehash"
	    "twodtablesimple"
	    ))
