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

;;; This file contains demos of the VRML functionality.

(load "graphics/drawing.scm")

(define H3-system (geom-family->cox-geometry H-family 3))

(symo:file-print-vrml (magic-spec->symmetric-object H3-system '(0 0 1))
		      "dodecahedron.wrl")

(symo:file-print-vrml (magic-spec->symmetric-object H3-system '(1 0 0))
		      "icosohedron.wrl")

(symo:file-print-vrml (magic-spec->symmetric-object H3-system '(0 1 0))
		      "icosidodecahedron.wrl")

(symo:file-print-vrml (magic-spec->symmetric-object H3-system '(1 1 0))
		      "trunc-icosohedron.wrl")

(symo:file-print-vrml (magic-spec->symmetric-object H3-system '(0 1 1))
		      "trunc-dodecahedron.wrl")

(symo:file-print-vrml (magic-spec->symmetric-object H3-system '(1 0 1))
		      "small-rhombicosidodecahedron.wrl")

(symo:file-print-vrml (magic-spec->symmetric-object H3-system '(1 1 1))
		      "great-rhombicosidodecahedron.wrl")
