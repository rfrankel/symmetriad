(let ((object (symmetric-object B-family '(0 0 0 1))))
  (symo:file-print-both
   object
   "images/01-tesseract"
   (highlight-all-cosets object '(s1 s2 s3) '(0.0 0.0 0.5 1.0))
   ))

(let ((object (symmetric-object B-family '(2 0 0 1))))
  (symo:file-print-both
   object
   "images/02-tesseract-spread"
   (highlight-all-cosets object '(s1 s2 s3) '(0.0 0.0 0.5 1.0))
   ))

(let ((object (symmetric-object A-family '(0 0 0 1))))
  (symo:file-print-both
   object
   "images/03-simplex"
   (highlight-all-cosets object '(s1 s2 s3) '(0.0 0.5 0.0 1.0))
   ))

(let ((object (symmetric-object A-family '(2 0 0 1))))
  (symo:file-print-both
   object
   "images/04-simplex-spread"
   (highlight-all-cosets object '(s1 s2 s3) '(0.0 0.5 0.0 1.0))
   ))

(let ((object (symmetric-object B-family '(1 0 0 0))))
  (symo:file-print-both
   object
   "images/05-cross"
   (highlight-all-cosets object '(s1 s2 s0) '(0.0 0.5 0.5 1.0))
   ))

(let ((object (symmetric-object B-family '(1 0 0 2))))
  (symo:file-print-both
   object
   "images/06-cross-spread"
   (highlight-all-cosets object '(s1 s2 s0) '(0.0 0.5 0.5 1.0))
   ))

(let ((object (symmetric-object F-family '(1 0 0 0))))
  (symo:file-print-both
   object
   "images/07-24-cell"
   (highlight-all-cosets object '(s1 s2 s0) '(0.5 0.0 0.0 1.0))
   ))

(let ((object (symmetric-object F-family '(1 0 0 2))))
  (symo:file-print-both
   object
   "images/08-24-cell-spread-a"
   (highlight-all-cosets object '(s1 s2 s0) '(0.5 0.0 0.0 1.0))
   ))

(let ((object (symmetric-object F-family '(2 0 0 1))))
  (symo:file-print-both
   object
   "images/08-24-cell-spread-b"
   (highlight-all-cosets object '(s1 s2 s3) '(0.0 0.5 0.0 1.0))
   ))

