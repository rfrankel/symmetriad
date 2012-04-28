(let ((object (symmetric-object B-family '(1 0 0 1))))
  (symo:file-print-gv
   object
   "../oogl/B4-1001-1.off"
   'off-conformal
   (highlight-all-cosets object '(s1 s2 s3) '(0.0 0.5 0.5 1.0))
   ))

(let ((object (symmetric-object B-family '(1 0 0 1))))
  (symo:file-print-gv
   object
   "../oogl/B4-1001-2.off"
   'off-conformal
   (highlight-all-cosets object '(s1 s2 s0) '(0.0 0.0 0.5 1.0))
   ))
