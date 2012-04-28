(let ((object (symmetric-object B-family '(1 1 5 5))))
  (symo:file-print-gv
   object
   "../oogl/B4-1155-3.off"
   'off-conformal
   (merge-by-first 
    (list
     (highlight-all-cosets object '(s0 s1 s2) '(0.5 0.0 0.5 1.0))
     (highlight-all-cosets object '(s0 s1 s3) '(0.0 0.5 0.5 1.0))))
   ))

(let ((object (symmetric-object B-family '(5 5 1 1))))
  (symo:file-print-gv
   object
   "../oogl/B4-5511.off"
   'off-conformal
   (merge-by-first 
    (list
     (highlight-all-cosets object '(s3 s1 s2) '(255 102 153 255))
     (highlight-all-cosets object '(s0 s2 s3) '(0 0 102 255))))
   ))
