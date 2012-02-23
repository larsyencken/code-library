#!/usr/bin/env clj
;
;  sum.py
;  code
;
;  Created by Lars Yencken on 2012-02-23.
;  Copyright 2012 Lars Yencken All rights reserved.
;
;

;  Calculate the sum of numbers provided line-by-line on stdin.

(println
  (reduce + 0
          (map read-string
               (line-seq (java.io.BufferedReader. *in*)))))
