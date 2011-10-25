#!/usr/bin/env clj
;
;  cat.clj
;
;  An implementation of the UNIX cat command in Clojure. Solution borrows from
;  http://stackoverflow.com/questions/2034059/how-to-read-lines-from-stdin-in-in-clojure
;

; XXX still throws an error when input is exhausted
(def cat
  (doall (map println
              (line-seq (java.io.BufferedReader. *in*)))))
(cat)
