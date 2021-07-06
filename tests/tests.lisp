(in-package :sb-graph-test)

(deftest compile-complicated-program-with-trace
    ;; If it didn't hit an AVER, I'd call that a success. The amount
    ;; of organic weird compiler things going on in that file is
    ;; incredible. In the graph is many thousands of nodes of all
    ;; types. If I don't AVER in there, I'm using the API correctly.
    ;;
    ;; In case you're curious, this is CL-PPCRE, but concatenated into
    ;; one file.
    (ignore-errors
     (compile-file #p"SYS:CONTRIB;SB-GRAPH;TESTS;PROG" :trace-file t)
     T)
  T)
