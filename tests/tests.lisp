(in-package :sb-graph-test)

(require 'uiop)

(deftest compile-complicated-program-with-trace
    ;; This is a semi-complicated program, it has a substantial amount
    ;; of stuff to graph. If I don't crash during compilation in
    ;; there, I'm using the API semi-correctly. I don't really see a
    ;; better way to test this, I think it's pretty good.
    (ignore-errors
     (progn
         (ensure-directories-exist #p"SYS:CONTRIB;SB-GRAPH;TESTS;TRACEDIR;")
         (compile-file #P"SYS:CONTRIB;SB-GRAPH;SRC;PACKAGE.LISP"
                       :print nil
                       :progress nil
                       :output-file #p"SYS:CONTRIB;SB-GRAPH;TESTS;TRACEDIR;package.fasl"
                       :trace-file #p"SYS:CONTRIB;SB-GRAPH;TESTS;TRACEDIR;tracefile1")
         (compile-file #P"SYS:CONTRIB;SB-GRAPH;SRC;GRAPHING.LISP"
                       :print nil
                       :progress nil
                       :output-file #p"SYS:CONTRIB;SB-GRAPH;TESTS;TRACEDIR;graphing.fasl"
                       :trace-file #p"SYS:CONTRIB;SB-GRAPH;TESTS;TRACEDIR;tracefile2")
         (compile-file #P"SYS:CONTRIB;SB-GRAPH;SRC;HOOKING.LISP"
                       :print nil
                       :progress nil
                       :output-file #p"SYS:CONTRIB;SB-GRAPH;TESTS;TRACEDIR;hooking.fasl"
                       :trace-file #p"SYS:CONTRIB;SB-GRAPH;TESTS;TRACEDIR;tracefile3")
         (uiop:delete-directory-tree (translate-logical-pathname #p"SYS:CONTRIB;SB-GRAPH;TESTS;TRACEDIR;") :validate t)
       t))
  t)
