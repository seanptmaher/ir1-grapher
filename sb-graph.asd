;;; -*-  Lisp -*-
;; #-(or sb-testing-contrib sb-building-contrib)
;; (error "Can't build contribs with ASDF")

(asdf:defsystem "sb-graph"
  :serial t
  :components
  ((:module src
            :serial t
            :components
            ((:file "package")
             (:file "graphing")
             (:file "hooking"))))
  :perform (load-op :after (o c) (provide 'sb-graph))
  :in-order-to ((test-op (test-op "sb-graph/tests"))))

(asdf:defsystem "sb-graph/tests"
  :depends-on ("sb-graph" "sb-rt")
  :components
  ((:module tests
    :serial t
    :components
    ((:file "package")
     (:file "tests"))))
  :perform (test-op (o c)
             (multiple-value-bind (soft strict pending)
                 (funcall (intern "DO-TESTS" (find-package "SB-RT")))
               (declare (ignorable pending))
               (fresh-line)
               (unless strict
                 #+sb-testing-contrib
                 ;; We create TEST-PASSED from a shell script if tests passed.  But
                 ;; since the shell script only `touch'es it, we can actually create
                 ;; it ahead of time -- as long as we're certain that tests truly
                 ;; passed, hence the check for SOFT.
                 (when soft
                   (with-open-file (s #p"SYS:CONTRIB;SB-GRAPH;TEST-PASSED.TEST-REPORT"
                                      :direction :output)
                     (dolist (pend pending)
                       (format s "Expected failure: ~A~%" pend))))
                 (warn "ignoring expected failures in test-op"))
               (unless soft
                 (error "test-op failed with unexpected failures")))))
