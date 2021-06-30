(eval-when (:compile-toplevel :load-toplevel)
  (sb-ext:unlock-package :sb-c))

(defpackage :ir1-grapher
  (:shadow :stream)
  (:use :cl :cl-user)
  (:export :make-and-dfs :interactively-graph :output :expand
           :render-graph :graph :make-graph :expand-graph-codename))
