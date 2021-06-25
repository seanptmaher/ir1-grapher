(asdf:defsystem "ir1-grapher"
  :depends-on ("cl-ppcre")
  :components
  ((:module src
            :serial t
            :components
            ((:file "package")
             (:file "hooking")
             (:file "graphing")))))
