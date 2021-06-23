(defpackage :ir1-grapher
  (:use :cl :cl-user)
  (:export :defhook))

(in-package :ir1-grapher)

(defvar *hook-originals* (make-hash-table))

(defmacro defhook (fun lambda-list &body body)
  (let ((orig (gensym))
        (ll (gensym)))
    `(let ((,orig #',fun))
       (when (gethash ',fun *hook-originals*)
           (unhook ,fun))
       (setf (gethash ',fun *hook-originals*)
             ,orig)
       (defun ,fun (&rest ,ll)
         (prog2
             (destructuring-bind ,lambda-list ,ll
               (block hook
                 ,@body))
             (apply ,orig ,ll))))))

(defmacro unhook (fun)
  (setf (fdefinition fun) (gethash fun *hook-originals*)))

;; (defun test-hook (a b c &rest d)
;;   (list (+ a b c) d))
;; (defhook test-hook (a b c &rest d)
;;   (format t "This is a hook! ~A ~A ~A ~A~%" a b c d))
;; (unhook test-hook)
;; (defhook sb-c::compile-toplevel (lambdas load-time-value-p)
;;   (format t "~%Hooking the compiler. compile-toplevel:~%lambdas: ~A~%load-time-value-p: ~A~%"
;;           lambdas load-time-value-p))
;; (unhook sb-c::compile-toplevel)

;; (defparameter *acc* nil)

;; (defhook sb-c::compile-component (component)
;;   (push component *acc*))

(defun edge (stream from to &optional label)
    (write-string
     (if label
         (format nil "\"~A\" -> \"~A\"[label=\"~A\",width=\"~A\"];~%"
                 (cl-ppcre:regex-replace-all "\"" (format nil "~A" from) "'")
                 (cl-ppcre:regex-replace-all "\"" (format nil "~A" to) "'")
                 label label)
         (format nil "\"~A\" -> \"~A\";~%"
                 (cl-ppcre:regex-replace-all "\"" (format nil "~A" from) "'")
                 (cl-ppcre:regex-replace-all "\"" (format nil "~A" to) "'")))
     stream))

;; look at the macroexpansion, it should have it all 
(defmacro with-digraph (graph-sym &body body)
  `(let ((,graph-sym (make-string-output-stream)))
     (progn
       (write-string (format nil "digraph {~%") ,graph-sym)
       ,@body
       (write-string "}" ,graph-sym)
       (get-output-stream-string ,graph-sym))))

(defun save-graph (str filename)
  (with-open-file (s filename :direction :output :if-does-not-exist :create :if-exists :supersede)
    (write-string str s))) 

(defparameter *test-component* nil)

(defun graph-component (component)
  (with-digraph g
    (edge g component (sb-c::component-head component) "head")
    (edge g component (sb-c::component-tail component) "tail")
    (add-cblock g (sb-c::component-head component))
    ))

(defun add-cblock (g cblock)
  (when (sb-c::block-start cblock)
    (edge g cblock (sb-c::block-start cblock) "next")
    (add-node g (sb-c::block-start cblock)))
  (dolist (succ (sb-c::block-succ cblock))
    (edge g cblock succ "succ")
    (add-cblock g succ)))

(defun add-node (g node)
  (macrolet ((frob (next)
               `(when (,next node)
                 (edge g node (,next node) "next")
                 (add-node g (,next node)))))
    (typecase node
      (sb-c::node 
       (frob sb-c::node-next))
      (sb-c::ctran
       (frob sb-c::ctran-next))))
  )
