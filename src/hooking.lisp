(in-package :sb-graph)

(eval-when (:compile-toplevel :load-toplevel)
  (defvar *hook-originals* (make-hash-table))
  (defvar *hook-enabled* (make-hash-table)))

(defmacro hook (fun lambda-list &body body)
  (let ((orig (gensym))
        (ll (gensym)))
    `(progn
       (when (gethash ',fun *hook-originals*)
         (unhook ,fun))
       (let ((,orig (fdefinition ',fun)))
         (setf (gethash ',fun *hook-originals*)
               ,orig
               (gethash ',fun *hook-enabled*)
               t)
         (defun ,fun (&rest ,ll)
           (when (hook-enabled ,fun)
             (destructuring-bind ,lambda-list ,ll
               (block hook
                 ,@body)))
           (apply ,orig ,ll))))))
(defmacro disable-hook (fun)
  `(setf (gethash ',fun *hook-enabled*) nil))
(defmacro enable-hook (fun)
  `(setf (gethash ',fun *hook-enabled*) t))
(defmacro unhook (fun)
  `(setf (fdefinition ',fun) (gethash ',fun *hook-originals*)))
(defmacro hook-enabled (fun)
  `(gethash ',fun *hook-enabled*))

;; (defun test-hook (a b c &rest d)
;;   (list (+ a b c) d))
;; (hook test-hook (a b c &rest d)
;;   (format t "This is a hook! ~A ~A ~A ~A~%" a b c d))
;; (unhook test-hook)
;; (hook sb-c::compile-toplevel (lambdas load-time-value-p)
;;   (format t "~%Hooking the compiler. compile-toplevel:~%lambdas: ~A~%load-time-value-p: ~A~%"
;;           lambdas load-time-value-p))
;; (unhook sb-c::compile-toplevel)
;; (eval-when (:compile-toplevel :load-toplevel)
;;   (defvar *acc* nil))
;; (hook sb-c::compile-component (component)
;;   (push component *acc*))

(eval-when (:compile-toplevel :load-toplevel)
  (defvar *trace-number* 0))

(hook sb-c::ir2-convert (component)
  (disable-hook sb-c::ir2-convert)
  (when (streamp sb-c::*compiler-trace-output*)
    (let* ((pn (pathname sb-c::*compiler-trace-output*))
           (out-pn (make-pathname
                    :host (pathname-host pn)
                    :directory (pathname-directory pn)
                    :name
                    (format nil "trace-~A-~A" (incf *trace-number*)
                            (coerce (loop for char
                                            across
                                            (let ((cn (sb-c::component-name component)))
                                              (cond
                                                ((symbolp cn) (symbol-name cn))
                                                ((stringp cn) cn)
                                                ((listp cn) (format nil "~{~a~}" cn))
                                                (t "")))
                                          when (or (alpha-char-p char)
                                                   (digit-char-p char)
                                                   (char= char #\-)
                                                   (char= char #\_))
                                            collect char)
                                    'string))
                    :type "dot")))
      (save-graph (render-graph (make-and-dfs component 9999999)) out-pn)
      (when sb-c::*compile-progress*
        (format *debug-io* "~%; Wrote graphviz of component ~A to ~A.~%" component out-pn))))
  (enable-hook sb-c::ir2-convert))
