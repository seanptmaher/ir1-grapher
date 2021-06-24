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
  `(setf (fdefinition ',fun) (gethash ',fun *hook-originals*)))

;; (defun test-hook (a b c &rest d)
;;   (list (+ a b c) d))
;; (defhook test-hook (a b c &rest d)
;;   (format t "This is a hook! ~A ~A ~A ~A~%" a b c d))
;; (unhook test-hook)
;; (defhook sb-c::compile-toplevel (lambdas load-time-value-p)
;;   (format t "~%Hooking the compiler. compile-toplevel:~%lambdas: ~A~%load-time-value-p: ~A~%"
;;           lambdas load-time-value-p))
;; (unhook sb-c::compile-toplevel)

(eval-when (:compile-toplevel :load-toplevel)
  (defvar *acc* nil))

(defhook sb-c::compile-component (component)
  (push component *acc*))

(defmacro with-digraph ((graph-sym) &body body)
  `(let ((,graph-sym (make-instance 'graph
                                    :table (make-hash-table :test 'eq :size 63)
                                    :stream (make-string-output-stream))))
     (progn
       (write-string (format nil "digraph {~%") (stream ,graph-sym))
       ,@body
       (write-string "}" (stream ,graph-sym))
       (get-output-stream-string (stream ,graph-sym)))))

(eval-when (:compile-toplevel :load-toplevel)
  (defparameter *subgraph-number* 0))
(defmacro with-subgraph ((graph-sym &optional label) &body body)
  `(progn (write-string ,(if label
                            `(format nil "subgraph ~A_~A {~%"
                                    ,label ,(incf *subgraph-number*))
                            `(format nil "subgraph subgraph_~A {~%"
                                    ,(incf *subgraph-number*)))
                        (stream ,graph-sym))
          ,@body
          (write-string (format nil "}~%") (stream ,graph-sym))))

(defun save-graph (str filename)
  (with-open-file (s filename :direction :output :if-does-not-exist :create :if-exists :supersede)
    (write-string str s))) 

(defparameter *test-component* nil)

(defclass graph ()
  ((stream :initarg :stream :reader stream)
   (table :initarg :table :reader table)))

(defmethod graph (object)
  (with-digraph (g)
    (add g object)))

(defmethod subgraph ((graph graph) object)
  (with-subgraph (graph)
    (add graph object)))

(defun in-graph (graph object)
  (nth-value 1 (gethash object (table graph))))

(defun table-add (graph object)
  (setf (gethash object (table graph)) t))

;; In order to deal with cycles in the graph, we need to keep track of
;; the stuff we've already rendered. We're not going to recursively
;; add something that is already present in the graph into the graph,
;; we're just going to add the lines from the current object to them.
;; The stuff to keep track of this is inside of the graph class. The
;; table is just going to store whether we've seen the key yet.

;; display goes from an object to the string representation that'll be
;; inside the graph nodes

;; returns a new plist which is identical except the value
;; associated with KEY has had func called on it

(defun modify-str-plist (plist key func)
  (assert (= 0 (mod (length plist) 2)))
  (apply #'nconc
         (loop with ck = nil
               with cv = nil
               while (setf ck (car plist)
                           cv (cadr plist))
               collect (let ((v (if (string= key ck)
                                    (list ck (funcall func cv))
                                    (list ck cv))))
                         (setf plist (cddr plist))
                         v))))

(defmethod edge ((graph graph) from to &rest options)
  (assert (= 0 (mod (length options) 2)))
  (format (stream graph) "\"~A\" -> \"~A\"[~A];~%"
          (cl-ppcre:regex-replace-all "\"" (display from) "'")
          (cl-ppcre:regex-replace-all "\"" (display to) "'")
          (apply #'concatenate
                 (cons 'string
                       (loop with ck = nil
                             with cv = nil
                             while (setf ck (car options)
                                         cv (cadr options))
                             collect (let ((v (format nil "~A=\"~A\"" ck cv)))
                                       (setf options (cddr options))
                                       v))))))

;; this is overriding the edge so that we can not render CTRANs
(defmethod edge ((graph graph) from (to sb-c::ctran) &rest options)
  (apply #'edge (nconc (list graph from (sb-c::ctran-next to))
                       (modify-str-plist options "label"
                                         (lambda (x)
                                           (concatenate 'string x (format nil "[ctran: ~A]" (sb-c::ctran-kind to)))))
                       (unless (find-if (lambda (x) (string= x "color")) options)
                         '("color" "red")))))

;; ;; same thing here with REFs
;; (defmethod edge ((graph graph) from (to sb-c::ref) &rest options)
;;   (apply #'edge (nconc (list graph from (sb-c::ref-leaf to))
;;                        (modify-str-plist options "label"
;;                                          (lambda (x)
;;                                            (concatenate 'string x
;;                                                         (format nil "[ref: %source-name: ~A]"
;;                                                                 (sb-c::ref-%source-name to)))))
;;                        (unless (find-if (lambda (x) (string= x "color")) options)
;;                          '("color" "yellow")))))

(defmethod edge ((graph graph) from (to list) &rest options)
  (when (null to)
    (apply #'edge (nconc (list graph from "NIL") options)))
  (let ((counter 0))
    (mapc #'(lambda (x)
              (apply #'edge
                     (nconc (list graph from x)
                            (modify-str-plist
                             options "label"
                             #'(lambda (x)
                                 (let ((res (format nil "~A[# ~A]"
                                                    x counter)))
                                   (incf counter)
                                   res))))))
          to)))

(defmethod display ((c sb-c::component))
  (format nil "COMPONENT [~A]: '~S'"
          (sb-c::component-kind c)
          (sb-c::component-name c)))

;; The sxhash is required to not end up with a single CBLOCK node
(defmethod display ((b sb-c::cblock))
  (format nil "CBLOCK [~A]"
          (sxhash b)))

(defmethod display ((ctran sb-c::ctran))
  (error "Trying to display a CTRAN, this shouldn't happen"))

(defmethod display ((cl sb-c::clambda))
  (format nil "CLAMBDA [~A]:~%%debug-name: ~A~%source-name: ~A~%kind: ~A"
          (sxhash cl)
          (sb-c::lambda-%debug-name cl)
          (sb-c::lambda-%source-name cl)
          (sb-c::lambda-kind cl)))

(defmethod display ((cr sb-c::creturn))
  (format nil "CRETURN [~A]:~%result-type: ~A"
          (sxhash cr)
          (sb-c::return-result-type cr)))

(defmethod display ((bind sb-c::bind))
  (format nil "BIND [~A]"
          (sxhash bind)))

;;; this is outdated code from when I wasn't displaying refs. If it is
;;; truly the case that refs need to be displayed, then this
;;; could be deleted

;; (defmethod display ((ref sb-c::ref))
;;   (error "trying to display a REF, this shouldn't happen"))
(defmethod display ((ref sb-c::ref))
  (format nil "REF [~A]:~%derived-type: ~A"
          (sxhash ref)
          (sb-c::ref-derived-type ref)))

(defmethod display ((comb sb-c::combination))
  (format nil "COMBINATION [~A]:~%kind: ~A~%fun-info: ~A~%info: ~A"
          (sxhash comb)
          (sb-c::combination-kind comb)
          (sb-c::combination-fun-info comb)
          (sb-c::combination-info comb)))

(defmethod display ((lvar sb-c::lvar))
  (format nil "LVAR [~A]:~%%derived-type: ~A~%dynamic-extent: ~A"
          (sxhash lvar)
          (sb-c::lvar-%derived-type lvar)
          (sb-c::lvar-dynamic-extent lvar)))

(defmethod display ((const sb-c::constant))
  (format nil "CONSTANT [~A]:~%value: ~A"
          (sxhash const)
          (sb-c::constant-value const)))

(defmethod display ((lamvar sb-c::lambda-var))
  (format nil "LAMBDA-VAR [~A]:~%arg-info: ~A~%flags: ~A"
          (sxhash lamvar)
          (sb-c::lambda-var-arg-info lamvar)
          (sb-c::lambda-var-flags lamvar)))

(defmethod display ((entry sb-c::entry))
  (format nil "ENTRY [~A]:~%source-path ~A"
          (sxhash entry)
          (sb-c::entry-source-path entry)))

(defmethod display (obj)
  (format nil "NOT SUPPORTED YET:~% ~A" obj))

(defmacro edge-and-add (graph from to &rest options)
  (let ((g (gensym))
        (tg (gensym)))
    `(let ((,g ,graph)
           (,tg ,to))
       (prog2
           (edge ,g ,from ,tg ,@options)
           (add ,g ,tg)))))

;; this creates the wrapper used inside the add methods to only run
;; the edge-adding/recursing code if we've already seen this object
(defmacro unig ((graph obj) &body body)
  (let ((g (gensym))
        (o (gensym)))
    `(let ((,g ,graph)
           (,o ,obj))
       (unless (in-graph ,g ,o)
         (format t "Not in graph ~A ~A~%" ,g ,o)
         (table-add ,g ,o)
         ,@body))))

(defmethod add ((graph graph) (objects list))
  (mapc #'(lambda (o) (add graph o)) objects))

;; a component should be rendered as a subgraph
(defmethod add ((graph graph) (component sb-c::component))
  (with-subgraph (graph "component")
    (unig (graph component)
      (edge-and-add graph component (sb-c::component-head component) "label" "head")
      (edge-and-add graph component (sb-c::component-tail component) "label" "tail"))))

(defmethod add ((graph graph) (cblock sb-c::cblock))
  (unig (graph cblock)
    (edge-and-add graph cblock (sb-c::block-component cblock) "label" "component")
    (edge-and-add graph cblock (sb-c::block-succ cblock) "label" "succ")
    (edge-and-add graph cblock (sb-c::block-pred cblock) "label" "pred")
    (edge-and-add graph cblock (sb-c::block-start cblock) "label" "start" "color" "red")))

;; Unfinished
(defmethod add ((graph graph) (cl sb-c::clambda))
  (unig (graph cl)
    (edge-and-add graph cl (sb-c::lambda-home cl) "label" "home")
    (edge-and-add graph cl (sb-c::lambda-vars cl) "label" "vars")
    )
  )

(defmethod add ((graph graph) (cr sb-c::creturn))
  (unig (graph cr)
    (edge-and-add graph cr (sb-c::return-lambda cr) "label" "lambda")
    (edge-and-add graph cr (sb-c::return-result cr) "label" "result")))

;; (defmethod add ((graph graph) (node sb-c::node))
;;   )

;; this is just a dummy function to skip the CTRAN
(defmethod add ((graph graph) (ct sb-c::ctran))
  (unig (graph ct)
    (add graph (sb-c::ctran-next ct))))

(defmethod add ((graph graph) (ref sb-c::ref))
  (unig (graph ref)
    (edge-and-add graph ref (sb-c::ref-leaf ref) "label" "leaf")
    (edge-and-add graph ref (sb-c::ref-next ref) "label" "next")
    (edge-and-add graph ref (sb-c::ref-lvar ref) "label" "lvar")))


(defmethod add ((graph graph) (bind sb-c::bind))
  (unig (graph bind)
    (edge-and-add graph bind (sb-c::bind-lambda bind) "label" "lambda")
    (edge-and-add graph bind (sb-c::bind-next bind) "label" "next" "color" "green")
    (edge-and-add graph bind (sb-c::bind-prev bind) "label" "prev" "color" "blue")))

(defmethod add ((graph graph) (comb sb-c::combination))
  (unig (graph comb)
    (edge-and-add graph comb (sb-c::combination-fun comb) "label" "fun")
    (edge-and-add graph comb (sb-c::combination-args comb) "label" "args")))

(defmethod add ((graph graph) (lvar sb-c::lvar))
  (unig (graph lvar)
    (edge-and-add graph lvar (sb-c::lvar-dest lvar) "label" "dest" "color" "brown")
    (edge-and-add graph lvar (sb-c::lvar-uses lvar) "label" "uses")))

(defmethod add ((graph graph) (lamvar sb-c::lambda-var))
  (unig (graph lamvar)
    (edge-and-add graph lamvar (sb-c::lambda-var-home lamvar) "label" "home")
    (edge-and-add graph lamvar (sb-c::lambda-var-sets lamvar) "label" "sets")
    ))

(defmethod add ((graph graph) (entry sb-c::entry))
  (unig (graph entry)
    (edge-and-add graph entry (sb-c::entry-exits entry) "label" "exits")
    (edge-and-add graph entry (sb-c::entry-cleanup entry) "label" "cleanup")
    (edge-and-add graph entry (sb-c::entry-next entry) "label" "next")
    (edge-and-add graph entry (sb-c::entry-prev entry) "label" "prev"))
  )

;; I don't know whether it would be useful to have any information
;; about constant values, but it can be added in here pretty (read:
;; very) easily
(defmethod add ((graph graph) (const sb-c::constant))
  (unig (graph const)))

(defmethod add ((graph graph) object)
  (declare (ignorable object))
  (unig (graph object)))
