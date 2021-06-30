(in-package :ir1-grapher)

(defun make-graph ()
  (make-instance 'graph
                 :table (make-hash-table :test 'eq :size 63)
                 :stream (make-string-output-stream)
                 :bfs-table (make-hash-table :test 'eq :size 63)
                 :obj-table (make-hash-table :test 'eq :size 63)
                 :codename-table (make-hash-table :test 'equal :size 63)
                 :codename-number 0))

(defmacro with-digraph ((graph-sym) &body body)
  `(let ((,graph-sym (make-graph)))
     (progn
       (write-string (format nil "digraph {~%") (stream ,graph-sym))
       ,@body
       (write-string "}" (stream ,graph-sym))
       (values (get-output-stream-string (stream ,graph-sym))
               ,graph-sym))))

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
          (format (stream ,graph-sym) "}~%")))

(defun save-graph (str filename)
  (with-open-file (s filename :direction :output :if-does-not-exist :create :if-exists :supersede)
    (write-string str s))) 

(defparameter *test-component* nil)

;; table: obj -> bool
;; obj-table: obj -> codename
;; codename-table: codename -> obj
(defclass graph ()
  ((stream :initarg :stream :accessor stream)
   (table :initarg :table :accessor table)
   (bfs-table :initarg :bfs-table :reader bfs-table)
   (obj-table :initarg :obj-table :reader obj-table)
   (codename-table :initarg :codename-table :reader codename-table)
   (codename-number :initarg :codename-number :accessor codename-number)))

(defmethod graph (object &optional (distance -1))
  (with-digraph (g)
    (add g object distance)))

(defmethod subgraph ((graph graph) object &optional (distance -1))
  (with-subgraph (graph)
    (add graph object distance)))

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

;; modify-str-plist returns a new plist which is identical except the
;; value associated with KEY has been replaced by what is returned by
;; funcalling func on the value
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

(defun clamp (str)
  (if (< 16300 (length str))
      (subseq str 0 16300)
      str))

(defmethod edge ((graph graph) from to &rest options)
  (assert (= 0 (mod (length options) 2)))
  (format (stream graph) "\"{~A} ~A\" -> \"{~A} ~A\"[~A];~%"
          (if (gethash from (obj-table graph))
              (gethash from (obj-table graph))
              (add-to-code-tables graph from))
          (cl-ppcre:regex-replace-all "\\\\" (cl-ppcre:regex-replace-all "\"" (clamp (display from)) "'") "\\\\\\\\")
          (if (gethash to (obj-table graph))
              (gethash to (obj-table graph))
              (add-to-code-tables graph to))
          (cl-ppcre:regex-replace-all "\\\\" (cl-ppcre:regex-replace-all "\"" (clamp (display to)) "'") "\\\\\\\\")
          ;; (cl-ppcre:regex-replace-all "\"" (clamp (display to)) "'")
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
                         '("color" "blue")))))


(defmethod edge ((graph graph) from (to list) &rest options)
  ;; (when (null to)
  ;;   (apply #'edge (nconc (list graph from "NIL") options)))
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

(defmethod display ((ref sb-c::ref))
  (format nil "REF [~A]:~%derived-type: ~A"
          (sxhash ref)
          (sb-c::ref-derived-type ref)))

(defmethod display ((comb sb-c::combination))
  (format nil "COMBINATION [~A]:~%kind: ~A~%info: ~A"
          (sxhash comb)
          (sb-c::combination-kind comb)
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
  (format nil "ENTRY [~A]:~%"
          (sxhash entry)))

(defmethod display (obj)
  (format nil "NOT SUPPORTED YET:~% ~A" obj))

;; New way to do this is to have two tables:

;; rec-table -> contains all the nodes from which to recurse
;; codenames -> goes from randomly generated bite to nodes in order to interactively expand

;; this creates the wrapper used inside the add methods to only run
;; the edge-adding/recursing code if we've already seen this object
(defmacro unig ((table obj) &body body)
  (let ((tab (gensym))
        (o (gensym)))
    `(let ((,tab ,table)
           (,o ,obj))
       (unless (nth-value 1 (gethash ,o ,tab))
         (setf (gethash ,o ,tab) t)
         ,@body))))

;; returns (values graph codeword-table)
(defmethod render-graph (graph)
  (get-output-stream-string (stream graph))
  (setf (table graph) (make-hash-table :test 'eq :size 63))
  (write-string (format nil "digraph {~%") (stream graph))
  (maphash #'(lambda (k v) (declare (ignore v)) (edges graph k)) (bfs-table graph))
  (write-string "}" (stream graph))
  (get-output-stream-string (stream graph)))

(defmethod expand-graph-codename (graph codename)
  (setf (gethash (gethash codename (codename-table graph))
                 (bfs-table graph))
        t))

(defun add-to-code-tables (graph object)
  (if (nth-value 1 (gethash object (obj-table graph)))
      (gethash object (obj-table graph))
      (let ((new-codename (new-codename graph)))
        (setf (gethash object (obj-table graph))
              new-codename
              (gethash new-codename (codename-table graph))
              object)
        new-codename)))

(defun new-codename (graph)
  (let ((res (format nil "~X" (codename-number graph))))
    (incf (codename-number graph))
    res))

(let ((curr-graph nil))
  (defun interactively-graph (graph)
    (setf curr-graph graph))
  (defun output (&optional (file nil))
    (if file
        (prog2 (Format t "ah") (save-graph (render-graph curr-graph) file))
        (render-graph curr-graph)))
  (defun expand (codename)
    (setf (gethash (gethash codename (codename-table curr-graph))
                   (bfs-table curr-graph))
          t)))

;; need way to simply output edges, adding is not done.

(defmethod edges ((graph graph) (objects list))
  (mapc #'(lambda (o) (edges graph o)) objects))

;; a component should be rendered as a subgraph
(defmethod edges ((graph graph) (component sb-c::component))
  (unig ((table graph) component)
    (edge graph component (sb-c::component-head component) "label" "head")
    (edge graph component (sb-c::component-tail component) "label" "tail")))

(defmethod edges ((graph graph) (cblock sb-c::cblock))
  (unig ((table graph) cblock)
    (edge graph cblock (sb-c::block-component cblock) "label" "component")
    (edge graph cblock (sb-c::block-succ cblock) "label" "succ")
    (edge graph cblock (sb-c::block-pred cblock) "label" "pred")
    (edge graph cblock (sb-c::block-start cblock) "label" "start")))

;; Unfinished
(defmethod edges ((graph graph) (cl sb-c::clambda))
  (unig ((table graph) cl)
    (edge graph cl (sb-c::lambda-home cl) "label" "home")
    (edge graph cl (sb-c::lambda-vars cl) "label" "vars")))

(defmethod edges ((graph graph) (cr sb-c::creturn))
  (unig ((table graph) cr)
    (edge graph cr (sb-c::return-lambda cr) "label" "lambda")
    (edge graph cr (sb-c::return-result cr) "label" "result")))

;; (defmethod edges ((graph graph) (node sb-c::node))
;;   )

;; this is just a dummy function to skip the CTRAN
(defmethod edges ((graph graph) (ct sb-c::ctran))
  (unig ((table graph) ct)
    ))

(defmethod edges ((graph graph) (ref sb-c::ref))
  (unig ((table graph) ref)
    (edge graph ref (sb-c::ref-leaf ref) "label" "leaf")
    (edge graph ref (sb-c::ref-next ref) "label" "next")
    (edge graph ref (sb-c::ref-lvar ref) "label" "lvar")))

(defmethod edges ((graph graph) (bind sb-c::bind))
  (unig ((table graph) bind)
    (edge graph bind (sb-c::bind-lambda bind) "label" "lambda")
    (edge graph bind (sb-c::bind-next bind) "label" "next" "color" "green")
    (edge graph bind (sb-c::bind-prev bind) "label" "prev" "color" "red")))

(defmethod edges ((graph graph) (comb sb-c::combination))
  (unig ((table graph) comb)
    (edge graph comb (sb-c::combination-fun comb) "label" "fun")
    (edge graph comb (sb-c::combination-args comb) "label" "args")))

(defmethod edges ((graph graph) (lvar sb-c::lvar))
  (unig ((table graph) lvar)
    (edge graph lvar (sb-c::lvar-dest lvar) "label" "dest" "color" "brown")
    (edge graph lvar (sb-c::lvar-uses lvar) "label" "uses")))

(defmethod edges ((graph graph) (lamvar sb-c::lambda-var))
  (unig ((table graph) lamvar)
    (edge graph lamvar (sb-c::lambda-var-home lamvar) "label" "home")
    (edge graph lamvar (sb-c::lambda-var-sets lamvar) "label" "sets")))

(defmethod edges ((graph graph) (entry sb-c::entry))
  (unig ((table graph) entry)
    (edge graph entry (sb-c::entry-exits entry) "label" "exits")
    (edge graph entry (sb-c::entry-cleanup entry) "label" "cleanup")
    (edge graph entry (sb-c::entry-next entry) "label" "next")
    (edge graph entry (sb-c::entry-prev entry) "label" "prev")))

(defmethod edges ((graph graph) object)
  (unig ((table graph) object)))

(defmacro bfs-unig ((graph obj) &body body)
  (let ((g (gensym))
        (o (gensym)))
    `(let ((,g ,graph)
           (,o ,obj))
       (unless (nth-value 1 (gethash ,o (bfs-table ,g)))
         (setf (gethash ,o (bfs-table ,g)) t)
         ,@body)))
  )

(defmethod bfs-add ((graph graph) (distance integer) (objects list))
  (mapc #'(lambda (o) (bfs-add graph distance o)) objects))

;; a component should be rendered as a subgraph
(defmethod bfs-add ((graph graph) (distance integer) (component sb-c::component))
  (when (> distance 0)
    (decf distance)
    (bfs-unig (graph component)
      (bfs-add graph distance (sb-c::component-head component))
      (bfs-add graph distance (sb-c::component-tail component)))))

(defmethod bfs-add ((graph graph) (distance integer) (cblock sb-c::cblock))
  (when (> distance 0)
    (decf distance)
    (bfs-unig (graph cblock)
      (bfs-add graph distance (sb-c::block-component cblock))
      (bfs-add graph distance (sb-c::block-succ cblock))
      (bfs-add graph distance (sb-c::block-pred cblock))
      (bfs-add graph distance (sb-c::block-start cblock)))))

;; Unfinished
(defmethod bfs-add ((graph graph) (distance integer) (cl sb-c::clambda))
  (when (> distance 0)
    (decf distance)
    (bfs-unig (graph cl)
      (bfs-add graph distance (sb-c::lambda-home cl))
      (bfs-add graph distance (sb-c::lambda-vars cl)))
    )
  )

(defmethod bfs-add ((graph graph) (distance integer) (cr sb-c::creturn))
  (when (> distance 0)
    (decf distance)
    (bfs-unig (graph cr)
      (bfs-add graph distance (sb-c::return-lambda cr))
      (bfs-add graph distance (sb-c::return-result cr)))))

;; (defmethod bfs-add ((graph graph) (distance integer) (node sb-c::node))
;;   )

;; this is just a dummy function to skip the CTRAN
(defmethod bfs-add ((graph graph) (distance integer) (ct sb-c::ctran))
  (when (> distance 0)
    (bfs-unig (graph ct)
      (bfs-add graph distance (sb-c::ctran-next ct)))))

(defmethod bfs-add ((graph graph) (distance integer) (ref sb-c::ref))
  (when (> distance 0)
    (decf distance)
    (bfs-unig (graph ref)
      (bfs-add graph distance (sb-c::ref-leaf ref))
      (bfs-add graph distance (sb-c::ref-next ref))
      (bfs-add graph distance (sb-c::ref-lvar ref)))))

(defmethod bfs-add ((graph graph) (distance integer) (bind sb-c::bind))
  (when (> distance 0)
    (decf distance)
    (bfs-unig (graph bind)
      (bfs-add graph distance (sb-c::bind-lambda bind))
      (bfs-add graph distance (sb-c::bind-next bind))
      (bfs-add graph distance (sb-c::bind-prev bind)))))

(defmethod bfs-add ((graph graph) (distance integer) (comb sb-c::combination))
  (when (> distance 0)
    (decf distance)
    (bfs-unig (graph comb)
      (bfs-add graph distance (sb-c::combination-fun comb))
      (bfs-add graph distance (sb-c::combination-args comb)))))

(defmethod bfs-add ((graph graph) (distance integer) (lvar sb-c::lvar))
  (when (> distance 0)
    (decf distance)
    (bfs-unig (graph lvar)
      (bfs-add graph distance (sb-c::lvar-dest lvar))
      (bfs-add graph distance (sb-c::lvar-uses lvar)))))

(defmethod bfs-add ((graph graph) (distance integer) (lamvar sb-c::lambda-var))
  (when (> distance 0)
    (decf distance)
    (bfs-unig (graph lamvar)
      (bfs-add graph distance (sb-c::lambda-var-home lamvar))
      (bfs-add graph distance (sb-c::lambda-var-sets lamvar)))
    ))

(defmethod bfs-add ((graph graph) (distance integer) (entry sb-c::entry))
  (when (> distance 0)
    (decf distance)
    (bfs-unig (graph entry)
      (bfs-add graph distance (sb-c::entry-exits entry))
      (bfs-add graph distance (sb-c::entry-cleanup entry))
      (bfs-add graph distance (sb-c::entry-next entry))
      (bfs-add graph distance (sb-c::entry-prev entry))))
  )

(defmethod bfs-add ((graph graph) (distance integer) object)
  (when (> distance 0)
    (decf distance)
    (bfs-unig (graph object))))
