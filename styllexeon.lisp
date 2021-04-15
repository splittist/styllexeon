;;;; styllexeon.lisp

(in-package #:styllexeon)

;; node
;;  parent
;;  properties
;;  children

(defclass node ()
  ())

(defclass child-node (node)
  ((%parent
    :initarg :parent
    :initform (error "Parent required")
    :accessor parent)))

(defgeneric child-node-p (object))

(defmethod child-node-p (object)
  nil)

(defmethod child-node-p ((object child-node))
  t)

(defclass parent-node (node)
  ((%children
    :initarg :children
    :initform (make-array 0 :adjustable t :fill-pointer 0 :element-type 'child-node)
    :accessor children)))

(defgeneric parent-node-p (object))

(defmethod parent-node-p (object)
  nil)

(defmethod parent-node-p ((object parent-node))
  t)

(defgeneric childrenp (parent))

(defmethod childrenp ((parent parent-node))
  (plusp (length (children parent))))

(defgeneric append-child (parent child))

(defmethod append-child ((parent parent-node) (child child-node))
  (setf (parent child) parent)
  (vector-push-extend child (children parent)))

(defgeneric prepend-child (parent child))

(defmethod prepend-child ((parent parent-node) (child child-node))
  (setf (parent child) parent)
  (array-utils:vector-push-extend-front child (children parent)))

(defgeneric first-child (parent))

(defmethod first-child ((parent parent-node))
  (when (< 0 (fill-pointer (children parent)))
    (elt (children parent) 0)))

(defgeneric last-child (parent))

(defmethod last-child ((parent parent-node))
  (when (< 0 (fill-pointer (children parent)))
    (elt (children parent) (1- (fill-pointer (children parent))))))

(defgeneric family (child))

(defmethod family ((child child-node))
  (children (parent child)))

(defgeneric progenitor (child))

(defmethod progenitor ((child child-node))
  (loop for node = (parent child)
        while (child-node-p node)
        finally (return node)))

(defgeneric child-position (child))

(defmethod child-position ((child child-node))
  (position child (family child)))

(defgeneric siblings (child))

(defmethod siblings ((child child-node))
  (remove child (family child)))

(defgeneric previous-sibling (child))

(defmethod previous-sibling ((child child-node))
  (let ((pos (child-position child)))
    (when (< 0 pos)
      (elt (family child) (1- pos)))))

(defgeneric next-sibling (child))

(defmethod next-sibling ((child child-node))
  (let ((pos (1+ (child-position child)))
        (family (family child)))
    (when (< pos (fill-pointer family))
      (elt family pos))))

(defgeneric remove-child (child))

(defmethod remove-child ((child child-node))
  (array-utils:vector-pop-position
   (family child)
   (child-position child))
  (set (parent child) nil)
  child)

(defgeneric remove-children (parent))

(defmethod remove-children ((parent parent-node))
  (loop for child across (children parent)
        do (setf (parent child) nil))
  (setf (fill-pointer (children parent)) 0)
  parent)

(defgeneric insert-before (child new-child))

(defmethod insert-before ((child child-node) (new-child child-node))
  (array-utils:vector-push-extend-position
   new-child
   (family child)
   (child-position child))
  new-child)

(defgeneric insert-after (child new-child))

(defmethod insert-after ((child child-node) (new-child child-node))
  (array-utils:vector-push-extend-position
   new-child
   (family child)
   (1+ (child-position child)))
  new-child)

(defclass properties-mixin ()
  ((%properties
    :initarg :properties
    :initform (make-hash-table :test 'equalp)
    :accessor properties)))

(defgeneric property (node property))

(defmethod property ((node properties-mixin) property)
  (gethash property (properties node)))

(defgeneric (setf property) (value node property))

(defmethod (setf property) (value (node properties-mixin) property)
  (setf (gethash property (properties node)) value))

(defgeneric remove-property (node property))

(defmethod remove-property ((node properties-mixin) property)
  (remhash property (properties node))
  nil)

(defgeneric has-property (node property))

(defmethod has-property ((node properties-mixin) property)
  (nth-value 1 (gethash property (properties node))))

(defclass lock-mixin ()
  ((%lock
    :initarg :lock
    :initform (bordeaux-threads:make-lock)
    :accessor lock)))

(defmethod remove-property :around ((node lock-mixin) property)
  (bordeaux-threads:with-lock-held ((lock node))
    (call-next-method)))

(defmethod (setf property) :around (value (node lock-mixin) property)
  (bordeaux-threads:with-lock-held ((lock node))
    (call-next-method)))

;; FIXME add children wrangling

;; style - inheritance
;;  doc defaults
;;  paragraph
;;  character
;;  table
;;  numbering

(defclass style (properties-mixin)
  ())

(defclass paragraph-style (style)
  ())

(defclass character-style (style)
  ())

(defclass table-style (style)
  ())

(defclass direct-style (style)
  ((%start-cursor
    :initarg :start-cursor
    :accessor start-cursor)
   (%end-cursor
    :initarg :end-cursor
    :accessor end-cursor)))

(defun make-direct-style (paragraph start end &rest properties)
  (let* ((start (make-instance 'flexichain:left-sticky-flexicursor
                               :chain (contents paragraph)
                               :position start))
         (end (make-instance 'flexichain:right-sticky-flexicursor
                             :chain (contents paragraph)
                             :position end))
         (ds (make-instance 'direct-style
                            :start-cursor start
                            :end-cursor end)))
    (loop for (key val) on properties by #'cddr
          do (setf (property ds key) val))
    ds))

(defgeneric merge-properties! (new old))

(defmethod merge-properties! ((new properties-mixin) (old properties-mixin))
  (let* ((old-keys (alexandria:hash-table-keys (properties old)))
         (new-keys (alexandria:hash-table-keys (properties new)))
         (additional-old (set-difference old-keys new-keys :test #'string-equal)))
    (loop for key in additional-old
          do (setf (property new key) (property old key)))
    new))

(defparameter *tick* 0)

(defun tick ()
  (incf *tick*))

(defclass modification-tick-mixin ()
  ((%modification-tick
    :initarg :modification-tick
    :accessor modification-tick)))

(defmethod initialize-instance :after ((instance modification-tick-mixin) &key &allow-other-keys)
  (setf (modification-tick instance) (tick)))

(defclass story (parent-node
                 properties-mixin
                 lock-mixin
                 modification-tick-mixin)
  ())

(defclass body (story)
  ())

;; story-content
;;  paragraph
;;  table
;;  uneditable content

(defclass paragraph (child-node
                     properties-mixin
                     lock-mixin
                     modification-tick-mixin)
  ((%contents
    :initarg :contents
    :initform (make-instance 'flexichain:standard-cursorchain)
    :accessor contents)))

(defgeneric contents-length (object))

(defmethod contents-length ((paragraph paragraph))
  (flexichain:nb-elements (contents paragraph)))

(defgeneric empty-p (object))

(defmethod empty-p ((paragraph paragraph))
  (flexichain:flexi-empty-p (contents paragraph)))

(defgeneric insert* (paragraph position object))

(defmethod insert* ((paragraph paragraph) position object)
  (bordeaux-threads:with-lock-held ((lock paragraph))
    (flexichain:insert* (contents paragraph) position object)))

(defgeneric insert-vector* (paragraph position vector))

(defmethod insert-vector* ((paragraph paragraph) position vector)
  (bordeaux-threads:with-lock-held ((lock paragraph))
    (flexichain:insert-vector* (contents paragraph) position vector)))

(defgeneric delete* (paragraph position))

(defmethod delete* ((paragraph paragraph) position)
  (bordeaux-threads:with-lock-held ((lock paragraph))
    (flexichain:delete* (contents paragraph) position)))

(defmethod delete-elements* (paragraph position count)
  (bordeaux-threads:with-lock-held ((lock paragraph))
    (flexichain:delete-elements* (contents paragraph) position count)))

(defgeneric element* (paragraph position))

(defmethod element* ((paragraph paragraph) position)
  (flexichain:element* (contents paragraph) position))

;; ommitting (setf element*)

(defgeneric ->string (element))

(defmethod ->string (element)
  (princ-to-string element))

(defgeneric ->string-stream (element stream))

(defmethod ->string-stream (element stream)
  (princ element stream))

(defun ->string (paragraph)
  (with-output-to-string (s)
    (loop for i from 0 below (contents-length paragraph)
          for element = (element* paragraph i)
          do (->string-stream element s))))

;; paragraph-content
;;  uneditable content
;;  field
;;  hyperlink
;;  ?bidi
;;  r
;;    br
;;    cr
;;    contentPart
;;    continuationSeparator, seperator
;;    drawing
;;    field
;;    noBreakHyphen
;;    softHyphen - U+00AD &shy; #\Soft_Hyphen
;;    uneditable content
;;    pgNum
;;    tab, ptab
;;    sym
;;    t

;; table
;;  table rows
;;    table cells

;; annotation
;;  bookmark
;;  comment
;;  del/ins/moves
;;  permission
;;  proof error

;; field
;;  simple
;;  complex
;;  legacy
;;  [pgNum/hyperlink?]

