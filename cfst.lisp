;;; cfst.lisp

;;; Compositing Flat Syntax Tree

(defpackage #:com.splittist.cfst
  (:use #:cl))

(in-package #:com.splittist.cfst)

(defclass tree ()
  ((%left-leaf
    :accessor left-leaf)
   (%right-leaf
    :accessor right-leaf)
   (%root-node
    :accessor root-node)
   (%initial-value
    :initarg :initial-value
    :reader initial-value)
   (%value=
    :initarg :value=
    :reader value=)
   (%validp
    :initform nil
    :accessor validp))
  (:default-initargs :value= #'eql))

(defclass node ()
  ())

(defclass non-leaf (node)
  ((%low-key
    :initarg :low-key
    :accessor low-key)
   (%high-key
    :initarg :high-key
    :accessor high-key)
   (%left-node
    :initarg :left-node
    :accessor left-node)
   (%right-node
    :initarg :right-node
    :accessor right-node)))

(defmethod print-object ((object non-leaf) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "[~A, ~A)" (low-key object) (high-key object))))

(defgeneric non-leaf-p (node)
  (:method (node) nil)
  (:method ((node non-leaf)) t))

(defun make-non-leaf (left right)
  (let* ((low (if (leafp left)
                  (key left)
                  (low-key left)))
         (high (if right
                   (if (leafp right)
                       (if (next right)
                           (key (next right))
                           (key right))
                       (high-key right))
                   (if (leafp left)
                       (key left)
                       (high-key left)))))
    (make-instance 'non-leaf
                   :low-key low
                   :high-key high
                   :left-node left
                   :right-node right)))

(defclass leaf (node)
  ((%key
    :initarg :key
    :accessor key)
   (%value
    :initarg :value
    :accessor value)
   (%prev
    :initarg :prev
    :accessor prev
    :initform nil)
   (%next
    :initarg :next
    :accessor next
    :initform nil)))

(defmethod print-object ((object leaf) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A:~A" (key object) (value object))))

(defgeneric leafp (node)
  (:method (node) nil)
  (:method ((node leaf)) t))

(defun end-leaf-p (node)
  (null (next node)))

(defun start-leaf-p (node)
  (null (prev node)))

(defun link (left-node right-node)
  (when left-node
    (setf (next left-node) right-node))
  (when right-node
    (setf (prev right-node) left-node)))

(defun find-leaf (head key)
  (loop for node = head then (next node)
          thereis (and (< key (key node))
                       (prev node))))

(defun shift-right (head pos size &optional (skip t))
  (let ((start (find-leaf head pos)))
    (unless skip
      (incf (key start) size))
    (loop for node = (next start) then (next node)
          while node
          do (incf (key node) size)))
  head)

(defun shift-left (head start end &optional (value= #'eql))
  (let ((start-node (next (find-leaf head start)))
        (size (- end start)))
    (labels ((shift-rest-left (from)
               (loop for node = from then (next node)
                     while node
                     do (decf (key node) size))))
      (cond
        ((end-leaf-p start-node)
         (decf (key start-node) size)
         (when (and (= (key start-node) (key (prev start-node)))
                    (prev (prev start-node)))
           (link (prev (prev start-node)) start-node)))
        ((< end (key start-node))
         (shift-rest-left start-node))
        (t
         (setf (key start-node) start)
         (loop for node = (next start-node) then (next node)
               while (and (next node) (<= (key node) end))
               finally (setf (value start-node) (value (prev node))
                             (next start-node) node
                             (prev node) start-node))
         (when (prev start-node)
           (cond ((funcall value=
                           (value (prev start-node))
                           (value start-node))
                  (link (prev start-node) (next start-node)))
                 ((= (key start-node) (key (prev start-node)))
                  (setf (value (prev start-node)) (value start-node))
                  (link (prev start-node) (next start-node)))))
         (shift-rest-left (next start-node))))
      head)))

(defun insert-segment-list (head start size val &optional (value= #'eql))
  (let ((node (find-leaf head start)))
    (if (funcall value= val (value node))
        (shift-right head start size)
        (let* ((new (make-instance 'leaf :value val :key start :prev node))
               (right (make-instance 'leaf :value (value node) :key (+ start size)
                                           :prev new :next (next node))))
          (setf (next new) right
                (prev (next right)) new
                (next node) new)
          (shift-right head (key right) size nil)))
    head))

(defun segment-count (head)
  (loop for node = head then (next node)
        while node
        counting node))

(defun build-tree (start-node)
  (flet
      ((do-leaves (node)
         (loop for left = node then (and right (next right))
               for right = (and left (next left))
               while left
               collect (make-non-leaf left right)))
       (do-non-leaves (list)
         (loop for (left right) on list by #'cddr
               collect (make-non-leaf left right))))
    (loop for row = (do-leaves start-node) then (do-non-leaves row)
          until (= 1 (length row))
          finally (return (first row)))))

(defun find-node-tree (node key)
  (cond ((leafp node)
         (values (value node) t (key node) (key (next node))))
        ((non-leaf-p node)
         (let ((left (left-node node))
               (right (right-node node)))
           (cond ((null right)
                  (find-node-tree left key))
                 ((and (non-leaf-p left)
                       (< key (high-key left)))
                  (find-node-tree left key))
                 ((non-leaf-p right)
                  (find-node-tree right key))
                 ((< key (key right))
                  (find-node-tree left key))
                 (t
                  (find-node-tree right key)))))
        (t
         (error "Something wrong: ~A ~A" node key))))

(defun find-node-list (first key)
  (loop for start = first then next
        for next = (next start)
        while (> key (key next))
        finally (return
                  (values (value start) t (key start) (key next)))))

;;; TESTING

(defun make-leaf-list (start stop &optional (step 1) (vals (list "A" "B" "C")))
  (let* ((vals (alexandria:ensure-list vals))
         (val-list (apply #'alexandria:circular-list vals))
         (first-val (car (last vals))))
    (loop with first = (make-instance 'leaf :key 0 :value first-val)
          for i from (+ start step) to stop by step
          for val in val-list
          for new = (make-instance 'leaf :key i :value val)
          and prev = first then new
          do (link prev new)
          finally (return first))))

(defun last-key (head)
  (loop for node = head then (next node)
        while (next node)
        finally (return (key node))))

(defun print-segments (head &key ruler (stream t))
  (serapeum:with-string (s stream)
    (when ruler
      (print-ruler (last-key head) s))
    (princ #\| s)
    (loop for start = head then next
          for next = (next start)
          while next
          do (format s "~{~A~^ ~}" (make-list (- (key next) (key start))
                                              :initial-element (value start)))
             (princ #\| s)
          finally (terpri s))))

(defun print-ruler (len &optional (stream t))
  (serapeum:with-string (s stream)
    (loop for index from 0 to len
          do (princ (mod index 10) s)
             (princ #\Space s)
          finally (terpri s))))

(defun print-nodes (head &optional (stream t))
  (serapeum:with-string (s stream)
    (loop for node = head then (next node)
          while node
          do (print node s))))

(defun compose-b (val)
  (cond ((string= "." val)
         "B")
        ((string= "B" val)
         ".")
        ((string= "A" val)
         "C")
        ((string= "C" val)
         "A")))

(defun test-shift (operation expected-string expected-count
                   &optional (list (make-leaf-list 0 25 5))))
  (handler-case
      (let ((result t))
        (apply (car operation) list (cdr operation))
        (let ((result-string (string-trim (list #\Space #\Newline)
                                          (print-segments list :stream nil)))
              (result-count (segment-count list)))
          (unless (string= expected-string result-string)
            (format t "Failed: ~A~%~
                       Wanted: ~A~%~
                       Got   : ~A~%"
                    operation expected-string result-string)
            (setf result nil))
          (unless (= expected-count result-count)
            (format t "Failed: ~A~%~
                       Wanted: ~D~%~
                       Got   : ~D~%" operation expected-count result-count)
            (print-nodes list)
            (setf result nil)))
        result)
    (error (e) (format t "Failed: ~A~%~
                          ERROR: ~S~%" operation e)
      nil)))

;; 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5
;; |C C C C C|A A A A A|B B B B B|C C C C C|A A A A A|

(defparameter *tests*
  '((test-shift
     '(shift-right 0 1)
     "|C C C C C C|A A A A A|B B B B B|C C C C C|A A A A A|"
     6)
    (test-shift
     '(shift-right 3 2)
     "|C C C C C C C|A A A A A|B B B B B|C C C C C|A A A A A|"
     6)
    (test-shift
     '(shift-right 24 1)
     "|C C C C C|A A A A A|B B B B B|C C C C C|A A A A A A|"
     6)
    (test-shift
     '(shift-right 20 2)
     "|C C C C C|A A A A A|B B B B B|C C C C C|A A A A A A A|"
     6)
    (test-shift
     '(shift-right 19 2)
     "|C C C C C|A A A A A|B B B B B|C C C C C C C|A A A A A|"
     6)
    (test-shift
     '(shift-left 0 2)
     "|C C C|A A A A A|B B B B B|C C C C C|A A A A A|"
     6)
    (test-shift
     '(shift-left 23 25)
     "|C C C C C|A A A A A|B B B B B|C C C C C|A A A|"
     6)
    (test-shift
     '(shift-left 0 5)
     "|A A A A A|B B B B B|C C C C C|A A A A A|"
     5)
    (test-shift
     '(shift-left 20 25)
     "|C C C C C|A A A A A|B B B B B|C C C C C|"
     5)
    (test-shift
     '(shift-left 3 5)
     "|C C C|A A A A A|B B B B B|C C C C C|A A A A A|"
     6)
    (test-shift
     '(shift-left 3 8)
     "|C C C|A A|B B B B B|C C C C C|A A A A A|"
     6)
    (test-shift
     '(shift-left 3 13)
     "|C C C|B B|C C C C C|A A A A A|"
     5)
    (test-shift
     '(shift-left 0 25)
     "||"
     2)
    (test-shift
     '(shift-left 0 23)
     "|A A|"
     2)
    (test-shift
     '(shift-left 3 17)
     "|C C C C C C|A A A A A|"
     3)
    (test-shift
     '(insert-segment-list 2 2 "C" string=)
     "|C C C C C C C|A A A A A|B B B B B|C C C C C|A A A A A|"
     6)
    (test-shift
     '(insert-segment-list 2 2 "A" string=)
     "|C C|A A|C C C|A A A A A|B B B B B|C C C C C|A A A A A|"
     8)
    (test-shift
     '(insert-segment-list 5 2 "A" string=)
     "|C C C C C|A A A A A A A|B B B B B|C C C C C|A A A A A|"
     6)
    (test-shift
     '(insert-segment-list 8 2 "Z" string=)
     "|C C C C C|A A A|Z Z|A A|B B B B B|C C C C C|A A A A A|"
     8)))

(defun run-tests ()
  (let ((total 0)
        (passed 0)
        (failed 0))
    (dolist (test *tests*)
      (incf total)
      (let ((result (eval test)))
        (if (eq t result)
            (progn
              (incf passed)
              (princ #\Check_Mark) (terpri))
            (progn (incf failed) (terpri)))))
    (format t "~%Ran ~D; Passed ~D; Failed ~D"
            total passed failed)
    (zerop failed)))
