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
    :accessor key
    :initform nil)
   (%value
    :initarg :value
    :accessor value
    :initform nil)
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
    (format stream "[~A,~A):~A"
            (key object)
            (when (next object)
              (key (next object)))
            (value object))))

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

(defun copy-leaf (leaf)
  (make-instance 'leaf :key (key leaf) :value (value leaf)))

(defun copy-leaves (head)
  (loop with new-head = nil
        with new = nil
        for node = head then (next node)
        and prev = nil then new
        while node
        do (setf new (copy-leaf node))
           (link prev new)
           (when (null new-head)
             (setf new-head new))
        finally (return new-head)))

(defun find-leaf (head key)
  (loop for node = head then (next node)
        while (next node)
        until (< key (key node))
        finally (return (or (prev node) node))))

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
    (labels ((shift-rest-right (from)
               (loop for node = from then (next node)
                     while node
                     do (incf (key node) size))))
      (if (funcall value= val (value node))
          (shift-rest-right (next node))
          (let* ((new (make-instance 'leaf :value val :key start))
                 (right (make-instance 'leaf :value (value node) :key (+ start size)))
                 (next (next node)))
            (link node new)
            (link new right)
            (link right next)
            (shift-rest-right next))))
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

(defun read-segments (string &optional (init-val "."))
  (assert (char= #\| (char string 0)))
  (assert (char= #\| (char string (1- (length string)))))
  (loop with node = (make-instance 'leaf :key 0 :value init-val)
       with head = node
     with val = init-val
     with key = 1
     for index from 1 below (length string)
     for char = (char string index)
     while (< key (length string))
     if (char= #\| char)
     do
       (setf (value node) val)
       (let ((new (make-instance 'leaf :key (1- key) :value init-val :prev node)))
         (setf (next node) new
               node new))
     else do
       (unless (char= #\Space char)
         (setf val char)
         (incf key))
     finally (return head)))

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

(defun test-from-file (path &key tests)
  (let ((list nil)
        (test nil)
        (total 0)
        (passed 0)
        (failed 0)
        (skipped 0))
    (let ((lines
           (with-open-file (f path :external-format :utf8)
             (loop for line = (read-line f nil nil)
                while line
                collect line))))
      (dolist (line lines)
        (let ((line (string-trim " " line)))
          (unless (or (string= "" line)
                      (char= #\# (char line 0)))
            (multiple-value-bind (first pos)
                (read-from-string line)
              (cond ((and (symbolp first) (string-equal "list" first))
                     (setf list (read-segments (subseq line pos))))
                    ((and (symbolp first) (string-equal "test" first))
                     (setf test (read-from-string (subseq line pos))))
                    ((and (numberp first)
                          (or (null tests)
                              (member first tests)))
                     (with-input-from-string (s line :start pos)
                       (let ((form (read s))
                             (other (loop for thing = (read s nil nil)
                                       while thing
                                       collecting thing)))
                         (incf total)
                         (let* ((list-list (list (copy-leaves list)))
                                (result (apply test first form (append other list-list))))
                           (if (eq t result)
                               (progn
                                 (incf passed)
                                 (princ #\Check_Mark) (princ #\Space))
                               (progn
                                 (incf failed)
                                 (terpri)))))))
                    ((numberp first)
                     (incf skipped))
                    (t (warn "Unknown test format '~S'" first)))))))
      (format t "~%Total ~D/~D; Passed ~D; Failed ~D~%" total (+ total skipped) passed failed))))

(defun compose-b (val)
  (cond ((string= "." val)
         "B")
        ((string= "B" val)
         ".")
        ((string= "A" val)
         "C")
        ((string= "C" val)
         "A")))

(defun compose-list (head start end composer &optional (value= #'eql))
  (loop repeat 10 ; DEBUG
        for node = (find-leaf head (1- end))
        while (> end start)
        do (let* (;(node (find-leaf head (1- end)))
                  (old-value (value node))
                  (new-value (funcall composer old-value)))
             (if (funcall value= new-value old-value)
                 (loop-finish))
                 (let ((new-end
                         (unless (and (next node)
                                      (= end (key (next node))))
                           (make-instance 'leaf)))
                       (new-start
                         (when (> start (key node))
                           (make-instance 'leaf))))
                   (cond
                     ((and new-end new-start)
                      (link new-start new-end)
                      (link new-end (next node))
                      (link node new-start)
                      (setf (value new-start) new-value
                            (value new-end) old-value
                            (key new-start) start
                            (key new-end) end)
                      (loop-finish))
                     (new-start
                      (link new-start (next node))
                      (link node new-start)
                      (setf (value new-start) new-value
                            (key new-start) start)
                      (loop-finish))
                     (new-end
                      (link new-end (next node))
                      (link node new-end)
                      (setf (value new-end) old-value
                            (value node) new-value
                            (key new-end) end
                            end (key node)))
                     (t
                      (setf (value node) new-value)
                      (when (and (next (next node))
                                 (funcall value= (value (next (next node))) new-value))
                        (link node (next (next node))))
                      (setf end (key node))))))
        finally (when (and (next node)
                           (next (next node))
                           (funcall value= (value node) (value (next node))))
                  (link node (next (next node)))))
  head)

(defun test-shift (number operation expected-string expected-count
                   &optional (list (make-leaf-list 0 25 5)))
  (handler-case
      (let ((result t))
        (apply (car operation) list (cdr operation))
        (let ((result-string (string-trim (list #\Space #\Newline)
                                          (print-segments list :stream nil)))
              (result-count (segment-count list)))
          (unless (string= expected-string result-string)
            (format t "~%Failed: ~D ~A~%~
                         Wanted: ~A~%~
                         Got   : ~A~%"
                    number operation expected-string result-string)
            (setf result nil))
          (unless (= expected-count result-count)
            (format t "~%Failed: ~D ~A~%~
                         Wanted: ~D~%~
                         Got   : ~D~%"
                    number operation expected-count result-count)
            (print-nodes list)
            (setf result nil)))
        result)
    (error (e) (format t "~%Failed: ~D ~A~%~
                            ERROR: [~S] ~A~%"
                       number operation (type-of e) e)
      nil)))
