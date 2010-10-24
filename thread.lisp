(in-package :rucksack)

(defun make-spinlock ()
  (cons nil nil))

(defun lock-spinlock (spinlock)
  (loop while (sb-ext:compare-and-swap (car spinlock) nil t)))

(defun unlock-spinlock (spinlock)
  (setf (car spinlock) nil))

(defmacro with-spinlock ((spinlock) &body body)
  (alexandria:once-only (spinlock)
    `(progn
       (lock-spinlock ,spinlock)
       (unwind-protect
            (progn ,@body)
         (unlock-spinlock ,spinlock)))))

(defun make-recursive-spinlock ()
  (cons nil 0))

(defun lock-recursive-spinlock (recursive-spinlock)
  (loop with self = sb-thread:*current-thread*
    for ret = (sb-ext:compare-and-swap (car recursive-spinlock) nil self)
        until (or (null ret) (eq ret self))
        finally (incf (cdr recursive-spinlock))))

(defun unlock-recursive-spinlock (recursive-spinlock)
  (when (decf (cdr recursive-spinlock))
    (setf (car recursive-spinlock) nil)))

(defmacro with-recursive-spinlock ((recursive-spinlock) &body body)
  (alexandria:once-only (recursive-spinlock)
    `(progn
       (lock-recursive-spinlock ,recursive-spinlock)
       (unwind-protect
            (progn ,@body)
         (unlock-recursive-spinlock ,recursive-spinlock)))))


(defclass spin-rw-lock ()
  ((spinlock :initform (make-spinlock))
   (write-count :initform 0)
   (read-count :initform 0)))


(defgeneric lock-writer (lock))
(defgeneric lock-writer-try (lock))
(defgeneric lock-reader (lock))
(defgeneric lock-reader-try (lock))
(defgeneric unlock (lock))
(defgeneric promote (lock))
(defgeneric demote (lock))

(defmethod lock-writer ((lock spin-rw-lock))
  (with-slots (spinlock write-count read-count) lock
    (loop
      (with-spinlock (spinlock)
        (when (and (zerop write-count)
                   (zerop read-count))
          (incf write-count)
          (return-from lock-writer))))))

(defmethod lock-writer-try ((lock spin-rw-lock))
  (with-slots (spinlock write-count read-count) lock
    (with-spinlock (spinlock)
      (when (and (zerop write-count)
                 (zerop read-count))
        (incf write-count)))))

(defmethod lock-reader ((lock spin-rw-lock))
  (with-slots (spinlock write-count read-count) lock
    (loop
      (with-spinlock (spinlock)
        (when (zerop write-count)
          (incf read-count)
          (return-from lock-reader))))))

(defmethod lock-reader-try ((lock spin-rw-lock))
  (with-slots (spinlock write-count read-count) lock
    (with-spinlock (spinlock)
      (when (zerop write-count)
        (incf read-count)))))

(defmethod unlock ((lock spin-rw-lock))
  (with-slots (spinlock write-count read-count) lock
    (with-spinlock (spinlock)
      (if (plusp write-count)
          (decf write-count)
          (decf read-count)))))

(defmethod promote ((lock spin-rw-lock))
  (with-slots (spinlock write-count read-count) lock
    (with-spinlock (spinlock)
      (when (= read-count 1)
        (decf read-count)
        (incf write-count)))))

(defmethod demote ((lock spin-rw-lock))
  (with-slots (spinlock write-count read-count) lock
    (with-spinlock (spinlock)
      (decf write-count)
      (incf read-count))))

(defclass mutex-rw-lock ()
  ((lock :initform (sb-thread:make-mutex))
   (write-count :initform 0)
   (read-count :initform 0)))

(defmethod lock-writer ((lock mutex-rw-lock))
  (with-slots (lock write-count read-count) lock
    (loop
      (sb-thread:with-mutex (lock)
        (when (and (zerop write-count)
                   (zerop read-count))
          (incf write-count)
          (return-from lock-writer))))))

(defmethod lock-writer-try ((lock mutex-rw-lock))
  (with-slots (lock write-count read-count) lock
    (sb-thread:with-mutex (lock)
      (when (and (zerop write-count)
                 (zerop read-count))
        (incf write-count)))))

(defmethod lock-reader ((lock mutex-rw-lock))
  (with-slots (lock write-count read-count) lock
    (loop
      (sb-thread:with-mutex (lock)
        (when (zerop write-count)
          (incf read-count)
          (return-from lock-reader))))))

(defmethod lock-reader-try ((lock mutex-rw-lock))
  (with-slots (lock write-count read-count) lock
    (sb-thread:with-mutex (lock)
      (when (zerop write-count)
        (incf read-count)))))

(defmethod unlock ((lock mutex-rw-lock))
  (with-slots (lock write-count read-count) lock
    (sb-thread:with-mutex (lock)
      (if (plusp write-count)
          (decf write-count)
          (decf read-count)))))

(defmethod promote ((lock mutex-rw-lock))
  (with-slots (lock write-count read-count) lock
    (sb-thread:with-mutex (lock)
      (when (= read-count 1)
        (decf read-count)
        (incf write-count)))))

(defmethod demote ((lock mutex-rw-lock))
  (with-slots (lock write-count read-count) lock
    (sb-thread:with-mutex (lock)
      (decf write-count)
      (incf read-count))))

(defmacro with-rw-lock ((lock writer) &body body)
  `(progn
     ,(case writer
        ((t)
           `(lock-writer ,lock))
        ((nil)
           `(lock-reader ,lock))
        (t
           `(if ,writer
                (lock-writer ,lock)
                (lock-reader ,lock))))
     (unwind-protect (progn ,@body)
       (unlock ,lock))))


(defclass atomic-int ()
  ((%value :initarg :value :initform 0)
   (lock :initform (make-spinlock) :reader atomic-int-lock)))

(defmethod print-object ((self atomic-int) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~a" (slot-value self '%value))))

(defmethod atomic-int-value ((self atomic-int))
  (with-spinlock ((atomic-int-lock self))
    (slot-value self '%value)))

(defmethod (setf atomic-int-value) (value (self atomic-int))
  (with-spinlock ((atomic-int-lock self))
    (setf (slot-value self '%value) value)))

(defmethod atomic-int-add ((self atomic-int) value)
  (with-spinlock ((atomic-int-lock self))
    (prog1 (slot-value self '%value)
      (incf (slot-value self '%value) value))))

(defmethod atomic-int-cas ((self atomic-int) old-value new-value)
  (with-spinlock ((atomic-int-lock self))
    (when (= (slot-value self '%value) old-value)
      (setf (slot-value self '%value) new-value))))

(defmethod atomic-int-secure-least ((self atomic-int) val)
  (loop for cur = (atomic-int-value self)
        if (>= cur val)
          do (return cur)
        if (atomic-int-cas self cur val)
          do (return val)))
