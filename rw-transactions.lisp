(in-package :rucksack)

(defclass rw-transaction-rucksack (standard-rucksack)
  ((transaction-lock :initform (make-instance 'mutex-rw-lock)
                     :reader rucksack-transaction-lock))
  (:documentation
   "pararel read transaction. only one write transaction."))

(defmethod transaction-start-1 :before ((cache standard-cache)
                                        (rucksack rw-transaction-rucksack)
                                        &key read-only &allow-other-keys)
  (if read-only
      (lock-reader (rucksack-transaction-lock rucksack))
      (lock-writer (rucksack-transaction-lock rucksack))))

(defmethod transaction-commit-1 :after ((transaction standard-transaction)
                                        (cache standard-cache)
                                        (rucksack rw-transaction-rucksack))
  (unlock (rucksack-transaction-lock rucksack)))

(defmethod transaction-rollback-1 :after ((transaction standard-transaction)
                                          (cache standard-cache)
                                          (rucksack rw-transaction-rucksack))
  (unlock (rucksack-transaction-lock rucksack)))
