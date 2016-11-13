(module (arguile data table)
  #:export (make-table))
(use (arguile ssyntax)
             (arguile core)
             (arguile data))

;;; TODO: allow init size and comparison operators
(data table () ()
  (let t (make-hash-table)
    (case-lambda
      (() t)
      ((k) (hash-ref t k))
      ((k v) (hash-set! t k v)))))
