(define-module (arguile data table)
  #:export (make-table))
(use-modules (arguile ssyntax)
             (arguile data))

(data table () ()
      (let t (make-hash-table)
        (case-lambda
          ((k) (hash-ref t k))
          ((k v) (hash-set! t k v)))))
