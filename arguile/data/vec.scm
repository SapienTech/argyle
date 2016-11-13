(define-module (arguile data vec)
  #:export (make-vec))
(use-modules (arguile ssyntax)
             (arguile core)
             (arguile data))

;;; TODO: add optional fill
(data vec (len) ((len vec-len))
      (let v (make-vector (vec-len self))
        (case-lambda
          (() v)
          ((k) (vector-ref v k))
          ((k obj) (vector-set! v k obj)))))
