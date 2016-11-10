(define-module (arguile data vec)
  #:export (make-vec))
(use-modules (arguile ssyntax)
             (arguile core)
             (arguile data))

(data vec (len fill) ((len vec-len) (fill vec-fill))
      (let v (make-vector (vec-len self) (vec-fill self))
        (case-lambda
          (() v)
          ((k) (vector-ref v k))
          ((k obj) (vector-set! v k obj)))))
