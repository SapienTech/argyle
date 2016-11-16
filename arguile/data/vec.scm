(module (arguile data vec)
  #:export (make-vec vec-fn vec-fn!
            vec? vec: vec! vec-len))
(use (arguile base)
     (arguile data))

;;; TODO: add optional fill
(data vec (len) ((len vec-len))
  (let v (make-vector (vec-len self))
    (fn-case
     (() v)
     ((k) (vector-ref v k))
     ((k obj) (vector-set! v k obj)))))

;;; : and ! may look to similar to eachother
(def vec: (v k) (v k))
(def vec! (v k obj) (v k obj))
