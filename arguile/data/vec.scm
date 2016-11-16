(module (arguile data vec)
  #:export (make-vec vec-fn vec-fn!
            vec? vec-len))
(use (arguile base)
     (arguile data))

;;; TODO: add optional fill
(data vec (len) ((len vec-len))
  (let v (make-vector (vec-len self))
    (fn-case
     (() v)
     ((k) (vector-ref v k))
     ((k obj) (vector-set! v k obj)))))

(defp vec make-vec)
(defp vec: (v k) (v k))
(defp vec! (v k obj) (v k obj))
(defp vec->lst (v) (vector->list (v)))
(defp vec-cpy (v) (vector-copy (v)))
(defp vec-fill! (v fill) (vector-fill! (v) fill))
(defp vec<-! (v1 s1 e1 v2 s2) (vector-move-left! (v1) s1 e1 (v2) s2))
(defp vec->! (v1 s1 e1 v2 s2) (vector-move-right! (v1) s1 e1 (v2) s2))
