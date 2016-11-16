(module (arguile data vec)
  #:export (make-vec vec-fn vec-fn!
            vec? vec-len))
(use (arguile base)
     (arguile data)
     (arguile generic))

;;; TODO: add optional fill
(data vec (len) ((len vec-len))
  (let v (make-vector (vec-len self))
    (fn-case
     (() v)
     ((k) (vector-ref v k))
     ((k obj) (vector-set! v k obj)))))

(defp vec: (v k) (v k))
(defp vec! (v k obj) (v k obj))
(defp vec->lst (v) (vector->list (v)))
;;; TODO: this is a bit verbose, but will change when
;;; we can have op args
(defp lst->vec (lst) (ret v (make-vec (len lst))
                          (vec-obj! v (list->vector lst))))
(defp vec-cpy (v) (ret v* (make-vec (vec-len v))
                       (vec-obj! v* (vector-copy (v)))))
(defp vec-fill! (v fill) (vector-fill! (v) fill))
(defp vec<-! (v1 s1 e1 v2 s2) (vector-move-left! (v1) s1 e1 (v2) s2))
(defp vec->! (v1 s1 e1 v2 s2) (vector-move-right! (v1) s1 e1 (v2) s2))

;;; Temporary helper
(def vec-obj! (v obj)
  (vec-fn! v
    (let v* obj
      (fn-case
       (() v*)
       ((k) (vector-ref v* k))
       ((k obj) (vector-set! v* k obj))))))
