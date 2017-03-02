(module (arguile data vec)
  #:export (vec vec? vec-fn vec-fn!))
(use (arguile base)
     (arguile data)
     (srfi srfi-43))

;;; TODO: add optional fill
(trans vec (v)
      :init (%mke-vec v)
      :app (let v (vec-v self)
              (fn-case
               (() v)
               ((k) (vector-ref v k))
               ((k obj) (vector-set! v k obj)))))

(defp mke-vec (len #:o fill)
  (%mke-vec (make-vector len fill)))
(defp vec args (lst->vec args))
(defp vec: (v k) (v k))
(defp vec! (v k obj) (v k obj))
(defp vec-len (v) (vector-length (v)))
(defp vec->lst (v) (vector->list (v)))
(defp lst->vec (lst) (%mke-vec (list->vector lst)))
(defp vec-cpy (v) (%mke-vec (vector-copy (v))))
(defp vec-fill! (v fill) (vector-fill! (v) fill))
(defp vec<-! (v1 s1 e1 v2 s2) (vector-move-left! (v1) s1 e1 (v2) s2))
(defp vec->! (v1 s1 e1 v2 s2) (vector-move-right! (v1) s1 e1 (v2) s2))
(defp vec-map (fun v . vs) (apply vector-map fun (v) (map (fn (v) (v)) vs)))
;;; Etc...
