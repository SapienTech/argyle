(ns (argyle data set))
(use (argyle base)
     (argyle data))

(trans set (t)
   :init (%mke-set t)
   :app (fn (v) (hash-set! (set-t self) v v) self))

(defp set args
  (ret set (%mke-set (make-hash-table))
    (for-each (\\ set _) args)))

(defp has? (set v)
  (hash-ref (set-t set) v))

(defp elements (set)
  (hash-map->list (fn (k v) v) (set-t set)))
