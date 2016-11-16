(module (arguile data table)
  #:export (make-table table-fn table-fn! table? table: table!))
(use (arguile base)
     (arguile data))

;;; TODO: allow init size and comparison operators
(data table () ()
  (let t (make-hash-table)
    (fn-case
      (() t)
      ((k) (hash-ref t k))
      ((k v) (hash-set! t k v)))))

(def table: (t k) (t k))
(def table! (t k obj) (t k obj))
