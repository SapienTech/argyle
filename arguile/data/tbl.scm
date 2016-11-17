(module (arguile data tbl)
  #:export (tbl make-tbl tbl-fn tbl-fn! tbl?))
(use (arguile base)
     (arguile data))

;;; TODO: allow init size and comparison operators
(data tbl () ()
  (let t (make-hash-table)
    (fn-case
      (() t)
      ((k) (hash-ref t k))
      ((k v) (hash-set! t k v)))))

;;; TODO: figure out opt-args
(defp tbl: (t k) (t k))
(defp tbl! (t k obj) (t k obj))
(defp tbl-del! (t k) (hash-remove! (t) k))
(defp tblq: (t k) (hashq-ref (t) k))
(defp tblq! (t k obj) (hashq-set! (t) k obj))
(defp tblq-del! (t k) (hashq-remove! (t) k))
(defp tblv: (t k) (hashv-ref (t) k))
(defp tblv! (t k obj) (hashv-set! (t) k obj))
(defp tblv-del! (t k) (hashv-remove! (t) k))
(defp tblx: (hash assoc t k) (hashx-ref hash assoc (t) k))
(defp tblx! (hash assoc t k obj) (hashx-set! hash assoc (t) k obj))
(defp tblx-del! (hash assoc t k) (hashx-remove! hash assoc (t) k))

(defp tbl-cnt (pred t) (hash-count pred (t)))
(defp tbl-clr! (t) (hash-clear! (t)))
(defp tbl-fold (fun init t) (hash-fold fun init (t)))
(defp tbl-each (fun t) (hash-for-each fun (t)))
(defp tbl-map->lst (fun t) (hash-map->list fun (t)))

