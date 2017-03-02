(module (arguile data tbl)
  #:export (<tbl> tbl tbl? tbl-t tbl-t! tbl-fn tbl-fn!))
(use (arguile base)
     ((arguile guile) :select (grp))
     (arguile data))

;;; TODO: allow init size and comparison operators
(trans tbl (t)
      :init (%mke-tbl t)
      :app (fn-case
             (() (tbl-t self))
             ((k) (hash-ref (tbl-t self) k))
             ((k v) (hash-set! (tbl-t self) k v))))

;;; Consider using w/tbl as dflt ctor
(defp mke-tbl (#:o (n 0))
  (%mke-tbl (make-hash-table n)))
(defp tbl args
  (ret tbl (mke-tbl)
       (for-each (\\ apply tbl _) (grp args 2))))
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
(defp tbl-fold (f init t) (hash-fold fun init (t)))
(defp tbl-each (f t) (hash-for-each fun (t)))
(defp tbl-map->lst (f t) (hash-map->list f (t)))
