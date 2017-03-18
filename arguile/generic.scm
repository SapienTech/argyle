(module (arguile generic)
  :replace (map)
  :export (gen gen-fn? xtnd type
                len rev join cpy clr!))
(use (arguile base)
     (arguile data)
     (arguile data tbl)
     (arguile data vec)
     (arguile data q)
     (arguile guile)
     (arguile loop)
     (srfi srfi-1))

(mac gen
  ((name) (id? #'name)
   #`(def name (%gen-fn 'name (ret t (mke-tbl)
                                #,(when (defd? (dat #'name))
                                    #'(t 'def name)))))))

(trans gen-fn (name tbl)
      :init (%gen-fn name tbl)
      :app (fn args
              (apply (resolve-fn (gen-fn-tbl self) args)
                     args)))

;;; This version works, but needs cleanup
(def resolve-fn (tbl args)
  (loop lp ((for arg (in-list args))
            (where t tbl (and=> t (\\ _ (type arg)))))
        => (cond (t (t 'fun) (t 'fun))
                 (t (t 'rst) (t 'rst)) 
                 ((tbl 'def) (tbl 'def))
                 (else (err "No generic fn for args1:" args)))
    ;; This handles . rest case
    (if t
        (aif (t 'rst) it (lp))
        (aif (tbl 'def) it 
            (err "No generic fn for args:" args)))))

(def type (x)
  (if (data? x) (data-type x)
      (base-type x)))

;;; Going to straight cpy for this version
(mac xtnd x
  (def split (lst)
    (c/vals (fn () (unzip2 (grp lst 2))) list))
  ((name (arg1 ... . rest) body ...) (~(nil? #'rest))
   (let-syn (args types) (split #'(arg1 ...))
     #`(loop ((for type  (in-list 'types))
              (where tbl (gen-fn-tbl name)
                (if (tbl type) (tbl type)
                     (tbl type (mke-tbl)))))
        => (tbl 'rst (fn (#,@#'args . rest) body ...)))))
  ((name (arg1 ...) body ...) (defd? (dat #'name))
   (let-syn (args types) (split #'(arg1 ...))
            ;; TODO: refactor
     #`(loop ((for type  (in-list 'types))
              (where tbl (gen-fn-tbl name)
                (if (tbl type) (tbl type)
                     (tbl type (mke-tbl)))))
        => (tbl 'fun (fn args body ...))))))

(eval-when (expand load eval)
  (def len length)
  (def rev reverse)
  (def join append)
  (def cpy lst-cpy)
  (def clr! (lst) (set-cdr! lst '()))
  (def map (@ (ice-9 r5rs) map)))

(gen len)
(gen rev)
(gen join)
(gen cpy)
(gen clr!)
(gen map)

(gen car)
(gen cdr)
(gen take)
(gen drop)
(gen nth)

(xtnd len (s <str>) (str-len s))
(xtnd len (n <int>) (len (str n)))
(xtnd len (t <tbl>) (tbl-cnt (const #t) t))
(xtnd len (v <vec>) (vec-len v))
(xtnd len (q <q>) (q-len q))
(xtnd len (stream <strm>) (strm-len stream))

;;; TODO: doesnt work
(xtnd rev (v <vec>) (ret v* (mke-vec (vec-len v))
                      (vec<-! v 0 (vec-len v) v* 0)))

(xtnd join (s1 <str> . rest) (apply str-join s1 rest))
(xtnd join (v1 <vec> v2 <vec>) (w/ (l1 (vec-len v1) l2 (vec-len v2))
                             (ret v (mke-vec (+ l1 l2))
                               (vec->! v1 0 l1 v 0)
                               (vec->! v2 0 l2 v l1))))
(xtnd cpy (v <vec>) (vec-cpy v))
(xtnd cpy (q <q>) (%mke-q (q-len q) (q-hd q) (q-tl q)))

(xtnd clr! (t <tbl>) (tbl-clr! t))
(xtnd clr! (q <q>) (q-hd! q '()) (q-tl! q '()) (q-len! q 0))
(xtnd map (f <fn> v <vec> . rst) (apply vec-map f v rst))
(xtnd map (f <fn> s <str> . rst) (apply str-map f s rst))
(xtnd map (f <fn> t <tbl>)       (tbl-map->lst f t))

(xtnd car (seq <strm>) (scar seq))
(xtnd cdr (seq <strm>) (scdr seq))
(xtnd take (seq <strm> k <int>) (strm-take k seq))
(xtnd drop (seq <strm> k <int>) (strm-drop k seq))
(xtnd kth (seq <lst> k <int>) (list-ref seq n))
