(module (arguile generic)
    #:export (gen gen-fn? xtnd type
              len rev join cpy clr!))
(use (arguile base)
     (arguile data)
     (arguile guile)
     (arguile loop)
     (srfi srfi-1))

(mac gen
  ((_ name) (id? #'name)
   #`(def name (%gen-fn 'name (ret t (make-tbl)
                                #,(when (defd? (-> dat #'name))
                                  #'(t 'fn name)))))))

(data gen-fn (name tbl)
      #:init (%gen-fn name tbl)
      #:app (fn args
              (apply (resolve-fn (gen-fn-tbl self) args)
                     args)))

(def resolve-fn (tbl args)
  (loop ((for arg (in-list args))
         (where t tbl (and=> t (\\ _ (type arg)))))
    => (if (& t (t 'fn)) (t 'fn)
           (aif (tbl 'fn) it
                (err "No generic fn for args:" args)))))

(def type (x)
  (if (data? x) (data-type x)
      (base-type x)))

(mac xtnd
  ((_ name (arg1 ...) body ...) (defd? (-> dat #'name))
   (let-syn (args types) (split #'(arg1 ...))
     #'(loop ((for type  (in-list 'types))
              (where tbl (gen-fn-tbl name)
                (aif (tbl type) it
                     (tbl type (make-tbl)))))
         => (tbl 'fn (fn args body ...))))))

(eval-when (expand load eval)
  (def split (lst)
    (call-with-values (fn () (unzip2 (grp lst 2)))
      list)))

(gen len)
(gen rev)
(gen join)
(gen cpy)
(gen clr!)

(xtnd len (t tbl) (tbl-cnt (const #t) t))
(xtnd len (v vec) (vec-len v))
(xtnd len (q q) (q-len q))

(xtnd rev (l lst) (reverse l))
(xtnd rev (v vec) (ret v* (make-vec (vec-len v))
                   (vec<-! v 0 (vec-len v) v* 0)))
(xtnd join (l1 lst l2 lst) (append l1 l2))
(xtnd join (s1 str s2 str) (str-join s1 s2))
(xtnd join (v1 vec v2 vec) (w/ (l1 (vec-len v1) l2 (vec-len v2))
                             (ret v (make-vec (+ l1 l2))
                               (vec->! v1 0 l1 v 0)
                               (vec->! v2 0 l2 v l1))))
(xtnd cpy (l lst) (lst-cpy l))
(xtnd cpy (v vec) (vec-cpy v))
(xtnd cpy (q q) (%make-q (q-len q) (q-hd q) (q-tl q)))

(xtnd clr! (t tbl) (tbl-clr! t))
(xtnd clr! (q q) (q-hd! q '()) (q-tl! q '()) (q-len! q 0))
