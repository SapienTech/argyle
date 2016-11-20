(module (arguile generic)
    #:export (generic gen-fn? xtnd))
(use (arguile base)
     (arguile data)
     (arguile guile)
     (arguile loop)
     (srfi srfi-1))

(mac generic
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
