(module (arguile generic)
    #:export (generic extend))
(use (arguile base)
     (arguile data)
     (arguile guile)
     (arguile loop)
     (srfi srfi-1))

(data gen-fn (name tbl)
      #:init (%gen-fn name tbl)
      #:app (fn args
              (apply (resolve-fn (gen-fn-tbl self) args)
                     args)))

(mac generic
  ((_ name) (id? #'name)
   #'(def name (%gen-fn 'name (make-tbl)))))

(mac extend
  ((_ name (arg1 ...) body ...)
   (defd? (-> dat #'name))
   (w/syn ((args types) (split #'(arg1 ...)))
     #'(loop ((for type (in-list 'types))
              (where tbl (gen-fn-tbl name)
                (let tbl* (tbl type)
                  (if tbl* tbl*
                      (do (tbl type (make-tbl))
                          (tbl type))))))
         => (tbl 'fn (fn args body ...))))))

(def resolve-fn (tbl args)
  (loop ((for arg (in-list args))
         (where t tbl (and=> t (\\ _ (type arg)))))
    => (if t (t 'fn) (err "No generic function for args:" args))))

(eval-when (expand load eval)
  (def split (lst)
    (call-with-values (fn () (unzip2 (grp lst 2)))
      list)))
