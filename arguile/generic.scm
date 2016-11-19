(module (arguile generic)
    #:export (generic extend))
(use (arguile base)
     (arguile loop)
     (arguile type)
     (arguile data)
     (arguile error)
     (arguile io)
     (arguile data tbl))

(data gen-fn (name tbl)
      #:init (%gen-fn name tbl)
      #:app (fn args
              (apply (resolve-fn (gen-fn-tbl self) args)
                     args)))

(mac generic
  ((_ name) (id? #'name)
   #'(def name (%gen-fn 'name (make-tbl)))))

;;; Will overwrite defs w/ same args
;;; args must be values, not variables
;;; fn must be a full proc
(mac extend
  ((_ name (arg1 ...) fn)
   (defd? (-> dat #'name))
   #'(loop ((for arg (in-list '(arg1 ...)))
            (where tbl (gen-fn-tbl name)
              (with (arg-t (type arg) t (tbl arg-t))
                (if t t (do (tbl arg-t (make-tbl))
                          (tbl arg-t))))))
        => (tbl 'fun fn))))

(def resolve-fn (tbl args)
  (loop ((for arg (in-list args))
         (where t tbl (and=> t (\\ _ (type arg)))))
        => (if t (t 'fun) (err "No generic function for args:" args))))
