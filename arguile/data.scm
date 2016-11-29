(module (arguile data)
    #:export (data trans data? data-type? data-type))
(use (arguile base)
     (arguile data records)
     (arguile data immutable)
     ((srfi srfi-1) #:select (first)))

;;; TODO: add #:init to field specs
;;;       order fields in printer
(mac data x
  ((_ name (field ...)
      #:init (mke arg ...)
      spec ...
      #:app fn)
   (%data x #t #'(_ name (field ...) #:init (mke arg ...) spec ... #:app fn)))
  ((_ name (field ...) spec ... #:app fn)
   (%data x #t #`(_ name (field ...) #:init (#,(std-mke #'name x) field ...)
                   spec ... #:app fn)))
  ((_ name (field ...) #:init (mke arg ...) spec ...)
   (%data x #t #`(_ name (field ...) #:init (mke arg ...) spec ... #:app (not-app 'name))))
  ((_ name (field ...) spec ...)
   (%data x #t #`(_ name (field ...) #:init (#,(std-mke #'name x) field ...)
                  spec ... #:app (not-app 'name)))))

;;; TODO: copying for now
(mac trans x
  ((_ name (field ...)
      #:init (mke arg ...)
      spec ...
      #:app fn)
   (%data x #f #'(_ name (field ...) #:init (mke arg ...) spec ... #:app fn)))
  ((_ name (field ...) spec ... #:app fn)
   (%data x #f #`(_ name (field ...) #:init (#,(std-mke #'name x) field ...)
                   spec ... #:app fn)))
  ((_ name (field ...) #:init (mke arg ...) spec ...)
   (%data x #f #`(_ name (field ...) #:init (mke arg ...) spec ... #:app (not-app 'name))))
  ((_ name (field ...) spec ...)
   (%data x #f #`(_ name (field ...) #:init (#,(std-mke #'name x) field ...)
                  spec ... #:app (not-app 'name)))))

(def data-type? record-type?)
(def data? record?)
(def data-type (obj)
  (struct-ref (struct-vtable obj) vtable-offset-user))

(eval-when (expand load eval)

  (def %data (ctx imm? syn-exp)
    (syn-case syn-exp ()
     ((_ name (field ...) #:init (mke arg ...) spec ... #:app fn)
      (let name' (-> dat #'name)
        (w/syn (%mke (-> syn (+ '% (-> dat #'mke)) ctx)
                pred (-> syn (+ name' '?) ctx)
                (app app: app!) (mke-app-spec name' ctx)
                self (-> syn 'self ctx))
          #`(do (#,(if imm? #'define-immutable-record-type
                       #'define-record-type)
                 name
                 (%mke arg ...) pred
                 (app app: app!)
                 #,@(mke-field-specs name' #'(field ...) #'(spec ...) ctx)
                 spec ...)
           (def mke args 
             (#,(if imm? #'let #'ret)
              self (apply %mke args) (app! self fn)))))))))

  (def std-mke (name ctx)
    (-> syn (+ 'mke- (-> dat name)) ctx))

  (def mke-app-spec (name ctx)
    (-> syn (mke-field-spec name 'fn) ctx))

  (def mke-field-spec (name field)
    `(,field ,@(map (\\ + name '- field _)
                    `(,(symbol) !))))

  (def mke-field-specs (name fields specs ctx)
    (-> syn (map (\\ mke-field-spec name _)
                 (set\ eq? (-> dat fields)
                          (map first (-> dat specs)))) ctx))
  (def not-app (name)
    (fn args (err "Wrong type to apply:" name
                  "data-type not applicable"))))

(use (arguile data tbl)
     (arguile data vec)
     (arguile data q))

(re-export-modules (arguile data tbl)
                   (arguile data vec)
                   (arguile data q))
