(module (arguile data)
    #:export (data trans data? data-type? data-type))
(use (arguile base)
     (arguile data records)
     (arguile data immutable)
     ((srfi srfi-1) #:select (first)))

(mac data (patt #'(%data #t patt)))
(mac trans (patt #'(%data #f patt)))

(mac %data x
  ((imm? (name (field ...)
              #:init (mke arg ...)
              spec ...
              #:app fn))
   #`(%%data imm? (name (field ...) #:init (mke arg ...) spec ... #:app fn)))
  ((imm? (name (field ...) spec ... #:app fn))
   #`(%data imm? (name (field ...) #:init (#,(std-mke #'name) field ...)
                       spec ... #:app fn)))
  ((imm? (name (field ...) #:init (mke arg ...) spec ...))
   #`(%data imm? (name (field ...) #:init (mke arg ...) spec ... #:app (not-app 'name))))
  ((imm? (name (field ...) spec ...))
   #`(%data imm? (name (field ...) #:init (#,(std-mke #'name) field ...)
                       spec ... #:app (not-app 'name)))))

(mac %%data
  ((imm? (name (field ...)
               #:init (mke arg ...) spec ...
               #:app fn))
   (let name' (dat #'name)
     (w/syn (type (syn (+ '< name' '>) #'name)
             %mke (syn (+ '% (dat #'mke)) #'name)
             pred (syn (+ name' '?) #'name)
             (app app: app!) (mke-app-spec name' #'name)
             self (syn 'self #'name))
       #`(do (#,(if (dat #'imm?) #'define-immutable-record-type
                  #'define-record-type)
              type
              (%mke arg ...) pred
              (app app: app!)
              #,@(mke-field-specs name' #'(field ...) #'(spec ...) #'name)
              spec ...)
             (def mke args 
               (#,(if (dat #'imm?) #'let #'ret)
                self (apply %mke args) (app! self fn))))))))

(def data-type? record-type?)
(def data? record?)
(def data-type (obj)
  (struct-ref (struct-vtable obj) vtable-offset-user))

(eval-when (expand load eval)

 (def std-mke (name) (syn (dat name) name))
 
 (def mke-app-spec (name ctx)
   (syn (mke-field-spec name 'fn) ctx))
 
 (def mke-field-spec (name field)
   `(,field ,@(map (\\ + name '- field _)
                   `(,(symbol) !))))
 
 (def mke-field-specs (name fields specs ctx)
   (syn (map (\\ mke-field-spec name _)
             (set\ eq? (-> dat fields)
                       (map first (-> dat specs)))) ctx))
 (def not-app (name)
   (fn args (err "Wrong type to apply:" name
                 "data-type not applicable"))))
