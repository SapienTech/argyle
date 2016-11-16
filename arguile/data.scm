(module (arguile data)
  #:export (data data-type? data?))

(use (arguile base)
     (arguile type)
     (arguile generic)
     (arguile data records))

(mac data x
  ((_ name (fields ...) (methods ...) fun ...)
   (with (app? (~(nil? (-> dat #'(fun ...))))
          name' (-> dat #'name))
     (w/syn (pred (-> syn (+ name' '?) x)
             const (-> syn (+ 'make- name') x)
             %const (-> syn (+ '%make- name') x)
             %fun (-> syn (+ name' '-fn) x)
             fun: (-> syn (+ name' '-fn:) x)
             fun! (-> syn (+ name' '-fn!) x)
             self (-> syn 'self x))
       #`(do (define-record-type name
               (#,(if app? #'%const #'const) fields ...)
               pred (%fun fun: fun!) methods ...)
           #,(when app?
               #'(def const args
                   (ret self
                     (apply %const args)
                     (fun! self fun ...)))))))))

(def data-type? record-type?)
(def data? record?)
