(module (arguile data)
  #:export (data data-type? data?))

(use (arguile core)
     (arguile ssyntax)
     (arguile generic)
     (arguile sugar)
     (arguile data records))

(mac data x
  ((_ name (fields ...) (methods ...) _proc ...)
   (with (proc? (not (null? (syn->dat #'(_proc ...))))
          name' (syn->dat #'name))
     (w/syn (pred (dat->syn x (+ name' '?))
             const (dat->syn x (+ 'make- name'))
             %const (dat->syn x (+ '%make- name'))
             proc-get (dat->syn x (+ name' '-fn))
             proc-set! (dat->syn x (+ name' '-fn!))
             self (dat->syn x 'self))
       #`(do (define-record-type name
               (#,(if proc? #'%const #'const) fields ...)
               pred (proc proc-get proc-set!) methods ...)
           #,(when proc?
               #'(def const args
                   (ret self
                     (apply %const args)
                     (proc-set! self _proc ...)))))))))

(def data-type? record-type?)
(def data? record?)
