;;; TODO:: merge data-match with match
(define-module (arguile data)
  #:export (<data> data? data))
(use-modules (arguile core)
             (arguile ssyntax)
             (arguile generic)
             (arguile sugar)
             (srfi srfi-9))

(def <data>
  ;function: 0, record: 1
  (make-struct <applicable-struct-vtable> 0 'pwpw))
(set-struct-vtable-name! <data> '<data>)

(def data? (obj)
  (or (record? obj)
      (eq? (struct-vtable obj) <data>)))

(mac data x
  ((_ name (fields ...) (methods ...) proc? ...)
   (with (proc? (not (null? (syn->dat #'(proc? ...))))
          name' (syn->dat #'name))
     (w/syn (pred (dat->syn x (+ name' '?))
                  const (dat->syn x (+ 'make- (syn->dat #'name)))
                  %const (dat->syn x (+ '%make- name')))
       #`(do (define-record-type name
               (#,(if proc? #'%const #'const) fields ...)
               pred methods ...)
           #,(when proc?
               #'(def const (proc)
                   (make-struct <data> 0 proc))))))))
