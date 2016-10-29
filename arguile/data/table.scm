(define-module (arguile data table)
  #:use-module (arguile)
  #:use-module (oop goops)
  #:export (make-table))
                                        ;TODO: put these in class module
(mac class
  ((class name supers slot ...) (define-class name supers slot ...)))

(mac inst
  ((inst name class initargs ...) (def name (make class initargs ...))))

(class table (<applicable-struct>)
  (procedure #:init-value (fn (o) (display o)))
  (%table #:init-value (make-hash-table)))
                                        ;TODO: Add optional size arg
(def make-table ()
 (inst tble table)
 (slot-set! tble 'procedure
            (let t (slot-ref tble '%table)
              (case-lambda
                ((k) (hash-ref t k))
                ((k v) (hash-set! t k v)))))
 tble)
