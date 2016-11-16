(module (arguile generic)
  #:export (generic extend
            + * len join rev))
(use (oop goops)
     (arguile ssyntax)
     (arguile core)
     (arguile type)
     (arguile data str)
     (arguile guile)
     (arguile sugar))

(mac generic
  ((_ name) #'(define-generic name)))

;;; TODO: allow multiple declarations
(mac extend
  ((_ ((setter name) . args) body ...)
   #'(define-method ((setter name) . args) body ...))
  ((_ name (args ...) body ...)
   #'(define-method (name args ...) body ...)))

(def + args
  (cond ((null? args) 0)
        ((one-of `(,string? ,char?) (car args))
         (apply str-join
                (map (\\ coerce _ 'str) args)))
        ((symbol? (car args))
         (apply symbol-append
                (map (\\ coerce _ 'sym) args)))
        (else (apply _+ args))))

;;; Add cartesian product for data
(def * args
  (cond ((null? args) 0)
        ((one-of `(,string? ,char?) (car args))
         (apply str-join
                (map (fn (val)
                         (coerce (car args) 'str))
                     (iota (apply _* (cdr args))))))
        ((symbol? (car args))
         (apply symbol-append
                (map (fn (val)
                         (coerce (car args) 'sym))
                     (iota (apply _* (cdr args))))))
        (else (apply _* args))))

(def one-of (tests val)
  (if (null? tests) #f
      (or ((car tests) val)
          (one-of (cdr tests) val))))

(def len (x)
  (cond ((string? x) (string-length x))
        ((hash-table? x) (hash-count (const #t) x))
        ((vector? x) (vector-length x))
        (else (length x))))

(generic join)
(extend join ((e1 <list>) (e2 <list>))
  (append e1 e2))

(generic rev)
(extend rev ((orderable <list>))
  (reverse orderable))
