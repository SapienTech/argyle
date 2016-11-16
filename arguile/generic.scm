(module (arguile generic)
  #:export (generic extend
            + * len join rev))
(use (oop goops)
     (arguile base)
     (arguile guile)
     (arguile type)) 

(mac generic
  ((_ name) #'(define-generic name)))

;;; TODO: allow multiple declarations
(mac extend
  ((_ ((setr name) . args) body ...)
   #'(define-method ((setr name) . args) body ...))
  ((_ name (args ...) body ...)
   #'(define-method (name args ...) body ...)))

(generic join)
(extend join ((e1 <list>) (e2 <list>))
  (append e1 e2))

(generic rev)
(extend rev ((orderable <list>))
  (reverse orderable))

(def + args
  (cond ((null? args) 0)
        ((one-of `(,str? ,chr?) (car args))
         (apply str-join
                (map (\\ coerce _ 'str) args)))
        ((sym? (car args))
         (apply sym-join
                (map (\\ coerce _ 'sym) args)))
        (else (apply _+ args))))

;;; Add cartesian product for data
(def * args
  (cond ((null? args) 0)
        ((one-of `(,str? ,chr?) (car args))
         (apply str-join
                (map (fn (val)
                         (coerce (car args) 'str))
                     (iota (apply _* (cdr args))))))
        ((sym? (car args))
         (apply sym-join
                (map (fn (val)
                         (coerce (car args) 'sym))
                     (iota (apply _* (cdr args))))))
        (else (apply _* args))))

(def len (x)
  (cond ((str? x) (str-len x))
        ((hash-table? x) (hash-count (const #t) x))
        ((vector? x) (vector-length x))
        (else (length x))))


(def one-of (tests val)
  (if (null? tests) #f
      (or ((car tests) val)
          (one-of (cdr tests) val))))
