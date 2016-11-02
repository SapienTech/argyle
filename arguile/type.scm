(define-module (arguile type)
  #:export (newtype type coerce))
(use-modules (arguile ssyntax)
             (arguile core)
             (arguile guile)
             (arguile error)
             (arguile sugar)
             (srfi srfi-9))

(mac newtype
     ;;TODO: provide default field accessors
  ((newtype name ctor pred fields ...)
   #'(define-record-type name ctor pred fields ...)))

(def type (x)
 (cond
  ((null? x)          'sym)
  ((string? x)        'str)
  ((number? x)        'num)
  ((procedure? x)     'fn)
  ((pair? x)          'pair)
  ((symbol? x)        'sym)
  ((hash-table? x)    'table)
  ((char? x)          'chr)
  ((vector? x)        'vec)
  ((keyword? x)       'kword)
  (#t                 (error "Type: unknown type" x))))

(def iround (compose inexact->exact round))

(def coercions
     ;;TODO: Allow extension
  (ret coercions
    (make-hash-table)
    (for-each
     (fn (e)
       (with (target-type (car e)
              conversions (make-hash-table))
         (hash-set! coercions target-type conversions)
         (for-each
          (fn (x) (hash-set! conversions (car x) (cadr x)))
          (cdr e))))
     `(
       ;; TODO: rectify fn coercion w/ applicable-structs
       (fn (str ,(fn (s) (fn (i) (string-ref s i))))
           (table  ,(fn (h)
                      (case-lambda
                        ((k) (hash-ref h k))
                        ((k v) (hash-set! h k v)))))
           (vec ,(fn (v) (fn (i) (vector-ref v i)))))
       
       (str (int ,number->string)
            (num ,number->string)
            (chr ,string)
            (sym ,(fn (x) (if (eqv? x 'nil) "" (symbol->string x)))))
       
       (sym (str ,string->symbol)
            (chr ,(fn (c) (string->symbol (string c)))))
       
       (int (chr ,(fn (c . args) (char->integer c)))
            (num ,(fn (x . args) (iround x)))
            (str ,(fn (x . args)
                    (let n (apply string->number '(x args))
                      (if n (iround n)
                          (error "Can't coerce " x 'int))))))
       
       (num (str ,(fn (x . args)
                    (or (apply string->number '(x args))
                        (error "Can't coerce " x 'num))))
            (int ,(fn (x) x)))
       
       (chr (int ,integer->char)
            (num ,(fn (x) (integer->char
                           (iround x)))))))
    coercions))

(def coerce (x to-type . args)
  (let x-type (type x)
    (if (eqv? to-type x-type) x
        (with (fail (fn () (error "Can't coerce " x to-type))
               conversions (hash-ref coercions to-type fail)
               converter (hash-ref conversions x-type fail))
          (apply converter (cons x args))))))
