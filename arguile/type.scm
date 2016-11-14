(module (arguile type)
  #:export (type coerce ->))
(use (arguile ssyntax)
     (arguile core)
     (arguile data table)
     (arguile guile)
     (arguile error)
     (arguile sugar)
     (srfi srfi-9))

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
  (else               (error "Type: unknown type" x))))

(def coerce (x to-type . args)
  (let x-type (type x)
    (if (eqv? to-type x-type) x
        (with (fail (fn () (error "Can't coerce " x to-type))
               conversions (hash-ref (coercions) to-type fail)
               converter (hash-ref (conversions) x-type fail))
          (apply converter (cons x args))))))

(mac -> ((_ type obj args ...) #'(coerce obj 'type args ...)))

(def coercions
    ;;TODO: Allow extension
  (ret coercions (make-table)
    (for-each
     (fn (e)
       (with (target-type (car e)
              conversions (make-table))
         (coercions target-type conversions)
         (for-each
          (fn (x) (conversions (car x) (cadr x)))
          (cdr e))))
     `(
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

(def iround (compose inexact->exact round))
