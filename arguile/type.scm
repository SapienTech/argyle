(module (arguile type)
  #:export (type coerce ->))
(use (arguile ssyntax)
     (arguile core)
     (arguile data str)
     (arguile guile)
     (arguile error)
     (arguile sugar))

(def type (x)
 (cond
  ((null? x)          'sym)
  ((str? x)           'str)
  ((number? x)        'num)
  ((procedure? x)     'fn)
  ((symbol? x)        'sym)
  ((syn? x)           'syn)
  ((hash-table? x)    'table)
  ((char? x)          'chr)
  ((vector? x)        'vec)
  ((keyword? x)       'kword)
  ((pair? x)          'pair)
  (else               (error "Type: unknown type" x))))

(def coerce (x to-type . args)
  (let x-type (type x)
    (if (eqv? to-type x-type) x
        (with (fail (fn () (error "Can't coerce " x to-type))
               conversions (hash-ref coercions to-type fail)
               converter (hash-ref conversions x-type fail))
          (apply converter (cons x args))))))

(mac -> ((_ type obj args ...) #'(coerce obj 'type args ...)))

(def coercions
    ;;TODO: Allow extension
  (ret coercions (make-hash-table)
    (for-each
     (fn (e)
       (with (target-type (car e)
              conversions (make-hash-table))
         (hash-ref coercions target-type conversions)
         (for-each
          (fn (x) (hash-ref conversions (car x) (cadr x)))
          (cdr e))))
     `(
       (dat (syn ,syntax->datum))
       (str (int ,number->string)
            (num ,number->string)
            (chr ,str)
            (sym ,(fn (x) (if (eqv? x 'nil) "" (symbol->string x)))))
       
       (sym (str ,str->sym)
            (chr ,(fn (c) (str->sym (str c)))))
       
       (int (chr ,(fn (c . args) (char->integer c)))
            (num ,(fn (x . args) (iround x)))
            (str ,(fn (x . args)
                    (let n (apply str->num '(x args))
                      (if n (iround n)
                          (error "Can't coerce " x 'int))))))
       
       (num (str ,(fn (x . args)
                    (or (apply str->num '(x args))
                        (error "Can't coerce " x 'num))))
            (int ,(fn (x) x)))
       
       (chr (int ,integer->char)
            (num ,(fn (x) (integer->char
                           (iround x)))))))
    coercions))

(def iround (compose inexact->exact round))
