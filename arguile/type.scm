(module (arguile type)
  #:export (type coerce ->))
(use (arguile base))

(def type (x)
 (cond
  ((null? x)          'sym)
  ((str? x)           'str)
  ((num? x)           'num)
  ((fn? x)            'fn)
  ((sym? x)           'sym)
  ((syn? x)           'syn)
  ((hash-table? x)    'table)
  ((chr? x)           'chr)
  ((vector? x)        'vec)
  ((kwrd? x)          'kwrd)
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
     `((dat (syn ,syn->dat))
       (str (int ,num->str)
            (num ,num->str)
            (chr ,str)
            (sym ,(fn (x) (if (eqv? x 'nil) "" (sym->str x)))))
       
       (sym (str ,str->sym)
            (chr ,(fn (c) (str->sym (str c)))))
       
       (int (chr ,(fn (c . args) (chr->int c)))
            (num ,(fn (x . args) (iround x)))
            (str ,(fn (x . args)
                    (let n (apply str->num '(x args))
                      (if n (iround n)
                          (error "Can't coerce " x 'int))))))
       
       (num (str ,(fn (x . args)
                    (or (apply str->num '(x args))
                        (error "Can't coerce " x 'num))))
            (int ,(fn (x) x)))
       
       (chr (int ,int->chr)
            (num ,(fn (x) (int->chr
                           (iround x)))))))
    coercions))

(def iround (compose inexact->exact round))
