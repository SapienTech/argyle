(module (arguile base type)
  :export (base-type coerce))
(use (arguile base mac)
     (arguile base fn)
     (arguile base ctrl)
     (arguile base err)
     (arguile base module)
     (arguile base type lst)
     (arguile base type str)
     (arguile base type num)
     (arguile base type fn)
     (arguile base type sym)
     (arguile base type syn)
     (arguile base type chr)
     (arguile base type kwd)
     (arguile base type strm))

;;; TODO: add simple heirarchy
(def base-type (x)
 (cond
  ;; TODO: nest related types
  ;((pair? x)       'pair)
  ((lst? x)        '<lst>)
  ((str? x)        '<str>)
  ((int? x)        '<int>)
  ((num? x)        '<num>)
  ((fn? x)         '<fn>)
  ((sym? x)        '<sym>)
  ((syn? x)        '<syn>)
  ((strm? x)       '<strm>)
  ((hash-table? x) '<hash-tbl>)
  ((chr? x)        '<chr>)
  ((vector? x)     '<vector>)
  ((kwd? x)        '<kwd>)
  ((null? x)       '<nil>)
  (else            (error "Type: unknown type" x))))

(def coerce (x to-type . args)
  (let x-type (base-type x)
    (if (eqv? to-type x-type) x
        (w/ (fail (fn args (error "Can't coerce" args '-> to-type))
             conversions (hash-ref coercions to-type fail)
             converter (hash-ref conversions x-type fail))
          (apply converter (cons x args))))))

(def coercions
  (ret coercions (make-hash-table)
    (for-each
     (fn (e)
       (w/ (target-type (car e)
            conversions (make-hash-table))
         (hash-set! coercions target-type conversions)
         (for-each
          (fn (x) (hash-set! conversions (car x) (cadr x)))
          (cdr e))))
     `((<dat> (<syn> ,syn->dat)
              (<lst> ,syn->dat))
       ;; So clearly this is a bit hacky
       (<syn> (<lst> ,(fn (dat ctx) (dat->syn ctx dat)))
              (<num> ,(fn (dat ctx) (dat->syn ctx dat)))
              (<str> ,(fn (dat ctx) (dat->syn ctx dat)))
              (<sym> ,(fn (dat ctx) (dat->syn ctx dat)))
              (<kwd> ,(fn (dat ctx) (dat->syn ctx dat))))
       (<str> (<int> ,num->str)
              (<num> ,num->str)
              (<chr> ,string)
              (<sym> ,(fn (x) (if (eqv? x (symbol)) "" (sym->str x)))))
       
       (<sym> (<str> ,str->sym)
              (<chr> ,(fn (c) (str->sym (string c))))
              (<num> ,(\\ (comp str->sym num->str) _)))
       
       (<int> (<chr> ,(fn (c . args) (chr->int c)))
              (<num> ,(fn (x . args) (iround x)))
              (<str> ,(fn (x . args)
                        (aif (str->num x) (iround it)
                             (err "Can't coerce" x '-> 'int)))))
       
       (<num> (<str> ,(fn (x . args)
                        (or (str->num x)
                            (err "Can't coerce " x '-> 'num))))
              (<int> ,(fn (x) x)))
       
       (<chr> (<int> ,int->chr)
              (<num> ,(fn (x) (int->chr
                               (iround x)))))))
    coercions))

(def iround (compose inexact->exact round))

(mac export-type-ctrs
  ((t1 ...)
   #`(do #,@(map (fn (t)
                   #`(defp #,t (obj . args)
                       (apply coerce obj
                              (sym-join '< '#,t '>) args)))
                 #'(t1 ...)))))

(export-type-ctrs str num int sym syn dat chr)

(re-export-modules 
 (arguile base type lst)
 (arguile base type str)
 (arguile base type num)
 (arguile base type fn)
 (arguile base type sym)
 (arguile base type syn)
 (arguile base type chr)
 (arguile base type kwd)
 (arguile base type strm))
