(define-module (arguile))
(use-modules (arguile guile)
             (srfi srfi-1)
             (ice-9 match)
             (ice-9 receive)
             (srfi srfi-45))
(re-export fold reduce match receive
           delay lazy force eager promise?)
(export mac def module use fn
        let with do = \\ pr prn
        err type coerce apply + len)

(define-syntax mac
  (syntax-rules ()
    ((mac keyword ((_keyword . pattern) template) ...)
     (mac keyword () ((_keyword . pattern) template) ...))
    ((mac keyword (aux ...)
          ((_keyword . pattern) template) ...)
     (define-syntax keyword
       (syntax-rules (aux ...) ((_keyword . pattern) template) ...)))))

(mac def
  ((def name args exp exp* ...)
   (define name (lambda args exp exp* ...)))
  ((def name val)
   (define name val)))

(mac module
  ((module (base ext ...)) (define-module (base ext ...)))
  ((module base ext ...) (module (base ext ...))))

(mac use ((use modules) (use-modules modules)))

;;; Make anaphoric
(mac fn
  ((fn args body body* ...)
   (lambda args body body* ...)))

(mac let
  ((let var val body body* ...)
   (with (var val) body body* ...)))

(mac with
  ((with (var val) body body* ...)
   (_let ((var val)) body body* ...))
  ((with (var val rest ...) body body* ...)
   (_let ((var val)) (with (rest ...) body body* ...))))

(mac do
  ((do expr expr* ...) (begin expr expr* ...)))

(mac =
  ((= var val) (set! var val))
  ((= var val rest ...)
   (do (set! var val)
       (= rest ...))))

(mac \\
  ((\\ fn args ...) (cut fn args ...)))

(mac pr
  ((pr arg) (display arg))
  ((pr arg arg* ...) (do (display arg) (pr arg* ...))))

(mac prn
  ((prn arg arg* ...) (do (pr arg arg* ...) (newline))))

(def err error)

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
  (#t                 (err "Type: unknown (or not included) type" x))))

(def iround (compose inexact->exact round))

;;; Allow table to be easily extended
(def coercions
  ;; define ret, which returns specified val
  (let coercions (make-hash-table)
    (for-each
     (fn (e)
       (with (target-type (car e)
              conversions (make-hash-table))
         (hash-set! coercions target-type conversions)
         (for-each
          (fn (x) (hash-set! conversions (car x) (cadr x)))
          (cdr e))))
     `((fn (str ,(fn (s) (fn (i) (string-ref s i))))
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
                          (err "Can't coerce " x 'int))))))
       
       (num (str ,(fn (x . args)
                    (or (apply string->number '(x args))
                        (err "Can't coerce " x 'num))))
            (int ,(fn (x) x)))
       
       (chr (int ,integer->char)
            (num ,(fn (x) (integer->char
                           (iround x)))))))
    coercions))

(def coerce (x to-type . args)
  (let x-type (type x)
    (if (eqv? to-type x-type) x
        (with (fail (fn () (err "Can't coerce " x to-type))
               conversions (hash-ref coercions to-type fail)
               converter (hash-ref conversions x-type fail))
          (apply converter (cons x args))))))

(def apply (fn args)
  (_apply (coerce fn 'fn) args))

(def one-of (tests val)
  (if (null? tests) #f
      (or ((car tests) val)
          (one-of (cdr tests) val))))

;;; Consider using goops for this or as an extension
(def + args
  (cond ((null? args) 0)
        ((one-of `(,string? ,char?) (car args))
         (apply string-append
                (map (\\ coerce _ 'str) args)))
        (else (apply _+ args))))

(def len (x)
  (cond ((string? x) (string-length x))
        ((hash-table? x) (hash-count (const #t) x))
        ((vector? x) (vector-length x))
        (else (length x))))
