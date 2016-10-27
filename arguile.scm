(define-module (arguile))
(use-modules (arguile guile)
             (srfi srfi-1)
             (ice-9 match)
             (ice-9 receive)
             (srfi srfi-45))
(re-export fold reduce match receive
           delay lazy force eager promise?)
(export mac def module use fn
        let with do = \\ pr prn)

(define-syntax mac
  (syntax-rules ()
    ((mac keyword ((_keyword . pattern) template) ...)
     (mac keyword () ((_keyword . pattern) template) ...))
    ((mac keyword (aux ...)
          ((_keyword . pattern) template) ...)
     (define-syntax keyword
       (syntax-rules (aux ...) ((_keyword . pattern) template) ...)))))

(mac def
  ((def name (arg ...) exp rest ...)
   (define (name arg ...) exp rest ...))
  ((def name val)
   (define name val)))

(mac module
  ((module (base ext ...)) (define-module (base ext ...)))
  ((module base ext ...) (module (base ext ...))))

(mac use ((use modules) (use-modules modules)))

;;; Make anaphoric
(mac fn
  ((fn (args ...) body body* ...)
   (lambda (args ...) body body* ...)))

(mac let
  ((let var val body body* ...)
   (with (var val) body body* ...)))

(mac with
  ((with (var val) body body* ...)
   (letM ((var val)) body body* ...))
  ((with (var val rest ...) body body* ...)
   (letM ((var val)) (with (rest ...) body body* ...))))

(mac do
  ((do expr expr* ...) (begin expr expr* ...)))

(mac =
  ((= var val) (set! var val))
  ((= var val rest ...)
   (do (set! var val)
       (= rest ...))))

(mac \\
  ((\\ proc args ...) (cut proc args ...)))

(mac pr
  ((pr arg) (display arg))
  ((pr arg arg* ...) (do (display arg) (pr arg* ...))))

(mac prn
  ((prn arg arg* ...) (do (pr arg arg* ...) (newline))))

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
  (#t                 (error "Type: unknown (or not included) type" x))))

(def coercions
  (let coercions (make-hash-table)
    (for-each
     (lambda (e)
       (let ((target-type (car e))
             (conversions (make-hash-table)))
         (hash-set! coercions target-type conversions)
         (for-each
          (lambda (x) (hash-set! conversions (car x) (cadr x)))
          (cdr e))))
     `((fn (str ,(lambda (s) (lambda (i) (string-ref s i))))
           (table  ,(lambda (h)
                      (case-lambda
                        ((k) (hash-ref h k 'nil))
                        ((k d) (hash-ref h k d)))))
           (vec ,(lambda (v) (lambda (i) (vector-ref v i)))))
       
       (str (int ,number->string)
            (num ,number->string)
            (chr ,string)
            (sym ,(lambda (x) (if (eqv? x 'nil) "" (symbol->string x)))))
       
       (sym (str ,string->symbol)
            (chr ,(lambda (c) (string->symbol (string c)))))
       
       (int (chr ,(lambda (c . args) (char->ascii c)))
            (num ,(lambda (x . args) (iround x)))
            (str ,(lambda (x . args)
                    (let ((n (apply string->number x args)))
                      (if n (iround n)
                          (err "Can't coerce " x 'int))))))
       
       (num (str ,(lambda (x . args)
                    (or (apply string->number x args)
                        (err "Can't coerce " x 'num))))
            (int ,(lambda (x) x)))
       
       (chr (int ,ascii->char)
            (num ,(lambda (x) (ascii->char (iround x)))))))))

(define (coerce x type . args)
  (let ((x-type (ar-type x)))
    (if (eqv? type x-type) x
        (let* ((fail (lambda () (err "Can't coerce " x type)))
               (conversions (hash-ref coercions type fail))
               (converter (hash-ref conversions x-type fail)))
          (apply converter (cons x args))))))
