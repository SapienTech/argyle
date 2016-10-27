(define-module (arguile))
(use-modules (arguile guile))
(export mac def module use fn let with do = \\ pr prn)

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
