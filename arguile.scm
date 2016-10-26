(define-module (arguile))
(export mac def module use fn)

;;; TODO allow mac to not require double parens
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

(mac fn
  ((fn (args ...) body body* ...)
   (lambda (args ...) body body* ...)))
