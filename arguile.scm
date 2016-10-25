(define-module (arguile))
(export mac def)

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
  ((def name val) (define name val)))

