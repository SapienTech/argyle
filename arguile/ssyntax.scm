(define-module (arguile ssyntax)
  #:export (mac syn syn-case w/syn))

(define-syntax mac
  (syntax-rules ()
    ((mac kword ((_ . patt) templ) ...)
     (mac kword () ((_ . patt) templ) ...))
    ((mac kword (aux ...) ((_ . patt) templ) ...)
     (define-syntax kword
         (lambda (ctx)
           (syntax-case ctx (aux ...)
             ((_ . patt) templ) ...))))))
(mac syn
     ;; Maybe change name to syntax
  ((_ name (ctx) body)
   #'(define-syntax name
       (lambda (ctx) body))))

(mac syn-case
     ;; May not handle all cases
  ((_ ctx (aux ...) ((kword . patt) templ) ...)
   #'(syntax-case ctx (aux ...)
     ((kword . patt) templ) ...)))

(mac w/syn
  ((_ () e1 e2 ...)
   #'(with () e1 e2 ...))
  ((_ (syn val) e1 e2 ...)
   #'(with-syntax ((syn val)) e1 e2 ...))
  ((_ (syn val rest ...) e1 e2 ...)
   #'(with-syntax ((syn val)))))
