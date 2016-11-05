(define-module (arguile ssyntax)
  #:export (mac syn syn-case w/syn))

(define-syntax mac
  (lambda (ctx)
    (syntax-case ctx ()
      ((mac name ((_ . patt) templ) ...)
       #'(mac name x () ((_ . patt) templ) ...))
      ((mac name x ((_ . patt) templ) ...)
       (identifier? #'x)
       #'(mac name x () ((_ . patt) templ) ...))
      ((mac name (aux ...) ((_ . patt) templ) ...)
       #'(mac name x (aux ...) ((_ . patt) templ) ...))
      ((mac name x (aux ...) ((_ . patt) templ) ...)
       #'(define-syntax name
           (lambda (x)
             (syntax-case x (aux ...)
               ((_ . patt) templ) ...)))))))

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
   #'(with-syntax ((syn val)) (w/syn (rest ...) e1 e2 ...))))
