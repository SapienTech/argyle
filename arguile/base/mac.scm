(module (arguile base mac)
    #:export (mac mac? syn-case let-syn w/syn
              syn-param w/syn-params gen-tmps))
(use (arguile guile)
     (arguile base fn))

(define-syntax mac
  (lambda (ctx)
    (syntax-case ctx ()
      ((mac name ((_ . patt) guard ... templ) ...)
       #'(mac name x () ((_ . patt) guard ... templ) ...))
      ((mac name x ((_ . patt) guard ... templ) ...)
       (identifier? #'x)
       #'(mac name x () ((_ . patt) guard ... templ) ...))
      ((mac name (aux ...) ((_ . patt) guard ... templ) ...)
       #'(mac name x (aux ...) ((_ . patt) guard ... templ) ...))
      ((mac name x (aux ...) ((_ . patt) guard ... templ) ...)
       #'(define-syntax name
           (lambda (x)
             (syntax-case x (aux ...)
               ((_ . patt) guard ... templ) ...)))))))

(mac mac?
  ((_ mac) #'(macro? (module-ref (current-module) 'mac))))

(mac syn-case
  ((_ ctx (aux ...) ((kword . patt) templ) ...)
   #'(syntax-case ctx (aux ...)
       ((kword . patt) templ) ...)))

(mac let-syn
  ((_ syn exp body ...)
   #'(w/syn (syn exp) body ...)))

(mac w/syn
  ((_ (item ...) e1 ...)
   (with-syntax ((items (grp #'(item ...) 2)))
     #'(with-syntax items e1 ...))))

(mac syn-param
  ((_ name fn) #'(define-syntax-parameter name fn)))

;;; TODO: change to w/ format
(mac w/syn-params
  ((_ ((param val) ...) body ...)
   #'(syntax-parameterize ((param val) ...) body ...)))

(mac gen-tmps
  ((_ syn) #'(generate-temporaries syn)))
