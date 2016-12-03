(module (arguile base mac)
    #:export (mac mac? syn-case let-syn w/syn
              syn-param w/syn-params gen-tmps))
(use (arguile guile)
     (ice-9 receive))

(define-syntax mac
  (lambda (x)
    (define (ids? exps)
      (and-map identifier? exps))
    (syntax-case x ()
      ((_ name ctx (f1 ...) exp ...)
       (and (and-map identifier? `(,#'name ,#'ctx))
            (ids? #'(f1 ...)))
       #'(%mac name ctx (f1 ...) exp ...))
      ((_ name ctx exp ...)
       (and-map identifier? `(,#'name ,#'ctx))
       #'(mac name ctx () exp ...))
      ((_ name (f1 ...) exp ...)
       (ids? #'(f1 ...))
       #'(mac name ctx (f1 ...) exp ...))
      ((_ name exp ...)
       (identifier? #'name)
       #'(mac name ctx () exp ...)))))

(define-syntax %mac
  (lambda (x)
    (define (parse-mac exps)
      (define (pattern? exp)
        (syntax-case exp ()
        (((_ . patt) guard ... templ) #t)
        (_ #f)))
      (let lp ((exps exps) (defs '()) (patts '()))
        (if (null? exps) (values (reverse defs) (reverse patts))
            (if (pattern? (car exps))
                (lp (cdr exps) defs (cons (car exps) patts))
                (lp (cdr exps) (cons (car exps) defs) patts)))))
    (syntax-case x ()
      ((_ name ctx (f1 ...) exp ...)
       #`(define-syntax name
           (lambda (ctx)
             #,@(receive (defs cases) (parse-mac #'(exp ...))
                  (if (null? cases) defs
                      #`(#,@defs
                          (syntax-case ctx (f1 ...) #,@cases))))))))))

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
