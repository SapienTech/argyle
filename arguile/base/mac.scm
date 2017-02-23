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
       (and (ids? `(,#'name ,#'ctx))
            (ids? #'(f1 ...)))
       #'(%mac name ctx (f1 ...) exp ...))
      ((_ name ctx exp ...)
       (ids? `(,#'name ,#'ctx))
       #'(mac name ctx () exp ...))
      ((_ name (f1 ...) exp ...)
       (ids? #'(f1 ...))
       #'(mac name ctx (f1 ...) exp ...))
      ((_ name exp ...)
       (identifier? #'name)
       #'(mac name ctx () exp ...)))))

(define-syntax %mac
  (lambda (x)
    (syntax-case x ()
      ((_ name ctx (f1 ...) exp ...)
       #`(define-syntax name
           (lambda (ctx)
             #,@(receive (defs cases) (parse-mac #'(exp ...))
                  (if (null? cases) defs
                      #`(#,@defs (syntax-case ctx (f1 ...)

                                   #,@(format cases)))))))))))

(eval-when (compile load eval)
  (define (format cases)
    (map (lambda (tmp case)
           (syntax-case case ()
             (((patt ... . rst) . rest)
              #`(#,(cons tmp #'(patt ... . rst)) . rest))))
         (generate-temporaries cases)
         cases))

  (define (parse-mac exps)
    (let lp ((exps exps) (defs '()) (patts '()))
      (if (null? exps) (values (reverse defs) (reverse patts))
          (if (patt? (car exps))
              (lp (cdr exps) defs (cons (car exps) patts))
              (lp (cdr exps) (cons (car exps) defs) patts)))))

  (define (patt? exp)
    (syntax-case exp ()
      (((patt ... . rst) (guard-exp ...) ... templ) #t)
      (_ #f))))

(mac mac?
  ((mac) #'(macro? (module-ref (current-module) 'mac))))

(mac syn-case
  ((ctx (aux ...) ((patt ... . rst) templ) ...)
   #'(syntax-case ctx (aux ...)
       ((patt ... . rst) templ) ...)))

(mac let-syn
  ((syn exp body ...)
   #'(w/syn (syn exp) body ...)))

(mac w/syn
  (((item ...) e1 ...)
   (with-syntax ((items (grp #'(item ...) 2)))
     #'(with-syntax items e1 ...))))

(mac syn-param
  ((name fn) #'(define-syntax-parameter name fn)))

;;; TODO: change to w/ format
(mac w/syn-params
  ((((param val) ...) body ...)
   #'(syntax-parameterize ((param val) ...) body ...)))

(mac gen-tmps
  ((syn) #'(generate-temporaries syn)))
