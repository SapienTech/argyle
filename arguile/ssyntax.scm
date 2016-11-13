(module (arguile ssyntax)
  #:export (mac syn syn-case w/syn
            syn->dat dat->syn))
(use (srfi srfi-1))

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
  ((_ ctx (aux ...) ((kword . patt) templ) ...)
   #'(syntax-case ctx (aux ...)
       ((kword . patt) templ) ...)))

(mac w/syn
  ((_ (item ...) e1 ...)
   (with-syntax ((items (group #'(item ...) 2)))
     #'(with-syntax items e1 ...))))

(define syn->dat syntax->datum)
(define dat->syn datum->syntax) 

(define (group lst n)
  ;; Move to other module
  (let lp ((lst lst) (acc '()))
    (if (> n (length lst))
        (reverse (append lst acc))
        (lp (drop lst 2) (cons (take lst 2) acc)))))
