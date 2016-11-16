(module (arguile base))
(export =? 0? 1?)
(export-syntax mac fn def defp let with do
               fn-case & \\ ret =)
(use (arguile guile))

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

(mac fn
  ((_ args e1 e2 ...)
   #'(lambda args e1 e2 ...)))

(mac def x
  ((_ name (arg1 ... . rest) e1 e2 ...)
   #`(#,@(if (has-kwargs? #'(arg1 ...))
             #`(define* (name #,@(expand-kwargs #'(arg1 ...) x) . rest))
             #'(define (name arg1 ... . rest)))
      e1 e2 ...))
  ((_ name val)
   #'(define name val)))

(mac defp
  ((_ name . rest) #'(do (def name . rest) (export name))))

(mac let
  ((_ var val e1 e2 ...)
   #'(with (var val) e1 e2 ...)))

(mac with
  ((_ () e1 e2 ...)
   #'(_let () e1 e2 ...))
  ((_ (var val) e1 e2 ...)
   #'(_let ((var val)) e1 e2 ...))
  ((_ (var val rest ...) e1 e2 ...)
   #'(_let ((var val)) (with (rest ...) e1 e2 ...))))

(mac do ((_ e1 ...) #'(begin e1 ...)))

;;; TODO: check if var is a free variable, and if so, define it
(mac =
  ((_ var val) #'(set! var val))
  ((_ var val rest ...)
   #'(do (set! var val)
       (= rest ...))))

(mac fn-case
  ((_ fn1 fn2 ...) #'(case-lambda fn1 fn2 ...)))

(mac &
  ((_ e1 ...) #'(and e1 ...)))

(mac \\ ((\\ fn args ...) #'(cut fn args ...)))

(mac ret ((ret var e1 e2 ...) #'(let var e1 e2 ... var)))

;;; Make generic
(def =? _=)
(def 0? zero?)
(def 1? (n) (=? 1 n))

;;; TODO: add macro to reexport submodules
