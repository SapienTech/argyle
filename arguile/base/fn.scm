(module (arguile base fn)
    #:replace (fn fn-case def defp let ret w/ \\ ->> inlite comp id? defd? wrap))
;(export-syntax fn fn-case def defp let ret w/ \\ ->> inline)
(use (arguile guile)
     (arguile base mac)
     (ice-9 receive))

(mac fn
  ((_ args body ...) #'(lambda args body ...)))

(mac fn-case
  ((_ fn1 fn2 ...) #'(case-lambda fn1 fn2 ...)))

(mac def x
  ((_ name (arg ... . rest) e1 e2 ...)
   #`(#,@(if (has-kwargs? #'(arg ...))
             #`(define* (name #,@(expand-kwargs #'(arg ...) x) . rest))
             #'(define (name arg ... . rest)))
      e1 e2 ...))
  ((_ name val) #'(define name val)))

(mac defp
  ((_ name . rest) #'(begin (def name . rest) (export name))))

(mac let
  ((_ var val e1 e2 ...) #'(w/ (var val) e1 e2 ...))
  ((_ () e1 e2 ...) #'(_let () e1 e2 ...)))

(mac ret ((ret var e1 e2 ...) #'(let var e1 e2 ... var)))

(mac w/
  ((_ () e1 e2 ...)
   #'(_let () e1 e2 ...))
  ((_ (var val) e1 e2 ...)
   (if (id? #'var)
       #'(_let ((var val)) e1 e2 ...)
       #'(receive var val e1 e2 ...)))
  ((_ (var val rest ...) e1 e2 ...)
   (if (id? #'var)
       #'(_let ((var val)) (w/ (rest ...) e1 e2 ...))
       #'(receive var val  (w/ (rest ...) e1 e2 ...)))))

(mac \\ ((\\ fn args ...) #'(cut fn args ...)))

;;; Replace w/ => when loop export is handled
(mac ->>
  ((_ exp fn ...) #'((compose fn ...) exp)))

(mac inline
  ((_ name (arg ...) body ...)
   #'(define-inlinable (name arg ...) body ...)))

(eval-when (expand load eval)
  (def comp compose)
  (def id? identifier?)
  (def defd? defined?)
  (def wrap const))
