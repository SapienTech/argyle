(module (arguile base fn)
    #:replace (fn fn-case def defp let ret w/ \\ ->> inline comp id? defd? wrap))
(use (arguile guile)
     (arguile base mac)
     (ice-9 receive))

(mac fn
  ((args body ...) #'(lambda args body ...)))

(mac fn-case
  ((fn1 fn2 ...) #'(case-lambda fn1 fn2 ...)))

(mac def x
  ((name (arg ... . rest) e1 e2 ...)
   #`(#,@(if (has-kwargs? #'(arg ...))
             #`(define* (name #,@(expand-kwargs #'(arg ...) x) . rest))
             #'(define (name arg ... . rest)))
      e1 e2 ...))
  ((name val) #'(define name val)))

(mac defp
  ((name . rest) #'(begin (def name . rest) (export name))))

(mac let
  ((var val e1 e2 ...) #'(w/ (var val) e1 e2 ...))
  ((() e1 e2 ...) #'(_let () e1 e2 ...)))

(mac ret ((var e1 e2 ...) #'(let var e1 e2 ... var)))

(mac w/
  ((() e1 e2 ...)
   #'(_let () e1 e2 ...))
  (((var val) e1 e2 ...)
   (if (id? #'var)
       #'(_let ((var val)) e1 e2 ...)
       #'(receive var val e1 e2 ...)))
  (((var val rest ...) e1 e2 ...)
   (if (id? #'var)
       #'(_let ((var val)) (w/ (rest ...) e1 e2 ...))
       #'(receive var val  (w/ (rest ...) e1 e2 ...)))))

(mac \\ ((fn args ...) #'(cut fn args ...)))

;;; Replace w/ => when loop export is handled
(mac ->>
  ((exp fn ...) #'((compose fn ...) exp)))

(mac inline
  ((name (arg ...) body ...)
   #'(define-inlinable (name arg ...) body ...)))

(defp app apply)
(defp comp compose)
(defp id? identifier?)
(defp defd? defined?)
(defp wrap const)
