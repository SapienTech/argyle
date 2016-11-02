(define-module (arguile core)
  #:export (fn def let with do =))
(use-modules (arguile guile)
             (arguile ssyntax))
(mac fn
  ((_ args e1 e2 ...)
   #'(lambda args e1 e2 ...)))

(mac def
  ((_ name args e1 e2 ...)
   #'(define name (fn args e1 e2 ...)))
  ((_ name val)
   #'(define name val)))

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

(mac do
  ((do e1 e2 ...) #'(begin e1 e2 ...)))

(mac =
  ((= var val) #'(set! var val))
  ((= var val rest ...)
   #'(do (set! var val)
         (= rest ...))))
