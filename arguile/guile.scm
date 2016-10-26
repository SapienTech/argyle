(define-module (arguile guile))
(export-syntax letM)

(define-syntax letM
  (syntax-rules ()
   ((letM args body ...)
    (let args body ...))))
