(define-module (arguile guile))
(export-syntax letM cut)

(define-syntax letM
  (syntax-rules ()
   ((letM args body ...)
    (let args body ...))))

(define-syntax cut
  (lambda (stx)
    (syntax-case stx ()
      ((cut slot0 slot1+ ...)
       (let loop ((slots #'(slot0 slot1+ ...))
                  (params '())
                  (args	'()))
         (if (null? slots)
             #`(lambda #,(reverse params) #,(reverse args))
             (let ((s (car slots))
                   (rest (cdr slots)))
               (with-syntax (((var) (generate-temporaries '(var))))
                 (syntax-case s (_ ___)
                   (_
                    (loop rest (cons #'var params) (cons #'var args)))
                   (___
                    (if (pair? rest)
                        (error "___ not on the end of cut expression"))
                    #`(lambda #,(append (reverse params) #'var)
                        (apply #,@(reverse (cons #'var args)))))
                   (else
                    (loop rest params (cons s args))))))))))))
