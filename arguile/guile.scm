(module (arguile guile))
(export _+ _* _= _length _apply grp expand-kwargs has-kwargs?)
(export-syntax _let cut)
(use (srfi srfi-1)
     (arguile loop))

(define _+ +)
(define _= =)
(define _* *)
(define _length length)
(define _apply apply)

(define-syntax _let
  (syntax-rules ()
   ((_let args e1 ...)
    (let args e1 ...))))

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

(define (grp lst n)
  (loop lp ((lst lst) (acc '()))
    (if (> n (length lst))
        (reverse (append lst acc))
        (lp (drop lst 2) (cons (take lst 2) acc)))))

(define (expand-kwargs args ctx)
  (loop ((for arg (in-list args))
         (where args* '()
                (cons (let ((arg* (syntax->datum arg)))
                        (if (and (keyword? arg*) (eq? #:o arg*))
                            (datum->syntax ctx #:optional)
                            arg))
                      args*)))
        => (reverse args*)))

(define (has-kwargs? args)
  (or-map (lambda (arg) (keyword? (syntax->datum arg)))
          args))
