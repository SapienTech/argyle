(module (arguile base))
(export =? 0? 1? flatn ~ nil? &map id? set\ defd?)
(export-syntax mac fn def defp let w/ do
               fn-case & \\ ret aif =
               re-export-modules)
(use (srfi srfi-1)
     (arguile guile)
     (ice-9 receive))

(define-syntax mac
  (lambda (ctx)
    (syntax-case ctx ()
      ((mac name ((_ . patt) guard ... templ) ...)
       #'(mac name x () ((_ . patt) guard ... templ) ...))
      ((mac name x ((_ . patt) guard ... templ) ...) (id? #'x)
       #'(mac name x () ((_ . patt) guard ... templ) ...))
      ((mac name (aux ...) ((_ . patt) guard ... templ) ...)
       #'(mac name x (aux ...) ((_ . patt) guard ... templ) ...))
      ((mac name x (aux ...) ((_ . patt) guard ... templ) ...)
       #'(define-syntax name
           (lambda (x)
             (syntax-case x (aux ...)
               ((_ . patt) guard ... templ) ...)))))))

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

(def defd? defined?)

(mac defp
  ((_ name . rest) #'(do (def name . rest) (export name))))

(mac let
  ((_ var val e1 e2 ...)
   #'(w/ (var val) e1 e2 ...)))

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

(mac aif x
  ((_ test then else)
   (let-syn it (-> syn 'it x)
     #'(let it test
         (if it then else)))))

;;; Make generic
(eval-when (expand load eval)
  (def =? _=)
  (def 0? zero?)
  (def 1? (n) (=? 1 n))
  (def ~ not)
  (def nil? null?)
  (def flatn append-map)
  (def &map and-map)
  (def id? identifier?)
  (def set\ lset-difference))

;;; Consider how we are going to expose above defs for base
;;; op1: put them in guile.scm
;;; TODO: export non-used submodules to avoid code duplication
(mac re-export-modules x
  ((_ m ...)
   #`(re-export 
      #,@(datum->syntax x
           (append-map
             (fn (m)
               (module-map (fn (name data) name)
                           (resolve-interface (syntax->datum m))))
             #'(m ...))))))

;;; Eventually, this `use` will be handled by export-modules
(use (arguile base type)
     (arguile base generic)
     (arguile base err)
     (arguile base io))

(re-export-modules (arguile base type)
                   (arguile base generic)
                   (arguile base err)
                   (arguile base io))
