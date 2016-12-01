(module (arguile base))
(export =? 0? 1? flatn ~ nil? &map id? set\ defd? wrap comp)
(export-syntax mac mac? syn-case let-syn w/syn
               fn def defp let w/ do
               fn-case & \\ ret = ->> inline
               re-export-modules aif it)
(use ((srfi srfi-1) #:select (append-map lset-difference))
     (arguile guile)
     (ice-9 receive))

;;; Consider moving this to guile.scm
(define-syntax mac
  (lambda (ctx)
    (syntax-case ctx ()
      ((mac name ((_ . patt) guard ... templ) ...)
       #'(mac name x () ((_ . patt) guard ... templ) ...))
      ((mac name x ((_ . patt) guard ... templ) ...)
       (identifier? #'x)
       #'(mac name x () ((_ . patt) guard ... templ) ...))
      ((mac name (aux ...) ((_ . patt) guard ... templ) ...)
       #'(mac name x (aux ...) ((_ . patt) guard ... templ) ...))
      ((mac name x (aux ...) ((_ . patt) guard ... templ) ...)
       #'(define-syntax name
           (lambda (x)
             (syntax-case x (aux ...)
               ((_ . patt) guard ... templ) ...)))))))

(mac mac?
  ((_ mac) #'(macro? (module-ref (current-module) 'mac))))



(mac syn-case
  ((_ ctx (aux ...) ((kword . patt) templ) ...)
   #'(syntax-case ctx (aux ...)
       ((kword . patt) templ) ...)))

(mac let-syn
  ((_ syn exp body ...)
   #'(w/syn (syn exp) body ...)))

(mac w/syn
  ((_ (item ...) e1 ...)
   (with-syntax ((items (grp #'(item ...) 2)))
     #'(with-syntax items e1 ...))))

;;; Add named fn support
(mac fn
  ((_ args body ...) #'(lambda args body ...)))

(mac def x
  ((_ name (arg ... . rest) e1 e2 ...)
   #`(#,@(if (has-kwargs? #'(arg ...))
             #`(define* (name #,@(expand-kwargs #'(arg ...) x) . rest))
             #'(define (name arg ... . rest)))
      e1 e2 ...))
  ((_ name val) #'(define name val)))

(def defd? defined?)

(mac defp
  ((_ name . rest) #'(do (def name . rest) (export name))))

(mac let
  ((_ var val e1 e2 ...) #'(w/ (var val) e1 e2 ...)))

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
  ((_ var val rest ...) #'(do (set! var val) (= rest ...))))

(mac fn-case
  ((_ fn1 fn2 ...) #'(case-lambda fn1 fn2 ...)))

(mac aif x
  ((_ test then else)
   (let-syn it (datum->syntax x 'it)
     #'(let it test (if it then else)))))

(mac & ((_ e1 ...) #'(and e1 ...)))

(mac \\ ((\\ fn args ...) #'(cut fn args ...)))

(mac ret ((ret var e1 e2 ...) #'(let var e1 e2 ... var)))

;;; Replace w/ => when loop export is handled
(mac ->>
  ((_ fn ... exp) #'((compose fn ...) exp)))

(mac inline
  ((_ name (arg ...) body ...)
   #'(define-inlinable (name arg ...) body ...)))

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
  (def set\ lset-difference)
  (def wrap const)
  (def comp compose))

;;; TODO: enable api modification
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
     (arguile base io)
     (arguile base ctrl))

(re-export-modules (arguile base type)
                   (arguile base generic)
                   (arguile base err)
                   (arguile base io)
                   (arguile base ctrl))
