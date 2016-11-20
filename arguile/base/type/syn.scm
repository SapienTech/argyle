(module (arguile base type syn)
    #:export (syn syn? syn-case let-syn
              w/syn syn->dat dat->syn))
(use (arguile base)
     (arguile guile))

(mac syn
  ((_ obj) #'(syntax obj)))

(def (syn? obj)
  (and (vector? obj) (eq? 'syntax-object (vector-ref obj 0))))

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

(def syn->dat syntax->datum)
(def dat->syn datum->syntax)
