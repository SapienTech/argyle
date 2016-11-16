(module (arguile base syn)
  #:export (syn? syn-case w/syn
            syn->dat dat->syn))
(use (arguile base)
     (arguile guile))

(def (syn? obj)
  (and (vector? obj) (eq? 'syntax-object (vector-ref obj 0))))

(mac syn-case
  ((_ ctx (aux ...) ((kword . patt) templ) ...)
   #'(syntax-case ctx (aux ...)
       ((kword . patt) templ) ...)))

(mac w/syn
  ((_ (item ...) e1 ...)
   (with-syntax ((items (group #'(item ...) 2)))
     #'(with-syntax items e1 ...))))

(def syn->dat syntax->datum)
(def dat->syn datum->syntax)
