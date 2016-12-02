(module (arguile base type syn)
    #:export (syn? syn->dat dat->syn))
(use (arguile base fn)
     (arguile guile))

(def syn? (obj)
  (and (vector? obj)
       (eq? 'syntax-object (vector-ref obj 0))))

(def syn->dat syntax->datum)
(def dat->syn datum->syntax)
