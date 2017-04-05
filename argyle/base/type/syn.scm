(module (argyle base type syn)
    :export (syn? syn->dat dat->syn))
(use (argyle base fn)
     (argyle guile))

(def syn? (obj)
  (and (vector? obj)
       (eq? 'syntax-object (vector-ref obj 0))))

(def syn->dat syntax->datum)
(def dat->syn datum->syntax)
