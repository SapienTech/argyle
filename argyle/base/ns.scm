(ns (argyle base ns)
    :export (re-export-ns))
(use ((srfi srfi-1) :select (append-map))
     (argyle base mac)
     (argyle base fn))


(mac re-export-ns x
  ((m ...)
   #`(re-export 
      #,@(datum->syntax x
           (append-map
             (fn (m)
               (module-map (fn (name data) name)
                           (resolve-interface (syntax->datum m))))
             #'(m ...))))))
