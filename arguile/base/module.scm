(module (arguile base module)
    :export (re-export-modules))
(use ((srfi srfi-1) :select (append-map))
     (arguile base mac)
     (arguile base fn))


(mac re-export-modules x
  ((m ...)
   #`(re-export 
      #,@(datum->syntax x
           (append-map
             (fn (m)
               (module-map (fn (name data) name)
                           (resolve-interface (syntax->datum m))))
             #'(m ...))))))
