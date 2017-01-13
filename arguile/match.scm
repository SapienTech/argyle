(module (arguile match))
(use (arguile base)
     (ice-9 match))

(mac mdef
  (def match-args (args mtchrs bdy)
    (if (nil? args) #`(do #,@bdy)
        #`(match #,(car args)
            (#,(car mtchrs)
             #,(match-args (cdr args) (cdr mtchrs) bdy)))))
  ((_ name (a0 ...) b0 b1 ...)
   (let tmps (gen-tmps #'(a0 ...))
     #`(def name #,tmps
         #,(match-args tmps #'(a0 ...) #'(b0 b1 ...))))))
