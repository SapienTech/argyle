(module (arguile fibers))
(use (fibers)
     (fibers internal)
     (arguile base))

(mac go
  ((_ exp ...)
   #`(run-fibers
      (fn ()
        #,@(map (fn (e)
                  #'(spawn-fiber (fn () #'e)
                                 (make-scheduler)))
                #'(exp ...))))))
