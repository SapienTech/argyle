(module (argyle fibers))
(use (fibers)
     (fibers internal)
     (argyle base))

(mac go
  ((exp ...)
   #`(run-fibers
      (fn ()
        #,@(map (fn (e)
                  #'(spawn-fiber (fn () #'e)
                                 (make-scheduler)))
                #'(exp ...))))))
