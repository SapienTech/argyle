(module (arguile conc)
    #:export (futr doasync))
(use (arguile base)
     (arguile loop)
     (arguile data)
     (ice-9 futures)
     (ice-9 threads))

(mac futr
  ((_ exp) #'(future exp)))

(defp @ touch)

(defp mke-future make-future)

(mac doasync
  ((_ e0 ...) #'(parallel e0 ...)))

(data ref (val mutx)
      #:init (%ref val mutx))

(defp ref (val) (%ref val (make-mutex)))

(mac dosync-wip x
     ((_ (refs ...) e0 ...)
      (let ref-lst (map (fn (ref)
                          (let-syn ref ref
                           #'(cons ref
                                   (ref-mutx! ref (make-mutex)))))
                        #'(refs ...))
        #`(w/refs (refs ...)
                  (prn ref-lst)
                  e0 ...
                  #,@(map (fn (ref ref-pair)
                            (w/syn (ref ref ref-pair ref-pair)
                              #'(set! ref (cdr ref-pair))))
                          #'(refs ...)
                          ref-lst)))))

(mac w/mutxs
  ((_ (m1 ...) e1 ...)
   #`(#,@(loop lp ((ms #'(m1 ...)))
               (if (nil? ms) #'(do e1 ...)
                   #`(with-mutex #,(car ms)
                       #,(lp (cdr ms))))))))

(mac w/refs
  ((_ (m1 ...) e1 ...)
   #`(#,@(loop lp ((ms #'(m1 ...)))
               (if (nil? ms) #'(do e1 ...)
                   #`(with-mutex (ref-mutx #,(car ms))
                       #,(lp (cdr ms))))))))

(mac alter
  ((_ ref val)
   #'(with-mutex (ref-mutx ref)
       (set! ref (ref-val! ref val))))) 
