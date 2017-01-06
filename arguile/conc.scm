(module (arguile conc)
    #:export (futr futr? mke-futr <ref> ref ref? @ doasync dosync alter))
(use (arguile base)
     (arguile generic)
     (arguile guile)
     (arguile loop)
     (arguile data)
     ((srfi srfi-1) #:select (zip))
     (ice-9 futures)
     (ice-9 threads))

(mac futr
  ((_ exp) #'(future exp)))

(defp futr? future?)
(defp mke-futr make-future)

(data ref (val mutx)
      #:init (%ref val mutx))

(defp ref (val) (%ref val (make-mutex)))

(defp @ (lzy-obj)
    (cond ((futr? lzy-obj) (touch lzy-obj))
          ((ref? lzy-obj) (ref-val lzy-obj))
          (else (err "attempt to deref a non-lazy obj" lzy-obj))))

(mac doasync
  ((_ e0 ...) #'(parallel e0 ...)))

(mac dosync x
  ((_ (refs ...) body ...) 
   (let ref-dats (map (fn (ref) (syn->dat ref)) #'(refs ...))
     (w/syn ((ref-cpys ...) (map (fn (ref) (datum->syntax x ref)) ref-dats)
             alter (syn 'alter x))
       #`(w/refs (refs ...)
           (let refs* (_let #,(zip #'(ref-cpys ...)
                                   (map (fn (ref) #`(cpy-ref #,ref))
                                        #'(refs ...)))
                            body ...
                            (list ref-cpys ...))
             (set-refs! (refs ...) refs*)))))))

;;; TODO: only allow user to use alter in a dosync block
(mac alter
  ((_ ref val) 
   #'(with-mutex (ref-mutx ref)
       (set! ref (ref-val! ref val)))))

(mac set-refs!
  ((_ (refs ...) vals)
   #`(do #,@(map (fn (ref val) #`(set! #,ref (ref-val! #,ref (ref-val #,val))))
                 #'(refs ...)
                 #`(#,@(map (fn (i) #`(list-ref vals #,i))
                            (iota (_length #'(refs ...)))))))))

(mac w/refs
  ((_ (r1 ...) e1 ...)
   #`(w/mutxs #,(map (fn (ref) #`(ref-mutx #,ref))
                     #'(r1 ...))
              e1 ...)))

(mac w/mutxs
  ((_ (m1 ...) e1 ...)
   #`(#,@(loop lp ((ms #'(m1 ...)))
           (if (nil? ms) #'(do e1 ...)
               #`(with-mutex #,(car ms)
                   #,(lp (cdr ms))))))))


(eval-when (expand load eval)
  (def cpy-ref (ref) (ref-mutx! ref (make-mutex))))
