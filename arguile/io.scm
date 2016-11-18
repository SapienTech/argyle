(module (arguile io)
  #:export (pr prn prnn))
(use (arguile base))

(mac pr
  ((_ v1) #'(display v1))
  ((_ v1 v2 ...) #'(do (display v1) (pr v2 ...))))

(mac prn
  ((_ v1 v2 ...) #'(do (pr v1 v2 ...) (newline))))

(mac prnn
  ((_ v1) #'(prn v1))
  ((_ v1 v2 ...) #'(do (prn v1) (prnn v2 ...))))
