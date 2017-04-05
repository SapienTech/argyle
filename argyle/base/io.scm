(module (argyle base io)
  :export (pr prn prnn))
(use (argyle base mac)
     (argyle base ctrl))

(mac pr
  ((v1) #'(display v1))
  ((v1 v2 ...) #'(do (display v1) (pr v2 ...))))

(mac prn
  ((v1 v2 ...) #'(do (pr v1 v2 ...) (newline))))

(mac prnn
  ((v1) #'(prn v1))
  ((v1 v2 ...) #'(do (prn v1) (prnn v2 ...))))
