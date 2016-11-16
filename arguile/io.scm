(module (arguile io)
  #:export (pr prn))
(use (arguile base))

(mac pr
  ((pr arg) #'(display arg))
  ((pr arg arg* ...) #'(do (display arg) (pr arg* ...))))

(mac prn
  ((prn arg arg* ...) #'(do (pr arg arg* ...) (newline))))
