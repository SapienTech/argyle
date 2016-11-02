(define-module (arguile io)
  #:export (pr prn))

(use-modules (arguile ssyntax)
             (arguile core))

(mac pr
  ((pr arg) #'(display arg))
  ((pr arg arg* ...) #'(do (display arg) (pr arg* ...))))

(mac prn
  ((prn arg arg* ...) #(do (pr arg arg* ...) (newline))))
