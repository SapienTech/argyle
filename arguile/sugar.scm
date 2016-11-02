(define-module (arguile sugar))

(use-modules (arguile ssyntax)
             (arguile core)
             (srfi srfi-26))
(export-syntax \\ ret)

(mac \\ ((\\ fn args ...) #'(cut fn args ...)))

(mac ret ((ret var e1 e2 ...) #'(let var e1 e2 ... var)))
