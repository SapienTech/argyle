(module (arguile sugar))
(export-syntax \\ ret)
(use (arguile ssyntax)
     (arguile guile)
     (arguile core))

(mac \\ ((\\ fn args ...) #'(cut fn args ...)))

(mac ret ((ret var e1 e2 ...) #'(let var e1 e2 ... var)))
