(module (arguile sugar))

(use (arguile ssyntax)
     (arguile guile)
     (arguile core))

(export-syntax \\ ret)

(mac \\ ((\\ fn args ...) #'(cut fn args ...)))

(mac ret ((ret var e1 e2 ...) #'(let var e1 e2 ... var)))
