(module (argyle base type kwd))
(use (argyle base fn))

(defp kwd? keyword?)
(defp kwd->sym keyword->symbol)
(defp kwd-like-sym->kwd keyword-like-symbol->keyword)
