(module (arguile base type sym))
(use (arguile base fn))
(defp sym? symbol?)
(defp sym->kwd symbol->keyword)
(defp sym->str symbol->string)
(defp sym-join symbol-append)
(defp sym: symbol-fref)
(defp sym! symbol-fset!)
(defp sym-hash symbol-hash)
(defp sym-interned? symbol-interned?)
(defp sym-pref symbol-pref)
(defp sym-pref-fn symbol-prefix-proc)
(defp sym-prop symbol-property)
(defp sym-prop-del! symbol-property-remove!)
