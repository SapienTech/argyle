(ns (argyle base type str))
(use (argyle base fn))

;;; Standardize a macro for ref, set, copy, etc

(defp str? string?)
(defp str: string-ref)
(defp str! string-set!)
(defp str-len string-length)
(defp str-join string-append)
(defp str->lst string->list)
(defp str->num string->number)
(defp str->sym string->symbol)
(defp str->chr-set string->char-set)
(defp str-cpy string-copy)
(defp str-cpy! string-copy!)
(defp str-map string-map)
(defp str-fold string-fold)
(defp str-take string-take)
(defp str-drop string-drop)

