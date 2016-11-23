(module (arguile base ctrl))
(use (arguile base)
     (ice-9 control))
(re-export abort (call/cc . c/cc) (call/ec . c/ec))

(defp c/prmt call-with-prompt)
(defp c/vals call-with-values)
(defp tag default-prompt-tag)

(mac $>
  ((_ exp)           #'(c/prmt (tag) (fn () exp) hdlr))
  ((_ exp hdlr)      #'(c/prmt (tag) (fn () exp) hdlr))
  ((_ tag expr hdlr) #'(c/prmt (tag) (fn () exp) hdlr)))

(def hdlr (cont f)
  ($> (tag) (f cont) hdlr))
