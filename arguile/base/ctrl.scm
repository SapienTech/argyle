(module (arguile base ctrl))
(use (arguile base)
     (ice-9 control))
(re-export abort)

(defp c/prmt call-with-prompt)
(defp c/vals call-with-values)

