(module (arguile base ctrl)
    #:replace (do = aif it & $> nil?))
(use ((srfi srfi-1) #:select (append-map lset-difference))
     (arguile guile)
     (arguile base mac)
     (arguile base fn)
     (ice-9 control))
(re-export abort (call/cc . c/cc) (call/ec . c/ec))

(mac do ((_ e1 ...) #'(begin e1 ...)))

;;; TODO: check if var is a free variable, and if so, define it
(mac =
  ((_ var val) #'(set! var val))
  ((_ var val rest ...) #'(do (set! var val) (= rest ...))))


(mac aif x
  ((_ test then else)
   (let-syn it (datum->syntax x 'it)
     #'(let it test (if it then else)))))

(mac & ((_ e1 ...) #'(and e1 ...)))

(defp c/prmt call-with-prompt)
(defp c/vals call-with-values)
(defp tag default-prompt-tag)
(defp vals values)
(defp =? _=)
(defp 0? zero?)
(defp 1? (n) (=? 1 n))
(defp ~ not)
(defp flatn append-map)
(defp &map and-map)
(defp set\ lset-difference)
(def nil? null?)

(mac $>
  ((_ exp)           #'(c/prmt (tag) (fn () exp) hdlr))
  ((_ exp hdlr)      #'(c/prmt (tag) (fn () exp) hdlr))
  ((_ tag expr hdlr) #'(c/prmt (tag) (fn () exp) hdlr)))

(def hdlr (cont f)
  ($> (tag) (f cont) hdlr))
