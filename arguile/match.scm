(module (arguile match))
(use (arguile base)
     (arguile match compat)
     ((srfi srfi-1) :select (zip reduce)))

(mac mdef
  (def splice (lst)                     ;TODO: use general ver
    (reduce append '() (reverse lst)))
  ((_ name (pat ...) . bdy)
   (let-syn exps (gen-tmps #'(pat ...))
     #`(def name exps
         (match-xpnd #,(splice (zip #'(pat ...) #'exps)) . bdy)))))

(mac mlet
  ((_ pat exp . bdy) #'(mw/ (pat exp) . bdy)))

(mac mw/
  ((_ pat/exp . bdy) #'(match-xpnd pat/exp . bdy)))

(mac match-xpnd
  ((_ () . bdy)
   #'(do . bdy))
  ((_ (pat exp . rst) . bdy)
   #'(match exp (pat (match-xpnd rst . bdy)))))


