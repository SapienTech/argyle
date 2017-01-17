(module (arguile match))
(export-syntax w/ let def)
(use (arguile base)
     ((arguile base fn)
      #:select ((def . _def)
                (let . _let)
                (w/ . _w/)))
     (arguile loop)
     (arguile match compat)
     ((srfi srfi-1) :select (zip reduce append-map)))

(mac def
  ((_ name (pat ... . rst) . bdy)
   (let-syn exps #`(#,@(gen-params #'(pat ...)) . rst)
     #`(_def name exps
         (match-xpnd #,(splice (zip #'(pat ...) #'exps)) . bdy)))))

(mac let
  ((_ pat exp . bdy) #'(w/ (pat exp) . bdy)))

(mac w/
  ((_ pat/exp . bdy) #'(match-xpnd pat/exp . bdy)))

(mac if-match
  ((_ exp (pat bdy) . rst)
   (_let pat-ids (flatten (rec-filter sym? `(,(-> dat #'pat))))
     (let-syn pat-vars (map (fn (id) (-> syn id #'pat)) pat-ids)
       #`(if exp (match exp (pat bdy) . rst)
             (_w/ #,(splice (zip #'pat-vars (map (const #f) #'pat-vars)))
               bdy))))))

(mac match-xpnd
  ((_ () . bdy)
   #'(do . bdy))
  ((_ (kwd _ . rst) . bdy) (keyword? (-> dat #'kwd))
   #'(op-match-xpnd rst . bdy))
  ((_ (pat exp . rst) . bdy)
   #'(match exp (pat (match-xpnd rst . bdy)))))

(mac op-match-xpnd
  ((_ () . bdy)
   #'(do . bdy))
  ((_ (pat exp . rst) . bdy)
   #'(if-match exp (pat (op-match-xpnd rst . bdy)))))

(eval-when (expand load eval)
  (_def gen-params (pats)
    (loop ((for pat (in-list pats))
           (where params '() (cons (if (keyword? (-> dat pat)) pat
                                       (-> syn (gensym) pat))
                                   params)))
        => (reverse params)))
  
  (_def flatten (lst)
    (append-map (fn (elt)
                  (if (list? elt) (flatten elt)
                      `(,elt)))
                lst))
  
  (_def rec-filter (pred lst)
    (loop lp ((for elt (in-list lst)))
      => '()
      (cond ((list? elt)
             (_let filtered-elt (rec-filter pred elt)
               (if (nil? filtered-elt) (lp)
                   (cons filtered-elt (lp)))))
            ((pred elt) (cons elt (lp)))
            (else (lp)))))
  
  (_def ids (pat)
    (flatten
     (loop lp ((for pat (in-list pat)))
       (cond ((identifier? pat) (list pat))
             ((list? pat) (ids pat)))
       pat)))
  (_def splice (lst)                     ;TODO: use general ver.
    (reduce append '() (reverse lst))))
