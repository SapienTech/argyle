(module (arguile match))
(export-syntax w/ let def)
(use (arguile base)
     ((arguile base fn)
      #:select ((def . _def)
                (let . _let)
                (w/ . _w/)))
     ((arguile data tbl) #:select (<tbl>))
     (arguile loop)
     (arguile match compat)
     ((srfi srfi-1) :select (zip reduce append-map)))

(mac def
  ((_ name exp)
   #'(_def name exp))
  ((_ name (pat ... . rst) b1 b2 ...)
   (let-syn exps #`(#,@(gen-params #'(pat ...)) . rst)
     #`(_def name exps
         (match-xpnd #,(splice (zip #'(pat ...) #'exps)) b1 b2 ...)))))

(mac let
  ((_ pat exp . bdy) #'(w/ (pat exp) . bdy)))

(mac w/
  ((_ pat/exp . bdy) #'(match-xpnd pat/exp . bdy)))

(mac if-match (:or)
     ((_ exp ((pat :or val) bdy) . rst)
      (_let pat-ids (flatten (rec-filter sym? `(,(-> dat #'pat))))
        (let-syn pat-vars (map (fn (id) (-> syn id #'pat)) pat-ids)
          #`(if exp (match exp (pat bdy) . rst)
                (_w/ #,(splice (zip #'pat-vars (map (const #f) #'pat-vars)))
                     bdy)))))
     ((_ exp (pat bdy) rst ...)
      #'(if-match exp ((pat :or #f) bdy) rst ...)))

(mac tbl-match (:keys)
  ((_ tbl ((:keys key ...) . bdy))
   #`(match tbl (($ <tbl>)
                 (w/keys (key ...) tbl
                         #,@#'bdy)))))

;;; TODO: determine if we want :keys or 'keys
(mac w/keys
  ((_ (key ...) tbl bdy)
   #`(w/ #,(splice
            (map (fn (key)
                   #`(#,key (tbl '#,key)))
                 #'(key ...)))
       bdy)))

(mac match-xpnd (:keys)
  ((_ () . bdy)
   #'(do . bdy))
  ((_ ((:keys key ...) tbl . rst) . bdy)
   #'(tbl-match tbl ((:keys key ...)
                     (match-xpnd rst . bdy))))
  ((_ (_ kwd . rst) . bdy) (keyword? (-> dat #'kwd))
   #'(op-match-xpnd rst . bdy))
  ((_ (pat exp . rst) . bdy)
   #'(match exp (pat (match-xpnd rst . bdy)))))

(mac op-match-xpnd
  ((_ () . bdy)
   #'(do . bdy))
  ((_ (pat exp . rst) . bdy)
   (do  (prn (dat #'pat) (dat #'exp))
       #'(if-match exp (pat (op-match-xpnd rst . bdy))))))

;;; TODO: determine if these can be namespaced with base/def
(eval-when (expand load eval)

  (_def gen-params (pats)
    (if (nil? pats) pats
        (syn-case pats (:o)
          ((:o . rst)
           #`(#:o #,@(gen-params #'rst)))
          ((pat . rst) #`(#,(syn (gensym) #'pat)
                          #,@(gen-params #'rst))))))
  
  (_def parse-req-pat (pat)
    (syn-case pat ()
      ((pat #:as var) #'var)
      ((_) (syn (gensym) #'pat))))

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
  (_def splice (lst)              ;TODO: use general ver.
        (reduce append '() (reverse lst))))
