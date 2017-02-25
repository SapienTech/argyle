(module (arguile match))
(export-syntax w/ let def)
(use (arguile base)
     ((arguile base fn)
      #:select ((def . _def)
                (let . _let)
                (w/ . _w/)
                (fn . _fn)))
     ((arguile data tbl) #:select (<tbl>))
     (arguile loop)
     (arguile match compat)
     ((srfi srfi-1) :select (zip reduce append-map)))

(mac def
  ((name exp)
   #'(_def name exp))
  ((name (pat ... . rst) b1 b2 ...)
   (let-syn pat:exps #`(#,@(parse-params #'(pat ...)) . rst)
     #`(_def name #,(map cadr #'pat:exps)
         (match-xpnd #,(splice #'pat:exps) b1 b2 ...)))))

(mac let
  ((pat exp . bdy) #'(w/ (pat exp) . bdy)))

(mac w/
  ((pat:exp . bdy) #'(match-xpnd pat:exp . bdy)))

;;; TODO: modularize w/ def
(mac fn
  (((pat ... . rst) b1 b2 ...)
   (let-syn pat:exps #`(#,@(parse-params #'(pat ...)) . rst)
     #`(_fn #,(map cadr #'pat:exps)
         (match-xpnd #,(splice #'pat:exps) b1 b2 ...)))))

(mac match-xpnd (:keys)
  ((() . bdy)
   #'(do . bdy))
  ((((:keys key ...) tbl . rst) . bdy)
   #'(tbl-match tbl ((:keys key ...)
                     (match-xpnd rst . bdy))))
  (((kwd kwd' . rst) . bdy) (kwd? (-> dat #'kwd))
   #'(op-match-xpnd rst . bdy))
  (((pat exp . rst) . bdy)
   #'(match exp (pat (match-xpnd rst . bdy)))))

(mac tbl-match (:keys)
  ((tbl ((:keys key ...) . bdy))
   #`(match tbl (($ <tbl>)
                 (w/keys (key ...) tbl
                         #,@#'bdy)))))

;;; TODO: determine if we want :keys or 'keys
(mac w/keys
  (((key ...) tbl bdy)
   #`(w/ #,(splice
            (map (_fn (key)
                   #`(#,key (tbl '#,key)))
                 #'(key ...)))
       bdy)))

;;; TODO: modularize
(mac if-match (:or)
     ((exp ((pat :or val) bdy) . rst)
      (_let pat-ids (flatten (rec-filter sym? `(,(-> dat #'pat))))
            (let-syn pat-vars (map (_fn (id) (-> syn id #'pat)) pat-ids)
              #`(if exp (match exp (pat bdy) . rst)
                    (match val (pat bdy) . rst)))))
     ((exp (pat bdy) . rst)
      (_let pat-ids (flatten (rec-filter sym? `(,(-> dat #'pat))))
            (let-syn pat-vars (map (_fn (id) (-> syn id #'pat)) pat-ids)
              #`(if exp (match exp (pat bdy) . rst)
                    (_w/ #,(splice (zip #'pat-vars (map (const #f) #'pat-vars)))
                         bdy))))))

(mac op-match-xpnd
  ((() . bdy)
   #'(do . bdy))
  (((pat exp . rst) . bdy)
   #'(if-match exp (pat (op-match-xpnd rst . bdy)))))

;;; TODO: determine if these can be namespaced with base/def
(eval-when (expand load eval)

  ;; TODO: mac instead?
  (_def parse-params (params)
    (if (nil? params) params
        (syn-case params (:o :as)
          ((:o . rst)
           #`((#:o #:o) #,@(parse-params #'rst)))
          (((pat :as var) . rst)
           #`((pat var) #,@(parse-params #'rst)))
          ((pat . rst) #`((pat #,(syn (gensym) #'pat))
                          #,@(parse-params #'rst))))))
  
  (_def parse-req-pat (pat)
    (syn-case pat ()
      ((pat #:as var) #'var)
      ((_) (syn (gensym) #'pat))))

  (_def flatten (lst)
    (append-map (_fn (elt)
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
