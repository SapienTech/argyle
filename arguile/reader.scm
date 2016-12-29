(module (arguile reader))
(use (arguile base)
     (arguile loop)
     (arguile generic)
     (arguile guile)                    ; tmp
     (arguile data tbl)
     (ice-9 regex)
     (ice-9 match)                      ; tmp
     (rnrs io ports)
     ((srfi srfi-1) #:select (reduce zip delete-duplicates)))

(read-disable 'square-brackets)

;;; TODO: pre-compute regex
;;; make recursive
(def readr-xtnd (chr ctor #:o strt end)
  (def parse (prt)
    (def parse-sym (sym-exp)
      (loop lp ((for exp (in-list (str-split regx (str sym-exp)))))
        => '()
        (cond                           ; match fails here for some reason
         ((string=? exp end) '())
         (else `(,(->dat exp) ,@(lp))))))
    (let nxt (get-datum prt)
      (cond ((eof-object? nxt) '())
            ((sym? nxt) (parse-sym nxt)) ; Assumind we are at the end
            (else `(,nxt ,@(parse prt))))))
  (def regx (join "[" end strt "]"))
  
  (read-hash-extend chr
    (fn (chr prt)
        `(,ctor ,@(parse prt)))))

(def str-split (regx str)
  (let matchs (list-matches regx str)
    (w/ (strts (map match:start matchs)
         ends (map match:end matchs)
         idxs `(0 ,@(splice (zip strts ends)) ,(len str)))
      ;; Use until
      (loop lp ((idxs (delete-duplicates idxs)))
        (if (< (_length idxs) 2) '()
            `(,(substring str (car idxs) (cadr idxs))
              ,@(lp (cdr idxs))))))))

(def splice (lst)
  (reduce join '() lst))

(def ->dat (str)
  (aif (str->num str) it (sym str)))


;; TODO: allow nested vectors
#;
(read-hash-extend
 #\[
 (fn (chr prt)
   (let end "]"
     `(vector ,@(loop lp ((nxt (get-datum prt)))
                  (if (symbol? nxt)
                      (let matches (list-matches end (symbol->string nxt))
                        (if (~(nil? matches))
                            (let match (car matches)
                              `(,@(let sub-str (substring
                                                (match:string match) 0
                                                (match:start match))
                                    (if (string=? sub-str "") '()
                                        (aif (string->number sub-str) `(,it)
                                             `(,(string->symbol sub-str)))))))
                            `(,nxt ,@(lp (read prt)))))
                      `(,nxt ,@(lp (read prt)))))))))


;; TODO: abstract
#;
(read-hash-extend
 #\{
 (fn (chr prt)
   (let end "}"
     `(tbl-init ,@(loop lp ((nxt (get-datum prt)))
                    (if (symbol? nxt)
                        (let matches (list-matches end (symbol->string nxt))
                          (if (~(nil? matches))
                              (let match (car matches)
                                `(,@(let sub-str (substring
                                                  (match:string match) 0
                                                  (match:start match))
                                      (if (string=? sub-str "") '()
                                          (aif (string->number sub-str) `(,it)
                                               `(,(string->symbol sub-str)))))))
                              `(,nxt ,@(lp (read prt)))))
                        `(,nxt ,@(lp (read prt)))))))))
