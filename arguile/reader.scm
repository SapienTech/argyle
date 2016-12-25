(module (arguile reader))
(use (arguile base)
     (arguile loop)
     (arguile generic)
     (rnrs io ports))


(read-disable 'square-brackets)

;; TODO: allow nested vectors
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
