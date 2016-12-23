(define-module (arguile reader))

;;; Will use default scm until i add my own arguile geiser file...

;;; First assuming that we can't do inner quoting
;;; Doesn't work to quote things sequentially?
(read-hash-extend
 #\[
 (lambda (chr prt)
   (let lp ((acc '(#\( #\#)))
     (let ((nxt (read-char prt)))
       (case nxt
         ((#\]) (string->symbol
                 (list->string
                  (reverse (cons #\) acc)))))
         (else (lp (cons nxt acc))))))))

(read-hash-extend
 #\y
 (lambda (chr prt)
   "(* 2 3)"))

;;; The ones that i want: vector and map rules
;;; TODO: build some sweet reader macros
