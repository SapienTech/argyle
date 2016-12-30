(module (arguile reader))
(use (arguile base)
     (arguile loop)
     (arguile generic)
     (arguile guile)                    ; tmp
     (arguile data tbl)
     (ice-9 regex)
     (ice-9 match)                      ; tmp
     (rnrs io ports)
     ((srfi srfi-1) #:select (reduce zip delete-duplicates drop)))

(read-disable 'square-brackets)

;;; TODO: better names
(def xtnd-readr (chr ctor #:o strt end)
  (def regx (join "[" end strt "]"))
  (def mke-buff (sym-exp)
    (str-split regx (str sym-exp)))
  (def mke-struct (obj prt #:o (buff '()))
    (if (nil? buff)
        (let nxt (get-datum prt)
          (cond ((eof-object? nxt) obj)
                ((sym? nxt) (mke-struct obj prt (mke-buff nxt)))
                (else (mke-struct (cons nxt obj) prt))))
        (cond ((end? end (car buff))
               (vals obj (cdr buff)))
              ((strt? strt (car buff))
               (let (obj* buff) (mke-struct `(,ctor) prt (cdr buff))
                    (mke-struct (cons (rev obj*) obj) prt buff)))
              (else (mke-struct (cons (->dat (car buff)) obj)
                                prt
                                (cdr buff))))))
  (read-hash-extend chr
    (fn (chr prt)
      (let (obj buff) (mke-struct `(,ctor) prt)
           (if (nil? buff) (rev obj)
               (error "reader broke, alpha software :("))))))

(def strt? (strt obj) (and (str? obj) (string=? obj strt)))
(def end?  (end obj)  (and (str? obj) (string=? obj end)))

(def str-split (regx str)
  (let matchs (list-matches regx str)
    (w/ (strts (map match:start matchs)
         ends (map match:end matchs)
         idxs `(0 ,@(splice (zip strts ends)) ,(len str)))
      ;; TODO: use until
      (loop lp ((idxs (delete-duplicates idxs)))
        (if (< (_length idxs) 2) '()
            `(,(substring str (car idxs) (cadr idxs))
              ,@(lp (cdr idxs))))))))

(def splice (lst)
  (reduce join '() (rev lst)))

(def ->dat (str)
  (aif (str->num str) it (sym str)))

(xtnd-readr #\[ 'vector "[" "]")
(xtnd-readr #\{ 'tbl-init "{" "}")
