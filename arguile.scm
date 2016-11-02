(define-module (arguile))

(use-modules (arguile ssyntax)
             (arguile core)
             (arguile loop)
             (arguile nested-loop)
             (arguile io)
             (arguile type)
             (arguile generic)
             (arguile sugar)
           
             (ice-9 match)
             (ice-9 receive)
             (srfi srfi-1)
             (srfi srfi-45))
(re-export mac syn syn-case w/syn
           fn def let with do =
           loop                         ; How to re-export whole module?
           pr prn
           newtype type coerce
           + len
           \\ ret
           fold reduce
           match
           receive
           delay lazy force eager promise?)
