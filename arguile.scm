(module (arguile)
  #:re-export
  (mac syn-case syn? syn-case w/syn syn->dat dat->syn
   fn def let with do fn-case & = =? 0? 1?
   loop                         ; How to re-export whole module?
   pr prn
   type coerce ->
   data data-type? data?
   generic extend + * len join rev
   \\ ret
   fold reduce
   match
   receive
   delay lazy force eager promise?))
(use (arguile ssyntax)
     (arguile core)
     (arguile loop)
     (arguile nested-loop)
     (arguile io)
     (arguile type)
     (arguile data)
     (arguile generic)
     (arguile sugar)
     
     (ice-9 match)
     (ice-9 receive)
     (srfi srfi-1)
     (srfi srfi-45))
