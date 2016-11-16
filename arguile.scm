(module (arguile))

(use (arguile base)
     (arguile type)
     (arguile generic)
     (arguile data)
     (arguile loop)
     (arguile nested-loop)
     (arguile io)
     
     (ice-9 match)
     (ice-9 receive)
     (srfi srfi-1)
     (srfi srfi-45))

(re-export-modules
 (arguile base)
 (arguile type)
 (arguile generic)
 (arguile data)
 (arguile loop)
 (arguile nested-loop)
 (arguile io)
 
 (ice-9 match)
 (ice-9 receive)
 (srfi srfi-1)
 (srfi srfi-45))
