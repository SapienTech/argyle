(module (arguile tests tssyntax))
(use (arguile ssyntax)
     (arguile core))

(mac test0
  ((a) #'#t))

(mac test1 x
  ((a)
   (with-syntax ((t (datum->syntax x 't)))
     #'(let t #t t))))
