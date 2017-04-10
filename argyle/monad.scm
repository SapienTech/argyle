(ns (argyle monad)
  :export (;; Monads.
            monad monad? monad-bind monad-return

            ;; Syntax.
            >>= return w/monad mlet mlet* mdo mwhen munless
            lift0 lift1 lift2 lift3 lift4 lift5 lift6 lift7 lift
            listm foldm mapm seq anym

            ;; Concrete monads.
            ident-monad state-monad
            state-return state-bind curr-state curr-state!
            state-push state-pop run-w/state))
(use ((system syntax)
      :select (syntax-local-binding))
     (argyle base) (argyle guile) (argyle data) (argyle loop)
     (argyle generic)
     (ice-9 match)
     (srfi srfi-26))

(data monad (bind return)
  :init (mke-monad bind return))      ; TODO: Add 'plus' and 'zero'

(mac monad (bind return)
  ((name (bind b) (return r))
   (let-syn data (syn (+ '% (dat #'name)) #'name)
     #'(do
           ;; The data type, for use at run time.
         (def data (mke-monad b r))
         (mac name (%bind %return)
              ;; An "inlined record", for use at expansion time. The goal is
              ;; to allow 'bind' and 'return' to be resolved at expansion time
               ((%bind)   #'b)
               ((%return) #'r)
               ((_)         #'rtd))))))

(syn-param >>=
  (fn (s)
    (syn-violation '>>= ">>= (bind) used outside of 'w/monad'" s)))

(syn-param return
  (fn (s)
    (syn-violation 'return "return used outside of 'w/monad'" s)))

(mac bind-syn
  "Return a macro transformer that handles the expansion of '>>=' expressions
using BIND as the binary bind operator.

This macro exists to allow the expansion of n-ary '>>=' expressions, even
though BIND is simply binary, as in:

  (w/monad state-monad
    (>>= (return 1)
         (lift 1+ state-monad)
         (lift 1+ state-monad))) "
  ((bind)
   #'(fn (stx)
       (def (expand body)
           (syntax-case body ()
             ((mval mproc)
              #'(bind mval mproc))
             ((x mval mproc0 mprocs (... ...))
              (expand #'(>>= (>>= mval mproc0)
                             mprocs (... ...))))))

       (expand stx))))

(mac w/monad
  "Evaluate BODY in the context of MONAD, and return its result."
  ((monad body ...)
   (if (eq? 'macro (syntax-local-binding #'monad))
       ;; Expansion time
       #'(w/syn-params ((>>= (bind-syn (monad %bind)))
                        (return (identifier-syntax (monad %return))))
           body ...)
       ;; Run time
       #'(w/syn-params ((>>= (bind-syn
                              (monad-bind monad)))
                        (return (identifier-syntax
                                 (monad-return monad))))
           body ...))))

(mac mlet* (->)
  "Bind the given monadic vals MVAL to the given variables VAR.  When the
form is (VAR -> VAL), bind VAR to the non-monadic value VAL in the same way as
'let'."
  ((monad () body ...)
   #'(w/monad monad body ...))
  ((monad ((var mval) rest ...) body ...)
   #'(w/monad monad
       (>>= mval
            (fn (var)
              (mlet* monad (rest ...)
                body ...)))))
  ((monad ((var -> val) rest ...) body ...)
   #'(let var val
       (mlet* monad (rest ...)
         body ...))))

(mac mlet
  ((monad ((var mval ...) ...) body ...)
   (let-syn (temp ...) (gen-tmps #'(var ...))
     #'(mlet* monad ((temp mval ...) ...)
         (_let ((var temp) ...)
           body ...)))))

(mac mdo 
  "Bind the given monadic expressions in seq, returning the result of
the last one."
  ((%curr-monad mexp) #'mexp)
  ((%curr-monad mexp rest ...)
   #'(>>= mexp
          (fn (unused-value)
            (mdo %curr-monad rest ...))))
  ((monad mexp)
   #'(w/monad monad mexp))
  ((monad mexp rest ...)
   #'(w/monad monad
       (>>= mexp
            (fn (unused-value)
              (mdo monad rest ...))))))

(mac mwhen
  "When CONDITION is true, evaluate EXP0..EXP* as in an 'mdo'.  When
CONDITION is false, return *unspecified* in the curr monad."
  ((condition exp0 exp* ...)
   #'(if condition
         (mdo %curr-monad
           exp0 exp* ...)
         (return *unspecified*))))

(mac munless
  "When CONDITION is false, evaluate EXP0..EXP* as in an 'mdo'.  When
  CONDITION is true, return *unspecified* in the curr monad."
  ((condition exp0 exp* ...)
   #'(if condition
         (return *unspecified*)
         (mdo %curr-monad
           exp0 exp* ...))))

(mac def-lift
  ((liftn (args ...))
   #'(mac liftn
       "Lift PROC to MONAD---i.e., return a monadic function in MONAD."
       ((liftn proc monad)
        ;; Inline the result of lifting PROC, such that 'return' can in
        ;; turn be open-coded.
        #'(fn (args ...)
            (w/monad monad
              (return (proc args ...)))))
       ((liftn id)
        (id? #'id)
        ;; Slow path: Return a closure-returning procedure (we don't
        ;; guarantee (eq? LIFTN LIFTN), but that's fine.)
        #'(fn (proc monad)
            (fn (args ...)
              (w/monad monad
                (return (proc args ...)))))))))

(def-lift lift0 ())
(def-lift lift1 (a))
(def-lift lift2 (a b))
(def-lift lift3 (a b c))
(def-lift lift4 (a b c d))
(def-lift lift5 (a b c d e))
(def-lift lift6 (a b c d e f))
(def-lift lift7 (a b c d e f g))

(def lift (proc monad)
  "Lift PROC, a procedure that accepts an arbitrary number of arguments, to
MONAD---i.e., return a monadic function in MONAD."
  (fn args
    (w/monad monad
      (return (apply proc args)))))

(def foldm (monad mproc init lst)
  "Fold MPROC over LST and return a monadic value seeded by INIT.

  (foldm state-monad (lift2 cons state-monad) '() '(a b c))
  => '(c b a)  ;monadic "
  (w/monad monad
    (loop lp ((lst    lst)
              (result init))
      (match lst
        (()
         (return result))
        ((head tail ...)
         (>>= (mproc head result)
              (fn (result)
                (lp tail result))))))))

(def mapm (monad mproc lst)
  "Map MPROC over LST and return a monadic list.

  (mapm state-monad (lift1 1+ state-monad) '(0 1 2))
  => (1 2 3)  ;monadic"
  (mlet monad ((result (foldm monad
                              (fn (item result)
                                (>>= (mproc item)
                                     (fn (item)
                                       (return (cons item result)))))
                              '()
                              lst)))
    (return (rev result))))


;; XXX: Making it a macro is a bit brutal as it leads to a lot of code
;; duplication.  However, it allows >>= and return to be open-coded, which
;; avoids struct-ref's to MONAD and a few closure allocations when using
;; STATE-MONAD.
(mac seq
  "Turn the list of monadic vals LST into a monadic list of vals, by
evaluating each item of LST in seq."
  ((monad lst)
   #'(w/monad monad
       (loop seq ((lstx   lst)
                  (result '()))
         (match lstx
           (()
            (return (rev result)))
           ((head . tail)
            (>>= head
                 (fn (item)
                   (seq tail (cons item result))))))))))

(def anym (monad mproc lst)
  "Apply MPROC to the list of vals LST; return as a monadic value the first ;
value for which MPROC returns a true monadic value or #f.  For example:

  (anym state-monad (lift1 odd? state-monad) '(0 1 2))
  => #t   ;monadic
"
  (w/monad monad
    (loop lp ((lst lst))
      (match lst
        (()
         (return #f))
        ((head tail ...)
         (>>= (mproc head)
              (fn (result)
                (if result
                    (return result)
                    (lp tail)))))))))

(mac listm
  "Return a monadic list in MONAD from the monadic vals MVAL."
  ((monad mval ...)
   (let-syn (val ...) (gen-tmps #'(mval ...))
     #'(mlet monad ((val mval) ...)
         (return (list val ...))))))

(inline identity-return (value)
  value)

(inline identity-bind (mvalue mproc)
  (mproc mvalue))

(monad ident-monad
       (bind identity-bind)
       (return identity-return))

(inline state-return (value)
  (fn (state)
    (vals value state)))

(inline state-bind (mvalue mproc)
  "Bind MVALUE, a value in the state monad, and pass it to MPROC."
  (fn (state)
    (c/vals
     (fn ()
       (mvalue state))
     (fn (value state)
       ;; Note: as of Guile 2.0.11, declaring a variable to hold the result
       ;; of (mproc value) prevents a bit of unfolding/inlining.
       ((mproc value) state)))))

(monad state-monad
  (bind state-bind)
  (return state-return))

(def run-w/state (mval #:o (state '()))
  "Run monadic value MVAL starting with STATE as the initial state.  Return
two vals: the resulting value, and the resulting state."
  (mval state))

(inline curr-state ()
  "Return the curr state as a monadic value."
  (fn (state)
    (vals state state)))

(inline curr-state! (value)
  "Set the curr state to VALUE and return the previous state as a monadic
value."
  (fn (state)
    (vals state value)))

(def state-pop ()
  "Pop a value from the curr state and return it as a monadic value.  The
state is assumed to be a list."
  (fn (state)
    (match state
      ((head . tail)
       (vals head tail)))))

(def state-push (value)
  "Push VALUE to the curr state, which is assumed to be a list, and return
the previous state as a monadic value."
  (fn (state)
    (vals state (cons value state))))
