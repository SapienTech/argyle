(module (arguile monad)
  #:export (;; Monads.
            monad
            monad?
            monad-bind
            monad-return

            ;; Syntax.
            >>=
            return
            w/monad
            mlet
            mlet*
            mbegin
            mwhen
            munless
            lift0 lift1 lift2 lift3 lift4 lift5 lift6 lift7 lift
            listm
            foldm
            mapm
            sequence
            anym

            ;; Concrete monads.
            %identity-monad

            %state-monad
            state-return
            state-bind
            current-state
            set-current-state
            state-push
            state-pop
            run-with-state))
(use ((system syntax)
      #:select (syntax-local-binding))
     (arguile base)
     (arguile guile)
     (arguile data)
     (arguile loop)
     (ice-9 match)
     (srfi srfi-9)
     (srfi srfi-26))
;;; Commentary:
;;;
;;; This module implements the general mechanism of monads, and provides in
;;; particular an instance of the "state" monad.  The API was inspired by that
;;; of Racket's "better-monads" module (see
;;; <http://planet.racket-lang.org/package-source/toups/functional.plt/1/1/planet-docs/better-monads-guide/index.html>).
;;; The implementation and use case were influenced by Oleg Kysielov's
;;; "Monadic Programming in Scheme" (see
;;; <http://okmij.org/ftp/Scheme/monad-in-Scheme.html>).
;;;
;;; Code:

;; Record type for monads manipulated at run time.
(data <monad> (>>= return)
      #:init (make-monad >>= return))      ; TODO: Add 'plus' and 'zero'

(mac monad (bind return)
  ((_ name (bind b) (return r))
   (let-syn rtd (-> syn (+ (-> sym "% ") (-> dat #'name) '-rtd) #'name)
     #'(do
           ;; The data type, for use at run time.
         (def rtd (make-monad b r))
         (mac name (%bind %return)
               ;; An "inlined record", for use at expansion time.  The goal is
               ;; to allow 'bind' and 'return' to be resolved at expansion
               ;; time, in the common case where the monad is accessed
               ;; directly as NAME.
               ((_ %bind)   #'b)
               ((_ %return) #'r)
               ((_)         #'rtd))))))

(define-syntax-parameter >>=
  (lambda (s)
    (syntax-violation '>>= ">>= (bind) used outside of 'w/monad'" s)))

(define-syntax-parameter return
  (lambda (s)
    (syntax-violation 'return "return used outside of 'w/monad'" s)))

(define-syntax-rule (bind-syntax bind)
  "Return a macro transformer that handles the expansion of '>>=' expressions
using BIND as the binary bind operator.

This macro exists to allow the expansion of n-ary '>>=' expressions, even
though BIND is simply binary, as in:

  (w/monad %state-monad
    (>>= (return 1)
         (lift 1+ %state-monad)
         (lift 1+ %state-monad)))
"
  (lambda (stx)
    (define (expand body)
      (syntax-case body ()
        ((_ mval mproc)
         #'(bind mval mproc))
        ((x mval mproc0 mprocs (... ...))
         (expand #'(>>= (>>= mval mproc0)
                        mprocs (... ...))))))

    (expand stx)))

(mac w/monad
  ;; "Evaluate BODY in the context of MONAD, and return its result."
  ((_ monad body ...)
   (eq? 'macro (syntax-local-binding #'monad))
   ;; MONAD is a syntax transformer, so we can obtain the bind and return
   ;; methods by directly querying it.
   #'(syntax-parameterize ((>>=    (bind-syntax (monad %bind)))
                           (return (identifier-syntax (monad %return))))
       body ...))
  ((_ monad body ...)
   ;; MONAD refers to the <monad> record that represents the monad at run
   ;; time, so use the slow method.
   #'(syntax-parameterize ((>>=    (bind-syntax
                                    (monad-bind monad)))
                           (return (identifier-syntax
                                    (monad-return monad))))
       body ...)))

(define-syntax mlet*
  (syntax-rules (->)
    "Bind the given monadic values MVAL to the given variables VAR.  When the
form is (VAR -> VAL), bind VAR to the non-monadic value VAL in the same way as
'let'."
    ;; Note: the '->' symbol corresponds to 'is:' in 'better-monads.rkt'.
    ((_ monad () body ...)
     (w/monad monad body ...))
    ((_ monad ((var mval) rest ...) body ...)
     (w/monad monad
       (>>= mval
            (lambda (var)
              (mlet* monad (rest ...)
                body ...)))))
    ((_ monad ((var -> val) rest ...) body ...)
     (_let ((var val))
       (mlet* monad (rest ...)
         body ...)))))

(define-syntax mlet
  (lambda (s)
    (syntax-case s ()
      ((_ monad ((var mval ...) ...) body ...)
       (with-syntax (((temp ...) (generate-temporaries #'(var ...))))
         #'(mlet* monad ((temp mval ...) ...)
             (_let ((var temp) ...)
               body ...)))))))

(define-syntax mbegin
  (syntax-rules (%current-monad)
    "Bind the given monadic expressions in sequence, returning the result of
the last one."
    ((_ %current-monad mexp)
     mexp)
    ((_ %current-monad mexp rest ...)
     (>>= mexp
          (lambda (unused-value)
            (mbegin %current-monad rest ...))))
    ((_ monad mexp)
     (w/monad monad
       mexp))
    ((_ monad mexp rest ...)
     (w/monad monad
       (>>= mexp
            (lambda (unused-value)
              (mbegin monad rest ...)))))))

(define-syntax mwhen
  (syntax-rules ()
    "When CONDITION is true, evaluate EXP0..EXP* as in an 'mbegin'.  When
CONDITION is false, return *unspecified* in the current monad."
    ((_ condition exp0 exp* ...)
     (if condition
         (mbegin %current-monad
           exp0 exp* ...)
         (return *unspecified*)))))

(define-syntax munless
  (syntax-rules ()
    "When CONDITION is false, evaluate EXP0..EXP* as in an 'mbegin'.  When
CONDITION is true, return *unspecified* in the current monad."
    ((_ condition exp0 exp* ...)
     (if condition
         (return *unspecified*)
         (mbegin %current-monad
           exp0 exp* ...)))))

(define-syntax define-lift
  (syntax-rules ()
    ((_ liftn (args ...))
     (define-syntax liftn
       (lambda (s)
         "Lift PROC to MONAD---i.e., return a monadic function in MONAD."
         (syntax-case s ()
           ((liftn proc monad)
            ;; Inline the result of lifting PROC, such that 'return' can in
            ;; turn be open-coded.
            #'(lambda (args ...)
                (w/monad monad
                  (return (proc args ...)))))
           (id
            (identifier? #'id)
            ;; Slow path: Return a closure-returning procedure (we don't
            ;; guarantee (eq? LIFTN LIFTN), but that's fine.)
            #'(lambda (proc monad)
                (lambda (args ...)
                  (w/monad monad
                    (return (proc args ...))))))))))))

(define-lift lift0 ())
(define-lift lift1 (a))
(define-lift lift2 (a b))
(define-lift lift3 (a b c))
(define-lift lift4 (a b c d))
(define-lift lift5 (a b c d e))
(define-lift lift6 (a b c d e f))
(define-lift lift7 (a b c d e f g))

(define (lift proc monad)
  "Lift PROC, a procedure that accepts an arbitrary number of arguments, to
MONAD---i.e., return a monadic function in MONAD."
  (lambda args
    (w/monad monad
      (return (apply proc args)))))

(define (foldm monad mproc init lst)
  "Fold MPROC over LST and return a monadic value seeded by INIT.

  (foldm %state-monad (lift2 cons %state-monad) '() '(a b c))
  => '(c b a)  ;monadic
"
  (w/monad monad
    (loop lp ((lst    lst)
              (result init))
      (match lst
        (()
         (return result))
        ((head tail ...)
         (>>= (mproc head result)
              (lambda (result)
                (lp tail result))))))))

(define (mapm monad mproc lst)
  "Map MPROC over LST and return a monadic list.

  (mapm %state-monad (lift1 1+ %state-monad) '(0 1 2))
  => (1 2 3)  ;monadic
"
  (mlet monad ((result (foldm monad
                              (lambda (item result)
                                (>>= (mproc item)
                                     (lambda (item)
                                       (return (cons item result)))))
                              '()
                              lst)))
    (return (reverse result))))

(define-syntax-rule (sequence monad lst)
  "Turn the list of monadic values LST into a monadic list of values, by
evaluating each item of LST in sequence."
  ;; XXX: Making it a macro is a bit brutal as it leads to a lot of code
  ;; duplication.  However, it allows >>= and return to be open-coded, which
  ;; avoids struct-ref's to MONAD and a few closure allocations when using
  ;; %STATE-MONAD.
  (w/monad monad
    (loop seq ((lstx   lst)
               (result '()))
      (match lstx
        (()
         (return (reverse result)))
        ((head . tail)
         (>>= head
              (lambda (item)
                (seq tail (cons item result)))))))))

(define (anym monad mproc lst)
  "Apply MPROC to the list of values LST; return as a monadic value the first
value for which MPROC returns a true monadic value or #f.  For example:

  (anym %state-monad (lift1 odd? %state-monad) '(0 1 2))
  => #t   ;monadic
"
  (w/monad monad
    (loop lp ((lst lst))
      (match lst
        (()
         (return #f))
        ((head tail ...)
         (>>= (mproc head)
              (lambda (result)
                (if result
                    (return result)
                    (lp tail)))))))))

(define-syntax listm
  (lambda (s)
    "Return a monadic list in MONAD from the monadic values MVAL."
    (syntax-case s ()
      ((_ monad mval ...)
       (with-syntax (((val ...) (generate-temporaries #'(mval ...))))
         #'(mlet monad ((val mval) ...)
             (return (list val ...))))))))



;;;
;;; Identity monad.
;;;

(define-inlinable (identity-return value)
  value)

(define-inlinable (identity-bind mvalue mproc)
  (mproc mvalue))

(monad %identity-monad
  (bind   identity-bind)
  (return identity-return))


;;;
;;; State monad.
;;;

(define-inlinable (state-return value)
  (lambda (state)
    (values value state)))

(define-inlinable (state-bind mvalue mproc)
  "Bind MVALUE, a value in the state monad, and pass it to MPROC."
  (lambda (state)
    (call-with-values
        (lambda ()
          (mvalue state))
      (lambda (value state)
        ;; Note: as of Guile 2.0.11, declaring a variable to hold the result
        ;; of (mproc value) prevents a bit of unfolding/inlining.
        ((mproc value) state)))))

(monad %state-monad
  (bind state-bind)
  (return state-return))

(define* (run-with-state mval #:optional (state '()))
  "Run monadic value MVAL starting with STATE as the initial state.  Return
two values: the resulting value, and the resulting state."
  (mval state))

(define-inlinable (current-state)
  "Return the current state as a monadic value."
  (lambda (state)
    (values state state)))

(define-inlinable (set-current-state value)
  "Set the current state to VALUE and return the previous state as a monadic
value."
  (lambda (state)
    (values state value)))

(define (state-pop)
  "Pop a value from the current state and return it as a monadic value.  The
state is assumed to be a list."
  (lambda (state)
    (match state
      ((head . tail)
       (values head tail)))))

(define (state-push value)
  "Push VALUE to the current state, which is assumed to be a list, and return
the previous state as a monadic value."
  (lambda (state)
    (values state (cons value state))))

;;; monads.scm end here
