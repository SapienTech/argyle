#! Ideas
- Pattern matching like haskell
  - This will allow easier macro defs!
!#

(define-syntax rule
  (syntax-rules (->)
    ((rule (exp -> exp*)) (exp exp*))))

;; Eventually use split-at!
(define (bag lst size)
  (if (< (length lst) size) '()
      (cons (take lst size)
            (bag (drop lst size) size))))

(define-syntax mac
  (lambda (x)
    (syntax-case x (->)
      ((mac keyword (aux ...) . patterns)
       #`(define-syntax keyword
           (syntax-rules (aux ...)
             #,@(map (lambda (pattern) #`(rule #,@pattern))
                     (bag (syntax->datum #'patterns) 3))))))))

(define-syntax rule-list
  (syntax-rules ()
    ((_ rules) (unless (null? rules)
                 ()))))

(define-syntax-rule (mac keyword pattern template . rest)
  (define-syntax keyword
    (syntax-rules ()
      (pattern template)))) 

;;; not sure if syntax-case is better here
(define-syntax mac
  (λ (x)
    (syntax-case x (=>)
      ((mac name (sym ...) _syntax => expansion)
       #'(define-syntax name
           (syntax-rules (sym ...)
             ((name _syntax) expansion)))))))

(define-syntax def
  (syntax-rules ()
    ((def name (arg ...) exp rest ...)
     (define (name arg ...) exp rest ...))
    ((def name val) (define name val))))

;;; Code for allowing lists to be in proc pos. Pretty cool!
(cons ,(lambda (l)
                    (lambda (i)
                      (car (ar-nthcdr
                            (if (< i 0)
                                (let* ((l (ar-denil-last l))
                                       (len (length l))
                                       (i (+ len i)))
                                  (if (< i 0) len i))
                                i)
                            l)))))
(cons ,(lambda (l) (apply string-append
                          (map (lambda (y) (ar-coerce y 'string))
                                                 (ar-denil-last l)))))
(cons    (str ,(lambda (x) (ac-niltree (string->list x)))))

#|

             adt.scm -- Basic Algebraic Data Type library
                      Pavel Panchekha, (C) 2013
                        License: BSD 3-clause


                               Summary

Algebraic data types feature prominently in a various functional
languages, like ML, Haskell, Scala, and others.  Unfortunately, Scheme
lacks support for them; this library fixes that flaw.

In all, it provides three functions:

 - data, which creates new algebraic data types
 - make, which constructs new values of an algebraic data type
 - match, which destructs values of an algebraic data type

                         Algebraic Data Types
We're all familiar with the *record* type, which is a data structure
which stores multiple values (fields) together in one structure.
Algebraic data types extend this notion to a value which stores one of
*several types* of records.  For example, a list can be written as
storing either a record of two fields (a cons cell), or a record of no
fields (nil).  Algebraic data types are perfect to represent this:

    (data our-list?
      (our-cons head tail)
      (our-nil)))
our-list? test)

    ;Unspecified return value

The symbol `our-list?` names the predicate associated with our
algebraic data type; calling `our-list?` on a value will return true
if and only if it represents a value of our new type.  Then, each of
the expressions in the body of the `data` define a variant.  In
the above example, `our-cons` is one variant of `our-list?` values,
of two fields, while `our-nil` is another variant of no fields.
To create new values of our algebraic data type, we can use `make`:

    (make our-cons 1 2)
    ;Value: (our-list? our-cons 1 2)

    (define (list->our-list lst)
      (if (null? args)
          (make our-nil)
          (make cons (car args) (list->our-list (cdr lst)))))

    (list->our-list '(1 2 3))
    ;Value: (:list? :cons 1 (:list? :cons 2 (:list? :cons 3 (:list? :nil))))

Just by itself, an algebraic data type value could be of one of
several variants of that type.  Since figuring out which variant we
have, and dispatching accordingly, is a common task, special sugar is
provided, called `match`:

    (define (our-map f lst)
      (match lst
        ((our-cons head tail)
         (make our-cons (f head) (our-map f tail)))
        ((our-nil)
         lst)))

Note the ease of destructuring algebraic data types!

If a variant is omitted from a match, the match will raise an error
when this unhandled variant is passed to it.  This is often safer than
passing this fault silently; however, if you wish, you can explicitly
handle this case:

    (define (our-car f lst)
      (match lst
        ((our-cons head tail)
         head)
        (else
         (error "Cannot take car of empty list" lst))))

|#

;;; ADT variants are implemented as tagged lists.  Each value is tagged
;;; with the name of the ADT, then the name of the variant, and finally
;;; the values of the fields.
;;;
;;; Each variant is internally represented by a function.  This
;;; function takes an operation an executes it, where operations are
;;; one of:
;;;
;;;  ! -- make a new value of this variant, by consing the ADT name and
;;;       variant name onto the rest of the arguments
;;;  ? -- test whether a value is of this variant, by checking the tags
;;;       and length.
;;;  @ -- destructure the value, by applying a function to the fields.
;;;
;;; For example, if we define an ADT by
;;;
;;;     (data :list? (:cons head tail) (:nil))
;;;
;;; Then we can create it with:
;;;
;;;    (:cons '! 1 2)
;;;    (:nil '!)
;;;
;;; Test it with
;;;
;;;    (:cons '? val)
;;;    (:nil '?  val)
;;;
;;; And destructure it with
;;;
;;;    (:cons '@ (lambda (head tail) body ...))
;;;    (:nil '! (lambda () body ...))
;;;
;;; As you can see, the actual destructuring is done by APPLY.  With
;;; this structure, it is very easy to write a MATCH macro.

;;; Trying to find a way to integrate syntax-rules with syntax-case
(define-syntax mac-test
  (lambda (x)
    (syntax-case x ()
      ((_ (k ...) ((keyword . pattern) template) ...)
       (let ((template (if (syntax?
                            (syntax->datum #'template))
                           (syntax->datum #'template)
                           #'template)))
         #'(lambda (x)
             (syntax-case x (k ...)
               ((dummy . pattern) #'template)
               ...)))))))

;;; Not sure how im going to make this work... check out guile source
#!(mac module
  ((module (base ext ...)) (define-module(base ext ...)))
  ((module base ext ...) (module (base ext ...))))

(mac use ((use modules) (use-modules modules)))
!#
(define-syntax mac
  (lambda (x)
    (syntax-case x ()
      ((mac name ((kword . pattern) template) ...)
       #'(mac name () ctx ((kword . pattern) template) ...))
      ((mac name (aux ...) ((kword . pattern) template) ...)
       (identifier? #'(aux ...))
       #'(mac name (aux ...) ctx ((kword . pattern) template) ...))
      ((mac name ctx ((kword . pattern) template) ...)
       (identifier? #'ctx)
       #'(mac name () ctx ((kword . pattern) template) ...))
      ((mac name (aux ...) ctx ((kword . pattern) template) ...)
       #'(define-syntax name
           (lambda (ctx)
             (syntax-case ctx (aux ...)
               ((kword . pattern) template) ...)))))))
