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

;;; TODO:: merge data-match with match
(define-module (arguile data)
  #:export (make data data-match))

(define (%adt-branch adt-name variant-name num-elts)
  (lambda (op . args)
    (apply ; This apply lets us use Scheme's parameter list length checking
     (case op
       ((!)
        (lambda elts
          (cons adt-name (cons variant-name elts))))
       ((?)
        (lambda (val)
          (and
           (list? val)
           (= (length val) (+ 2 num-elts)) ; 2 for the ADT and variant name
           (eq? (car val) adt-name)
           (eq? (cadr val) variant-name))))
       ((@)
        (lambda (val f)
          (apply f (cddr val)))))
     args)))

;; A small helper function to hide the '!
(define (make type . args)
  (apply type '! args))

#|

(define :cons (%adt-branch ':list? ':cons 2))

(:cons '! 1 2)
;Value: (:list? :cons 1 2)

(:cons '? (:cons '! 1 2))
; Value: #t

(make :cons 1 2)
;Value: (:list? :cons 1 2)

(:cons '@ (:cons '! 1 2)
          (lambda (head tail)
            `(head ,head : tail ,tail)))
;Value: (head 1 : tail 2)

|#

;; Create a function representing a top-level ADT
(define (%adt-predicate adt-name)
  (lambda val
    (and (list? val)
         (> (length val) 1)
         (eq? (car val) adt-name))))

#|

(define :list? (%adt-predicate ':list?))

(:list? (:cons '! 1 2))
#t

(:list? (:nil '!))
#t

(:list? (cons 1 2))
#f

|#

;; A data call like
;;
;;    (data :list? (:cons head tail) (:nil))
;;
;; should be roughly equivalent to:
;;
;;    (begin
;;      (define :cons (%adt-branch ':list? ':cons 2))
;;      (define :nil  (%adt-branch ':list? ':nil  0))
;;      (define :list? (%adt-predicate ':list?)))
;;
;; Technically, we expand into a nested set of BEGINs.

(define-syntax data
  (syntax-rules ()
    ((data adt-name (name1 fields1 ...) rest ...)
     (begin
       ; This (length '(fields1 ...)) trick is very useful, though
       ; doing it at compile-time would be better.
       (define name1 (%adt-branch 'adt-name 'name1 (length '(fields1 ...))))
       (data adt-name rest ...)))
    ((adt adt-name)
     (define adt-name (%adt-predicate 'adt-name)))))

#|
(data :list?
  (:cons head tail)
  (:nil))

(:list? (make :cons 1 2))
;Value: #t

(define (:map f :lst)
  (cond
   ((:cons '? :lst)
    (:cons '@ :lst (lambda (head tail)
                     (make :cons (f head) (:map f tail)))))
   ((:nil '? :lst)
    '())))

(:map (lambda (x) (* x x))
      (make :cons 3 (make :cons 1 (make :cons 4 (make :nil)))))
;Value: (:list? :cons 9 (:list? :cons 1 (:list? :cons 16 ())))

|#

;; Finally, MATCH hides the
;;
;;    (if (variant '? var)
;;        (variant '@ var (lambda (...) ...)))
;;
(define-syntax data-match
  (syntax-rules (else)
    ((data-match var ((name vars ...) body ...) rest ...)
     (begin
       (if (name '? var)
           (name '@ var (lambda (vars ...) body ...))
           (match var rest ...))))
    ((data-match var (else body ...))
     (begin body ...))
    ((data-match var)
     (error "Incomplete pattern; no match for case" var))))

#|
(define (:map f :lst)
  (match :lst
    ((:cons head :tail)
     (make :cons (f head) (:map f :tail)))
    ((:nil)
     :lst)))

(:map (lambda (x) (* x x))
      (make :cons 3 (make :cons 1 (make :cons 4 (make :nil)))))
;Value: (:list? :cons 9 (:list? :cons 1 (:list? :cons 16 ())))

|#
