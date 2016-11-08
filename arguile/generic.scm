(define-module (arguile generic)
  #:export (+ * len))

(use-modules (arguile core)
             (arguile type)
             (arguile guile)
             (arguile sugar))

;;; TODO: goops instead?

(def + args
  (cond ((null? args) 0)
        ((one-of `(,string? ,char?) (car args))
         (apply string-append
                (map (\\ coerce _ 'str) args)))
        ((symbol? (car args))
         (apply symbol-append
                (map (\\ coerce _ 'sym) args)))
        (else (apply _+ args))))

(def * args
  (cond ((null? args) 0)
        ((one-of `(,string? ,char?) (car args))
         (apply string-append
                (map (fn (val)
                         (coerce (car args) 'str))
                     (iota (apply _* (cdr args))))))
        ((symbol? (car args))
         (apply symbol-append
                (map (fn (val)
                         (coerce (car args) 'sym))
                     (iota (apply _* (cdr args))))))
        (else (apply _* args))))

(def one-of (tests val)
  (if (null? tests) #f
      (or ((car tests) val)
          (one-of (cdr tests) val))))

(def len (x)
  (cond ((string? x) (string-length x))
        ((hash-table? x) (hash-count (const #t) x))
        ((vector? x) (vector-length x))
        (else (length x))))
