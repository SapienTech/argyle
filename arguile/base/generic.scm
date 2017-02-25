(module (arguile base generic)
  #:replace (+ * length))
(use (arguile guile)
     (arguile base fn)
     (arguile base type)) 

(def + args
  (cond ((null? args) 0)
        ((num? (car args)) (apply _+ args))
        ((one-of `(,str? ,chr?) (car args))
         (apply str-join (map (\\ -> str _) args)))
        ((sym? (car args))
         (apply sym-join (map (\\ -> sym _) args)))
        (else (apply _+ args))))

;;; TODO: Add cartesian product for data
(def * args
  (cond ((null? args) 0)
        ((num? (car args)) (apply _* args))
        ((one-of `(,str? ,chr?) (car args))
         (apply str-join
                (map (fn (val) (-> str (car args)))
                     (iota (apply _* (cdr args))))))
        ((sym? (car args))
         (apply sym-join
                (map (fn (val) (-> sym (car args)))
                     (iota (apply _* (cdr args))))))
        (else (apply _* args))))

(def length (x)
  (cond ((lst? x) (_length x))
        ((str? x) (str-len x))
        ((hash-table? x) (hash-count (const #t) x))
        ((vector? x) (vector-length x))
        (else (_length x))))

(def one-of (tests val)
  (if (null? tests) #f
      (or ((car tests) val)
          (one-of (cdr tests) val))))
