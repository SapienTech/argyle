(module (arguile data queue)
  #:export (make-q q? q-nil?
            q-fn q-fn!
            q-hd q-tl q-len
            enq! deq!
            q->lst lst->q
            q->vec vec->q
            queue-empty-condition?))

(use (arguile base)
     (arguile data)
     (arguile data vec)
     (arguile generic)
     (rnrs conditions))

(data queue (len hd tl)
  ((len q-len q-len!)
   (hd q-hd q-hd!)
   (tl q-tl q-tl!))
  (case-lambda
    (() (deq! self))
    ((k) (enq! self k))))

(def make-q ()
  (make-queue 0 '() '()))

(def q-nil? (q) (0? (q-len q)))

(def enq! (q obj)
  (q-tl! q (cons obj (q-tl q)))
  (q-len! q (1+ (q-len q))))

(def deq! (q)
  (when (q-nil? q)
    (raise (condition
            (make-queue-empty-condition)
            (make-who-condition 'dequeue)
            (make-message-condition "There are no elements to dequeue")
            (make-irritants-condition (list queue)))))
  (with (h (q-hd q)
         t (q-tl q))
    (q-len! q (1- (q-len q)))
    (if (nil? h)
        (let h* (reverse t)
          (q-hd! q (cdr h*))
          (q-tl! q '())
          (car h*))
        (do
          (q-hd! q (cdr h))
          (car h)))))

(def lst->q (lst)
  (make-queue (len lst) lst '()))
  
(def q->lst (q)
  (join (q-hd q)
        (rev (q-tl q))))

;;; Could be more efficient
(def vec->q (compose lst->q vec->lst))
(def q->vec (compose lst->vec q->lst))

;;; TODO: make less verbose
(define-condition-type &queue-empty
  &assertion
  make-queue-empty-condition
  queue-empty-condition?)
