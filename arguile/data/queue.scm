(module (arguile data queue)
  #:export (q make-q q? q-nil?
            enq! deq!
            q-hd q-len q-fn q-fn!
            q->lst lst->q q->vec vec->q))

(use (arguile base)
     (arguile data)
     (arguile data vec)
     (arguile generic)
     (arguile error))

(data q (len hd tl)
      #:init (%make-q len hd tl)
      #:app (case-lambda
              (() (deq! self))
              ((k) (enq! self k))))

(defp make-q () (%make-q 0 '() '()))
(defp q-nil? (q) (0? (q-len q)))

(defp enq! (q obj)
  (q-tl! q (cons obj (q-tl q)))
  (q-len! q (1+ (q-len q))))

(defp deq! (q)
  (if (q-nil? q) (err "Can't dequeue an empty queue!")
      (%deq! q)))

(def %deq! (q)
  (when (nil? (q-hd q)) (move-tl->hd! q))
  (ret val (car (q-hd q))
       (q-len! q (1- (q-len q)))
       (q-hd! q (cdr (q-hd q)))))

(def move-tl->hd! (q)
  (q-hd! q (rev (q-tl q)))
  (q-tl! q '()))

(defp q->lst (q)
  (join (q-hd q) (rev (q-tl q))))

(defp lst->q (lst)
  (%make-q (len lst) lst '()))

(defp q->vec (compose lst->vec q->lst))
(defp vec->q (compose lst->q vec->lst))
