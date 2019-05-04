#lang racket

(require data/heap)

(define h (make-heap
           (λ (p1 p2)
             (<= (car p1) (car p2)))))

(define (add! p) (heap-add! h p))

(add! '(1 a))
(add! '(2 b))
(add! '(3 c))
(add! '(3 d))
(add! '(3 e))
(add! '(3 f))
(add! '(3 g))
(add! '(3 h))
(add! '(3 i))
(add! '(0 i))

(for ([p (in-heap h)])
      (display p))
(heap-min h)

(heap-remove-min! h)

(heap-min h)
