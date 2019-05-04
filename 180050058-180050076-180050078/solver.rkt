#lang racket
(require racket/mpair)
(require data/heap)
(define stack '())
(define fl 0)
(define ans '())
;(require "comp-solver.rkt")
(provide all-defined-out)
(provide bfs dfs start ans vis m n 2d-vector-set! 2d-vector-ref player-pos setp setm! setn! sets!)
;(displayln "hey")
; player cant occupy destination spot
; ans will store the final ans as (list number of moves ,move ('u,'d,'l,'r) , vector,player coordi)
; stack element-> (list number of moves ,move ('u,'d,'l,'r) , vector,player coordi))
; box by 'b , goal by'o , wall by 'w, player by 'p, empty by 'e, '* box on destination,'q means player on 'o
(define-syntax while
 ; (begin (display "hey")
  (syntax-rules ()
    [(while conditions statements ...)
     (begin (define (iter)
              (cond [conditions (begin statements ... (iter))]))
            (iter))]))

(define-syntax for
  (syntax-rules (:)
    [(for init : condition : step : statements)
       (begin
         init
         (define (iter)
           (cond [condition (begin statements
                                   step
                                   (iter))]))
         (iter))]))
(define (manhattan v)
  (define i 0)
  (define j 0)
  (define sum 0)
  (define k1 (list->mlist '()))
  (define k2 (list->mlist '()))
  (begin (for (begin (set! i 0) (set! j 0)) : (and (< i m) (< j n)) : (begin (set! i (+ i 1)) (set! j (+ j 1))) :
           (cond [(equal? 'b (2d-vector-ref v i j)) (set! k1 (mappend! k1 (mlist (cons i j))))]
                 [(or (equal? 'q (2d-vector-ref v i j)) (equal? 'o (2d-vector-ref v i j))) (set! k2 (mappend! k2 (mlist (cons i j))))]))
         (displayln (mlist->list k1)) (displayln (mlist->list k2))
         (for (begin (set! i 0) (set! j 0)) : (and (< i (length (mlist->list k1))) (< j (length (mlist->list k2)))) : (begin (set! i (+ i 1)) (set! j (+ j 1))) :
           (let* ([p1 (list-ref (mlist->list k1) i)]
                 [p2 (list-ref (mlist->list k2) j)])(set! sum (+ sum (abs (- (car p1) (car p2))) (abs (- (cdr p1) (cdr p2))))))) sum))
         
  ; k1 mai box and k2 m goals then manhattan by nested loops

(define i 0)
(define sum 0)
(for (begin
       (set! i 0)
       (set! sum 0)) :
  (< i 10) :
  (set! i (+ i 1)) :
  (set! sum (+ sum i)))
;(while (> j 0)
 ;       (set! j (- j 1))
  ;     (set! y (+ y j)))
;(define (push frame) (set! stack (cons frame stack)))
(define (pop) (if (null? stack) (error "Empty stack")
                        (set! stack (cdr stack))))
(define (top) (if (null? stack) (error "Empty stack")
                        (car stack)))
(define vis (mlist '()))

(define (make-2d-vector r c initial)
  (build-vector r (lambda (x) (build-vector c (lambda (y) initial)))))

(define (2d-vector-ref vec r c)
  (if (or (< r 0) (< c 0) (>= r m) (>= c n)) 'w
  (vector-ref (vector-ref vec r) c)))

(define (2d-vector-set! vec r c val)
  (let ((v (vector-ref vec r)))
    (begin
      (vector-set! v c val))))
(define m 7)
(define n 7)
(define (setm! k)
  (set! m k))
(define (setn! k)
  (set! n k))

;(define start '#(#(w e w e e e w) #(w e e e e e w) #(w b b e e e w) #(w e o o e e w) #(w p o b e e w) #(w o e e e e w)))
(define start (make-2d-vector m n 'e ))
(define (sets! m n)
  (set! start (make-2d-vector m n 'e)))
;;(2d-vector-set! start 1 1 'o)
;;(2d-vector-set! start 1 2 'e)
;;(2d-vector-set! start 1 3 'e)
;;(2d-vector-set! start 2 1 'e)
;(2d-vector-set! start 1 1 'o)
;(2d-vector-set! start 2 1 'b)
;(2d-vector-set! start 1 2 'o)
;(2d-vector-set! start 2 2 'b)
;(2d-vector-set! start 3 1 'o)
;(2d-vector-set! start 4 1 'p)
;
;(2d-vector-set! start 3 2 'e)
;(2d-vector-set! start 4 2 'e)
;(2d-vector-set! start 4 2 'o)
(define player-pos (cons 4 1))
(define (setp i j)
  (set! player-pos (cons i j)))
;(push (list 0 'n start (cons 3 2) '()))

(define (empty stack)
  (set! stack '()))

(define (is-valid? move vec)
  #t)

(define (is-sol? vec)
  ;(displayln vec)
  (if (not vec) #f
  (let ([l1 (vector->list vec)])
   ;(displayln l1)
    (andmap (lambda (x) (andmap (lambda (y) (not (equal? y 'b))) (vector->list x))) l1))))

(define (not-deadlock? vec)
  (define vec (build-vector m (lambda (x) (build-vector n (lambda (y) (cons x y)))))) 
  (define (f r c)
    (if(not (equal? 'b (2d-vector-ref vec r c))) #t
       (let ([sym (2d-vector-ref vec r c)]
             [right (equal? 'w (2d-vector-ref vec r (+ c 1) ))]
             [left (equal? 'w (2d-vector-ref vec r (- c 1)))]
             [up (equal? 'w (2d-vector-ref vec (+ r 1) c))]
             [down (equal? 'w (2d-vector-ref vec (- r 1) c))])
         (if (or (and up (or left right)) (and down (or left right))) #f #t)))) 
   (andmap (lambda (x) (andmap (lambda (y) (f (car y) (cdr y))) (vector->list x)))  (vector->list vec)))




(define (check v2 pr pc ar ac )
 ; (displayln v2);ar means rowadd , ac coladd
(define ans '())
(define v (build-vector m (lambda (x) (build-vector n (lambda (y) (2d-vector-ref v2 x y))))))
  (define p1r (+ pr ar))
   (define p1c (+ pc ac))
   (define p2r (+ p1r ar))
   (define p2c (+ p1c ac))
   (define s1 (2d-vector-ref v p1r p1c))
   (define s2 (2d-vector-ref v p2r p2c))
   (define s0 (2d-vector-ref v pr pc))
   (cond [ (equal? s1 'e) (begin (if (equal? s0 'p) (2d-vector-set! v pr pc 'e) (2d-vector-set! v pr pc 'o)) (2d-vector-set! v p1r p1c 'p) v)]
         [(equal? s1 'o) (begin (if (equal? s0 'p) (2d-vector-set! v pr pc 'e) (2d-vector-set! v pr pc 'o))
                         (2d-vector-set! v p1r p1c 'q) v)]
   
         [ (and (equal? s1 'b) (equal? s2 'o))  (begin (if (equal? s0 'p) (2d-vector-set! v pr pc 'e) (2d-vector-set! v pr pc 'o))
                                                       (2d-vector-set! v p1r p1c 'p)
                                                       (2d-vector-set! v p2r p2c '*) v)]
         [ (and (equal? s1 'b) (equal? s2 'e))  (begin (if (equal? s0 'p) (2d-vector-set! v pr pc 'e) (2d-vector-set! v pr pc 'o))
                                                       (2d-vector-set! v p1r p1c 'p)
                                                       (2d-vector-set! v p2r p2c 'b) v)]
         
         ;[ (and (equal? s1 '*) (equal? s2 'o))  (begin (2d-vector-set! v pr pc 'e) (2d-vector-set! v p1r p1c 'p)
          ;                                             (2d-vector-set! v p2r p2c '*) v)]
         ;[ (and (equal? s1 '*) (equal? s2 'e))  (begin (2d-vector-set! v pr pc 'e) (2d-vector-set! v p1r p1c 'p)
          ;                                             (2d-vector-set! v p2r p2c '*) v)]
         [else #f]
         ))
(define (dfs)
  (define (push x)
    (set! stack (cons x stack)))
  (begin (set! fl 0)
(set! stack '())
(set! vis (mlist '()))
(push (list 0 'n start player-pos '()))  (strat push)))
(define (bfs)
  (define (push x)
    (set! stack (append stack (list x))))
  (begin (set! fl 0)
(set! stack '())
(set! vis (mlist '()))
(push (list 0 'n start player-pos '())) (strat push)))
(define (strat push )
  ;(displayln (top))
  ;(displayln fl)
  (while (and (not (null? stack)) (equal? fl 0))
         (let* ([v1 (top)]
               [moves (car v1)]
               [vec (caddr v1)]
               [player (cadddr v1)]
               [lis (car (cddddr v1))]
               [pr (car player)]
               [pc (cdr player)]
               [vu (check vec pr pc -1 0)]
               [pu (cons (- pr 1) pc)]
               [vd (check vec pr pc +1 0)]
               [pd (cons (+ pr 1) pc)]
               [vr (check vec pr pc 0 +1)]
               [prr (cons pr (+ 1 pc))]
               [vl (check vec pr pc 0 -1)]
               [pl (cons pr (- pc 1))]
               )
         (begin  (pop) (add vu pu moves 'u lis push) (add vd pd moves 'd lis push) (add vr prr moves 'r lis push) (add vl pl moves 'l lis push)
                (set! fl 1)
                (cond [(is-sol? vu) (set! ans (list (+ 1 moves) 'u vu pu (reverse (cons 'u lis))))]
                      [(is-sol? vd) (set! ans (list (+ 1 moves) 'd vd pd (reverse (cons 'd lis))))]
                      [(is-sol? vr) (set! ans (list (+ 1 moves) 'r vr prr (reverse (cons 'r lis))))]
                      [(is-sol? vl) (set! ans (list (+ 1 moves) 'l vl pl (reverse (cons 'l lis))))]
                      [else (set! fl 0)]
                      ) (strat push )
                        ))))
(define (add vu pu moves c l push)
  (cond  [(not (not-deadlock? vu)) (void)]
         [(and vu (not (member vu (mlist->list vis))))
         (begin (push (list (+ 1 moves) c vu pu (cons c l)))
         (mappend! vis (mlist vu)))]))
;(bfs)
;start
;(if (equal? ans '()) "No Solution Found"
;(begin (caddr ans)
;(car ans)
;(car (cddddr ans))))

;(displayln)

;(push (list 0 'n start (cons 3 2) '()))
(newline)
;
;(dfs)
;start
;(if (equal? ans '()) "No Solution Found"
;(begin (caddr ans)
;(car ans)
;(car (cddddr ans))))


; queue and heap

; define push pop top and function to order
;(define proc '())
;(define (manhattan+cost)
;  (define stack (make-heap
;           (λ (p1 p2)
;             (<= (car p1) (car p2)))))
;  (define (push p) (heap-add! stack p))
;  (define (pop) (heap-remove-min! stack))
;  (define (top) (heap-min stack))
;(begin (set! fl 0)
;;(set! stack '())
;(set! vis (mlist (cons'() '())))
;(set! proc (mlist '()))
;(push (cons 0 (list 0 'n start (cons 4 1) '()))) (strat2 push stack pop top)))
;
;
;(define (strat2 push stack pop top)
;  (displayln (top))
;  ;(displayln fl)
;  (while (and (not (null? stack)) (equal? fl 0))
;         (cond [(member (cadddr (top)) (mlist->list proc)) (begin (pop) (strat2 push stack pop top))]
;               [else 
;         (let* ([t (top)]
;                [v1 (cdr t)]
;                [cost (car t)]
;               [moves (car v1)]
;               [vec (caddr v1)]
;               [player (cadddr v1)]
;               [lis (car (cddddr v1))]
;               [pr (car player)]
;               [pc (cdr player)]
;               [vu (check2 vec pr pc -1 0 moves)]
;               [pu (cons (- pr 1) pc)]
;               [vd (check2 vec pr pc +1 0 moves)]
;               [pd (cons (+ pr 1) pc)]
;               [vr (check2 vec pr pc 0 +1 moves)]
;               [prr (cons pr (+ 1 pc))]
;               [vl (check2 vec pr pc 0 -1 moves)]
;               [pl (cons pr (- pc 1))]
;               )
;         (begin  (pop) (displayln vu) (displayln vis) (add2 vu pu moves 'u lis push) (add2 vd pd moves 'd lis push)
;                 (add2 vr prr moves 'r lis push) (add2 vl pl moves 'l lis push)
;                (set! fl 1) (mappend! proc (mlist vec))
;                (cond [(is-sol? (car vu)) (set! ans (list (+ 1 moves) 'u (car vu) pu (reverse (cons 'u lis))))]
;                      [(is-sol? (car vd)) (set! ans (list (+ 1 moves) 'd (car vd) pd (reverse (cons 'd lis))))]
;                      [(is-sol? (car vr)) (set! ans (list (+ 1 moves) 'r (car vr) prr (reverse (cons 'r lis))))]
;                      [(is-sol? (car vl)) (set! ans (list (+ 1 moves) 'l (car vl) pl (reverse (cons 'l lis))))]
;                      [else (set! fl 0)]
;                      ) (strat2 push stack pop top)
;                        ))])))
;(define (add2 vu pu moves c l push)
;  (cond [(not (car vu)) (void)]
;        [(not (assoc (car vu)  (mlist->list vis))) (begin (push (cons (cdr vu) (list (+ 1 moves) c (car vu) pu (cons c l))))
;         (mappend! vis (mlist (cons (car vu) (cdr vu)))))]
;        [(<= (cdr (assoc (car vu) (mlist->list vis))) (cdr vu)) (void)]
;    [else
;         (begin (push (cons (cdr vu) (list (+ 1 moves) c (car vu) pu (cons c l))))
;         (mappend! vis (mlist (cons (car vu) (cdr vu)))))]))
;
;(define (check2 v2 pr pc ar ac cos)
; ; (displayln v2);ar means rowadd , ac coladd
;(define ans '())
;(define v (build-vector m (lambda (x) (build-vector n (lambda (y) (2d-vector-ref v2 x y))))))
;  (define p1r (+ pr ar))
;   (define p1c (+ pc ac))
;   (define p2r (+ p1r ar))
;   (define p2c (+ p1c ac))
;   (define s1 (2d-vector-ref v p1r p1c))
;   (define s2 (2d-vector-ref v p2r p2c))
;   (define s0 (2d-vector-ref v pr pc))
;   (cond [ (equal? s1 'e) (begin (if (equal? s0 'p) (2d-vector-set! v pr pc 'e) (2d-vector-set! v pr pc 'o))
;                                 (2d-vector-set! v p1r p1c 'p) (cons v (+ 1 cos (manhattan v)) ))]
;         [(equal? s1 'o) (begin (if (equal? s0 'p) (2d-vector-set! v pr pc 'e) (2d-vector-set! v pr pc 'o))
;                         (2d-vector-set! v p1r p1c 'q) (cons v (+ 1 cos (manhattan v) ) ))]
;   
;         [ (and (equal? s1 'b) (equal? s2 'o))  (begin (if (equal? s0 'p) (2d-vector-set! v pr pc 'e) (2d-vector-set! v pr pc 'o))
;                                                       (2d-vector-set! v p1r p1c 'p)
;                                                       (2d-vector-set! v p2r p2c '*) (cons v (+ 1 cos (manhattan v)) ))]
;         [ (and (equal? s1 'b) (equal? s2 'e))  (begin (if (equal? s0 'p) (2d-vector-set! v pr pc 'e) (2d-vector-set! v pr pc 'o))
;                                                       (2d-vector-set! v p1r p1c 'p)
;                                                       (2d-vector-set! v p2r p2c 'b) (cons v (+ 1 cos (manhattan v)) ))]
;     
;         [else (cons #f #f)]
;         ))
;
;;(manhattan+cost)
;;start
;;(caddr ans)
;;(car ans)
;;(car (cddddr ans))
;           
;           
;                      
                
                            
         
    
 
