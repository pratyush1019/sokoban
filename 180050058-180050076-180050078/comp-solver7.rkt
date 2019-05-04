#lang racket
(require 2htdp/universe)
(require 2htdp/image)
(provide all-defined-out csol7) 
(require "solver.rkt")
;(setm! 9)
;(setn! 9)
;(sets! 9 9)
(struct pos (x y)#:transparent)
(define boxes (make-vector  100 #f))
(define box (square 40 150 "brown"))
(define r (empty-scene 660 640))
(define fp2 (list (pos -200 -200 ) '() r))
(define score 0)
;(set! r (place-image (rectangle (* 40 m) (* 40 n) 100 "yellow") 330 320 r))
(define q 0)
(define w 0)
(define place (pos 50 40))
(define (iterative)
  (cond [(not (= w m)) (begin
                          (iter)
                          (set! place (pos 50 (+ (pos-y place) 40)))
                           (set! q 0)
                           (set! w (+ w 1))
                           (iterative))]))
                          
(define (iter)
  (cond [(not (= q n)) (begin 
(set! r (place-image (square 40 "outline" "black") (pos-x place) (pos-y place) r))
(set! r (place-image (square 40 100 "yellow") (pos-x place) (pos-y place) r))
(if  (or (= 0 w) (= w (- m 1)) (= 0 q) (= q (- n 1))) 
    (begin (set! r (place-image (square 40 "solid" "red") (pos-x place) (pos-y place) r))
           (2d-vector-set! start w q 'w))
    (void))
(set! place (pos (remainder (+ (pos-x place) 40) 630) (pos-y place) ))
(set! q (+ q 1))
(iter))]))
;(iterative)

(define (findxid x)
  (/ (- x 40) 40))

(define (findyid y)
  (/ (- y 50) 40))

(define (draw-box lst scn)
  (cond [(null? lst) scn]
        [else (draw-box (cdr lst) (place-image box (pos-x (car lst)) (pos-y (car lst)) scn))]))


(define (draw t)
  (define q (draw-box (cadr t) (caddr t)))
 (place-image (circle 20 100 "red")  (pos-x (car t)) (pos-y (car t))
              (place-image (text (string-append "Score:" (number->string score)) 20 "black") 330 10 q)))


(define count 0)                                

(define i 0)

(define (nearest n)
  (if (< (remainder n 40) 20) (+ (- n (remainder n 40)) 10)
       (+ (- n (remainder n 40)) 50)))
(define (nearest1 n)
  (if (< (remainder n 40) 20)  (- n (remainder n 40))
       (+ (- n (remainder n 40)) 40)))
(define state -1)
(define j 0)

(define (mouse-handler t x y event)
  (cond  [(>= state 3) t]
         [(mouse=? event "button-up") (begin
                                       (vector-set! boxes i (pos (nearest x) (nearest1 y)))
                                       (cond [(equal? state 2)
                                           (begin
                                       (set! i (+ i 1))
                                       (2d-vector-set! start (findxid (nearest1 y)) (findyid (nearest x)) 'w)
                                       (list (car t) (cadr t) (place-image (square 40 "solid" "red")
                                                                           (nearest x) (nearest1 y) (caddr t))))]
                                             
                                           [(equal? state 1) (begin (set! i (+ i 1))
                                            (2d-vector-set! start (findxid (nearest1 y)) (findyid (nearest x)) 'o)
                                            (list (car t) (cadr t) (place-image (square 40 50 "blue")
                                                                                (nearest x) (nearest1 y) (caddr t))))]
                                            [(equal? state 0) (begin
                                             (set! i (+ i 1))
                                             (2d-vector-set! start (findxid (nearest1 y)) (findyid (nearest x)) 'b)
                                            (list (car t) (append (list (pos (nearest x) (nearest1 y))) (cadr t)) (caddr t)))]
                                           [(equal? state -1) (begin
                                                      (set! i (+ i 1))
                                                      (set! state (+ state 1))
                                                      (2d-vector-set! start (findxid (nearest1 y)) (findyid (nearest x)) 'p)
                                                      (setp  (findxid (nearest1 y)) (findyid (nearest x)))
                                                      (list (pos (nearest x) (nearest1 y)) (cadr t) (caddr t)))]))]
        [(mouse=? event "button-down") t]
        [(mouse=? event "drag") t]
        [(mouse=? event "move") t]
        [(mouse=? event "leave") t]
        [(mouse=? event "enter") t]))

(define (box-coincide? p l)
  (cond [(null? l) #f]
        [(equal? p (car l)) #t]
        [else (box-coincide? p (cdr l))]))

(define (which-box p l)
  (cond [(equal? p (car l)) (car l)]
        [else (which-box p (cdr l))]))

(define (hit? p1 l)
  (cond [(null? l) #f]
        [(equal? (car l) p1) #t]
        [else (hit? p1 (cdr l))]))



(define (change t a-key)
  (cond [(= state 3)
  (let ([s (car t)]
        [u (cadr t)])
  (cond
    [(key=? a-key "left") (let
                          ([posn (pos (- (pos-x s) 40) (pos-y s))])
                            (cond [(coincide? posn) t]
                                  [(box-coincide? posn u) (let* ([b (which-box posn u)]
                                                                [e (remove b u)]
                                                                [po (pos (- (pos-x b) 40) (pos-y b))])
                                                            (if (or (hit? po e) (coincide? po )) t
                                                            
                                                              (begin (set! score (+ 1 score)) (list posn (append (list po) e) (caddr t)))))]
                               [else (begin (set! score (+ 1 score)) (list posn (cadr t) (caddr t)))]))]
    [(key=? a-key "up")   (let
                          ([posn (pos  (pos-x s) (- (pos-y s) 40))])
                            (cond [(coincide? posn) t]
                                  [(box-coincide? posn u) (let* ([b (which-box posn u)]
                                                                [e (remove b u)]
                                                                [po (pos (pos-x b) (- (pos-y b) 40))])
                                                            (if (or (hit? po e) (coincide? po )) t
                                                            
                                                               (begin (set! score (+ 1 score)) (list posn (append (list po) e) (caddr t)))))]
                               [else (begin (set! score (+ 1 score)) (list posn (cadr t) (caddr t)))]))]
    [(key=? a-key "down") (let
                          ([posn (pos  (pos-x s) (+ (pos-y s) 40))])
                            (cond [(coincide? posn) t]
                                  [(box-coincide? posn u) (let* ([b (which-box posn u)]
                                                                [e (remove b u)]
                                                                [po (pos (pos-x b) (+ (pos-y b) 40))])
                                                            (if (or (hit? po e) (coincide? po )) t
                                                            
                                                               (begin (set! score (+ 1 score)) (list posn (append (list po) e) (caddr t)))))]
                               [else (begin (set! score (+ 1 score)) (list posn (cadr t) (caddr t)))]))]
    [(key=? a-key "right") (let
                          ([posn (pos (+ (pos-x s) 40) (pos-y s))])
                            (cond [(coincide? posn) t]
                                  [(box-coincide? posn u) (let* ([b (which-box posn u)]
                                                                [e (remove b u)]
                                                                [po (pos (+ (pos-x b) 40) (pos-y b))])
                                                            (if (or (hit? po e) (coincide? po )) t
                                                            
                                                               (begin (set! score (+ 1 score)) (list posn (append (list po) e) (caddr t)))))]
                               [else (begin (set! score (+ 1 score)) (list posn (cadr t) (caddr t)))]))]
    [else t]))]
        [(= state 2) (cond 
    [(key=? a-key " ") (begin (set! state (+ state 1))
                                 
                                  t)]
    [else 
                 t])]
         [(= state 1) (cond 
    [(key=? a-key " ") (begin (set! state (+ state 1))
                               (set! j i)  
                                  t)]
    [else 
                 t])]
    [else  (cond 
    [(key=? a-key " ") (begin (set! state (+ state 1))
                              (set! fp2 t)
                              ;(displayln fp2)
                               (set! count  (- i 1) )
                                  t)]
    [else t])]))



(define (coincide? t)
  (coincide-helper t j))

(define (coincide-helper t i)
  (cond [(= i 100) #f]
        [(equal? (vector-ref boxes i) t) #t]
        [else (coincide-helper t (+ i 1))]))


(define fl 0)
(define (done? t)
  (cond [(= fl 1) #t]
        [(= count 0) #f] 
        [else     (let ([m (makelist boxes count count '())])
                 (if (andmap (lambda (x) (check x m)) (cadr t)) (begin (set! fl 1) #f)
      #f))]))


(define (makelist v n i l)
  (if (= (length l) n) l
      (makelist v n (+ i 1) (append l (list (vector-ref v (+ i 1)))))))

(define (check e l)
  (cond [(null? l) #f]
        [(equal? e (car l)) #t]
        [else (check e (cdr l))]))
      

;(big-bang fp
;  ;(display-mode 'fullscreen)
;  (to-draw draw)
;  (on-mouse mouse-handler)
;  ;(on-tick (f l) 1)
;  (on-key change)
;  (stop-when (lambda (x) (begin (set! fp2 x) (= state 3)))))
(define lis '())
;(bfs)
;start
;(if (equal? ans '()) "No Solution Found"
;(begin 
;(car ans)
;(set! lis (car (cddddr ans)))))
;lis
(define (f t)
(let ([s (car t)]
      [u (cadr t)])
  (cond [(null? lis) t]
    [(equal? (car lis) 'l) (begin (set! lis (cdr lis))
                                  )(let
                          ([posn (pos (- (pos-x s) 40) (pos-y s))])
                            (cond [(coincide? posn) t]
                                  [(box-coincide? posn u) (let* ([b (which-box posn u)]
                                                                [e (remove b u)]
                                                                [po (pos (- (pos-x b) 40) (pos-y b))])
                                                            (if (or (hit? po e) (coincide? po )) t
                                                            
                                                               (begin (set! score (+ 1 score)) (list posn (append (list po) e) (caddr t)))))]
                            [else (begin (set! score (+ 1 score)) (list posn (cadr t) (caddr t)))]))]
    [(equal? (car lis) 'u)  (begin (set! lis (cdr lis))
                                   (let
                          ([posn (pos  (pos-x s) (- (pos-y s) 40))])
                            (cond [(coincide? posn) t]
                                  [(box-coincide? posn u) (let* ([b (which-box posn u)]
                                                                [e (remove b u)]
                                                                [po (pos (pos-x b) (- (pos-y b) 40))])
                                                            (if (or (hit? po e) (coincide? po )) t
                                                            
                                                               (begin (set! score (+ 1 score)) (list posn (append (list po) e) (caddr t)))))]
                              [else (begin (set! score (+ 1 score)) (list posn (cadr t) (caddr t)))])))]
    [(equal? (car lis) 'd) (begin (set! lis (cdr lis))
                                  (let
                          ([posn (pos  (pos-x s) (+ (pos-y s) 40))])
                            (cond [(coincide? posn) t]
                                  [(box-coincide? posn u) (let* ([b (which-box posn u)]
                                                                [e (remove b u)]
                                                                [po (pos (pos-x b) (+ (pos-y b) 40))])
                                                            (if (or (hit? po e) (coincide? po )) t
                                                            
                                                               (begin (set! score (+ 1 score)) (list posn (append (list po) e) (caddr t)))))]
                               [else (begin (set! score (+ 1 score)) (list posn (cadr t) (caddr t)))])))]
    [(equal? (car lis) 'r) (begin (set! lis (cdr lis))
                                  (let
                          ([posn (pos (+ (pos-x s) 40) (pos-y s))])
                            (cond [(coincide? posn) t]
                                  [(box-coincide? posn u) (let* ([b (which-box posn u)]
                                                                [e (remove b u)]
                                                                [po (pos (+ (pos-x b) 40) (pos-y b))])
                                                            (if (or (hit? po e) (coincide? po )) t
                                                            
                                                              (begin (set! score (+ 1 score)) (list posn (append (list po) e) (caddr t)))))]
                            [else (begin (set! score (+ 1 score)) (list posn (cadr t) (caddr t)))])))]
    [else t])))
(define state2 1)
(define (end? t)
  (cond [(= 0 state2) #t]
        [(null? lis) (begin (set! state2 0) #f)]
        [else #f]))
;(big-bang fp2
;  (to-draw draw)
;  (on-mouse mouse-handler)
;  (on-tick f 0.5)
;  (on-key change)
;  (stop-when end?))

(define (csol7)
  (begin
(define t (empty-scene 660 640))
(set! t (place-image (rectangle 500 100 100 "yellow") 330 150 r))
(set! t (place-image (text "NO SOLUTION FOUND" 40 "black") 330 150 t))

(define nosol 0)
(setm! 7)
(setn! 7)
(sets! 7 7)
(iterative)
(define fp (list (pos -200 -200 ) '() r))
;(define fp2 fp)

  (big-bang fp
  ;(display-mode 'fullscreen)
  (to-draw draw)
  (on-mouse mouse-handler)
  ;(on-tick (f l) 1)
  (on-key change)
  (stop-when (lambda (x) (begin (set! fp2 x) (= state 3)))))
 (bfs)
start
(if (equal? ans '())  (big-bang t
  (to-draw (lambda(x) (begin (set! nosol 1) x)))
  (stop-when (lambda (x) (= nosol 1))))
 
(begin 
(car ans)
(set! lis (car (cddddr ans)))))
lis
(big-bang fp2
  (to-draw draw)
  (on-mouse mouse-handler)
  (on-tick f 0.5)
  (on-key change)
  (stop-when end?))
))

