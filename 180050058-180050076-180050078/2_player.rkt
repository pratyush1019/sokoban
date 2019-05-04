#lang racket


(require 2htdp/universe)
(require 2htdp/image)
(provide all-defined-out 2play)
(define (2play)
  (begin 
(struct pos (x y)#:transparent)
(define boxes (make-vector  100 #f))
(define boxes2 (make-vector  100 #f))
(define box (square 40 150 "brown"))
(define state -1)


(define r (empty-scene 1320 640))
(set! r (place-image (rectangle 600 600 100 "yellow") 330 320 r))
(set! r (place-image (rectangle 600 600 100 "yellow") 990 320 r))

(define q 0)
(define w 0)
(define place (pos 90 80))
(define place2 (pos 750 80))
(define (iterative)
  (cond [(not (= w 13)) (begin
                          (iter)
                          (set! place (pos 90 (+ (pos-y place) 40)))
                           (set! q 0)
                           (set! w (+ w 1))
                           (iterative))]))
                          
(define (iter)
  (cond [(not (= q 13)) (begin (set! q (+ q 1))
(set! r (place-image (square 40 "outline" "black") (pos-x place) (pos-y place) r))
(set! place (pos (remainder (+ (pos-x place) 40) 630) (pos-y place) ))
(iter))]))
(define (iterative2)
  (cond [(not (= w 13)) (begin
                          (iter2)
                          (set! place2 (pos 750 (+ (pos-y place2) 40)))
                           (set! q 0)
                           (set! w (+ w 1))
                           (iterative2))]))
                          
(define (iter2)
  (cond [(not (= q 13)) (begin (set! q (+ q 1))
(set! r (place-image (square 40 "outline" "black") (pos-x place2) (pos-y place2) r))
(set! place2 (pos (remainder (+ (pos-x place2) 40) 1290) (pos-y place2) ))
(iter2))]))
(iterative)
(set! w 0)
(iterative2)
(define wallp (pos 50 40))
(define wallp2 (pos 710 40))
(define m 99)
(define n 99)
(define e 0)
(define f 0)
(define (iteration)
  (cond [(= e 0) (begin (iter4) 
                         (set! wallp (pos 50 (+ (pos-y wallp) 40)))
                         (set! f 0)
                         (set! e (+ e 1))
                         (iteration))]
        [(= e 14) (begin (iter4) 
                         (set! wallp (pos 50 (+ (pos-y wallp) 40)))
                         (set! f 0)
                         (set! e (+ e 1))
                         (iteration))]
        [(and (> e 0) (< e 14)) (begin (iter3) 
                         (set! wallp (pos 50 (+ (pos-y wallp) 40)))
                         (set! f 0)
                         (set! e (+ e 1))
                         (iteration))]))

(define (iter3)
  (cond [(or (= f 14) (= f 0))  (begin (set! f (+ f 1))
                                       (vector-set! boxes m wallp)
                                       (set! m (- m 1))
                 (set! r (place-image (square 40 "solid" "red") (pos-x wallp) (pos-y wallp) r))
                 (set! wallp (pos (remainder (+ (pos-x wallp) 40) 630) (pos-y wallp) ))
                 (iter3))]
        [(and (> f 0) (< f 14)) (begin (set! f (+ f 1)) 
                 (set! wallp (pos (remainder (+ (pos-x wallp) 40) 630) (pos-y wallp) ))
                 (iter3))] ))


(define (iter4)
  (cond [(not (= f 15))  (begin (set! f (+ f 1))
                                (vector-set! boxes m wallp)
                                       (set! m (- m 1))
                 (set! r (place-image (square 40 "solid" "red") (pos-x wallp) (pos-y wallp) r))
                 (set! wallp (pos (remainder (+ (pos-x wallp) 40) 630) (pos-y wallp) ))
                 (iter4))]))

(iteration)
(set! e 0)
(define (iteration2)
  (cond [(= e 0) (begin (iter5) 
                         (set! wallp2 (pos 710 (+ (pos-y wallp2) 40)))
                         (set! f 0)
                         (set! e (+ e 1))
                         (iteration2))]
        [(= e 14) (begin (iter5) 
                         (set! wallp2 (pos 710 (+ (pos-y wallp2) 40)))
                         (set! f 0)
                         (set! e (+ e 1))
                         (iteration2))]
        [(and (> e 0) (< e 14)) (begin (iter6) 
                         (set! wallp2 (pos 710 (+ (pos-y wallp2) 40)))
                         (set! f 0)
                         (set! e (+ e 1))
                         (iteration2))]))

(define (iter6)
  (cond [(or (= f 14) (= f 0))  (begin (set! f (+ f 1))
                                       (vector-set! boxes2 n wallp2)
                                       (set! n (- n 1))
                 (set! r (place-image (square 40 "solid" "red") (pos-x wallp2) (pos-y wallp2) r))
                 (set! wallp2 (pos (remainder (+ (pos-x wallp2) 40) 1290) (pos-y wallp2) ))
                 (iter6))]
        [(and (> f 0) (< f 14)) (begin (set! f (+ f 1)) 
                 (set! wallp2 (pos (remainder (+ (pos-x wallp2) 40) 1290) (pos-y wallp2) ))
                 (iter6))] ))


(define (iter5)
  (cond [(not (= f 15))  (begin (set! f (+ f 1))
                                (vector-set! boxes2 n wallp2)
                                       (set! n (- n 1))
                 (set! r (place-image (square 40 "solid" "red") (pos-x wallp2) (pos-y wallp2) r))
                 (set! wallp2 (pos (remainder (+ (pos-x wallp2) 40) 1290) (pos-y wallp2) ))
                 (iter5))]))

(iteration2)
(define score 0)
(define score1 0)

(define (draw-box lst scn)
  (cond [(null? lst) scn]
        [else (draw-box (cdr lst) (place-image box (pos-x (car lst)) (pos-y (car lst)) scn))]))


(define (draw t)
  (define q (draw-box (cadr t) (list-ref t 4)))
  (define y (draw-box (cadddr t) q))
  (define s (cond [(= fl 0) y]
                  [(= fl 1) (place-image (text/font "PLAYER ONE WINS!!" 40 "black"
                                                    "Gill Sans" "decorative" "italic" "normal" #f) 660 610 y)]
                  [(= fl 2) (place-image (text/font "PLAYER TWO WINS!!" 40 "black"
                                                    "Gill Sans" "decorative" "italic" "normal" #f) 660 610 y)]))
 (place-image (circle 20 100 "red")  (pos-x (car t)) (pos-y (car t))  
(place-image (circle 20 100 "red") (pos-x (caddr t)) (pos-y (caddr t))
(place-image (text (string-append "Player 2:" (number->string score)) 20 "black")  330 10 
 (place-image (text (string-append "Player 1:" (number->string score1)) 20 "black")  990 10 s)))))
(define fp (list (pos -200 -200 ) '() (pos -200 -200) '() r))
                                

(define i 0)
(define j 0)

(define (nearest n)
  (if (< (remainder n 40) 20) (+ (- n (remainder n 40)) 10)
       (+ (- n (remainder n 40)) 50)))
(define (nearest1 n)
  (if (< (remainder n 40) 20)  (- n (remainder n 40))
       (+ (- n (remainder n 40)) 40)))

(define (nearest2 n)
  (if (< (remainder n 40) 20)  (- (- n (remainder n 40)) 10)
       (+ (- n (remainder n 40)) 30)))



(define (mouse-handler t x y event)
  (cond [(< state 3)
  (cond [(mouse=? event "button-up")(begin
                                       (vector-set! boxes i (pos (nearest x) (nearest1 y)))
                                       (cond [(equal? state 2)
                                           (begin
                                       (set! i (+ i 1))
                                       (list (car t) (cadr t) (caddr t) (cadddr t) (place-image (square 40 "solid" "red")
                                                                           (nearest x) (nearest1 y) (list-ref t 4))))]
                                             
                                           [(equal? state 1) (begin (set! i (+ i 1))
                                            (list (car t) (cadr t) (caddr t) (cadddr t) (place-image (square 40 50 "blue")
                                                                                (nearest x) (nearest1 y) (list-ref t 4))))]
                                            [(equal? state 0) (begin
                                             (set! i (+ i 1))
                                            (list (car t) (append (list (pos (nearest x) (nearest1 y))) (cadr t)) (caddr t) (cadddr t)
                                                  (list-ref t 4)))]
                                           [(equal? state -1) (begin
                                                      (set! i (+ i 1))
                                                      (set! state (+ state 1))
                                                      (list (pos (nearest x) (nearest1 y)) (cadr t) (caddr t) (cadddr t) (list-ref t 4)))]))]
        [(mouse=? event "button-down") t]
        [(mouse=? event "drag") t]
        [(mouse=? event "move") t]
        [(mouse=? event "leave") t]
        [(mouse=? event "enter") t])]
        [(< state 7)
  (cond [(mouse=? event "button-up") (begin
                                       (vector-set! boxes2 j (pos (nearest2 x) (nearest1 y)))
                                       (cond [(equal? state 6)
                                           (begin
                                       (set! j (+ j 1))
                                       (list (car t) (cadr t) (caddr t) (cadddr t) (place-image (square 40 "solid" "red")
                                                                           (nearest2 x) (nearest1 y) (list-ref t 4))))]
                                             
                                           [(equal? state 5) (begin (set! j (+ j 1))
                                            (list (car t) (cadr t) (caddr t) (cadddr t) (place-image (square 40 50 "blue")
                                                                                (nearest2 x) (nearest1 y) (list-ref t 4))))]
                                            [(equal? state 4) (begin
                                             (set! j (+ j 1))
                                            (list (car t) (cadr t) (caddr t) (append (list (pos (nearest2 x) (nearest1 y))) (cadddr t))
                                                  (list-ref t 4)))]
                                           [(equal? state 3) (begin
                                                      (set! j (+ j 1))
                                                      (set! state (+ state 1))
                                                      (list (car t) (cadr t) (pos (nearest2 x) (nearest1 y)) (cadddr t) (list-ref t 4)))]))]
        [(mouse=? event "button-down") t]
        [(mouse=? event "drag") t]
        [(mouse=? event "move") t]
        [(mouse=? event "leave") t]
        [(mouse=? event "enter") t])]
        [else t]))
  



(define control 1)
(define k 0)
(define l 0)
(define count 0)
(define count1 0)
(define one 0)
(define one1 0)

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
  (cond [(= state 7)
  (let ([s (car t)]
        [u (cadr t)]
        [v (caddr t)]
        [w (cadddr t)]
        [z (list-ref t 4)])
  (cond [(= control 0) 
  (cond 
  [(key=? a-key "left") (let
                          ([posn (pos (- (pos-x s) 40) (pos-y s))])
                            (cond [(coincide? posn) t]
                                  [(box-coincide? posn u) (let* ([b (which-box posn u)]
                                                                [e (remove b u)]
                                                                [po (pos (- (pos-x b) 40) (pos-y b))])
                                                            (if (or (hit? po e) (coincide? po )) t
                                                                (begin (set! control 1) (set! score (+ score 1))
                                                               (list posn (append (list po) e) v w z))))]
                               [else (begin (set! control 1)
                                            (set! score (+ score 1))
                                            (list posn u v w z))]))]
    [(key=? a-key "up")   (let
                          ([posn (pos  (pos-x s) (- (pos-y s) 40))])
                            (cond [(coincide? posn) t]
                                  [(box-coincide? posn u) (let* ([b (which-box posn u)]
                                                                [e (remove b u)]
                                                                [po (pos (pos-x b) (- (pos-y b) 40))])
                                                            (if (or (hit? po e) (coincide? po )) t
                                                            (begin (set! control 1) (set! score (+ score 1))
                                                               (list posn (append (list po) e) v w z))))]
                               [else (begin (set! control 1)
                                            (set! score (+ score 1))
                                            (list posn u v w z))]))]
    [(key=? a-key "down") (let
                          ([posn (pos  (pos-x s) (+ (pos-y s) 40))])
                            (cond [(coincide? posn) t]
                                  [(box-coincide? posn u) (let* ([b (which-box posn u)]
                                                                [e (remove b u)]
                                                                [po (pos (pos-x b) (+ (pos-y b) 40))])
                                                            (if (or (hit? po e) (coincide? po )) t
                                                            (begin (set! control 1) (set! score (+ score 1))
                                                               (list posn (append (list po) e) v w z))))]
                               [else (begin (set! control 1)
                                            (set! score (+ score 1))
                                            (list posn  u v w z))]))]
    [(key=? a-key "right") (let
                          ([posn (pos (+ (pos-x s) 40) (pos-y s))])
                            (cond [(coincide? posn) t]
                                  [(box-coincide? posn u) (let* ([b (which-box posn u)]
                                                                [e (remove b u)]
                                                                [po (pos (+ (pos-x b) 40) (pos-y b))])
                                                            (if (or (hit? po e) (coincide? po )) t
                                                            (begin (set! control 1) (set! score (+ score 1))
                                                               (list posn (append (list po) e) v w z))))]
                               [else (begin (set! control 1)
                                            (set! score (+ score 1))
                                            (list posn u v w z))]))]
     [(key=? a-key " ") (begin (set! control 1) (set! score +inf.0)
                              t)]
    [else t])]
        [else
         (cond 
  [(key=? a-key "left") (let
                          ([posn (pos (- (pos-x v) 40) (pos-y v))])
                            (cond [(coincide? posn) t]
                                  [(box-coincide? posn w) (let* ([b (which-box posn w)]
                                                                [e (remove b w)]
                                                                [po (pos (- (pos-x b) 40) (pos-y b))])
                                                            (if (or (hit? po e) (coincide? po )) t
                                                            (begin (set! control 0) (set! score1 (+ score1 1))
                                                               (list s u posn (append (list po) e) z))))]
                               [else (begin (set! control 0)
                                            (set! score1 (+ score1 1))
                                            (list s u posn w z))]))]
    [(key=? a-key "up")   (let
                          ([posn (pos  (pos-x v) (- (pos-y v) 40))])
                            (cond [(coincide? posn) t]
                                  [(box-coincide? posn w) (let* ([b (which-box posn w)]
                                                                [e (remove b w)]
                                                                [po (pos (pos-x b) (- (pos-y b) 40))])
                                                            (if (or (hit? po e) (coincide? po )) t
                                                            (begin (set! control 0) (set! score1 (+ score1 1))
                                                               (list s u posn (append (list po) e)  z))))]
                               [else (begin (set! control 0)
                                            (set! score1 (+ score1 1))
                                            (list s u posn w z))]))]
    [(key=? a-key "down") (let
                          ([posn (pos  (pos-x v) (+ (pos-y v) 40))])
                            (cond [(coincide? posn) t]
                                  [(box-coincide? posn w) (let* ([b (which-box posn w)]
                                                                [e (remove b w)]
                                                                [po (pos (pos-x b) (+ (pos-y b) 40))])
                                                            (if (or (hit? po e) (coincide? po )) t
                                                            (begin (set! control 0) (set! score1 (+ score1 1))
                                                               (list s u posn (append (list po) e) z))))]
                               [else (begin (set! control 0)
                                            (set! score1 (+ score1 1))
                                            (list s u posn w z))]))]
    [(key=? a-key "right") (let
                          ([posn (pos (+ (pos-x v) 40) (pos-y v))])
                            (cond [(coincide? posn) t]
                                  [(box-coincide? posn w) (let* ([b (which-box posn w)]
                                                                [e (remove b w)]
                                                                [po (pos (+ (pos-x b) 40) (pos-y b))])
                                                            (if (or (hit? po e) (coincide? po )) t
                                                            (begin (set! control 0) (set! score1 (+ score1 1))
                                                               (list s u posn (append (list po) e) z))))]
                               [else (begin (set! control 0)
                                            (set! score1 (+ score1 1))
                                            (list s u posn w z))]))]
     [(key=? a-key " ") (begin (set! control 0) (set! score1 +inf.0) 
                              t)]
    [else t])]))]
       [(= state 2) (cond 
    [(key=? a-key " ") (begin (set! state (+ state 1))
                               t)]
    [else t])]
         [(= state 1) (cond 
    [(key=? a-key " ") (begin (set! state (+ state 1))
                              (set! count  (- i one 1))
                               (set! k i)  
                                  t)]
    [else t])]
         [(= state 0) (cond 
    [(key=? a-key " ") (begin (set! state (+ state 1))
                              (set! one (- i 1))
                                  t)]
    [else t])]
         
         [(= state 4) (cond 
    [(key=? a-key " ") (begin (set! state (+ state 1))
                              (set! one1 (- j 1)) 
                                  t)]
    [else t])]
     
    [(= state 5) (cond 
    [(key=? a-key " ") (begin (set! state (+ state 1))
                              (set! count1  (- j one1 1))
                              (set! l j)   
                                  t)]
    [else t])]
         [(= state 6) (cond 
    [(key=? a-key " ") (begin (set! state (+ state 1))
                               t)]
    [else t])]))



(define (coincide? t)
  (if (= control 0)
  (coincide-helper t k boxes)
  (coincide-helper t l boxes2)))

(define (coincide-helper t i v)
  (cond [(= i 100) #f]
        [(equal? (vector-ref v i) t) #t]
        [else (coincide-helper t (+ i 1) v)]))

(define fl 0)
(define (done? t)
  (cond [(or (= fl 2) (= fl 1)) #t]
        [(= count 0) #f]
        [(= count1 0) #f]
        [else  (let ([m1 (makelist boxes count one '())]
                     [m2 (makelist boxes2 count1 one1 '())])
                (cond [(or (andmap (lambda (x) (check x m1)) (cadr t)) (= score1 +inf.0)) (begin (set! fl 2) #f)] 
                      [(or (andmap (lambda (x) (check x m2)) (cadddr t)) (= score +inf.0)) (begin (set! fl 1) #f)]
                      [else #f]))]))

(define (makelist v n i l)
  (if (= (length l) n) l
      (makelist v n (+ i 1) (append l (list (vector-ref v (+ i 1)))))))

(define (check e l)
  (cond [(null? l) #f]
        [(equal? e (car l)) #t]
        [else (check e (cdr l))]))

      

(big-bang fp
  (to-draw draw)
  (on-mouse mouse-handler)
  (on-key change)
  (stop-when done?))
))