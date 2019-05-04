#lang racket
(require 2htdp/universe)
(require 2htdp/image)
(require "comp-solver9.rkt")
(require "comp-solver7.rkt")
(require "comp-vs-player.rkt")
(require "1_player.rkt")
(require "2_player.rkt")
(define r (empty-scene 660 640))
(set! r (place-image (rectangle 500 100 100 "yellow") 330 150 r))
(set! r (place-image (rectangle 500 100 100 "yellow") 330 250 r))
(set! r (place-image (rectangle 500 100 100 "yellow") 330 350 r))
(set! r (place-image (rectangle 500 100 100 "yellow") 330 450 r))
(set! r (place-image (rectangle 500 100 100 "yellow") 330 550 r))
(set! r (place-image (text "1-PLAYER" 20 "black") 330 150
        (place-image (text "2-PLAYER" 20 "black") 330 250
        (place-image (text "COMPUTER VS PLAYER" 20 "black") 330 350
        (place-image (text "COMPUTER SOLVER (7*7)" 20 "black") 330 450
         (place-image (text "COMPUTER SOLVER (9*9)" 20 "black") 330 550 r))))))
(define state 0)
(define state2 0)
(define (mouse-handler t x y event)
  (cond  [(>= state 3) t]
         [(mouse=? event "button-up") (begin (set! state (quotient y 100)) t)]
        [(mouse=? event "button-down") t]
        [(mouse=? event "drag") t]
        [(mouse=? event "move") t]
        [(mouse=? event "leave") t]
        [(mouse=? event "enter") t]))
(define (f t)
  (if (> state2 0) #t
      (begin (set! state2 state) #f)))
(big-bang r
  ;(display-mode 'fullscreen)
  (to-draw (lambda (x) x))
  (on-mouse mouse-handler)
  ;(on-tick (f l) 1)
  ;(on-key change)
  (stop-when (lambda (x) (> state 0))))
(cond [(= state 4) (csol7)]
      [(= state 5) (csol9)]
      [(= state 3) (play)]
      [(= state 1) (1play)]
      [(= state 2) (2play)]
      )
