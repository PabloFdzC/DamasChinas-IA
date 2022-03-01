#lang racket
(require racket/gui/base)

(define cChekersBox%
  (class object%
    (super-new)
    (init-field color
                posX
                posY
                [circle (new dc-path%)]
                )
    (define/public (get-color) color)
    (define/public (get-pos-x) posX)
    (define/public (get-pos-y) posY)
    (define/public (get-circle) circle)
    (define/public (set-color c)
      (set! color c))
    (define/public (draw-circle mc)
      (send (send mc get-dc) set-brush color 'solid)
      (send circle arc posX posY 20 20 0 (* 2 pi))
      (send (send mc get-dc) draw-path circle)
      )
    ))

(define (create-board)
  (append (create-board-aux2 '() 0 3 "green" 150 10 21 22 #f)
          (reverse (create-board-aux2 '() 8 12 "white" 70 182 21 22 #t))
          (create-board-aux2 '() 9 12 "white" 60 204 21 22 #f)
          (reverse (create-board-aux2 '() 0 3 "red" 150 356 21 22 #t))
          ))

(define (create-board-aux lst curr end c x y spaceX spaceY)
  (if (> curr end)
      lst
      (create-board-aux
       (append lst (list (new cChekersBox%
                        [color c]
                        [posX x]
                        [posY y])))
       (add1 curr) end c (+ x spaceX) y spaceX spaceY)))

(define (create-board-aux2 lst curr end c x y spaceX spaceY rev)
  (if (> curr end)
      lst
      (create-board-aux2 (append lst (list (create-board-aux '() 0 curr c x y spaceX spaceY)))
                         (add1 curr) end c (- x (/ (sub1 spaceY) 2)) (if rev (- y spaceX) (+ y spaceY)) spaceX spaceY rev)))

(define (paint-board board mc)
  (if (not (empty? board))
      ((lambda (board mc)
         (paint-board-aux (first board) mc)
         (paint-board (rest board) mc))
       board mc)
      (void)))

(define (paint-board-aux board mc)
  (if (not (empty? board))
      ((lambda (board mc)
         (send (first board) draw-circle mc)
         (paint-board-aux (rest board) mc))
       board mc)
      (void)))
;(define paint-board-aux
;  (lambda (board mc)
;    (if (not (empty? board)) (send (first board) draw-circle (send mc get-dc)) void)
;    (if (not (empty? board)) (paint-board-aux (rest board) mc) void)))

#|(define a (new cChekersBox%
                   [color "Red"]
                   [posX 400]
                   [posY 400]))
(display (send a get-color))
(send a set-color "Blue")
(display (send a get-color))|#


(define frame (new frame%
                   [label "Damas chinas"]
                   [width 400]
                   [height 400]))


(define my-canvas%
  (class canvas%
    (super-new)
    (inherit/super get-dc)
    (define/override (on-event event)
      (if (equal? (send event get-event-type) 'left-down)
          ;(printf "Hello ~a\n" (is-a? (send this get-dc) dc<%>))
          (let ([a (new dc-path%)])
            (send a arc (send event get-x) (send event get-y) 10 10 0 (* 2 pi))
            (send (send this get-dc) draw-path a )
            (display "----------------------\n")
            (display (call-with-values (lambda ()(send a get-datum)) list))
            (display "----------------------\n")
            )
          #|(display (string-append
                    "("
                    (number->string (send event get-x))
                    ", "
                    (number->string (send event get-x)) ") "))|#
          void))
    ))


(define mc (new my-canvas%
     [parent frame]
     [paint-callback
      (lambda (canvas dc)
        (send dc set-background "black")
        (send dc clear)
        (paint-board (create-board) mc)
        )]))




(send frame show #t)
(send frame maximize #t)
