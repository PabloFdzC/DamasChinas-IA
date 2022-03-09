#lang racket
(require racket/gui/base)
(require "board.rkt")
(require "cCheckersBox.rkt")

#|
paint-board se encarga de recorrer la lista de listas
que representa el tablero
dado
Parámetros:
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
- mc: el canvas donde se dibujará el tablero
Salida:
- Ninguna
|#
(define (paint-board board mc)
  #|
  paint-board-aux dibuja el tablero de juego en un canvas.
  Esta función es llamada por paint-board
  dado
  Parámetros:
  - board: recibe una hilera del tablero (lista de objetos
    cCheckersBox)
  - mc: el canvas donde se dibujará el tablero
  Salida:
  - Ninguna
  |#
  (define (paint-board-aux board mc)
    (if (not (empty? board))
        ((lambda (board mc)
           (send (first board) draw-circle mc)
           (paint-board-aux (rest board) mc))
         board mc)
        (void)
        )
    )
  
  (if (not (empty? board))
      ((lambda (board mc)
         (paint-board-aux (first board) mc)
         (paint-board (rest board) mc))
       board mc)
      (void)
      )
  )


#|
Crea la ventana del programa, frame se puede
interpretar como una variable que contiene al
objeto que muestra la ventana
|#
(define frame (new frame%
                   [label "Damas chinas"]
                   [width 400]
                   [height 400]))
#|
  Inicializa un objeto board%
|#
(define game (new board% [turn #t]))

#|
my-canvas% es una clase que hereda de la
clase canvas%. Se hizo de esta manera para
poder tener acceso a la interación con el mouse
|#
(define my-canvas%
  (class canvas%
    (super-new)
    (inherit/super get-dc)
    #|
      Método para interactuar con el mouse, captura la posición del 
      click izquierdo en el canvas y selecciona una de las casillas
      del tablero en caso de que corresponda
      Entradas:
      - event: objeto on-event
      Salida:
      - Ninguna
    |# 
    (define/override (on-event event)
      (if (equal? (send event get-event-type) 'left-down)
          (send game select-box (send event get-x) (send event get-y) mc (if (send game get-turn) "red" "green"))
          (void)))
    ))


#|
mc es la instanciación del canvas
|#
(define mc (new my-canvas%
     [parent frame]
     [paint-callback
      (lambda (canvas dc)
        (send dc set-background "black")
        (send dc clear)
        (send game new-game) ; Se crea un nuevo juego
        (paint-board (send game get-current-game) mc) ; Se pinta el tablero inicial
        ;(paint-board (list-ref (check-possible-moves (send game get-current-game) "red") 0) mc)
        )]))


(define (check-possible-moves board color)
  (check-possible-moves-aux board color 0 (length board) '()))

(define (check-possible-moves-aux board color pos max moves)
  (if (= pos max)
      moves
      (check-possible-moves-aux
       board color (add1 pos) max (check-possible-moves-aux2 board color pos 0 (length (list-ref board pos)) moves))))

(define (check-possible-moves-aux2 board color pos pos2 max moves)
  (if (= pos2 max)
      moves
      (if (equal? (send (list-ref (list-ref board pos) pos2) get-color) color)
          (check-possible-moves-aux2 board color pos (add1 pos2) max
                                  (check-possible-moves-pos board pos pos2 color moves))
          (check-possible-moves-aux2 board color pos (add1 pos2) max moves))))

(define (check-possible-moves-pos board pos pos2 color moves)
  (remove-empty-lists
  (append moves
   (list(if (= pos 0)
       '()
        (check-move-up-left board pos pos2 color))
   (if (= pos 0)
       '()
        (check-move-up-right board pos pos2 color))
   (check-move-left board pos pos2 color)
   (check-move-right board pos pos2 color)
   (if (= pos 16)
       '()
        (check-move-down-left board pos pos2 color))
   (if (= pos 16)
       '()
        (check-move-down-right board pos pos2 color)))
   )))

(define (remove-empty-lists lst)
  (if (empty? lst)
      '()
      (if (empty? (first lst))
          (remove-empty-lists (rest lst))
          (append (list (first lst)) (remove-empty-lists (rest lst))))))

(define (check-move-up-left board pos pos2 c)
  (if (< (length (list-ref board (sub1 pos))) (length (list-ref board pos)))
      (if (= pos2 0)
          '()
          (if (= (- (length (list-ref board pos)) (length (list-ref board (sub1 pos)))) 1)
              (if ( equal? (send (list-ref (list-ref board (sub1 pos)) (sub1 pos2)) get-color) "white")
                  (append (take board (sub1 pos))
                          (list (append (take (list-ref board (sub1 pos)) (sub1 pos2)) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (sub1 pos)) (sub1 pos2)) get-posX)]
                                                                [posY (send (list-ref (list-ref board (sub1 pos)) (sub1 pos2)) get-posY)]))
                                                                (drop (list-ref board (sub1 pos)) pos2)))
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-posX)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-posY)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (drop board (add1 pos)))
                  (if (= pos 1) '() (check-move-up-left-jump board pos pos2 (sub1 pos)(sub1 pos2) c)))
              (if (and (> pos2 4) (< pos2 9))
                  (if ( equal? (send (list-ref (list-ref board (sub1 pos)) (- pos2 5)) get-color) "white")
                      (append (take board (sub1 pos))
                          (list (append (take (list-ref board (sub1 pos)) (- pos2 5)) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (sub1 pos)) (- pos2 5)) get-posX)]
                                                                [posY (send (list-ref (list-ref board (sub1 pos)) (- pos2 5)) get-posY)]))
                                                                (drop (list-ref board (sub1 pos)) (- pos2 4))))
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-posX)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-posY)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (drop board (add1 pos)))
                      (if (= pos 1) '() (check-move-up-left-jump board pos pos2 (sub1 pos)(- pos2 5) c)))
                  '()
                  )))
      (if (= (- (length (list-ref board (sub1 pos))) (length (list-ref board pos))) 1)
          (if ( equal? (send (list-ref (list-ref board (sub1 pos)) pos2) get-color) "white")
                  (append (take board (sub1 pos))
                          (list (append (take (list-ref board (sub1 pos)) pos2) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (sub1 pos)) pos2) get-posX)]
                                                                [posY (send (list-ref (list-ref board (sub1 pos)) pos2) get-posY)]))
                                                                (drop (list-ref board (sub1 pos)) (add1 pos2))))
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-posX)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-posY)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (drop board (add1 pos)))
                  (check-move-up-left-jump board pos pos2 (sub1 pos) pos2 c))
          (if ( equal? (send (list-ref (list-ref board (sub1 pos)) (+ pos2 4)) get-color) "white")
                      (append (take board (sub1 pos))
                          (list (append (take (list-ref board (sub1 pos)) (+ pos2 4)) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (sub1 pos)) (+ pos2 4)) get-posX)]
                                                                [posY (send (list-ref (list-ref board (sub1 pos)) (+ pos2 4)) get-posY)]))
                                                                (drop (list-ref board (sub1 pos)) (+ pos2 5))))
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-posX)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-posY)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (drop board (add1 pos)))
                      (check-move-up-left-jump board pos pos2 (sub1 pos)(+ pos2 4) c)))))

(define (check-move-up-left-jump board pos pos2 posUp posUp2 c)
  (if (< (length (list-ref board (sub1 posUp))) (length (list-ref board posUp)))
      (if (= posUp2 0)
          '()
          (if (= (- (length (list-ref board posUp)) (length (list-ref board (sub1 posUp)))) 1)
              (if ( equal? (send (list-ref (list-ref board (sub1 posUp)) (sub1 posUp2)) get-color) "white")
                  (append (take board (sub1 posUp))
                          (list (append (take (list-ref board (sub1 posUp)) (sub1 posUp2)) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (sub1 posUp)) (sub1 posUp2)) get-posX)]
                                                                [posY (send (list-ref (list-ref board (sub1 posUp)) (sub1 posUp2)) get-posY)]))
                                                                (drop (list-ref board (sub1 posUp)) posUp2)))
                          (list (list-ref board posUp))
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-posX)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-posY)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (drop board (add1 pos)))
                  '())
              (if (and (> posUp2 4) (< posUp2 9))
                  (if ( equal? (send (list-ref (list-ref board (sub1 posUp)) (- posUp2 5)) get-color) "white")
                      (append (take board (sub1 pos))
                          (list (append (take (list-ref board (sub1 posUp)) (- posUp2 5)) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (sub1 posUp)) (- posUp2 5)) get-posX)]
                                                                [posY (send (list-ref (list-ref board (sub1 posUp)) (- posUp2 5)) get-posY)]))
                                                                (drop (list-ref board (sub1 posUp)) (- posUp2 4))))
                          (list (list-ref board posUp))
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-posX)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-posY)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (drop board (add1 pos)))
                      '())
                  '()
                  )))
      (if (= (- (length (list-ref board (sub1 posUp))) (length (list-ref board posUp))) 1)
          (if ( equal? (send (list-ref (list-ref board (sub1 posUp)) posUp2) get-color) "white")
                  (append (take board (sub1 pos))
                          (list (append (take (list-ref board (sub1 posUp)) posUp2) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (sub1 posUp)) posUp2) get-posX)]
                                                                [posY (send (list-ref (list-ref board (sub1 posUp)) posUp2) get-posY)]))
                                                                (drop (list-ref board (sub1 posUp)) (add1 posUp2))))
                          (list (list-ref board posUp))
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-posX)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-posY)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (drop board (add1 pos)))
                  '())
          (if ( equal? (send (list-ref (list-ref board (sub1 posUp)) (+ posUp2 4)) get-color) "white")
                      (append (take board (sub1 posUp))
                          (list (append (take (list-ref board (sub1 posUp)) (+ posUp2 4)) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (sub1 posUp)) (+ posUp2 4)) get-posX)]
                                                                [posY (send (list-ref (list-ref board (sub1 posUp)) (+ posUp2 4)) get-posY)]))
                                                                (drop (list-ref board (sub1 posUp)) (+ posUp2 5))))
                          (list (list-ref board posUp))
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-posX)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-posY)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (drop board (add1 pos)))
                      '()))))

(define (check-move-up-right board pos pos2 c)
  (if (< (length (list-ref board (sub1 pos))) (length (list-ref board pos)))
      (if (= pos2 (- (length (list-ref board pos)) 1))
          '()
          (if (= (- (length (list-ref board pos)) (length (list-ref board (sub1 pos)))) 1)
              (if ( equal? (send (list-ref (list-ref board (sub1 pos)) pos2) get-color) "white")
                  (append (take board (sub1 pos))
                          (list (append (take (list-ref board (sub1 pos)) pos2) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (sub1 pos)) pos2) get-posX)]
                                                                [posY (send (list-ref (list-ref board (sub1 pos)) pos2) get-posY)]))
                                                                (drop (list-ref board (sub1 pos)) (add1 pos2))))
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-posX)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-posY)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (drop board (add1 pos)))
                  (if (= pos 1) '() (check-move-up-right-jump board pos pos2 (sub1 pos) pos2 c)))
              (if (and (> pos2 3) (< pos2 8))
                  (if ( equal? (send (list-ref (list-ref board (sub1 pos)) (- pos2 4)) get-color) "white")
                      (append (take board (sub1 pos))
                          (list (append (take (list-ref board (sub1 pos)) (- pos2 4)) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (sub1 pos)) (- pos2 4)) get-posX)]
                                                                [posY (send (list-ref (list-ref board (sub1 pos)) (- pos2 4)) get-posY)]))
                                                                (drop (list-ref board (sub1 pos)) (- pos2 3))))
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-posX)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-posY)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (drop board (add1 pos)))
                      (if (= pos 1) '() (check-move-up-right-jump board pos pos2 (sub1 pos)(- pos2 4) c)))
                  '()
                  )))
      (if (= (- (length (list-ref board (sub1 pos))) (length (list-ref board pos))) 1)
          (if ( equal? (send (list-ref (list-ref board (sub1 pos)) (add1 pos2)) get-color) "white")
                  (append (take board (sub1 pos))
                          (list (append (take (list-ref board (sub1 pos)) (add1 pos2)) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (sub1 pos)) (add1 pos2)) get-posX)]
                                                                [posY (send (list-ref (list-ref board (sub1 pos)) (add1 pos2)) get-posY)]))
                                                                (drop (list-ref board (sub1 pos)) (+ pos2 2))))
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-posX)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-posY)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (drop board (add1 pos)))
                  (check-move-up-right-jump board pos pos2 (sub1 pos) (add1 pos2) c))
          (if ( equal? (send (list-ref (list-ref board (sub1 pos)) (+ pos2 5)) get-color) "white")
                      (append (take board (sub1 pos))
                          (list (append (take (list-ref board (sub1 pos)) (+ pos2 5)) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (sub1 pos)) (+ pos2 5)) get-posX)]
                                                                [posY (send (list-ref (list-ref board (sub1 pos)) (+ pos2 5)) get-posY)]))
                                                                (drop (list-ref board (sub1 pos)) (+ pos2 6))))
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-posX)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-posY)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (drop board (add1 pos)))
                      (check-move-up-right-jump board pos pos2 (sub1 pos)(+ pos2 5) c)))))

(define (check-move-up-right-jump board pos pos2 posUp posUp2 c)
  (if (< (length (list-ref board (sub1 posUp))) (length (list-ref board posUp)))
      (if (= posUp2 (- (length (list-ref board posUp)) 1))
          '()
          (if (= (- (length (list-ref board posUp)) (length (list-ref board (sub1 posUp)))) 1)
              (if ( equal? (send (list-ref (list-ref board (sub1 posUp)) posUp2) get-color) "white")
                  (append (take board (sub1 posUp))
                          (list (append (take (list-ref board (sub1 posUp)) posUp2) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (sub1 posUp)) posUp2) get-posX)]
                                                                [posY (send (list-ref (list-ref board (sub1 posUp)) posUp2) get-posY)]))
                                                                (drop (list-ref board (sub1 posUp)) (add1 posUp2))))
                          (list (list-ref board posUp))
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-posX)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-posY)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (drop board (add1 pos)))
                  '())
              (if (and (> posUp2 3) (< posUp2 8))
                  (if ( equal? (send (list-ref (list-ref board (sub1 posUp)) (- posUp2 4)) get-color) "white")
                      (append (take board (sub1 pos))
                          (list (append (take (list-ref board (sub1 posUp)) (- posUp2 4)) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (sub1 posUp)) (- posUp2 4)) get-posX)]
                                                                [posY (send (list-ref (list-ref board (sub1 posUp)) (- posUp2 4)) get-posY)]))
                                                                (drop (list-ref board (sub1 posUp)) (- posUp2 3))))
                          (list (list-ref board posUp))
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-posX)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-posY)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (drop board (add1 pos)))
                      '())
                  '()
                  )))
      (if (= (- (length (list-ref board (sub1 posUp))) (length (list-ref board posUp))) 1)
          (if ( equal? (send (list-ref (list-ref board (sub1 posUp)) (add1 posUp2)) get-color) "white")
                  (append (take board (sub1 pos))
                          (list (append (take (list-ref board (sub1 posUp)) (add1 posUp2)) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (sub1 posUp)) (add1 posUp2)) get-posX)]
                                                                [posY (send (list-ref (list-ref board (sub1 posUp)) (add1 posUp2)) get-posY)]))
                                                                (drop (list-ref board (sub1 posUp)) (+ posUp2 2))))
                          (list (list-ref board posUp))
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-posX)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-posY)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (drop board (add1 pos)))
                  '())
          (if ( equal? (send (list-ref (list-ref board (sub1 posUp)) (+ posUp2 5)) get-color) "white")
                      (append (take board (sub1 posUp))
                          (list (append (take (list-ref board (sub1 posUp)) (+ posUp2 5)) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (sub1 posUp)) (+ posUp2 5)) get-posX)]
                                                                [posY (send (list-ref (list-ref board (sub1 posUp)) (+ posUp2 5)) get-posY)]))
                                                                (drop (list-ref board (sub1 posUp)) (+ posUp2 6))))
                          (list (list-ref board posUp))
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-posX)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-posY)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (drop board (add1 pos)))
                      '()))))

(define (check-move-down-left board pos pos2 c)
  (if (< (length (list-ref board (add1 pos))) (length (list-ref board pos)))
      (if (= pos2 0)
          '()
          (if (= (- (length (list-ref board pos)) (length (list-ref board (add1 pos)))) 1)
              (if ( equal? (send (list-ref (list-ref board (add1 pos)) (sub1 pos2)) get-color) "white")
                  (append (take board pos)
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-posX)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-posY)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (list (append (take (list-ref board (add1 pos)) (sub1 pos2)) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (add1 pos)) (sub1 pos2)) get-posX)]
                                                                [posY (send (list-ref (list-ref board (add1 pos)) (sub1 pos2)) get-posY)]))
                                                                (drop (list-ref board (add1 pos)) pos2)))
                          (drop board (+ pos 2)))
                  (if (= pos 15) '() (check-move-down-left-jump board pos pos2 (add1 pos)(sub1 pos2) c)))
              (if (and (> pos2 4) (< pos2 9))
                  (if ( equal? (send (list-ref (list-ref board (add1 pos)) (- pos2 5)) get-color) "white")
                      (append (take board pos)
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-posX)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-posY)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (list (append (take (list-ref board (add1 pos)) (- pos2 5)) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (add1 pos)) (- pos2 5)) get-posX)]
                                                                [posY (send (list-ref (list-ref board (add1 pos)) (- pos2 5)) get-posY)]))
                                                                (drop (list-ref board (add1 pos)) (- pos2 4))))
                          (drop board (+ pos 2)))
                      (if (= pos 15) '() (check-move-down-left-jump board pos pos2 (add1 pos)(- pos2 5) c)))
                  '()
                  )))
      (if (= (- (length (list-ref board (add1 pos))) (length (list-ref board pos))) 1)
          (if ( equal? (send (list-ref (list-ref board (add1 pos)) pos2) get-color) "white")
                  (append (take board pos)
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-posX)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-posY)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (list (append (take (list-ref board (add1 pos)) pos2) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (add1 pos)) pos2) get-posX)]
                                                                [posY (send (list-ref (list-ref board (add1 pos)) pos2) get-posY)]))
                                                                (drop (list-ref board (add1 pos)) (add1 pos2))))
                          (drop board (+ pos 2)))
                  (check-move-down-left-jump board pos pos2 (add1 pos) pos2 c))
          (if ( equal? (send (list-ref (list-ref board (add1 pos)) (+ pos2 4)) get-color) "white")
                      (append (take board pos)
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-posX)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-posY)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (list (append (take (list-ref board (add1 pos)) (+ pos2 4)) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (add1 pos)) (+ pos2 4)) get-posX)]
                                                                [posY (send (list-ref (list-ref board (add1 pos)) (+ pos2 4)) get-posY)]))
                                                                (drop (list-ref board (add1 pos)) (+ pos2 5))))
                          (drop board (+ pos 2)))
                      (check-move-down-left-jump board pos pos2 (add1 pos)(+ pos2 4) c)))))

(define (check-move-down-left-jump board pos pos2 posDown posDown2 c)
  (if (< (length (list-ref board (add1 posDown))) (length (list-ref board posDown)))
      (if (= posDown2 0)
          '()
          (if (= (- (length (list-ref board posDown)) (length (list-ref board (add1 posDown)))) 1)
              (if ( equal? (send (list-ref (list-ref board (add1 posDown)) (sub1 posDown2)) get-color) "white")
                  (append (take board pos)
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-posX)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-posY)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (list (list-ref board posDown))
                          (list (append (take (list-ref board (add1 posDown)) (sub1 posDown2)) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (add1 posDown)) (sub1 posDown2)) get-posX)]
                                                                [posY (send (list-ref (list-ref board (add1 posDown)) (sub1 posDown2)) get-posY)]))
                                                                (drop (list-ref board (add1 posDown)) posDown2)))
                          (drop board (+ posDown 2)))
                  '())
              (if (and (> posDown2 4) (< posDown2 9))
                  (if ( equal? (send (list-ref (list-ref board (add1 posDown)) (- posDown2 5)) get-color) "white")
                      (append (take board pos)
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-posX)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-posY)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (list (list-ref board posDown))
                          (list (append (take (list-ref board (add1 posDown)) (- posDown2 5)) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (add1 posDown)) (- posDown2 5)) get-posX)]
                                                                [posY (send (list-ref (list-ref board (add1 posDown)) (- posDown2 5)) get-posY)]))
                                                                (drop (list-ref board (add1 posDown)) (- posDown2 4))))
                          (drop board (+ posDown 2)))
                      '())
                  '()
                  )))
      (if (= (- (length (list-ref board (add1 posDown))) (length (list-ref board posDown))) 1)
          (if ( equal? (send (list-ref (list-ref board (add1 posDown)) posDown2) get-color) "white")
                  (append (take board pos)
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-posX)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-posY)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (list (list-ref board posDown))
                          (list (append (take (list-ref board (add1 posDown)) posDown2) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (add1 posDown)) posDown2) get-posX)]
                                                                [posY (send (list-ref (list-ref board (add1 posDown)) posDown2) get-posY)]))
                                                                (drop (list-ref board (add1 posDown)) (add1 posDown2))))
                          (drop board (+ posDown 2)))
                  '())
          (if ( equal? (send (list-ref (list-ref board (add1 posDown)) (+ posDown2 4)) get-color) "white")
                      (append (take board pos)
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-posX)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-posY)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (list (list-ref board posDown))
                          (list (append (take (list-ref board (add1 posDown)) (+ posDown2 4)) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (add1 posDown)) (+ posDown2 4)) get-posX)]
                                                                [posY (send (list-ref (list-ref board (add1 posDown)) (+ posDown2 4)) get-posY)]))
                                                                (drop (list-ref board (add1 posDown)) (+ posDown2 5))))
                          (drop board (+ posDown 2)))
                      '()))))

(define (check-move-down-right board pos pos2 c)
  (if (< (length (list-ref board (add1 pos))) (length (list-ref board pos)))
      (if (= pos2 (- (length (list-ref board pos)) 1))
          '()
          (if (= (- (length (list-ref board pos)) (length (list-ref board (add1 pos)))) 1)
              (if ( equal? (send (list-ref (list-ref board (add1 pos)) pos2) get-color) "white")
                  (append (take board pos)
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-posX)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-posY)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (list (append (take (list-ref board (add1 pos)) pos2) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (add1 pos)) pos2) get-posX)]
                                                                [posY (send (list-ref (list-ref board (add1 pos)) pos2) get-posY)]))
                                                                (drop (list-ref board (add1 pos)) (add1 pos2))))
                          (drop board (+ pos 2)))
                  (if (= pos 15) '() (check-move-down-right-jump board pos pos2 (add1 pos) pos2 c)))
              (if (and (> pos2 3) (< pos2 8))
                  (if ( equal? (send (list-ref (list-ref board (add1 pos)) (- pos2 4)) get-color) "white")
                      (append (take board pos)
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-posX)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-posY)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (list (append (take (list-ref board (add1 pos)) (- pos2 4)) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (add1 pos)) (- pos2 4)) get-posX)]
                                                                [posY (send (list-ref (list-ref board (add1 pos)) (- pos2 4)) get-posY)]))
                                                                (drop (list-ref board (add1 pos)) (- pos2 3))))
                          (drop board (+ pos 2)))
                      (if (= pos 15) '() (check-move-down-right-jump board pos pos2 (add1 pos)(- pos2 4) c)))
                  '()
                  )))
      (if (= (- (length (list-ref board (add1 pos))) (length (list-ref board pos))) 1)
          (if ( equal? (send (list-ref (list-ref board (add1 pos)) (add1 pos2)) get-color) "white")
                  (append (take board pos)
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-posX)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-posY)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (list (append (take (list-ref board (add1 pos)) (add1 pos2)) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (add1 pos)) (add1 pos2)) get-posX)]
                                                                [posY (send (list-ref (list-ref board (add1 pos)) (add1 pos2)) get-posY)]))
                                                                (drop (list-ref board (add1 pos)) (+ pos2 2))))
                          (drop board (+ pos 2)))
                  (check-move-down-right-jump board pos pos2 (add1 pos) (add1 pos2) c))
          (if ( equal? (send (list-ref (list-ref board (add1 pos)) (+ pos2 5)) get-color) "white")
                      (append (take board pos)
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-posX)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-posY)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (list (append (take (list-ref board (add1 pos)) (+ pos2 5)) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (add1 pos)) (+ pos2 5)) get-posX)]
                                                                [posY (send (list-ref (list-ref board (add1 pos)) (+ pos2 5)) get-posY)]))
                                                                (drop (list-ref board (add1 pos)) (+ pos2 6))))
                          (drop board (+ pos 2)))
                      (check-move-down-right-jump board pos pos2 (add1 pos)(+ pos2 5) c)))))

(define (check-move-down-right-jump board pos pos2 posDown posDown2 c)
  (if (< (length (list-ref board (add1 posDown))) (length (list-ref board posDown)))
      (if (= posDown2 (- (length (list-ref board posDown)) 1))
          '()
          (if (= (- (length (list-ref board posDown)) (length (list-ref board (add1 posDown)))) 1)
              (if ( equal? (send (list-ref (list-ref board (add1 posDown)) posDown2) get-color) "white")
                  (append (take board pos)
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-posX)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-posY)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (list (list-ref board posDown))
                          (list (append (take (list-ref board (add1 posDown)) posDown2) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (add1 posDown)) posDown2) get-posX)]
                                                                [posY (send (list-ref (list-ref board (add1 posDown)) posDown2) get-posY)]))
                                                                (drop (list-ref board (add1 posDown)) (add1 posDown2))))
                          (drop board (+ posDown 2)))
                  '())
              (if (and (> posDown2 3) (< posDown2 8))
                  (if ( equal? (send (list-ref (list-ref board (add1 posDown)) (- posDown2 4)) get-color) "white")
                      (append (take board pos)
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-posX)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-posY)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (list (list-ref board posDown))
                          (list (append (take (list-ref board (add1 posDown)) (- posDown2 4)) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (add1 posDown)) (- posDown2 4)) get-posX)]
                                                                [posY (send (list-ref (list-ref board (add1 posDown)) (- posDown2 4)) get-posY)]))
                                                                (drop (list-ref board (add1 posDown)) (- posDown2 3))))
                          (drop board (+ posDown 2)))
                      '())
                  '()
                  )))
      (if (= (- (length (list-ref board (add1 posDown))) (length (list-ref board posDown))) 1)
          (if ( equal? (send (list-ref (list-ref board (add1 posDown)) (add1 posDown2)) get-color) "white")
                  (append (take board pos)
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-posX)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-posY)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (list (list-ref board posDown))
                          (list (append (take (list-ref board (add1 posDown)) (add1 posDown2)) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (add1 posDown)) (add1 posDown2)) get-posX)]
                                                                [posY (send (list-ref (list-ref board (add1 posDown)) (add1 posDown2)) get-posY)]))
                                                                (drop (list-ref board (add1 posDown)) (+ posDown2 2))))
                          (drop board (+ posDown 2)))
                  '())
          (if ( equal? (send (list-ref (list-ref board (add1 posDown)) (+ posDown2 5)) get-color) "white")
                      (append (take board pos)
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-posX)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-posY)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (list (list-ref board posDown))
                          (list (append (take (list-ref board (add1 posDown)) (+ posDown2 5)) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (add1 posDown)) (+ posDown2 5)) get-posX)]
                                                                [posY (send (list-ref (list-ref board (add1 posDown)) (+ posDown2 5)) get-posY)]))
                                                                (drop (list-ref board (add1 posDown)) (+ posDown2 6))))
                          (drop board (+ posDown 2)))
                      '()))))

(define (check-move-left board pos pos2 c)
  (if (= pos2 0)
      '()
      (if ( equal? (send (list-ref (list-ref board pos) (sub1 pos2)) get-color) "white")
          (append (take board pos)
                  (list (append (take (list-ref board pos) (sub1 pos2)) (list (new cCheckersBox%
                                                                                     [color c]
                                                                                     [posX (send (list-ref (list-ref board pos) (sub1 pos2)) get-posX)]
                                                                                     [posY (send (list-ref (list-ref board pos) (sub1 pos2)) get-posY)])
                                                                                (new cCheckersBox%
                                                                                     [color "white"]
                                                                                     [posX (send (list-ref (list-ref board pos) pos2) get-posX)]
                                                                                     [posY (send (list-ref (list-ref board pos) pos2) get-posY)]))
                                (drop (list-ref board pos) (add1 pos2))))
                  (drop board (add1 pos)))
          (if (= pos2 1)
              '()
              (if ( equal? (send (list-ref (list-ref board pos) (- pos2 2)) get-color) "white")
                  (append (take board pos)
                          (list (append (take (list-ref board pos) (- pos2 2)) (list (new cCheckersBox%
                                                                                     [color c]
                                                                                     [posX (send (list-ref (list-ref board pos) (- pos2 2)) get-posX)]
                                                                                     [posY (send (list-ref (list-ref board pos) (- pos2 2)) get-posY)])
                                                                                (list-ref (list-ref board pos) (sub1 pos2))
                                                                                (new cCheckersBox%
                                                                                     [color "white"]
                                                                                     [posX (send (list-ref (list-ref board pos) pos2) get-posX)]
                                                                                     [posY (send (list-ref (list-ref board pos) pos2) get-posY)]))
                                (drop (list-ref board pos) (add1 pos2))))
                  (drop board (add1 pos)))
                  '())))))

(define (check-move-right board pos pos2 c)
  (if (= pos2 (- (length (list-ref board pos)) 1))
      '()
      (if ( equal? (send (list-ref (list-ref board pos) (add1 pos2)) get-color) "white")
          (append (take board pos)
                  (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                                     [color "white"]
                                                                                     [posX (send (list-ref (list-ref board pos) pos2) get-posX)]
                                                                                     [posY (send (list-ref (list-ref board pos) pos2) get-posY)])
                                                                                (new cCheckersBox%
                                                                                     [color c]
                                                                                     [posX (send (list-ref (list-ref board pos) (add1 pos2)) get-posX)]
                                                                                     [posY (send (list-ref (list-ref board pos) (add1 pos2)) get-posY)]))
                                (drop (list-ref board pos) (+ pos2 2))))
                  (drop board (add1 pos)))
          (if (= pos2 (- (length (list-ref board pos)) 2))
              '()
              (if ( equal? (send (list-ref (list-ref board pos) (+ pos2 2)) get-color) "white")
                  (append (take board pos)
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                                     [color "white"]
                                                                                     [posX (send (list-ref (list-ref board pos) pos2) get-posX)]
                                                                                     [posY (send (list-ref (list-ref board pos) pos2) get-posY)])
                                                                                (list-ref (list-ref board pos) (add1 pos2))
                                                                                (new cCheckersBox%
                                                                                     [color c]
                                                                                     [posX (send (list-ref (list-ref board pos) (+ pos2 2)) get-posX)]
                                                                                     [posY (send (list-ref (list-ref board pos) (+ pos2 2)) get-posY)]))
                                (drop (list-ref board pos) (+ pos2 3))))
                  (drop board (add1 pos)))
                  '())))))

(define lst (list 1 2 3 4 5))


; Muestra la ventana
(send frame show #t)
; Hace grande la ventana
(send frame maximize #t)