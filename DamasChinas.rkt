#lang racket
(require racket/gui/base)
(require "board.rkt")
(require "cCheckersBox.rkt")


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
        (send dc set-text-foreground "black")
        (send (send mc get-dc) set-brush "white" 'solid)
        (send dc draw-rounded-rectangle 490 10 110 30)
        (send dc draw-text "Nuevo Juego" 500 15)
        (send dc set-text-foreground "white")
        (send (send mc get-dc) set-brush "blue" 'solid)
        (send dc draw-rounded-rectangle 490 300 110 45)
        (send dc draw-text "Confirmar" 510 305)
        (send dc draw-text "Movimiento" 505 320)
        (send game new-game) ; Se crea un nuevo juego
        (send game paint-board (send game get-current-game) mc) ; Se pinta el tablero inicial
        ;(paint-board (list-ref (check-possible-moves (send game get-current-game) "red") 0) mc)
        )]))

#|
check-possible-moves se encarga de revisar una posición del tablero
y devolver todos los posibles moviemientos que pueden hacer las
fichas de un color.
Parámetros:
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
- color: el color de las fichas de las que se quieren
  saber sus posibles movimientos
Salida:
- Una lista de tableros que representan los movimientos 
|#
(define (check-possible-moves board color)
  (check-possible-moves-aux board color 0 (length board) '()))


#|
check-possible-moves-aux se encarga de ir revisando
fila por fila del tablero devolviendo los posibles
movimientos de las fichas de un color es llamada por
check-possible-moves
Parámetros:
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
- color: el color de las fichas de las que se quieren
  saber sus posibles movimientos
- pos: el número de fila actual
- max: la cantidad de filas que tiene el tablero
- moves: los movimientos que se van obteniendo
Salida:
- Una lista de tableros que representan los movimientos 
|#
(define (check-possible-moves-aux board color pos max moves)
  (if (= pos max)
      moves
      (check-possible-moves-aux
       board color (add1 pos) max (check-possible-moves-aux2 board color pos 0 (length (list-ref board pos)) moves))))

#|
check-possible-moves-aux2 se encarga de recorrer una fila
recopilando los movimientos de las casillas que se encuentre
del color que se indique, es llamada por check-possible-moves-aux
Parámetros:
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
- color: el color de las fichas de las que se quieren
  saber sus posibles movimientos
- pos: el número de fila actual
- pos2: el número de columna que se está revisando
  actualmente
- max: la cantidad de columnas que tiene la fila
- moves: los movimientos que se van obteniendo
Salida:
- Una lista de tableros que representan los movimientos 
|#
(define (check-possible-moves-aux2 board color pos pos2 max moves)
  (if (= pos2 max)
      moves
      (if (equal? (send (list-ref (list-ref board pos) pos2) get-color) color)
          (check-possible-moves-aux2 board color pos (add1 pos2) max
                                  (check-possible-moves-pos board pos pos2 color moves))
          (check-possible-moves-aux2 board color pos (add1 pos2) max moves))))

#|
check-possible-moves-pos se encarga de obtener todos
los movimientos posibles de una ficha, es llamada por
check-possible-moves-aux2
Parámetros:
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
- pos: el número de la fila donde se encuentra la
  ficha
- pos2: el número de la columna donde se encuentra
  la ficha
- color: el color de la casilla
- moves: los movimientos que se van obteniendo
Salida:
- Una lista de tableros que representan los movimientos
  que puede realizar esa ficha
|#
(define (check-possible-moves-pos board pos pos2 color moves)
  (append moves
   (append(if (= pos 0)
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
   ))

#|
check-move-up-left revisa si una ficha se puede mover
hacia arriba a la izquierda.
Parámetros:
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
- pos: la fila donde se encuentra la ficha
- pos2: la columna donde se encuentra la ficha
- c: el color de la ficha
Salida:
- El movimiento que puede realizar la ficha hacia
  arriba a la izquierda o una lista vacía
|#
(define (check-move-up-left board pos pos2 c)
  (if (< (length (list-ref board (sub1 pos))) (length (list-ref board pos)))
      (if (= pos2 0)
          '()
          (if (= (- (length (list-ref board pos)) (length (list-ref board (sub1 pos)))) 1)
              (if ( equal? (send (list-ref (list-ref board (sub1 pos)) (sub1 pos2)) get-color) "white")
                  (list (append (take board (sub1 pos))
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
                          (drop board (add1 pos))))
                  (if (= pos 1) '() (check-move-up-left-jump board pos pos2 (sub1 pos)(sub1 pos2) c)))
              (if (and (> pos2 4) (< pos2 9))
                  (if ( equal? (send (list-ref (list-ref board (sub1 pos)) (- pos2 5)) get-color) "white")
                      (list (append (take board (sub1 pos))
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
                          (drop board (add1 pos))))
                      (if (= pos 1) '() (check-move-up-left-jump board pos pos2 (sub1 pos)(- pos2 5) c)))
                  '()
                  )))
      (if (= (- (length (list-ref board (sub1 pos))) (length (list-ref board pos))) 1)
          (if ( equal? (send (list-ref (list-ref board (sub1 pos)) pos2) get-color) "white")
                  (list (append (take board (sub1 pos))
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
                          (drop board (add1 pos))))
                  (check-move-up-left-jump board pos pos2 (sub1 pos) pos2 c))
          (if ( equal? (send (list-ref (list-ref board (sub1 pos)) (+ pos2 4)) get-color) "white")
                      (list (append (take board (sub1 pos))
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
                          (drop board (add1 pos))))
                      (check-move-up-left-jump board pos pos2 (sub1 pos)(+ pos2 4) c)))))

#|
En caso de que la ficha no pueda moverse hacia arriba
a la izquierda check-move-up-left-jump se encarga de
revisar si la ficha puede hacer un salto en esta
dirección, si lo puede hacer se llama a la función
check-jumps para revisar si puede realizar más saltos
Parámetros:
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
- pos: la fila donde se encuentra la ficha
- pos2: la columna donde se encuentra la ficha
- posUp: la fila donde se encuentra el espacio
  que está arriba a la izquierda de la ficha
- pos2: la columna donde se encuentra el espacio
  que está arriba a la izquierda de la ficha
- c: el color de la ficha
Salida:
- Una lista de movimientos que puede hacer a través de
  saltos o una lista vacía
|#
(define (check-move-up-left-jump board pos pos2 posUp posUp2 c)
  (if (< (length (list-ref board (sub1 posUp))) (length (list-ref board posUp)))
      (if (= posUp2 0)
          '()
          (if (= (- (length (list-ref board posUp)) (length (list-ref board (sub1 posUp)))) 1)
              (if ( equal? (send (list-ref (list-ref board (sub1 posUp)) (sub1 posUp2)) get-color) "white")
                  (check-jumps
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
                   (sub1 posUp) (sub1 posUp2) "up-left")
                  '())
              (if (and (> posUp2 4) (< posUp2 9))
                  (if ( equal? (send (list-ref (list-ref board (sub1 posUp)) (- posUp2 5)) get-color) "white")
                      (check-jumps
                   (append (take board (sub1 posUp))
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
                   (sub1 posUp) (- posUp2 5) "up-left")
                      '())
                  '()
                  )))
      (if (= (- (length (list-ref board (sub1 posUp))) (length (list-ref board posUp))) 1)
          (if ( equal? (send (list-ref (list-ref board (sub1 posUp)) posUp2) get-color) "white")
              (check-jumps
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
                   (sub1 posUp) posUp2 "up-left")
                  '())
          (if ( equal? (send (list-ref (list-ref board (sub1 posUp)) (+ posUp2 4)) get-color) "white")
              (check-jumps
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
                   (sub1 posUp) (+ posUp2 4) "up-left")
                      '()))))

#|
check-move-up-right revisa si una ficha se puede mover
hacia arriba a la derecha.
Parámetros:
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
- pos: la fila donde se encuentra la ficha
- pos2: la columna donde se encuentra la ficha
- c: el color de la ficha
Salida:
- El movimiento que puede realizar la ficha hacia
  arriba a la derecha o una lista vacía
|#
(define (check-move-up-right board pos pos2 c)
  (if (< (length (list-ref board (sub1 pos))) (length (list-ref board pos)))
      (if (= pos2 (- (length (list-ref board pos)) 1))
          '()
          (if (= (- (length (list-ref board pos)) (length (list-ref board (sub1 pos)))) 1)
              (if ( equal? (send (list-ref (list-ref board (sub1 pos)) pos2) get-color) "white")
                  (list (append (take board (sub1 pos))
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
                          (drop board (add1 pos))))
                  (if (= pos 1) '() (check-move-up-right-jump board pos pos2 (sub1 pos) pos2 c)))
              (if (and (> pos2 3) (< pos2 8))
                  (if ( equal? (send (list-ref (list-ref board (sub1 pos)) (- pos2 4)) get-color) "white")
                      (list (append (take board (sub1 pos))
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
                          (drop board (add1 pos))))
                      (if (= pos 1) '() (check-move-up-right-jump board pos pos2 (sub1 pos)(- pos2 4) c)))
                  '()
                  )))
      (if (= (- (length (list-ref board (sub1 pos))) (length (list-ref board pos))) 1)
          (if ( equal? (send (list-ref (list-ref board (sub1 pos)) (add1 pos2)) get-color) "white")
                  (list (append (take board (sub1 pos))
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
                          (drop board (add1 pos))))
                  (check-move-up-right-jump board pos pos2 (sub1 pos) (add1 pos2) c))
          (if ( equal? (send (list-ref (list-ref board (sub1 pos)) (+ pos2 5)) get-color) "white")
                      (list (append (take board (sub1 pos))
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
                          (drop board (add1 pos))))
                      (check-move-up-right-jump board pos pos2 (sub1 pos)(+ pos2 5) c)))))


#|
En caso de que la ficha no pueda moverse hacia arriba
a la derecha check-move-up-right-jump se encarga de
revisar si la ficha puede hacer un salto en esta
dirección, si lo puede hacer se llama a la función
check-jumps para revisar si puede realizar más saltos
Parámetros:
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
- pos: la fila donde se encuentra la ficha
- pos2: la columna donde se encuentra la ficha
- posUp: la fila donde se encuentra el espacio
  que está arriba a la derecha de la ficha
- pos2: la columna donde se encuentra el espacio
  que está arriba a la derecha de la ficha
- c: el color de la ficha
Salida:
- Una lista de movimientos que puede hacer a través de
  saltos o una lista vacía
|#
(define (check-move-up-right-jump board pos pos2 posUp posUp2 c)
  (if (< (length (list-ref board (sub1 posUp))) (length (list-ref board posUp)))
      (if (= posUp2 (- (length (list-ref board posUp)) 1))
          '()
          (if (= (- (length (list-ref board posUp)) (length (list-ref board (sub1 posUp)))) 1)
              (if ( equal? (send (list-ref (list-ref board (sub1 posUp)) posUp2) get-color) "white")
                  (check-jumps
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
                   (sub1 posUp) posUp2 "up-right")
                  '())
              (if (and (> posUp2 3) (< posUp2 8))
                  (if ( equal? (send (list-ref (list-ref board (sub1 posUp)) (- posUp2 4)) get-color) "white")
                      (check-jumps
                   (append (take board (sub1 posUp))
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
                   (sub1 posUp) (- posUp2 4) "up-right")
                      '())
                  '()
                  )))
      (if (= (- (length (list-ref board (sub1 posUp))) (length (list-ref board posUp))) 1)
          (if ( equal? (send (list-ref (list-ref board (sub1 posUp)) (add1 posUp2)) get-color) "white")
              (check-jumps
                   (append (take board (sub1 posUp))
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
                   (sub1 posUp) (add1 posUp2) "up-right")
                  '())
          (if ( equal? (send (list-ref (list-ref board (sub1 posUp)) (+ posUp2 5)) get-color) "white")
              (check-jumps
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
                   (sub1 posUp) (+ posUp2 5) "up-right")
                      '()))))

#|
check-move-down-left revisa si una ficha se puede mover
hacia abajo a la izquierda.
Parámetros:
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
- pos: la fila donde se encuentra la ficha
- pos2: la columna donde se encuentra la ficha
- c: el color de la ficha
Salida:
- El movimiento que puede realizar la ficha hacia
  abajo a la izquierda o una lista vacía
|#
(define (check-move-down-left board pos pos2 c)
  (if (< (length (list-ref board (add1 pos))) (length (list-ref board pos)))
      (if (= pos2 0)
          '()
          (if (= (- (length (list-ref board pos)) (length (list-ref board (add1 pos)))) 1)
              (if ( equal? (send (list-ref (list-ref board (add1 pos)) (sub1 pos2)) get-color) "white")
                  (list (append (take board pos)
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
                          (drop board (+ pos 2))))
                  (if (= pos 15) '() (check-move-down-left-jump board pos pos2 (add1 pos)(sub1 pos2) c)))
              (if (and (> pos2 4) (< pos2 9))
                  (if ( equal? (send (list-ref (list-ref board (add1 pos)) (- pos2 5)) get-color) "white")
                      (list (append (take board pos)
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
                          (drop board (+ pos 2))))
                      (if (= pos 15) '() (check-move-down-left-jump board pos pos2 (add1 pos)(- pos2 5) c)))
                  '()
                  )))
      (if (= (- (length (list-ref board (add1 pos))) (length (list-ref board pos))) 1)
          (if ( equal? (send (list-ref (list-ref board (add1 pos)) pos2) get-color) "white")
                  (list (append (take board pos)
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
                          (drop board (+ pos 2))))
                  (check-move-down-left-jump board pos pos2 (add1 pos) pos2 c))
          (if ( equal? (send (list-ref (list-ref board (add1 pos)) (+ pos2 4)) get-color) "white")
                      (list (append (take board pos)
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
                          (drop board (+ pos 2))))
                      (check-move-down-left-jump board pos pos2 (add1 pos)(+ pos2 4) c)))))


#|
En caso de que la ficha no pueda moverse hacia abajo
a la izquierda check-move-down-left-jump se encarga de
revisar si la ficha puede hacer un salto en esta
dirección, si lo puede hacer se llama a la función
check-jumps para revisar si puede realizar más saltos
Parámetros:
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
- pos: la fila donde se encuentra la ficha
- pos2: la columna donde se encuentra la ficha
- posUp: la fila donde se encuentra el espacio
  que está abajo a la izquierda de la ficha
- pos2: la columna donde se encuentra el espacio
  que está abajo a la izquierda de la ficha
- c: el color de la ficha
Salida:
- Una lista de movimientos que puede hacer a través de
  saltos o una lista vacía
|#
(define (check-move-down-left-jump board pos pos2 posDown posDown2 c)
  (if (< (length (list-ref board (add1 posDown))) (length (list-ref board posDown)))
      (if (= posDown2 0)
          '()
          (if (= (- (length (list-ref board posDown)) (length (list-ref board (add1 posDown)))) 1)
              (if ( equal? (send (list-ref (list-ref board (add1 posDown)) (sub1 posDown2)) get-color) "white")
                  (check-jumps
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
                   (add1 posDown) (sub1 posDown2) "down-left")
                  '())
              (if (and (> posDown2 4) (< posDown2 9))
                  (if ( equal? (send (list-ref (list-ref board (add1 posDown)) (- posDown2 5)) get-color) "white")
                      (check-jumps
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
                   (add1 posDown) (- posDown2 5) "down-left")
                      '())
                  '()
                  )))
      (if (= (- (length (list-ref board (add1 posDown))) (length (list-ref board posDown))) 1)
          (if ( equal? (send (list-ref (list-ref board (add1 posDown)) posDown2) get-color) "white")
              (check-jumps
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
                   (add1 posDown) posDown2 "down-left")
                  '())
          (if ( equal? (send (list-ref (list-ref board (add1 posDown)) (+ posDown2 4)) get-color) "white")
              (check-jumps
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
                   (add1 posDown) (+ posDown2 4) "down-left")
                      '()))))

#|
check-move-down-right revisa si una ficha se puede mover
hacia abajo a la derecha.
Parámetros:
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
- pos: la fila donde se encuentra la ficha
- pos2: la columna donde se encuentra la ficha
- c: el color de la ficha
Salida:
- El movimiento que puede realizar la ficha hacia
  abajo a la derecha o una lista vacía
|#
(define (check-move-down-right board pos pos2 c)
  (if (< (length (list-ref board (add1 pos))) (length (list-ref board pos)))
      (if (= pos2 (- (length (list-ref board pos)) 1))
          '()
          (if (= (- (length (list-ref board pos)) (length (list-ref board (add1 pos)))) 1)
              (if ( equal? (send (list-ref (list-ref board (add1 pos)) pos2) get-color) "white")
                  (list (append (take board pos)
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
                          (drop board (+ pos 2))))
                  (if (= pos 15) '() (check-move-down-right-jump board pos pos2 (add1 pos) pos2 c)))
              (if (and (> pos2 3) (< pos2 8))
                  (if ( equal? (send (list-ref (list-ref board (add1 pos)) (- pos2 4)) get-color) "white")
                      (list (append (take board pos)
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
                          (drop board (+ pos 2))))
                      (if (= pos 15) '() (check-move-down-right-jump board pos pos2 (add1 pos)(- pos2 4) c)))
                  '()
                  )))
      (if (= (- (length (list-ref board (add1 pos))) (length (list-ref board pos))) 1)
          (if ( equal? (send (list-ref (list-ref board (add1 pos)) (add1 pos2)) get-color) "white")
                  (list (append (take board pos)
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
                          (drop board (+ pos 2))))
                  (check-move-down-right-jump board pos pos2 (add1 pos) (add1 pos2) c))
          (if ( equal? (send (list-ref (list-ref board (add1 pos)) (+ pos2 5)) get-color) "white")
                      (list (append (take board pos)
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
                          (drop board (+ pos 2))))
                      (check-move-down-right-jump board pos pos2 (add1 pos)(+ pos2 5) c)))))

#|
En caso de que la ficha no pueda moverse hacia abajo
a la derecha check-move-down-right-jump se encarga de
revisar si la ficha puede hacer un salto en esta
dirección, si lo puede hacer se llama a la función
check-jumps para revisar si puede realizar más saltos
Parámetros:
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
- pos: la fila donde se encuentra la ficha
- pos2: la columna donde se encuentra la ficha
- posUp: la fila donde se encuentra el espacio
  que está abajo a la derecha de la ficha
- pos2: la columna donde se encuentra el espacio
  que está abajo a la derecha de la ficha
- c: el color de la ficha
Salida:
- Una lista de movimientos que puede hacer a través de
  saltos o una lista vacía
|#
(define (check-move-down-right-jump board pos pos2 posDown posDown2 c)
  (if (< (length (list-ref board (add1 posDown))) (length (list-ref board posDown)))
      (if (= posDown2 (- (length (list-ref board posDown)) 1))
          '()
          (if (= (- (length (list-ref board posDown)) (length (list-ref board (add1 posDown)))) 1)
              (if ( equal? (send (list-ref (list-ref board (add1 posDown)) posDown2) get-color) "white")
                  (check-jumps
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
                   (add1 posDown) posDown2 "down-right")
                  '())
              (if (and (> posDown2 3) (< posDown2 8))
                  (if ( equal? (send (list-ref (list-ref board (add1 posDown)) (- posDown2 4)) get-color) "white")
                      (check-jumps
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
                   (add1 posDown) (- posDown2 4) "down-right")
                      '())
                  '()
                  )))
      (if (= (- (length (list-ref board (add1 posDown))) (length (list-ref board posDown))) 1)
          (if ( equal? (send (list-ref (list-ref board (add1 posDown)) (add1 posDown2)) get-color) "white")
              (check-jumps
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
                   (add1 posDown) (add1 posDown2) "down-right")
                  '())
          (if ( equal? (send (list-ref (list-ref board (add1 posDown)) (+ posDown2 5)) get-color) "white")
              (check-jumps
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
                   (add1 posDown) (+ posDown2 5) "down-right")
                      '()))))

#|
check-move-down-right revisa si una ficha se puede mover
hacia la izquierda y si no fuera el caso si puede realizar
un salto hacia este lado
Parámetros:
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
- pos: la fila donde se encuentra la ficha
- pos2: la columna donde se encuentra la ficha
- c: el color de la ficha
Salida:
- El movimiento que puede realizar la ficha hacia
  la izquierda o una lista vacía
|#
(define (check-move-left board pos pos2 c)
  (if (= pos2 0)
      '()
      (if ( equal? (send (list-ref (list-ref board pos) (sub1 pos2)) get-color) "white")
          (list (append (take board pos)
                  (list (append (take (list-ref board pos) (sub1 pos2)) (list (new cCheckersBox%
                                                                                     [color c]
                                                                                     [posX (send (list-ref (list-ref board pos) (sub1 pos2)) get-posX)]
                                                                                     [posY (send (list-ref (list-ref board pos) (sub1 pos2)) get-posY)])
                                                                                (new cCheckersBox%
                                                                                     [color "white"]
                                                                                     [posX (send (list-ref (list-ref board pos) pos2) get-posX)]
                                                                                     [posY (send (list-ref (list-ref board pos) pos2) get-posY)]))
                                (drop (list-ref board pos) (add1 pos2))))
                  (drop board (add1 pos))))
          (if (= pos2 1)
              '()
              (if ( equal? (send (list-ref (list-ref board pos) (- pos2 2)) get-color) "white")
                  (check-jumps
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
                   pos
                   (- pos2 2)
                   "left")
                  '())))))

#|
check-move-down-right revisa si una ficha se puede mover
hacia la derecha y si no fuera el caso si puede realizar
un salto hacia este lado
Parámetros:
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
- pos: la fila donde se encuentra la ficha
- pos2: la columna donde se encuentra la ficha
- c: el color de la ficha
Salida:
- El movimiento que puede realizar la ficha hacia
  la derecha o una lista vacía
|#
(define (check-move-right board pos pos2 c)
  (if (= pos2 (- (length (list-ref board pos)) 1))
      '()
      (if ( equal? (send (list-ref (list-ref board pos) (add1 pos2)) get-color) "white")
          (list (append (take board pos)
                  (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                                     [color "white"]
                                                                                     [posX (send (list-ref (list-ref board pos) pos2) get-posX)]
                                                                                     [posY (send (list-ref (list-ref board pos) pos2) get-posY)])
                                                                                (new cCheckersBox%
                                                                                     [color c]
                                                                                     [posX (send (list-ref (list-ref board pos) (add1 pos2)) get-posX)]
                                                                                     [posY (send (list-ref (list-ref board pos) (add1 pos2)) get-posY)]))
                                (drop (list-ref board pos) (+ pos2 2))))
                  (drop board (add1 pos))))
          (if (= pos2 (- (length (list-ref board pos)) 2))
              '()
              (if ( equal? (send (list-ref (list-ref board pos) (+ pos2 2)) get-color) "white")
                  (check-jumps
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
                   pos
                   (+ pos2 2)
                   "right")
                  '()
                  )
              )
          )
      )
  )

#|
Cuando una ficha realiza un salto, puede realizar más
saltos seguidos en un solo turno, la función check-jumps
se encarga de revisar si una ficha que realizó un salto
puede realizar más saltos
Parámetros:
- board: recibe un tablero con la posición del
  salto realizado (lista de listas de objetos
  cCheckersBox)
- pos: la fila donde se encuentra la ficha
- pos2: la columna donde se encuentra la ficha
- direction: la dirección del salto realizado
  con el fin de no volver a revisar en esa
  dirección
Salida:
- Una lista de movimientos que puede hacer a
  través de saltos
|#
(define (check-jumps board pos pos2 direction)
  (append
   (list board)
   (if (equal? direction "up-right") '() (check-jump-down-left board pos pos2))
   (if (equal? direction "up-left") '() (check-jump-down-right board pos pos2))
   (if (equal? direction "down-right") '() (check-jump-up-left board pos pos2))
   (if (equal? direction "down-left") '() (check-jump-up-right board pos pos2))
   (if (equal? direction "right") '() (check-jump-left board pos pos2))
   (if (equal? direction "left") '() (check-jump-right board pos pos2)))
  )

#|
check-jump-down-left revisa si una ficha puede realizar
un salto hacia abajo a la izquierda
Entrada:
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
- pos: el número de fila de la ficha
- pos2: el número de columna de la ficha
Salida:
- Una lista de movimientos que puede hacer a
  través de saltos
|#
(define (check-jump-down-left board pos pos2)
  (if (< (length (list-ref board (add1 pos))) (length (list-ref board pos)))
      (if (= pos2 0)
          '()
          (if (= (- (length (list-ref board pos)) (length (list-ref board (add1 pos)))) 1)
              (if (or (= pos 15)
                      (equal? (send (list-ref (list-ref board (add1 pos)) (sub1 pos2)) get-color) "white")) 
                  '()
                  (check-jump-down-left-aux board (add1 pos)(sub1 pos2) pos pos2))
              (if (and (> pos2 4) (< pos2 9))
                  (if (or (= pos 15)
                          (equal? (send (list-ref (list-ref board (add1 pos)) (- pos2 5)) get-color) "white"))
                      '()
                      (check-jump-down-left-aux board (add1 pos)(- pos2 5) pos pos2))
                  '()
                  )))
      (if (= (- (length (list-ref board (add1 pos))) (length (list-ref board pos))) 1)
          (if ( equal? (send (list-ref (list-ref board (add1 pos)) pos2) get-color) "white")
                  '()
                  (check-jump-down-left-aux board (add1 pos) pos2 pos pos2))
          (if ( equal? (send (list-ref (list-ref board (add1 pos)) (+ pos2 4)) get-color) "white")
                      '()
                      (check-jump-down-left-aux board (add1 pos)(+ pos2 4) pos pos2)))))

#|
check-jump-down-left-aux es usada por check-jump-down-left
ayudando a revisar si la ficha puede realizar un salto hacia
abajo a la izquierda
Entrada:
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
- posDown: el número de fila de la casilla que está abajo a
  la izquierda de la ficha
- posDown2: el número de columna de la casilla que está abajo a
  la izquierda de la ficha
- pos: el número de fila de la ficha
- pos2: el número de columna de la ficha
Salida:
- Una lista de movimientos que puede hacer a
  través de saltos
|#
(define (check-jump-down-left-aux board posDown posDown2 pos pos2)
  (if (< (length (list-ref board (add1 posDown))) (length (list-ref board posDown)))
      (if (= posDown2 0)
          '()
          (if (= (- (length (list-ref board posDown)) (length (list-ref board (add1 posDown)))) 1)
              (if ( equal? (send (list-ref (list-ref board (add1 posDown)) (sub1 posDown2)) get-color) "white")
                  (check-jumps
                   (append (take board pos)
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-posX)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-posY)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (list (list-ref board posDown))
                          (list (append (take (list-ref board (add1 posDown)) (sub1 posDown2)) (list (new cCheckersBox%
                                                                [color (send (list-ref (list-ref board pos) pos2) get-color)]
                                                                [posX (send (list-ref (list-ref board (add1 posDown)) (sub1 posDown2)) get-posX)]
                                                                [posY (send (list-ref (list-ref board (add1 posDown)) (sub1 posDown2)) get-posY)]))
                                                                (drop (list-ref board (add1 posDown)) posDown2)))
                          (drop board (+ posDown 2)))
                   (add1 posDown) (sub1 posDown2) "down-left")
                  '())
              (if (and (> posDown2 4) (< posDown2 9))
                  (if ( equal? (send (list-ref (list-ref board (add1 posDown)) (- posDown2 5)) get-color) "white")
                      (check-jumps
                       (append (take board pos)
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-posX)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-posY)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (list (list-ref board posDown))
                          (list (append (take (list-ref board (add1 posDown)) (- posDown2 5)) (list (new cCheckersBox%
                                                                [color (send (list-ref (list-ref board pos) pos2) get-color)]
                                                                [posX (send (list-ref (list-ref board (add1 posDown)) (- posDown2 5)) get-posX)]
                                                                [posY (send (list-ref (list-ref board (add1 posDown)) (- posDown2 5)) get-posY)]))
                                                                (drop (list-ref board (add1 posDown)) (- posDown2 4))))
                          (drop board (+ posDown 2)))
                       (add1 posDown) (- posDown2 5) "down-left")
                      '())
                  '()
                  )))
      (if (= (- (length (list-ref board (add1 posDown))) (length (list-ref board posDown))) 1)
          (if ( equal? (send (list-ref (list-ref board (add1 posDown)) posDown2) get-color) "white")
                  (check-jumps
                       (append (take board pos)
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-posX)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-posY)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (list (list-ref board posDown))
                          (list (append (take (list-ref board (add1 posDown)) posDown2) (list (new cCheckersBox%
                                                                [color (send (list-ref (list-ref board pos) pos2) get-color)]
                                                                [posX (send (list-ref (list-ref board (add1 posDown)) posDown2) get-posX)]
                                                                [posY (send (list-ref (list-ref board (add1 posDown)) posDown2) get-posY)]))
                                                                (drop (list-ref board (add1 posDown)) (add1 posDown2))))
                          (drop board (+ posDown 2)))
                       (add1 posDown) posDown2 "down-left")
                  '())
          (if ( equal? (send (list-ref (list-ref board (add1 posDown)) (+ posDown2 4)) get-color) "white")
                      (check-jumps
                       (append (take board pos)
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-posX)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-posY)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (list (list-ref board posDown))
                          (list (append (take (list-ref board (add1 posDown)) (+ posDown2 4)) (list (new cCheckersBox%
                                                                [color (send (list-ref (list-ref board pos) pos2) get-color)]
                                                                [posX (send (list-ref (list-ref board (add1 posDown)) (+ posDown2 4)) get-posX)]
                                                                [posY (send (list-ref (list-ref board (add1 posDown)) (+ posDown2 4)) get-posY)]))
                                                                (drop (list-ref board (add1 posDown)) (+ posDown2 5))))
                          (drop board (+ posDown 2)))
                       (add1 posDown) (+ posDown2 4) "down-left")
                      '()))))

#|
check-jump-down-right revisa si una casilla puede realizar
un salto hacia abajo a la derecha
Entrada:
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
- pos: el número de fila de la ficha
- pos2: el número de columna de la ficha
Salida:
- Una lista de movimientos que puede hacer a
  través de saltos
|#
(define (check-jump-down-right board pos pos2)
  (if (< (length (list-ref board (add1 pos))) (length (list-ref board pos)))
      (if (= pos2 (- (length (list-ref board pos)) 1))
          '()
          (if (= (- (length (list-ref board pos)) (length (list-ref board (add1 pos)))) 1)
              (if (or (>= pos 15) ( equal? (send (list-ref (list-ref board (add1 pos)) pos2) get-color) "white"))
                  '()
                  (check-jump-down-right-aux board (add1 pos) pos2 pos pos2))
              (if (and (> pos2 3) (< pos2 8))
                  (if (or (>= pos 15) ( equal? (send (list-ref (list-ref board (add1 pos)) (- pos2 4)) get-color) "white"))
                      '()
                      (check-jump-down-right-aux board (add1 pos)(- pos2 4) pos pos2))
                  '()
                  )))
      (if (= (- (length (list-ref board (add1 pos))) (length (list-ref board pos))) 1)
          (if ( equal? (send (list-ref (list-ref board (add1 pos)) (add1 pos2)) get-color) "white")
                  '()
                  (check-jump-down-right-aux board (add1 pos) (add1 pos2) pos pos2))
          (if ( equal? (send (list-ref (list-ref board (add1 pos)) (+ pos2 5)) get-color) "white")
                      '()
                      (check-jump-down-right-aux board (add1 pos)(+ pos2 5) pos pos2))))
  )

#|
check-jump-down-right-aux es usada por check-jump-down-right
ayudando a revisar si la ficha puede realizar un salto hacia
abajo a la derecha
Entrada:
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
- posDown: el número de fila de la casilla que está abajo a
  la derecha de la ficha
- posDown2 el número de columna de la casilla que está abajo a
  la derecha de la ficha
- pos: el número de fila de la ficha
- pos2: el número de columna de la ficha
Salida:
- Una lista de movimientos que puede hacer a
  través de saltos
|#
(define (check-jump-down-right-aux board posDown posDown2 pos pos2)
  (if (< (length (list-ref board (add1 posDown))) (length (list-ref board posDown)))
      (if (= posDown2 (- (length (list-ref board posDown)) 1))
          '()
          (if (= (- (length (list-ref board posDown)) (length (list-ref board (add1 posDown)))) 1)
              (if ( equal? (send (list-ref (list-ref board (add1 posDown)) posDown2) get-color) "white")
                  (check-jumps
                       (append (take board pos)
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-posX)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-posY)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (list (list-ref board posDown))
                          (list (append (take (list-ref board (add1 posDown)) posDown2) (list (new cCheckersBox%
                                                                [color (send (list-ref (list-ref board pos) pos2) get-color)]
                                                                [posX (send (list-ref (list-ref board (add1 posDown)) posDown2) get-posX)]
                                                                [posY (send (list-ref (list-ref board (add1 posDown)) posDown2) get-posY)]))
                                                                (drop (list-ref board (add1 posDown)) (add1 posDown2))))
                          (drop board (+ posDown 2)))
                       (add1 posDown) posDown2 "down-right")
                  '())
              (if (and (> posDown2 3) (< posDown2 8))
                  (if ( equal? (send (list-ref (list-ref board (add1 posDown)) (- posDown2 4)) get-color) "white")
                      (check-jumps
                       (append (take board pos)
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-posX)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-posY)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (list (list-ref board posDown))
                          (list (append (take (list-ref board (add1 posDown)) (- posDown2 4)) (list (new cCheckersBox%
                                                                [color (send (list-ref (list-ref board pos) pos2) get-color)]
                                                                [posX (send (list-ref (list-ref board (add1 posDown)) (- posDown2 4)) get-posX)]
                                                                [posY (send (list-ref (list-ref board (add1 posDown)) (- posDown2 4)) get-posY)]))
                                                                (drop (list-ref board (add1 posDown)) (- posDown2 3))))
                          (drop board (+ posDown 2)))
                       (add1 posDown) (- posDown2 4) "down-right")
                      '())
                  '()
                  )))
      (if (= (- (length (list-ref board (add1 posDown))) (length (list-ref board posDown))) 1)
          (if ( equal? (send (list-ref (list-ref board (add1 posDown)) (add1 posDown2)) get-color) "white")
                  (check-jumps
                       (append (take board pos)
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-posX)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-posY)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (list (list-ref board posDown))
                          (list (append (take (list-ref board (add1 posDown)) (add1 posDown2)) (list (new cCheckersBox%
                                                                [color (send (list-ref (list-ref board pos) pos2) get-color)]
                                                                [posX (send (list-ref (list-ref board (add1 posDown)) (add1 posDown2)) get-posX)]
                                                                [posY (send (list-ref (list-ref board (add1 posDown)) (add1 posDown2)) get-posY)]))
                                                                (drop (list-ref board (add1 posDown)) (+ posDown2 2))))
                          (drop board (+ posDown 2)))
                       (add1 posDown) (add1 posDown2) "down-right")
                  '())
          (if ( equal? (send (list-ref (list-ref board (add1 posDown)) (+ posDown2 5)) get-color) "white")
                      (check-jumps
                       (append (take board pos)
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-posX)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-posY)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (list (list-ref board posDown))
                          (list (append (take (list-ref board (add1 posDown)) (+ posDown2 5)) (list (new cCheckersBox%
                                                                [color (send (list-ref (list-ref board pos) pos2) get-color)]
                                                                [posX (send (list-ref (list-ref board (add1 posDown)) (+ posDown2 5)) get-posX)]
                                                                [posY (send (list-ref (list-ref board (add1 posDown)) (+ posDown2 5)) get-posY)]))
                                                                (drop (list-ref board (add1 posDown)) (+ posDown2 6))))
                          (drop board (+ posDown 2)))
                       (add1 posDown) (+ posDown2 5) "down-right")
                      '()))))

#|
check-jump-up-left revisa si una casilla puede realizar
un salto hacia arriba a la izquierda
Entrada:
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
- pos: el número de fila de la ficha
- pos2: el número de columna de la ficha
Salida:
- Una lista de movimientos que puede hacer a
  través de saltos
|#
(define (check-jump-up-left board pos pos2)
  (if (< (length (list-ref board (sub1 pos))) (length (list-ref board pos)))
      (if (= pos2 0)
          '()
          (if (= (- (length (list-ref board pos)) (length (list-ref board (sub1 pos)))) 1)
              (if (or (= pos 1) ( equal? (send (list-ref (list-ref board (sub1 pos)) (sub1 pos2)) get-color) "white"))
                  '()
                  (check-jump-up-left-aux board (sub1 pos)(sub1 pos2) pos pos2))
              (if (and (> pos2 4) (< pos2 9))
                  (if (or (= pos 1) ( equal? (send (list-ref (list-ref board (sub1 pos)) (- pos2 5)) get-color) "white"))
                      '()
                      (check-jump-up-left-aux board (sub1 pos)(- pos2 5) pos pos2))
                  '()
                  )))
      (if (= (- (length (list-ref board (sub1 pos))) (length (list-ref board pos))) 1)
          (if ( equal? (send (list-ref (list-ref board (sub1 pos)) pos2) get-color) "white")
                  '()
                  (check-jump-up-left-aux board (sub1 pos) pos2 pos pos2))
          (if ( equal? (send (list-ref (list-ref board (sub1 pos)) (+ pos2 4)) get-color) "white")
                      '()
                      (check-jump-up-left-aux board (sub1 pos)(+ pos2 4) pos pos2)))))

#|
check-jump-up-left-aux es usada por check-jump-up-left
ayudando a revisar si la ficha puede realizar un salto hacia
arriba a la izquierda
Entrada:
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
- posDown: el número de fila de la casilla que está arriba a
  la izquierda de la ficha
- posDown2 el número de columna de la casilla que está arriba a
  la izquierda de la ficha
- pos: el número de fila de la ficha
- pos2: el número de columna de la ficha
Salida:
- Una lista de movimientos que puede hacer a
  través de saltos
|#
(define (check-jump-up-left-aux board posUp posUp2 pos pos2)
  (if (< (length (list-ref board (sub1 posUp))) (length (list-ref board posUp)))
      (if (= posUp2 0)
          '()
          (if (= (- (length (list-ref board posUp)) (length (list-ref board (sub1 posUp)))) 1)
              (if ( equal? (send (list-ref (list-ref board (sub1 posUp)) (sub1 posUp2)) get-color) "white")
                  (check-jumps
                       (append (take board (sub1 posUp))
                          (list (append (take (list-ref board (sub1 posUp)) (sub1 posUp2)) (list (new cCheckersBox%
                                                                [color (send (list-ref (list-ref board pos) pos2) get-color)]
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
                       (sub1 posUp) (sub1 posUp2) "up-left")
                  '())
              (if (and (> posUp2 4) (< posUp2 9))
                  (if ( equal? (send (list-ref (list-ref board (sub1 posUp)) (- posUp2 5)) get-color) "white")
                      (check-jumps
                       (append (take board (sub1 pos))
                          (list (append (take (list-ref board (sub1 posUp)) (- posUp2 5)) (list (new cCheckersBox%
                                                                [color (send (list-ref (list-ref board pos) pos2) get-color)]
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
                       (sub1 posUp) (- posUp2 5) "up-left")
                      '())
                  '()
                  )))
      (if (= (- (length (list-ref board (sub1 posUp))) (length (list-ref board posUp))) 1)
          (if ( equal? (send (list-ref (list-ref board (sub1 posUp)) posUp2) get-color) "white")
                  (check-jumps
                       (append (take board (sub1 pos))
                          (list (append (take (list-ref board (sub1 posUp)) posUp2) (list (new cCheckersBox%
                                                                [color (send (list-ref (list-ref board pos) pos2) get-color)]
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
                       (sub1 posUp) posUp2 "up-left")
                  '())
          (if ( equal? (send (list-ref (list-ref board (sub1 posUp)) (+ posUp2 4)) get-color) "white")
                      (check-jumps
                       (append (take board (sub1 posUp))
                          (list (append (take (list-ref board (sub1 posUp)) (+ posUp2 4)) (list (new cCheckersBox%
                                                                [color (send (list-ref (list-ref board pos) pos2) get-color)]
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
                       (sub1 posUp) (+ posUp2 4) "up-left")
                      '()))))

#|
check-jump-up-right revisa si una casilla puede realizar
un salto hacia arriba a la derecha
Entrada:
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
- pos: el número de fila de la ficha
- pos2: el número de columna de la ficha
Salida:
- Una lista de movimientos que puede hacer a
  través de saltos
|#
(define (check-jump-up-right board pos pos2)
  (if (< (length (list-ref board (sub1 pos))) (length (list-ref board pos)))
      (if (= pos2 (- (length (list-ref board pos)) 1))
          '()
          (if (= (- (length (list-ref board pos)) (length (list-ref board (sub1 pos)))) 1)
              (if (or (= pos 1) ( equal? (send (list-ref (list-ref board (sub1 pos)) pos2) get-color) "white"))
                  '()
                  (check-jump-up-right-aux board (sub1 pos) pos2 pos pos2))
              (if (and (> pos2 3) (< pos2 8))
                  (if (or (= pos 1) ( equal? (send (list-ref (list-ref board (sub1 pos)) (- pos2 4)) get-color) "white"))
                      '()
                      (check-jump-up-right-aux board (sub1 pos)(- pos2 4) pos pos2))
                  '()
                  )))
      (if (= (- (length (list-ref board (sub1 pos))) (length (list-ref board pos))) 1)
          (if ( equal? (send (list-ref (list-ref board (sub1 pos)) (add1 pos2)) get-color) "white")
                  '()
                  (check-jump-up-right-aux board (sub1 pos) (add1 pos2) pos pos2))
          (if ( equal? (send (list-ref (list-ref board (sub1 pos)) (+ pos2 5)) get-color) "white")
                      '()
                      (check-jump-up-right-aux board (sub1 pos)(+ pos2 5) pos pos2)))))

#|
check-jump-up-right-aux es usada por check-jump-up-right
ayudando a revisar si la ficha puede realizar un salto hacia
arriba a la derecha
Entrada:
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
- posDown: el número de fila de la casilla que está arriba a
  la derecha de la ficha
- posDown2 el número de columna de la casilla que está arriba a
  la derecha de la ficha
- pos: el número de fila de la ficha
- pos2: el número de columna de la ficha
Salida:
- Una lista de movimientos que puede hacer a
  través de saltos
|#
(define (check-jump-up-right-aux board posUp posUp2 pos pos2)
  (if (< (length (list-ref board (sub1 posUp))) (length (list-ref board posUp)))
      (if (= posUp2 (- (length (list-ref board posUp)) 1))
          '()
          (if (= (- (length (list-ref board posUp)) (length (list-ref board (sub1 posUp)))) 1)
              (if ( equal? (send (list-ref (list-ref board (sub1 posUp)) posUp2) get-color) "white")
                  (check-jumps
                       (append (take board (sub1 posUp))
                          (list (append (take (list-ref board (sub1 posUp)) posUp2) (list (new cCheckersBox%
                                                                [color (send (list-ref (list-ref board pos) pos2) get-color)]
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
                       (sub1 posUp) posUp2 "up-right")
                  '())
              (if (and (> posUp2 3) (< posUp2 8))
                  (if ( equal? (send (list-ref (list-ref board (sub1 posUp)) (- posUp2 4)) get-color) "white")
                      (check-jumps
                       (append (take board (sub1 pos))
                          (list (append (take (list-ref board (sub1 posUp)) (- posUp2 4)) (list (new cCheckersBox%
                                                                [color (send (list-ref (list-ref board pos) pos2) get-color)]
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
                       (sub1 posUp) (- posUp2 4) "up-right")
                      '())
                  '()
                  )))
      (if (= (- (length (list-ref board (sub1 posUp))) (length (list-ref board posUp))) 1)
          (if ( equal? (send (list-ref (list-ref board (sub1 posUp)) (add1 posUp2)) get-color) "white")
                  (check-jumps
                       (append (take board (sub1 pos))
                          (list (append (take (list-ref board (sub1 posUp)) (add1 posUp2)) (list (new cCheckersBox%
                                                                [color (send (list-ref (list-ref board pos) pos2) get-color)]
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
                       (sub1 posUp) (add1 posUp2) "up-right")
                  '())
          (if ( equal? (send (list-ref (list-ref board (sub1 posUp)) (+ posUp2 5)) get-color) "white")
                      (check-jumps
                       (append (take board (sub1 posUp))
                          (list (append (take (list-ref board (sub1 posUp)) (+ posUp2 5)) (list (new cCheckersBox%
                                                                [color (send (list-ref (list-ref board pos) pos2) get-color)]
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
                       (sub1 posUp) (+ posUp2 5) "up-right")
                      '()))))

#|
check-jump-left revisa si una casilla puede realizar
un salto hacia la izquierda
Entrada:
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
- pos: el número de fila de la ficha
- pos2: el número de columna de la ficha
Salida:
- Una lista de movimientos que puede hacer a
  través de saltos
|#
(define (check-jump-left board pos pos2)
  (if (or (= pos2 0) (= pos2 1))
      '()
      (if ( equal? (send (list-ref (list-ref board pos) (sub1 pos2)) get-color) "white")
          '()
          (if ( equal? (send (list-ref (list-ref board pos) (- pos2 2)) get-color) "white")
              (check-jumps
               (append (take board pos)
                       (list (append (take (list-ref board pos) (- pos2 2)) (list (new cCheckersBox%
                                                                                     [color (send (list-ref (list-ref board pos) pos2) get-color)]
                                                                                     [posX (send (list-ref (list-ref board pos) (- pos2 2)) get-posX)]
                                                                                     [posY (send (list-ref (list-ref board pos) (- pos2 2)) get-posY)])
                                                                                  (list-ref (list-ref board pos) (sub1 pos2))
                                                                                (new cCheckersBox%
                                                                                     [color "white"]
                                                                                     [posX (send (list-ref (list-ref board pos) pos2) get-posX)]
                                                                                     [posY (send (list-ref (list-ref board pos) pos2) get-posY)]))
                                (drop (list-ref board pos) (add1 pos2))))
                  (drop board (add1 pos)))
               pos (- pos2 2) "left")
              '()
              )
          )
      )
  )


#|
check-jump-left revisa si una casilla puede realizar
un salto hacia la derecha
Entrada:
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
- pos: el número de fila de la ficha
- pos2: el número de columna de la ficha
Salida:
- Una lista de movimientos que puede hacer a
  través de saltos
|#
(define (check-jump-right board pos pos2)
  (if (or (= pos2 (- (length (list-ref board pos)) 1)) (= pos2 (- (length (list-ref board pos)) 2)))
      '()
      (if ( equal? (send (list-ref (list-ref board pos) (add1 pos2)) get-color) "white")
          '()
          (if ( equal? (send (list-ref (list-ref board pos) (+ pos2 2)) get-color) "white")
              (check-jumps
               (append (take board pos)
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                                     [color "white"]
                                                                                     [posX (send (list-ref (list-ref board pos) pos2) get-posX)]
                                                                                     [posY (send (list-ref (list-ref board pos) pos2) get-posY)])
                                                                                (list-ref (list-ref board pos) (add1 pos2))
                                                                                (new cCheckersBox%
                                                                                     [color (send (list-ref (list-ref board pos) pos2) get-color)]
                                                                                     [posX (send (list-ref (list-ref board pos) (+ pos2 2)) get-posX)]
                                                                                     [posY (send (list-ref (list-ref board pos) (+ pos2 2)) get-posY)]))
                                (drop (list-ref board pos) (+ pos2 3))))
                  (drop board (add1 pos)))
               pos (+ pos2 2) "right")
              '()
              )
          )
      )
  )

#|
la función eval es una parte esencial del minimax que
se encarga de recibir una posición del tablero y
regresar un valor dependiendo de que tan buena sea
la posición para la IA
Parámetros:
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
Salida:
- El valor de la posición del tablero
|#
(define (eval board)
  (eval-aux board 0 (length board) 0))


#|
eval-aux es usada por eval para ir recorriendo las filas
del tablero en busca de las fichas verdes
Parámetros:
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
- pos: el número de fila actual
- max: la cantidad de filas que tiene el tablero
- value: el valor que se va obteniendo
Salida:
- El valor que va acumulando la fila
|#
(define (eval-aux board pos max value)
  (if (= pos max)
      value
      (eval-aux
       board (add1 pos) max (eval-aux2 board pos 0 (length (list-ref board pos)) value))))

#|
eval-aux2 es usada por eval-aux con cada fila para
recorrer todas las casillas de una fila, revisa si
la casilla es verde o roja para calcular el valor
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
- pos: el número de fila actual
- pos2: el número de columna que se está revisando
  actualmente
- max: la cantidad de columnas que tiene la fila
- moves: el valor que se va acumulando
Salida:
- Devuelve el valor obtenido por la casilla 
|#
(define (eval-aux2 board pos pos2 max value)
  (if (= pos2 max)
      value
      (if (equal? (send (list-ref (list-ref board pos) pos2) get-color) "green")
          (eval-aux2 board pos (add1 pos2) max
                                  (+ value (check-distance-green-pos board pos pos2) (check-jumps-green-pos board pos pos2)))
          (if (equal? (send (list-ref (list-ref board pos) pos2) get-color) "red")
              (eval-aux2 board pos (add1 pos2) max
                         (- value (check-distance-red-pos board pos pos2) (check-jumps-red-pos board pos pos2)))
              (eval-aux2 board pos (add1 pos2) max value)
              ))
      )
  )

#|
check-distance-green-pos se encarga de revisar
la distancia entre una ficha verde en el tablero
y la posición de más abajo del tablero, con el
fin de saber que tan cerca está del objetivo, el
resultado se le resta a 352 debido a que esta es la
distancia más larga y entre más larga sea la distancia
peor debería ser el resultado
Entrada:
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
- pos: el número de fila de la ficha
- pos2: el número de columna de la ficha
Salida:
- El valor obtenido basado en la distancia
|#

(define (check-distance-green-pos board pos pos2)
  (- 352 (sqrt (+ (expt (- 150 (send (list-ref (list-ref board pos) pos2) get-posX)) 2) (expt (- 362 (send (list-ref (list-ref board pos) pos2) get-posY)) 2)))))

#|
check-jumps-green-pos se encarga de revisar
los saltos que pueden hacer las fichas verdes
hacia abajo, es decir hacia el objetivo, por
cada uno de estos saltos se suma 10 al valor
final
Entrada:
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
- pos: el número de fila de la ficha
- pos2: el número de columna de la ficha
Salida:
- El valor obtenido por los saltos
|#

(define (check-jumps-green-pos board pos pos2)
  (+ (check-if-jump-down-left board pos pos2) (check-if-jump-down-right board pos pos2)))

#|
check-distance-green-pos se encarga de revisar
la distancia entre una ficha roja en el tablero
y la posición de más arriba del tablero, con el
fin de saber que tan cerca está del objetivo, el
resultado se le resta a 352 debido a que esta es la
distancia más larga y entre más larga sea la distancia
peor debería ser el resultado.
Entrada:
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
- pos: el número de fila de la ficha
- pos2: el número de columna de la ficha
Salida:
- El valor obtenido basado en la distancia
|#

(define (check-distance-red-pos board pos pos2)
  (- 352 (sqrt (+ (expt (- 150 (send (list-ref (list-ref board pos) pos2) get-posX)) 2) (expt (- 10 (send (list-ref (list-ref board pos) pos2) get-posY)) 2)))))

#|
check-jumps-red-pos se encarga de revisar
los saltos que pueden hacer las fichas rojas
hacia arriba, es decir hacia el objetivo, por
cada uno de estos saltos se suma 10 al valor
final
Entrada:
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
- pos: el número de fila de la ficha
- pos2: el número de columna de la ficha
Salida:
- El valor obtenido por los saltos
|#
(define (check-jumps-red-pos board pos pos2)
  (+ (check-if-jump-up-left board pos pos2) (check-if-jump-up-right board pos pos2)))

#|
check-if-jump-down-left revisa si una casilla puede realizar
un salto hacia abajo a la izquierda
Entrada:
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
- pos: el número de fila de la ficha
- pos2: el número de columna de la ficha
Salida:
- 0 si no puede realizar el salto, si lo puede realizar sería 10
  por cada salto que realice
|#
(define (check-if-jump-down-left board pos pos2)
  (if (< (length (list-ref board (add1 pos))) (length (list-ref board pos)))
      (if (= pos2 0)
          0
          (if (= (- (length (list-ref board pos)) (length (list-ref board (add1 pos)))) 1)
              (if (or (= pos 15)
                      (equal? (send (list-ref (list-ref board (add1 pos)) (sub1 pos2)) get-color) "white")) 
                  0
                  (check-if-jump-down-left-aux board (add1 pos)(sub1 pos2)))
              (if (and (> pos2 4) (< pos2 9))
                  (if (or (= pos 15)
                          (equal? (send (list-ref (list-ref board (add1 pos)) (- pos2 5)) get-color) "white"))
                      0
                      (check-if-jump-down-left-aux board (add1 pos)(- pos2 5)))
                  0
                  )))
      (if (= (- (length (list-ref board (add1 pos))) (length (list-ref board pos))) 1)
          (if ( equal? (send (list-ref (list-ref board (add1 pos)) pos2) get-color) "white")
                  0
                  (check-if-jump-down-left-aux board (add1 pos) pos2))
          (if ( equal? (send (list-ref (list-ref board (add1 pos)) (+ pos2 4)) get-color) "white")
                      0
                      (check-if-jump-down-left-aux board (add1 pos)(+ pos2 4))))))

#|
check-if-jump-down-left-aux es usada por check-if-jump-down-left
ayudando a revisar si la ficha puede realizar un salto hacia
abajo a la izquierda
Entrada:
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
- posDown: el número de fila de la casilla que está abajo a
  la izquierda de la ficha
- posDown2 el número de columna de la casilla que está abajo a
  la izquierda de la ficha
Salida:
- un 10 si puede realizar el salto, si no un 0
|#
(define (check-if-jump-down-left-aux board posDown posDown2)
  (if (< (length (list-ref board (add1 posDown))) (length (list-ref board posDown)))
      (if (= posDown2 0)
          0
          (if (= (- (length (list-ref board posDown)) (length (list-ref board (add1 posDown)))) 1)
              (if ( equal? (send (list-ref (list-ref board (add1 posDown)) (sub1 posDown2)) get-color) "white")
                  (+ 10 (check-jumps-green-pos board (add1 posDown) (sub1 posDown2)))
                  0)
              (if (and (> posDown2 4) (< posDown2 9))
                  (if ( equal? (send (list-ref (list-ref board (add1 posDown)) (- posDown2 5)) get-color) "white")
                      (+ 10 (check-jumps-green-pos board (add1 posDown) (- posDown2 5)))
                      0)
                  0
                  )))
      (if (= (- (length (list-ref board (add1 posDown))) (length (list-ref board posDown))) 1)
          (if ( equal? (send (list-ref (list-ref board (add1 posDown)) posDown2) get-color) "white")
                  (+ 10 (check-jumps-green-pos board (add1 posDown) posDown2))
                  0)
          (if ( equal? (send (list-ref (list-ref board (add1 posDown)) (+ posDown2 4)) get-color) "white")
                      (+ 10 (check-jumps-green-pos board (add1 posDown) (+ posDown2 4)))
                      0))))

#|
check-if-jump-down-right revisa si una casilla puede realizar
un salto hacia abajo a la derecha
Entrada:
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
- pos: el número de fila de la ficha
- pos2: el número de columna de la ficha
Salida:
- un 10 si puede realizar el salto, si no un 0
|#
(define (check-if-jump-down-right board pos pos2)
  (if (< (length (list-ref board (add1 pos))) (length (list-ref board pos)))
      (if (= pos2 (- (length (list-ref board pos)) 1))
          0
          (if (= (- (length (list-ref board pos)) (length (list-ref board (add1 pos)))) 1)
              (if (or (= pos 15) ( equal? (send (list-ref (list-ref board (add1 pos)) pos2) get-color) "white"))
                  0
                  (check-if-jump-down-right-aux board (add1 pos) pos2))
              (if (and (> pos2 3) (< pos2 8))
                  (if (or (= pos 15) ( equal? (send (list-ref (list-ref board (add1 pos)) (- pos2 4)) get-color) "white"))
                      0
                      (check-if-jump-down-right-aux board (add1 pos)(- pos2 4)))
                  0
                  )))
      (if (= (- (length (list-ref board (add1 pos))) (length (list-ref board pos))) 1)
          (if ( equal? (send (list-ref (list-ref board (add1 pos)) (add1 pos2)) get-color) "white")
                  0
                  (check-if-jump-down-right-aux board (add1 pos) (add1 pos2)))
          (if ( equal? (send (list-ref (list-ref board (add1 pos)) (+ pos2 5)) get-color) "white")
                      0
                      (check-if-jump-down-right-aux board (add1 pos)(+ pos2 5))))))

#|
check-if-jump-down-right-aux es usada por check-if-jump-down-right
ayudando a revisar si la ficha puede realizar un salto hacia
abajo a la derecha
Entrada:
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
- posDown: el número de fila de la casilla que está abajo a
  la derecha de la ficha
- posDown2 el número de columna de la casilla que está abajo a
  la derecha de la ficha
Salida:
- un 10 si puede realizar el salto, si no un 0
|#
(define (check-if-jump-down-right-aux board posDown posDown2)
  (if (< (length (list-ref board (add1 posDown))) (length (list-ref board posDown)))
      (if (= posDown2 (- (length (list-ref board posDown)) 1))
          0
          (if (= (- (length (list-ref board posDown)) (length (list-ref board (add1 posDown)))) 1)
              (if ( equal? (send (list-ref (list-ref board (add1 posDown)) posDown2) get-color) "white")
                  (+ 10 (check-jumps-green-pos board (add1 posDown) posDown2))
                  0)
              (if (and (> posDown2 3) (< posDown2 8))
                  (if ( equal? (send (list-ref (list-ref board (add1 posDown)) (- posDown2 4)) get-color) "white")
                      (+ 10 (check-jumps-green-pos board (add1 posDown) (- posDown2 4)))
                      0)
                  0
                  )))
      (if (= (- (length (list-ref board (add1 posDown))) (length (list-ref board posDown))) 1)
          (if ( equal? (send (list-ref (list-ref board (add1 posDown)) (add1 posDown2)) get-color) "white")
                  (+ 10 (check-jumps-green-pos board (add1 posDown) (add1 posDown2)))
                  0)
          (if ( equal? (send (list-ref (list-ref board (add1 posDown)) (+ posDown2 5)) get-color) "white")
                      (+ 10 (check-jumps-green-pos board (add1 posDown) (+ posDown2 5)))
                      0))))

#|
check-if-jump-up-left revisa si una casilla puede realizar
un salto hacia arriba a la izquierda
Entrada:
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
- pos: el número de fila de la ficha
- pos2: el número de columna de la ficha
Salida:
- un 10 si puede realizar el salto, si no un 0
|#
(define (check-if-jump-up-left board pos pos2)
  (if (< (length (list-ref board (sub1 pos))) (length (list-ref board pos)))
      (if (= pos2 0)
          0
          (if (= (- (length (list-ref board pos)) (length (list-ref board (sub1 pos)))) 1)
              (if (or (= pos 1) ( equal? (send (list-ref (list-ref board (sub1 pos)) (sub1 pos2)) get-color) "white"))
                  0
                  (check-if-jump-up-left-aux board (sub1 pos)(sub1 pos2)))
              (if (and (> pos2 4) (< pos2 9))
                  (if (or (= pos 1) ( equal? (send (list-ref (list-ref board (sub1 pos)) (- pos2 5)) get-color) "white"))
                      0
                      (check-if-jump-up-left-aux board (sub1 pos)(- pos2 5)))
                  0
                  )))
      (if (= (- (length (list-ref board (sub1 pos))) (length (list-ref board pos))) 1)
          (if ( equal? (send (list-ref (list-ref board (sub1 pos)) pos2) get-color) "white")
                  0
                  (check-if-jump-up-left-aux board (sub1 pos) pos2))
          (if ( equal? (send (list-ref (list-ref board (sub1 pos)) (+ pos2 4)) get-color) "white")
                      0
                      (check-if-jump-up-left-aux board (sub1 pos)(+ pos2 4))))))

#|
check-if-jump-up-left-aux es usada por check-if-jump-up-left
ayudando a revisar si la ficha puede realizar un salto hacia
arriba a la izquierda
Entrada:
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
- posDown: el número de fila de la casilla que está arriba a
  la izquierda de la ficha
- posDown2 el número de columna de la casilla que está arriba a
  la izquierda de la ficha
Salida:
- un 10 si puede realizar el salto, si no un 0
|#
(define (check-if-jump-up-left-aux board posUp posUp2)
  (if (< (length (list-ref board (sub1 posUp))) (length (list-ref board posUp)))
      (if (= posUp2 0)
          0
          (if (= (- (length (list-ref board posUp)) (length (list-ref board (sub1 posUp)))) 1)
              (if ( equal? (send (list-ref (list-ref board (sub1 posUp)) (sub1 posUp2)) get-color) "white")
                  (+ 10 (check-jumps-red-pos board (sub1 posUp) (sub1 posUp2)))
                  0)
              (if (and (> posUp2 4) (< posUp2 9))
                  (if ( equal? (send (list-ref (list-ref board (sub1 posUp)) (- posUp2 5)) get-color) "white")
                      (+ 10 (check-jumps-red-pos board (sub1 posUp) (- posUp2 5)))
                      0)
                  0
                  )))
      (if (= (- (length (list-ref board (sub1 posUp))) (length (list-ref board posUp))) 1)
          (if ( equal? (send (list-ref (list-ref board (sub1 posUp)) posUp2) get-color) "white")
                  (+ 10 (check-jumps-red-pos board (sub1 posUp) posUp2))
                  0)
          (if ( equal? (send (list-ref (list-ref board (sub1 posUp)) (+ posUp2 4)) get-color) "white")
                      (+ 10 (check-jumps-red-pos board (sub1 posUp) (+ posUp2 4)))
                      0))))

#|
check-if-jump-up-right revisa si una casilla puede realizar
un salto hacia arriba a la derecha
Entrada:
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
- pos: el número de fila de la ficha
- pos2: el número de columna de la ficha
Salida:
- un 10 si puede realizar el salto, si no un 0
|#
(define (check-if-jump-up-right board pos pos2)
  (if (< (length (list-ref board (sub1 pos))) (length (list-ref board pos)))
      (if (= pos2 (- (length (list-ref board pos)) 1))
          0
          (if (= (- (length (list-ref board pos)) (length (list-ref board (sub1 pos)))) 1)
              (if (or (= pos 1) ( equal? (send (list-ref (list-ref board (sub1 pos)) pos2) get-color) "white"))
                  0
                  (check-if-jump-up-right-aux board (sub1 pos) pos2))
              (if (and (> pos2 3) (< pos2 8))
                  (if (or (= pos 1) ( equal? (send (list-ref (list-ref board (sub1 pos)) (- pos2 4)) get-color) "white"))
                      0
                      (check-if-jump-up-right-aux board (sub1 pos)(- pos2 4)))
                  0
                  )))
      (if (= (- (length (list-ref board (sub1 pos))) (length (list-ref board pos))) 1)
          (if ( equal? (send (list-ref (list-ref board (sub1 pos)) (add1 pos2)) get-color) "white")
                  0
                  (check-if-jump-up-right-aux board (sub1 pos) (add1 pos2)))
          (if ( equal? (send (list-ref (list-ref board (sub1 pos)) (+ pos2 5)) get-color) "white")
                      0
                      (check-if-jump-up-right-aux board (sub1 pos)(+ pos2 5))))))

#|
check-if-jump-up-right-aux es usada por check-if-jump-up-right
ayudando a revisar si la ficha puede realizar un salto hacia
arriba a la derecha
Entrada:
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
- posDown: el número de fila de la casilla que está arriba a
  la derecha de la ficha
- posDown2 el número de columna de la casilla que está arriba a
  la derecha de la ficha
Salida:
- un 10 si puede realizar el salto, si no un 0
|#
(define (check-if-jump-up-right-aux board posUp posUp2)
  (if (< (length (list-ref board (sub1 posUp))) (length (list-ref board posUp)))
      (if (= posUp2 (- (length (list-ref board posUp)) 1))
          0
          (if (= (- (length (list-ref board posUp)) (length (list-ref board (sub1 posUp)))) 1)
              (if ( equal? (send (list-ref (list-ref board (sub1 posUp)) posUp2) get-color) "white")
                  (+ 10 (check-jumps-red-pos board (sub1 posUp) posUp2))
                  0)
              (if (and (> posUp2 3) (< posUp2 8))
                  (if ( equal? (send (list-ref (list-ref board (sub1 posUp)) (- posUp2 4)) get-color) "white")
                      (+ 10 (check-jumps-red-pos board (sub1 posUp) (- posUp2 4)))
                      0)
                  0
                  )))
      (if (= (- (length (list-ref board (sub1 posUp))) (length (list-ref board posUp))) 1)
          (if ( equal? (send (list-ref (list-ref board (sub1 posUp)) (add1 posUp2)) get-color) "white")
                  (+ 10 (check-jumps-red-pos board (sub1 posUp) (add1 posUp2)))
                  0)
          (if ( equal? (send (list-ref (list-ref board (sub1 posUp)) (+ posUp2 5)) get-color) "white")
                      (+ 10 (check-jumps-red-pos board (sub1 posUp) (+ posUp2 5)))
                      0))))

#|
alpha-beta-search devuelve el tablero que sería la
decisión tomada después de haber hecho minimax con
poda alpha beta
Parámetros:
- board: lista de listas de objetos cCheckersBox
- color1: color del jugador a maximizar
- color2: color del jugador a minimizar
Salida:
- Devuelve una lista de listas de objetos cCheckersBox
|#
(define (alpha-beta-search board color1 color2)
  (cadr (max-move board -inf.0 +inf.0 color1 color2 10 10))
  )

#|
max-move max del minimax
Parámetros:
- board: lista de listas de objetos cCheckersBox
- alpha: el mejor valor para max
- beta: el mejor valor para min
- color1: color del jugador a maximizar
- color2: color del jugador a minimizar
- expandDeep: cuanto se expande en profundidad el árbol
- expandWide: cuanto se expande en anchura el árbol
Salida:
- Devuelve una lista con un valor y una lista de listas de objetos cCheckersBox
|#
(define (max-move board alpha beta color1 color2 expandDeep expandWide)
  (if (<= expandDeep 0)
    (eval board)
    (max-move-aux (check-possible-moves board color1) alpha beta (list -inf.0 null) color1 color2 expandDeep expandWide)
    )
  )

#|
max-move-aux ayuda a iterar la lista de movimientos
Parámetros:
- board: lista de listas de objetos cCheckersBox
- alpha: el mejor valor para max
- beta: el mejor valor para min
- color1: color del jugador a maximizar
- color2: color del jugador a minimizar
- expandDeep: cuanto se expande en profundidad el árbol
- expandWide: cuanto se expande en anchura el árbol
Salida:
- Devuelve una lista con un valor y una lista de listas de objetos cCheckersBox
|#
(define (max-move-aux moves alpha beta v color1 color2 expandDeep expandWide)
  ((lambda (x)
    (if (> (car x) (car v))
      (set! v x)
      (void))
    (if (or (>= (car v) beta) (<= expandWide 0))
      v
      (max-move-aux (cdr moves) (max alpha (car v)) v beta color1 color2 expandDeep (sub1 expandWide))))

      (min-move (car moves) alpha beta color2 color1 (sub1 expandDeep) expandWide); Este es el valor de x inicial
    )
      
  )

#|
min-move min del minimax
Parámetros:
- board: lista de listas de objetos cCheckersBox
- alpha: el mejor valor para max
- beta: el mejor valor para min
- color1: color del jugador a maximizar
- color2: color del jugador a minimizar
- expandDeep: cuanto se expande en profundidad el árbol
- expandWide: cuanto se expande en anchura el árbol
Salida:
- Devuelve una lista con un valor y una lista de listas de objetos cCheckersBox
|#
(define (min-move board alpha beta color1 color2 expandDeep expandWide)
  (if (<= expandDeep 0)
    (eval board)
    (min-move-aux (check-possible-moves board color1) alpha beta (list +inf.0 null) expandDeep expandWide)
    )
  )

#|
min-move-aux ayuda a iterar la lista de movimientos
Parámetros:
- board: lista de listas de objetos cCheckersBox
- alpha: el mejor valor para max
- beta: el mejor valor para min
- color1: color del jugador a maximizar
- color2: color del jugador a minimizar
- expandDeep: cuanto se expande en profundidad el árbol
- expandWide: cuanto se expande en anchura el árbol
Salida:
- Devuelve una lista con un valor y una lista de listas de objetos cCheckersBox
|#
(define (min-move-aux moves alpha beta v color1 color2 expandDeep expandWide)
  ((lambda (x)
    (if (< (car x) (car v))
      (set! v x)
      (void))
    (if (or (<= (car v) alpha) (<= expandWide 0))
      v
      (min-move-aux (cdr moves) alpha (min (car v) beta) expandDeep (sub1 expandWide))))
      
      (max-move (car moves) alpha beta color2 color1 (sub1 expandDeep) expandWide)); Este es el valor de x inicial
  )

; Muestra la ventana
(send frame show #t)
; Hace grande la ventana
(send frame maximize #t)