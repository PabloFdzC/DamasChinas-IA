#lang racket
(require "cCheckersBox.rkt")

#|
check-possible-moves se encarga de revisar una posición del tablero
y devolver todos los posibles moviemientos que pueden hacer las
fichas de un color.
Parámetros:
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
- color: el color de las fichas de las que se quieren
  saber sus posibles movimientos
- max-moves: La cantidad máxima de movimientos
  que se deben obtener
Salida:
- Una lista de tableros que representan los movimientos 
|#
(define (check-possible-moves board color max-moves)
  (if (equal? color "red")
      (check-possible-moves-aux-green board color (sub1 (length board)) '() max-moves)
      (check-possible-moves-aux board color 0 (length board) '() max-moves)
      )
  )


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
- max-moves: La cantidad máxima de movimientos
  que se deben obtener
Salida:
- Una lista de tableros que representan los movimientos 
|#
(define (check-possible-moves-aux board color pos max moves max-moves)
  (if (or (= pos max) (>= (length moves) max-moves))
      moves
      (check-possible-moves-aux
       board color (add1 pos) max (check-possible-moves-aux2 board color pos 0 (length (list-ref board pos)) moves max-moves) max-moves)))

#|
check-possible-moves-aux-green realiza lo mismo que
check-possible-moves-aux pero revisando el tablero
de abajo para arriba en el caso de las fichas verdes
para obtener los movimientos de más abajo que
probablemente sean los de mayor prioridad
Parámetros:
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
- color: el color de las fichas de las que se quieren
  saber sus posibles movimientos
- pos: el número de fila actual
- moves: los movimientos que se van obteniendo
- max-moves: La cantidad máxima de movimientos
  que se deben obtener
Salida:
- Una lista de tableros que representan los movimientos 
|#

(define (check-possible-moves-aux-green board color pos moves max-moves)
  (if (or (< pos 0) (>= (length moves) max-moves))
      moves
      (check-possible-moves-aux-green
       board color (sub1 pos) (check-possible-moves-aux2 board color pos 0 (length (list-ref board pos)) moves max-moves) max-moves)))

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
- max-moves: La cantidad máxima de movimientos
  que se deben obtener
Salida:
- Una lista de tableros que representan los movimientos 
|#
(define (check-possible-moves-aux2 board color pos pos2 max moves max-moves)
  (if (or (= pos2 max) (>= (length moves) max-moves))
      moves
      (if (equal? (send (list-ref (list-ref board pos) pos2) get-color) color)
          (check-possible-moves-aux2 board color pos (add1 pos2) max
                                  (check-possible-moves-pos board pos pos2 color moves) max-moves)
          (check-possible-moves-aux2 board color pos (add1 pos2) max moves max-moves))))


#|
check-possible-moves-pos se encarga de obtener todos
los movimientos posibles de una ficha, es llamada por
check-possible-moves-aux2, si la ficha es verde, revisa
los movimientos hacia abajo y si es roja, los movimientos
hacia arriba
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
  (if (equal? color "green")
      (append moves
              (if (>= pos 16) 
                  '()
                  (if (and (> pos 7) (<= pos 12) (< pos2 (- (length (list-ref board pos)) 8)))
                      (append (check-move-right board pos pos2 color) (check-move-up-right board pos pos2 color))
                      (if (< pos2 (- (length (list-ref board pos)) 7))
                          '()
                          (check-move-down-left board pos pos2 color))))
              (if (>= pos 16) 
                  '()
                  (if (and (> pos 7) (<= pos 12) (> pos2 8))
                      (append (check-move-left board pos pos2 color) (check-move-up-left board pos pos2 color))
                      (if (= pos2 8)
                          '()
                          (check-move-down-right board pos pos2 color)))
                  )
              (if (or
                   (and (> pos 7) (<= pos 12) (< pos2 (- (length (list-ref board pos)) 8)))
                   (and (> pos 7) (<= pos 12) (> pos2 8))
                   (> pos 12))
                      '()
                      (if (empty? (append (check-move-down-left board pos pos2 color) (check-move-down-right board pos pos2 color)))
                          (append (if (<= pos 0)
                                      '()
                                      (append (check-move-up-right board pos pos2 color) (check-move-up-left board pos pos2 color)))
                                  (if (<= pos2 0)
                                      '()
                                      (check-move-left board pos pos2 color))
                                  (if (>= pos2 (- (length (list-ref board pos)) 1))
                                      '()
                                      (check-move-right board pos pos2 color))
                                  )
                          '()
                          )
                      )
              )
      (append moves
              (if (<= pos 0)
                  '()
                  (check-move-up-left board pos pos2 color))
              (if (<= pos 0)
                  '()
                  (check-move-up-right board pos pos2 color))
   )))

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
          (if (< (- (length (list-ref board (sub1 pos))) 1) (+ pos2 4))
              '()
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
                      (check-move-up-left-jump board pos pos2 (sub1 pos)(+ pos2 4) c)))
          )))

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
                   (sub1 posUp) (sub1 posUp2) c)
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
                   (sub1 posUp) (- posUp2 5) c)
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
                   (sub1 posUp) posUp2 c)
                  '())
          (if (< (- (length (list-ref board (sub1 posUp))) 1) (+ posUp2 4))
              '()
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
                   (sub1 posUp) (+ posUp2 4) c)
                      '()))
          )))

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
          (if (< (- (length (list-ref board (sub1 pos))) 1) (+ pos2 5))
              '()
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
                      (check-move-up-right-jump board pos pos2 (sub1 pos)(+ pos2 5) c)))
          )))


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
                   (sub1 posUp) posUp2 c)
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
                   (sub1 posUp) (- posUp2 4) c)
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
                   (sub1 posUp) (add1 posUp2) c)
                  '())
          (if (< (- (length (list-ref board (sub1 posUp))) 1) (+ posUp2 5))
              '()
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
                   (sub1 posUp) (+ posUp2 5) c)
                      '()))
          )))

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
                  (if (or (= pos 15) (and (= pos 7) (= pos2 1)) (and (> pos 7) (< pos 11) (= pos2 (- (length (list-ref board pos)) 8))))
                      '()
                      (check-move-down-left-jump board pos pos2 (add1 pos)(sub1 pos2) c)))
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
                      (if (or (= pos 15) (and (= pos 7) (= pos2 1)) (and (> pos 7) (< pos 11) (= pos2 (- (length (list-ref board pos)) 8))))
                          '()
                          (check-move-down-left-jump board pos pos2 (add1 pos)(- pos2 5) c)))
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
                  (if (or (= pos 15) (and (= pos 7) (= pos2 1)) (and (> pos 7) (< pos 11) (= pos2 (- (length (list-ref board pos)) 8))))
                      '()
                      (check-move-down-left-jump board pos pos2 (add1 pos) pos2 c)))
          (if (< (- (length (list-ref board (add1 pos))) 1) (+ pos2 4))
              '()
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
                      (if (or (= pos 15) (and (= pos 7) (= pos2 1)) (and (> pos 7) (< pos 11) (= pos2 (- (length (list-ref board pos)) 8))))
                          '()
                          (check-move-down-left-jump board pos pos2 (add1 pos)(+ pos2 4) c))))
          )))


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
                   (add1 posDown) (sub1 posDown2) c)
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
                   (add1 posDown) (- posDown2 5) c)
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
                   (add1 posDown) posDown2 c)
                  '())
          (if (< (- (length (list-ref board (add1 posDown))) 1) (+ posDown2 4))
              '()
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
                   (add1 posDown) (+ posDown2 4) c)
                      '()))
          )))

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
                  (if (or (= pos 15) (and (= pos 7) (= pos2 8)) (and (> pos 7) (< pos 11) (= pos2 7))) '() (check-move-down-right-jump board pos pos2 (add1 pos) pos2 c)))
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
                      (if (or (= pos 15) (and (= pos 7) (= pos2 8)) (and (> pos 7) (< pos 11) (= pos2 7))) '() (check-move-down-right-jump board pos pos2 (add1 pos)(- pos2 4) c)))
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
                  (if (or (= pos 15) (and (= pos 7) (= pos2 8)) (and (> pos 7) (< pos 11) (= pos2 7))) '()(check-move-down-right-jump board pos pos2 (add1 pos) (add1 pos2) c)))
          (if (< (- (length (list-ref board (add1 pos))) 1) (+ pos2 5))
              '()
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
                      (if (or (= pos 15) (and (= pos 7) (= pos2 8)) (and (> pos 7) (< pos 11) (= pos2 7))) '() (check-move-down-right-jump board pos pos2 (add1 pos)(+ pos2 5) c))))
          )))

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
                   (add1 posDown) posDown2 c)
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
                   (add1 posDown) (- posDown2 4) c)
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
                   (add1 posDown) (add1 posDown2) c)
                  '())
          (if (< (- (length (list-ref board (add1 posDown))) 1) (+ posDown2 5))
              '()
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
                   (add1 posDown) (+ posDown2 5) c)
                      '()))
          )))

#|
check-move-left revisa si una ficha se puede mover
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
                   c)
                  '())))))

#|
check-move-right revisa si una ficha se puede mover
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
                   c)
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
(define (check-jumps board pos pos2 color)
  (append
   (if (or (>= pos 15) (equal? color "red") (and (= pos 7) (= pos2 1)) (and (> pos 7) (< pos 11) (= pos2 (- (length (list-ref board pos)) 8)))) '() (check-jump-down-left board pos pos2 color))
   (if (or (>= pos 15) (equal? color "red") (and (= pos 7) (= pos2 8)) (and (> pos 7) (< pos 11) (= pos2 7))) '() (check-jump-down-right board pos pos2 color))
   (if (or (<= pos 1) (equal? color "green")) '() (check-jump-up-left board pos pos2 color))
   (if (or (<= pos 1) (equal? color "green")) '() (check-jump-up-right board pos pos2 color))
   (list board))
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
(define (check-jump-down-left board pos pos2 color)
  (if (< (length (list-ref board (add1 pos))) (length (list-ref board pos)))
      (if (= pos2 0)
          '()
          (if (= (- (length (list-ref board pos)) (length (list-ref board (add1 pos)))) 1)
              (if (or (= pos 15)
                      (equal? (send (list-ref (list-ref board (add1 pos)) (sub1 pos2)) get-color) "white")) 
                  '()
                  (check-jump-down-left-aux board (add1 pos)(sub1 pos2) pos pos2 color))
              (if (and (> pos2 4) (< pos2 9))
                  (if (or (= pos 15)
                          (equal? (send (list-ref (list-ref board (add1 pos)) (- pos2 5)) get-color) "white"))
                      '()
                      (check-jump-down-left-aux board (add1 pos)(- pos2 5) pos pos2 color))
                  '()
                  )))
      (if (= (- (length (list-ref board (add1 pos))) (length (list-ref board pos))) 1)
          (if ( equal? (send (list-ref (list-ref board (add1 pos)) pos2) get-color) "white")
                  '()
                  (check-jump-down-left-aux board (add1 pos) pos2 pos pos2 color))
          (if (< (- (length (list-ref board (add1 pos))) 1) (+ pos2 4))
              '()
              (if ( equal? (send (list-ref (list-ref board (add1 pos)) (+ pos2 4)) get-color) "white")
                      '()
                      (check-jump-down-left-aux board (add1 pos)(+ pos2 4) pos pos2 color)))
          )))

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
(define (check-jump-down-left-aux board posDown posDown2 pos pos2 c)
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
                   (add1 posDown) (sub1 posDown2) c)
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
                       (add1 posDown) (- posDown2 5) c)
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
                       (add1 posDown) posDown2 c)
                  '())
          (if (< (- (length (list-ref board (add1 posDown))) 1) (+ posDown2 4))
              '()
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
                       (add1 posDown) (+ posDown2 4) c)
                      '()))
          )))

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
(define (check-jump-down-right board pos pos2 color)
  (if (< (length (list-ref board (add1 pos))) (length (list-ref board pos)))
      (if (= pos2 (- (length (list-ref board pos)) 1))
          '()
          (if (= (- (length (list-ref board pos)) (length (list-ref board (add1 pos)))) 1)
              (if (or (>= pos 15) ( equal? (send (list-ref (list-ref board (add1 pos)) pos2) get-color) "white"))
                  '()
                  (check-jump-down-right-aux board (add1 pos) pos2 pos pos2 color))
              (if (and (> pos2 3) (< pos2 8))
                  (if (or (>= pos 15) ( equal? (send (list-ref (list-ref board (add1 pos)) (- pos2 4)) get-color) "white"))
                      '()
                      (check-jump-down-right-aux board (add1 pos)(- pos2 4) pos pos2 color))
                  '()
                  )))
      (if (= (- (length (list-ref board (add1 pos))) (length (list-ref board pos))) 1)
          (if ( equal? (send (list-ref (list-ref board (add1 pos)) (add1 pos2)) get-color) "white")
                  '()
                  (check-jump-down-right-aux board (add1 pos) (add1 pos2) pos pos2 color))
          (if (< (- (length (list-ref board (add1 pos))) 1) (+ pos2 5))
              '()
              (if ( equal? (send (list-ref (list-ref board (add1 pos)) (+ pos2 5)) get-color) "white")
                      '()
                      (check-jump-down-right-aux board (add1 pos)(+ pos2 5) pos pos2 color)))
          ))
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
(define (check-jump-down-right-aux board posDown posDown2 pos pos2 c)
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
                       (add1 posDown) posDown2 c)
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
                       (add1 posDown) (- posDown2 4) c)
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
                       (add1 posDown) (add1 posDown2) c)
                  '())
          (if (< (- (length (list-ref board (add1 posDown))) 1) (+ posDown2 5))
              '()
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
                       (add1 posDown) (+ posDown2 5) c)
                      '()))
          )))

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
(define (check-jump-up-left board pos pos2 color)
  (if (< (length (list-ref board (sub1 pos))) (length (list-ref board pos)))
      (if (= pos2 0)
          '()
          (if (= (- (length (list-ref board pos)) (length (list-ref board (sub1 pos)))) 1)
              (if (or (= pos 1) ( equal? (send (list-ref (list-ref board (sub1 pos)) (sub1 pos2)) get-color) "white"))
                  '()
                  (check-jump-up-left-aux board (sub1 pos)(sub1 pos2) pos pos2 color))
              (if (and (> pos2 4) (< pos2 9))
                  (if (or (= pos 1) ( equal? (send (list-ref (list-ref board (sub1 pos)) (- pos2 5)) get-color) "white"))
                      '()
                      (check-jump-up-left-aux board (sub1 pos)(- pos2 5) pos pos2 color))
                  '()
                  )))
      (if (= (- (length (list-ref board (sub1 pos))) (length (list-ref board pos))) 1)
          (if ( equal? (send (list-ref (list-ref board (sub1 pos)) pos2) get-color) "white")
                  '()
                  (check-jump-up-left-aux board (sub1 pos) pos2 pos pos2 color))
          (if (< (- (length (list-ref board (sub1 pos))) 1) (+ pos2 4))
              '()
              (if ( equal? (send (list-ref (list-ref board (sub1 pos)) (+ pos2 4)) get-color) "white")
                      '()
                      (check-jump-up-left-aux board (sub1 pos)(+ pos2 4) pos pos2 color)))
          )))

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
(define (check-jump-up-left-aux board posUp posUp2 pos pos2 c)
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
                       (sub1 posUp) (sub1 posUp2) c)
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
                       (sub1 posUp) (- posUp2 5) c)
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
                       (sub1 posUp) posUp2 c)
                  '())
          (if (< (- (length (list-ref board (sub1 posUp))) 1) (+ posUp2 4))
              '()
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
                       (sub1 posUp) (+ posUp2 4) c)
                      '()))
          )))

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
(define (check-jump-up-right board pos pos2 color)
  (if (< (length (list-ref board (sub1 pos))) (length (list-ref board pos)))
      (if (= pos2 (- (length (list-ref board pos)) 1))
          '()
          (if (= (- (length (list-ref board pos)) (length (list-ref board (sub1 pos)))) 1)
              (if (or (= pos 1) ( equal? (send (list-ref (list-ref board (sub1 pos)) pos2) get-color) "white"))
                  '()
                  (check-jump-up-right-aux board (sub1 pos) pos2 pos pos2 color))
              (if (and (> pos2 3) (< pos2 8))
                  (if (or (= pos 1) ( equal? (send (list-ref (list-ref board (sub1 pos)) (- pos2 4)) get-color) "white"))
                      '()
                      (check-jump-up-right-aux board (sub1 pos)(- pos2 4) pos pos2 color))
                  '()
                  )))
      (if (= (- (length (list-ref board (sub1 pos))) (length (list-ref board pos))) 1)
          (if ( equal? (send (list-ref (list-ref board (sub1 pos)) (add1 pos2)) get-color) "white")
                  '()
                  (check-jump-up-right-aux board (sub1 pos) (add1 pos2) pos pos2 color))
          (if (< (- (length (list-ref board (sub1 pos))) 1) (+ pos2 5))
              '()
              (if ( equal? (send (list-ref (list-ref board (sub1 pos)) (+ pos2 5)) get-color) "white")
                      '()
                      (check-jump-up-right-aux board (sub1 pos)(+ pos2 5) pos pos2 color)))
          )))

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
(define (check-jump-up-right-aux board posUp posUp2 pos pos2 c)
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
                       (sub1 posUp) posUp2 c)
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
                       (sub1 posUp) (- posUp2 4) c)
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
                       (sub1 posUp) (add1 posUp2) c)
                  '())
          (if (< (- (length (list-ref board (sub1 posUp))) 1) (+ posUp2 5))
              '()
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
                       (sub1 posUp) (+ posUp2 5) c)
                      '()))
          )))

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
(define (check-jump-left board pos pos2 c)
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
               pos (- pos2 2) c)
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
(define (check-jump-right board pos pos2 c)
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
               pos (+ pos2 2) c)
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
  (if (won-green board)
      100000000
      (if (won-red board)
          -100000000
          (- (+ (eval-aux board 0 (length board) 0) (count-green-final board)) (count-red-final board))))
  )


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
  (if (>= pos 15)
      0
      (+ (check-if-jump-down-left board pos pos2) (check-if-jump-down-right board pos pos2))))

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
  (if (<= pos 1)
      0
      (+ (check-if-jump-up-left board pos pos2) (check-if-jump-up-right board pos pos2))))

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
          (if (< (- (length (list-ref board (add1 pos))) 1) (+ pos2 4))
              0
              (if ( equal? (send (list-ref (list-ref board (add1 pos)) (+ pos2 4)) get-color) "white")
                      0
                      (check-if-jump-down-left-aux board (add1 pos)(+ pos2 4))))
          )))

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
          (if (< (- (length (list-ref board (add1 posDown))) 1) (+ posDown2 4))
              0
              (if ( equal? (send (list-ref (list-ref board (add1 posDown)) (+ posDown2 4)) get-color) "white")
                      (+ 10 (check-jumps-green-pos board (add1 posDown) (+ posDown2 4)))
                      0))
          )))

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
          (if (< (- (length (list-ref board (add1 pos))) 1) (+ pos2 5))
              0
              (if ( equal? (send (list-ref (list-ref board (add1 pos)) (+ pos2 5)) get-color) "white")
                      0
                      (check-if-jump-down-right-aux board (add1 pos)(+ pos2 5))))
          )))

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
          (if (< (- (length (list-ref board (add1 posDown))) 1) (+ posDown2 5))
              0
              (if ( equal? (send (list-ref (list-ref board (add1 posDown)) (+ posDown2 5)) get-color) "white")
                      (+ 10 (check-jumps-green-pos board (add1 posDown) (+ posDown2 5)))
                      0))
          )))

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
          (if (< (- (length (list-ref board (sub1 pos))) 1) (+ pos2 4))
              0
              (if ( equal? (send (list-ref (list-ref board (sub1 pos)) (+ pos2 4)) get-color) "white")
                      0
                      (check-if-jump-up-left-aux board (sub1 pos)(+ pos2 4))))
          )))

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
          (if (< (- (length (list-ref board (sub1 posUp))) 1) (+ posUp2 4))
              0
              (if ( equal? (send (list-ref (list-ref board (sub1 posUp)) (+ posUp2 4)) get-color) "white")
                      (+ 10 (check-jumps-red-pos board (sub1 posUp) (+ posUp2 4)))
                      0))
          )))

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
          (if (< (- (length (list-ref board (sub1 pos))) 1) (+ pos2 5))
              0
              (if ( equal? (send (list-ref (list-ref board (sub1 pos)) (+ pos2 5)) get-color) "white")
                      0
                      (check-if-jump-up-right-aux board (sub1 pos)(+ pos2 5)))
              )
          )))

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
          (if (< (- (length (list-ref board (sub1 posUp))) 1) (+ posUp2 5))
              0
              (if ( equal? (send (list-ref (list-ref board (sub1 posUp)) (+ posUp2 5)) get-color) "white")
                      (+ 10 (check-jumps-red-pos board (sub1 posUp) (+ posUp2 5)))
                      0))
          )))

#|
final-position revisa si el tablero está en posición final,
es decir, si alguno de los jugadores ya ganó
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
Salida:
- true si está en posición final, y false si no
|#

(define (final-position board)
  (or (won-green board) (won-red board)))

#|
won-green revisa si el jugador de fichas verdes ganó
o no
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
Salida:
- true si el jugador ganó y false si no
|#
(define (won-green board)
  (won-green-aux board (sub1 (length board)) #t)
  )

#|
won-green-aux es usado por won-green para
recorrer las filas de abajo del tablero para ver si
son verdes
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
- pos: la fila que se está revisando
- result: el resultado que se lleva hasta el momento
Salida:
- true si todas las fichas que se han encontrado son
  verdes y false si se encuentra alguna que no lo es
|#
(define (won-green-aux board pos result)
  (if result
      (if (< pos 13)
          #t
          (won-green-aux board (sub1 pos) (won-green-aux2 board pos 0 (length (list-ref board pos)))))
      #f))

#|
won-green-aux2 es usado por won-green-aux para
recorrer las casillas de cada fila para ver si
son verdes
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
- pos: la fila que se está revisando
- max: el número de columnas de la fila
Salida:
- true si todas las fichas que se han encontrado son
  verdes y false si se encuentra alguna que no lo es
|#
(define (won-green-aux2 board pos pos2 max)
  (if (>= pos2 max)
      #t
      (if ( equal? (send (list-ref (list-ref board pos) pos2) get-color) "green")
                      (won-green-aux2 board pos (add1 pos2) max)
                      #f))
  )

#|
won-red revisa si el jugador de fichas verdes ganó
o no
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
Salida:
- true si el jugador ganó y false si no
|#
(define (won-red board)
  (won-red-aux board 0 #t)
  )

#|
won-red-aux es usado por won-red para recorrer las
filas de arriba del tablero para ver si son rojas
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
- pos: la fila que se está revisando
- result: el resultado que se lleva hasta el momento
Salida:
- true si todas las fichas que se han encontrado son
  rojas y false si se encuentra alguna que no lo es
|#
(define (won-red-aux board pos result)
  (if result
      (if (> pos 3)
          #t
          (won-red-aux board (add1 pos) (won-red-aux2 board pos 0 (length (list-ref board pos)))))
      #f))

#|
won-red-aux2 es usado por won-red-aux para
recorrer las casillas de cada fila para ver si
son rojas
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
- pos: la fila que se está revisando
- max: el número de columnas de la fila
Salida:
- true si todas las fichas que se han encontrado son
  rojas y false si se encuentra alguna que no lo es
|#
(define (won-red-aux2 board pos pos2 max)
  (if (>= pos2 max)
      #t
      (if ( equal? (send (list-ref (list-ref board pos) pos2) get-color) "red")
                      (won-red-aux2 board pos (add1 pos2) max)
                      #f))
  )

#|
count-green-final cuenta cuantas fichas verdes hay en
la posición final, es decir, el triángulo de abajo, por
cada una de estas se suma 10
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
Salida:
- La suma de las fichas
|#
(define (count-green-final board)
  (count-green-final-aux board (sub1 (length board)) 0)
  )

#|
count-green-final-aux es usada por count-green-final
para revisar las filas de abajo del tablero contando
las fichas verdes que se encuentran
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
- pos: el número de fila actual
- result: La suma de las fichas que se lleva hasta el
  momento
Salida:
- La suma de las fichas
|#
(define (count-green-final-aux board pos result)
      (if (< pos 13)
          result
          (count-green-final-aux board (sub1 pos) (count-green-final-aux2 board pos 0 (length (list-ref board pos)) result)))
      )

#|
count-green-final-aux2 es usada por count-green-final-aux
para revisar las casillas de abajo del tablero contando
las fichas verdes que se encuentran
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
- pos: el número de fila actual
- pos2: el número de columna actual
- max: el número de columnas de la fila
- result: La suma de las fichas que se lleva hasta el
  momento
Salida:
- La suma de las fichas
|#
(define (count-green-final-aux2 board pos pos2 max result)
  (if (>= pos2 max)
      result
      (if ( equal? (send (list-ref (list-ref board pos) pos2) get-color) "green")
                      (count-green-final-aux2 board pos (add1 pos2) max (+ result 10))
                      (count-green-final-aux2 board pos (add1 pos2) max result)))
  )

#|
count-red-final cuenta cuantas fichas rojas hay en
la posición final, es decir, el triángulo de arriba, por
cada una de estas se suma 10
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
Salida:
- La suma de las fichas
|#
(define (count-red-final board)
  (count-red-final-aux board 0 0)
  )

#|
count-red-final-aux es usada por count-red-final
para revisar las filas de arriba del tablero contando
las fichas rojas que se encuentran
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
- pos: el número de fila actual
- result: La suma de las fichas que se lleva hasta el
  momento
Salida:
- La suma de las fichas
|#
(define (count-red-final-aux board pos result)
      (if (> pos 3)
          result
          (count-red-final-aux board (add1 pos) (count-red-final-aux2 board pos 0 (length (list-ref board pos)) result)))
  )

#|
count-red-final-aux2 es usada por count-red-final-aux
para revisar las casillas de arriba del tablero contando
las fichas rojas que se encuentran
- board: recibe un tablero (lista de listas de objetos
  cCheckersBox)
- pos: el número de fila actual
- pos2: el número de columna actual
- max: el número de columnas de la fila
- result: La suma de las fichas que se lleva hasta el
  momento
Salida:
- La suma de las fichas
|#
(define (count-red-final-aux2 board pos pos2 max result)
  (if (>= pos2 max)
      result
      (if ( equal? (send (list-ref (list-ref board pos) pos2) get-color) "red")
                      (count-red-final-aux2 board pos (add1 pos2) max (+ result 10))
                      (count-red-final-aux2 board pos (add1 pos2) max result)))
  )

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
  (cadr (max-move board -inf.0 +inf.0 color1 color2 6 20))
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
  (if (or (<= expandDeep 0) (final-position board))
    (list (eval board) board)
    (max-move-aux (check-possible-moves board color1 expandWide) alpha beta (list -inf.0 null) color1 color2 expandDeep expandWide)
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
  (if (empty? moves)
    v
    ((lambda (x)
      (if (> (car x) (car v))
        (set! v (list (car x) (car moves)))
        (void))
      (if (or (>= (car v) beta) (<= expandWide 0))
        v
        (max-move-aux (cdr moves) (max alpha (car v)) beta v color1 color2 expandDeep (sub1 expandWide))))

        (min-move (car moves) alpha beta color2 color1 (sub1 expandDeep) expandWide); Este es el valor de x inicial
      ))
      
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
  (if (or (<= expandDeep 0) (final-position board))
    (list (eval board) board)
    (min-move-aux (check-possible-moves board color1 expandWide) alpha beta (list +inf.0 null) color1 color2 expandDeep expandWide)
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
  (if (empty? moves)
    v
    ((lambda (x)
      (if (< (car x) (car v))
        (set! v (list (car x) (car moves)))
        (void))
      (if (or (<= (car v) alpha) (<= expandWide 0))
        v
        (min-move-aux (cdr moves) alpha (min (car v) beta) v color1 color2 expandDeep (sub1 expandWide))))
        
        (max-move (car moves) alpha beta color2 color1 (sub1 expandDeep) expandWide)); Este es el valor de x inicial
      )
  )

(provide alpha-beta-search)