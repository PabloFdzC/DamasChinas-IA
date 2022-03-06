#lang racket
(require racket/gui/base)
#|
Definición del objeto que representa las casillas
del tablero.
Parametros:
- color: que diferencia los espacios
  libres de los que son piezas en el tablero
- posX: posición en X de la casilla en el canvas
- posY: posición en Y de la casilla en el canvas
- circle: que es un objeto dc-path que permite
  dibujar la casilla en el canvas
|#
(define cCheckersBox%
  (class object%
    (super-new)
    (init-field color
                posX
                posY
                [circle (new dc-path%)]
                )
    ; Los siguientes 5 métodos son getters y setters
    (define/public (get-color) color)
    (define/public (get-pos-x) posX)
    (define/public (get-pos-y) posY)
    (define/public (get-circle) circle)
    (define/public (set-color c)
      (set! color c))
    #|
    draw-circle se usa para dibujar la casilla en un canvas
    dado.
    Parámetro:
    - mc: el canvas en el que se dibuja el circulo
    Salida:
    - Ninguna
    |#
    (define/public (draw-circle mc)
      (send (send mc get-dc) set-brush color 'solid)
      (send circle arc posX posY 20 20 0 (* 2 pi))
      (send (send mc get-dc) draw-path circle)
      )
    ))
#|
create-board crea una lista que representa el tablero
de juego
Parámetros:
- Ninguno
Salida:
- Devuelve una lista de listas de objetos cCheckersBox
Ejemplo:
(create-board)
Devuelve:
(list
 (list (object:cCheckersBox% ...))
 (list (object:cCheckersBox% ...) (object:cCheckersBox% ...))
...)
|#
(define (create-board)
  (append (create-board-aux2 '() 0 3 "green" 150 10 21 22 #f)
          (reverse (create-board-aux2 '() 8 12 "white" 70 182 21 22 #t))
          (create-board-aux2 '() 9 12 "white" 60 204 21 22 #f)
          (reverse (create-board-aux2 '() 0 3 "red" 150 356 21 22 #t))
          ))

#|
create-board-aux mete en una lista tantos objetos cCheckersBox
como le sean indicados por los parámetros curr y end, estos
objetos los mete en la lista que se pasó por el parámetro lst
Parámetros:
- lst: lista donde se meten los objetos
- curr: sirve para contar los objetos que hemos metido
- end: determina cuando se debe detener el proceso
- c: color que debe llevar la casilla
- x: posición x de la casilla en el canvas
- y: posición y de la casilla en el canvas
- spaceX: cantidad que modifica la distancia entre cada
  casilla para que queden separadas en el eje x
Salida:
- Devuelve una lista objetos cCheckersBox
Ejemplo:
(create-board-aux '() 0 3 "green" 10 10 20)
Devuelve:
(list (object:cCheckersBox% ...) (object:cCheckersBox% ...) (object:cCheckersBox% ...) (object:cCheckersBox% ...))
|#
(define (create-board-aux lst curr end c x y spaceX)
  (if (> curr end)
      lst
      (create-board-aux
       (append lst (list (new cCheckersBox%
                        [color c]
                        [posX x]
                        [posY y])))
       (add1 curr) end c (+ x spaceX) y spaceX)))

#|
create-board-aux2 mete en una lista las listas de objetos
generadas por la función create-board-aux, básicamente
es la que se encarga de que los circulos que representan
las casillas queden ordenados en forma de triángulos
Parámetros:
- lst: lista donde se meten los objetos
- curr: sirve para contar los objetos que hemos metido
- end: determina cuando se debe detener el proceso
- c: color que debe llevar la casilla
- x: posición x de la casilla en el canvas
- y: posición y de la casilla en el canvas
- spaceX: cantidad que modifica la distancia entre cada
  casilla para que queden separadas en el eje x
- spaceY: cantidad que modifica la distancia entre cada
  casilla para que queden separadas en el eje y
- rev: indica si se va a acomodar el triángulo generado
  en reverso (en lugar de apuntar hacia arriba apunta hacia
  abajo)
Salida:
- Devuelve una lista de listas con objetos cCheckersBox
Ejemplo:
(create-board-aux2 '() 0 3 "green" 150 10 21 22 #f)
Devuelve:
(list
 (list (object:cCheckersBox% ...))
 (list (object:cCheckersBox% ...) (object:cCheckersBox% ...))
 (list (object:cCheckersBox% ...) (object:cCheckersBox% ...) (object:cCheckersBox% ...))
 (list (object:cCheckersBox% ...) (object:cCheckersBox% ...) (object:cCheckersBox% ...) (object:cCheckersBox% ...)))
|#
(define (create-board-aux2 lst curr end c x y spaceX spaceY rev)
  (if (> curr end)
      lst
      (create-board-aux2 (append lst (list (create-board-aux '() 0 curr c x y spaceX)))
                         (add1 curr) end c (- x (/ (sub1 spaceY) 2)) (if rev (- y spaceX) (+ y spaceY)) spaceX spaceY rev)))

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
  (if (not (empty? board))
      ((lambda (board mc)
         (paint-board-aux (first board) mc)
         (paint-board (rest board) mc))
       board mc)
      (void)))

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
      (void)))

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
my-canvas% es una clase que hereda de la
clase canvas%. Se hizo de esta manera para
poder tener acceso a la interación con el mouse
|#
(define my-canvas%
  (class canvas%
    (super-new)
    (inherit/super get-dc)
    ; Método para interactuar con el mouse (ahorita solo pinta un punto en el canvas)
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

#|
mc es la instanciación del canvas
|#
(define mc (new my-canvas%
     [parent frame]
     [paint-callback
      (lambda (canvas dc)
        (send dc set-background "black")
        (send dc clear)
        ; Aqui es donde se manda a llamar para que se pinte el tablero
        ; (create-board) es el tablero, igual se puede sacar a que sea como una variable
        ; lo que importa realmete es que paint-board se llame desde aqui para que se
        ; imprima desde el inicio, mc es este mismo canvas
        (paint-board (list-ref (check-possible-moves (create-board) "red") 0) mc)
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
                                                                [posX (send (list-ref (list-ref board (sub1 pos)) (sub1 pos2)) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board (sub1 pos)) (sub1 pos2)) get-pos-y)]))
                                                                (drop (list-ref board (sub1 pos)) pos2)))
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-pos-y)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (drop board (add1 pos)))
                  (if (= pos 1) '() (check-move-up-left-jump board pos pos2 (sub1 pos)(sub1 pos2) c)))
              (if (and (> pos2 4) (< pos2 9))
                  (if ( equal? (send (list-ref (list-ref board (sub1 pos)) (- pos2 5)) get-color) "white")
                      (append (take board (sub1 pos))
                          (list (append (take (list-ref board (sub1 pos)) (- pos2 5)) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (sub1 pos)) (- pos2 5)) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board (sub1 pos)) (- pos2 5)) get-pos-y)]))
                                                                (drop (list-ref board (sub1 pos)) (- pos2 4))))
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-pos-y)]))
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
                                                                [posX (send (list-ref (list-ref board (sub1 pos)) pos2) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board (sub1 pos)) pos2) get-pos-y)]))
                                                                (drop (list-ref board (sub1 pos)) (add1 pos2))))
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-pos-y)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (drop board (add1 pos)))
                  (check-move-up-left-jump board pos pos2 (sub1 pos) pos2 c))
          (if ( equal? (send (list-ref (list-ref board (sub1 pos)) (+ pos2 4)) get-color) "white")
                      (append (take board (sub1 pos))
                          (list (append (take (list-ref board (sub1 pos)) (+ pos2 4)) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (sub1 pos)) (+ pos2 4)) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board (sub1 pos)) (+ pos2 4)) get-pos-y)]))
                                                                (drop (list-ref board (sub1 pos)) (+ pos2 5))))
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-pos-y)]))
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
                                                                [posX (send (list-ref (list-ref board (sub1 posUp)) (sub1 posUp2)) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board (sub1 posUp)) (sub1 posUp2)) get-pos-y)]))
                                                                (drop (list-ref board (sub1 posUp)) posUp2)))
                          (list (list-ref board posUp))
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-pos-y)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (drop board (add1 pos)))
                  '())
              (if (and (> posUp2 4) (< posUp2 9))
                  (if ( equal? (send (list-ref (list-ref board (sub1 posUp)) (- posUp2 5)) get-color) "white")
                      (append (take board (sub1 pos))
                          (list (append (take (list-ref board (sub1 posUp)) (- posUp2 5)) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (sub1 posUp)) (- posUp2 5)) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board (sub1 posUp)) (- posUp2 5)) get-pos-y)]))
                                                                (drop (list-ref board (sub1 posUp)) (- posUp2 4))))
                          (list (list-ref board posUp))
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-pos-y)]))
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
                                                                [posX (send (list-ref (list-ref board (sub1 posUp)) posUp2) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board (sub1 posUp)) posUp2) get-pos-y)]))
                                                                (drop (list-ref board (sub1 posUp)) (add1 posUp2))))
                          (list (list-ref board posUp))
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-pos-y)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (drop board (add1 pos)))
                  '())
          (if ( equal? (send (list-ref (list-ref board (sub1 posUp)) (+ posUp2 4)) get-color) "white")
                      (append (take board (sub1 posUp))
                          (list (append (take (list-ref board (sub1 posUp)) (+ posUp2 4)) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (sub1 posUp)) (+ posUp2 4)) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board (sub1 posUp)) (+ posUp2 4)) get-pos-y)]))
                                                                (drop (list-ref board (sub1 posUp)) (+ posUp2 5))))
                          (list (list-ref board posUp))
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-pos-y)]))
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
                                                                [posX (send (list-ref (list-ref board (sub1 pos)) pos2) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board (sub1 pos)) pos2) get-pos-y)]))
                                                                (drop (list-ref board (sub1 pos)) (add1 pos2))))
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-pos-y)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (drop board (add1 pos)))
                  (if (= pos 1) '() (check-move-up-right-jump board pos pos2 (sub1 pos) pos2 c)))
              (if (and (> pos2 3) (< pos2 8))
                  (if ( equal? (send (list-ref (list-ref board (sub1 pos)) (- pos2 4)) get-color) "white")
                      (append (take board (sub1 pos))
                          (list (append (take (list-ref board (sub1 pos)) (- pos2 4)) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (sub1 pos)) (- pos2 4)) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board (sub1 pos)) (- pos2 4)) get-pos-y)]))
                                                                (drop (list-ref board (sub1 pos)) (- pos2 3))))
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-pos-y)]))
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
                                                                [posX (send (list-ref (list-ref board (sub1 pos)) (add1 pos2)) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board (sub1 pos)) (add1 pos2)) get-pos-y)]))
                                                                (drop (list-ref board (sub1 pos)) (+ pos2 2))))
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-pos-y)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (drop board (add1 pos)))
                  (check-move-up-right-jump board pos pos2 (sub1 pos) (add1 pos2) c))
          (if ( equal? (send (list-ref (list-ref board (sub1 pos)) (+ pos2 5)) get-color) "white")
                      (append (take board (sub1 pos))
                          (list (append (take (list-ref board (sub1 pos)) (+ pos2 5)) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (sub1 pos)) (+ pos2 5)) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board (sub1 pos)) (+ pos2 5)) get-pos-y)]))
                                                                (drop (list-ref board (sub1 pos)) (+ pos2 6))))
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-pos-y)]))
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
                                                                [posX (send (list-ref (list-ref board (sub1 posUp)) posUp2) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board (sub1 posUp)) posUp2) get-pos-y)]))
                                                                (drop (list-ref board (sub1 posUp)) (add1 posUp2))))
                          (list (list-ref board posUp))
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-pos-y)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (drop board (add1 pos)))
                  '())
              (if (and (> posUp2 3) (< posUp2 8))
                  (if ( equal? (send (list-ref (list-ref board (sub1 posUp)) (- posUp2 4)) get-color) "white")
                      (append (take board (sub1 pos))
                          (list (append (take (list-ref board (sub1 posUp)) (- posUp2 4)) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (sub1 posUp)) (- posUp2 4)) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board (sub1 posUp)) (- posUp2 4)) get-pos-y)]))
                                                                (drop (list-ref board (sub1 posUp)) (- posUp2 3))))
                          (list (list-ref board posUp))
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-pos-y)]))
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
                                                                [posX (send (list-ref (list-ref board (sub1 posUp)) (add1 posUp2)) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board (sub1 posUp)) (add1 posUp2)) get-pos-y)]))
                                                                (drop (list-ref board (sub1 posUp)) (+ posUp2 2))))
                          (list (list-ref board posUp))
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-pos-y)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (drop board (add1 pos)))
                  '())
          (if ( equal? (send (list-ref (list-ref board (sub1 posUp)) (+ posUp2 5)) get-color) "white")
                      (append (take board (sub1 posUp))
                          (list (append (take (list-ref board (sub1 posUp)) (+ posUp2 5)) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (sub1 posUp)) (+ posUp2 5)) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board (sub1 posUp)) (+ posUp2 5)) get-pos-y)]))
                                                                (drop (list-ref board (sub1 posUp)) (+ posUp2 6))))
                          (list (list-ref board posUp))
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-pos-y)]))
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
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-pos-y)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (list (append (take (list-ref board (add1 pos)) (sub1 pos2)) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (add1 pos)) (sub1 pos2)) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board (add1 pos)) (sub1 pos2)) get-pos-y)]))
                                                                (drop (list-ref board (add1 pos)) pos2)))
                          (drop board (+ pos 2)))
                  (if (= pos 15) '() (check-move-down-left-jump board pos pos2 (add1 pos)(sub1 pos2) c)))
              (if (and (> pos2 4) (< pos2 9))
                  (if ( equal? (send (list-ref (list-ref board (add1 pos)) (- pos2 5)) get-color) "white")
                      (append (take board pos)
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-pos-y)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (list (append (take (list-ref board (add1 pos)) (- pos2 5)) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (add1 pos)) (- pos2 5)) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board (add1 pos)) (- pos2 5)) get-pos-y)]))
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
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-pos-y)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (list (append (take (list-ref board (add1 pos)) pos2) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (add1 pos)) pos2) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board (add1 pos)) pos2) get-pos-y)]))
                                                                (drop (list-ref board (add1 pos)) (add1 pos2))))
                          (drop board (+ pos 2)))
                  (check-move-down-left-jump board pos pos2 (add1 pos) pos2 c))
          (if ( equal? (send (list-ref (list-ref board (add1 pos)) (+ pos2 4)) get-color) "white")
                      (append (take board pos)
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-pos-y)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (list (append (take (list-ref board (add1 pos)) (+ pos2 4)) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (add1 pos)) (+ pos2 4)) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board (add1 pos)) (+ pos2 4)) get-pos-y)]))
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
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-pos-y)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (list (list-ref board posDown))
                          (list (append (take (list-ref board (add1 posDown)) (sub1 posDown2)) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (add1 posDown)) (sub1 posDown2)) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board (add1 posDown)) (sub1 posDown2)) get-pos-y)]))
                                                                (drop (list-ref board (add1 posDown)) posDown2)))
                          (drop board (+ posDown 2)))
                  '())
              (if (and (> posDown2 4) (< posDown2 9))
                  (if ( equal? (send (list-ref (list-ref board (add1 posDown)) (- posDown2 5)) get-color) "white")
                      (append (take board pos)
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-pos-y)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (list (list-ref board posDown))
                          (list (append (take (list-ref board (add1 posDown)) (- posDown2 5)) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (add1 posDown)) (- posDown2 5)) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board (add1 posDown)) (- posDown2 5)) get-pos-y)]))
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
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-pos-y)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (list (list-ref board posDown))
                          (list (append (take (list-ref board (add1 posDown)) posDown2) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (add1 posDown)) posDown2) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board (add1 posDown)) posDown2) get-pos-y)]))
                                                                (drop (list-ref board (add1 posDown)) (add1 posDown2))))
                          (drop board (+ posDown 2)))
                  '())
          (if ( equal? (send (list-ref (list-ref board (add1 posDown)) (+ posDown2 4)) get-color) "white")
                      (append (take board pos)
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-pos-y)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (list (list-ref board posDown))
                          (list (append (take (list-ref board (add1 posDown)) (+ posDown2 4)) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (add1 posDown)) (+ posDown2 4)) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board (add1 posDown)) (+ posDown2 4)) get-pos-y)]))
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
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-pos-y)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (list (append (take (list-ref board (add1 pos)) pos2) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (add1 pos)) pos2) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board (add1 pos)) pos2) get-pos-y)]))
                                                                (drop (list-ref board (add1 pos)) (add1 pos2))))
                          (drop board (+ pos 2)))
                  (if (= pos 15) '() (check-move-down-right-jump board pos pos2 (add1 pos) pos2 c)))
              (if (and (> pos2 3) (< pos2 8))
                  (if ( equal? (send (list-ref (list-ref board (add1 pos)) (- pos2 4)) get-color) "white")
                      (append (take board pos)
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-pos-y)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (list (append (take (list-ref board (add1 pos)) (- pos2 4)) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (add1 pos)) (- pos2 4)) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board (add1 pos)) (- pos2 4)) get-pos-y)]))
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
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-pos-y)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (list (append (take (list-ref board (add1 pos)) (add1 pos2)) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (add1 pos)) (add1 pos2)) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board (add1 pos)) (add1 pos2)) get-pos-y)]))
                                                                (drop (list-ref board (add1 pos)) (+ pos2 2))))
                          (drop board (+ pos 2)))
                  (check-move-down-right-jump board pos pos2 (add1 pos) (add1 pos2) c))
          (if ( equal? (send (list-ref (list-ref board (add1 pos)) (+ pos2 5)) get-color) "white")
                      (append (take board pos)
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-pos-y)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (list (append (take (list-ref board (add1 pos)) (+ pos2 5)) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (add1 pos)) (+ pos2 5)) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board (add1 pos)) (+ pos2 5)) get-pos-y)]))
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
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-pos-y)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (list (list-ref board posDown))
                          (list (append (take (list-ref board (add1 posDown)) posDown2) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (add1 posDown)) posDown2) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board (add1 posDown)) posDown2) get-pos-y)]))
                                                                (drop (list-ref board (add1 posDown)) (add1 posDown2))))
                          (drop board (+ posDown 2)))
                  '())
              (if (and (> posDown2 3) (< posDown2 8))
                  (if ( equal? (send (list-ref (list-ref board (add1 posDown)) (- posDown2 4)) get-color) "white")
                      (append (take board pos)
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-pos-y)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (list (list-ref board posDown))
                          (list (append (take (list-ref board (add1 posDown)) (- posDown2 4)) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (add1 posDown)) (- posDown2 4)) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board (add1 posDown)) (- posDown2 4)) get-pos-y)]))
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
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-pos-y)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (list (list-ref board posDown))
                          (list (append (take (list-ref board (add1 posDown)) (add1 posDown2)) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (add1 posDown)) (add1 posDown2)) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board (add1 posDown)) (add1 posDown2)) get-pos-y)]))
                                                                (drop (list-ref board (add1 posDown)) (+ posDown2 2))))
                          (drop board (+ posDown 2)))
                  '())
          (if ( equal? (send (list-ref (list-ref board (add1 posDown)) (+ posDown2 5)) get-color) "white")
                      (append (take board pos)
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                [color "white"]
                                                                [posX (send (list-ref (list-ref board pos) pos2) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board pos) pos2) get-pos-y)]))
                                                                (drop (list-ref board pos) (add1 pos2))))
                          (list (list-ref board posDown))
                          (list (append (take (list-ref board (add1 posDown)) (+ posDown2 5)) (list (new cCheckersBox%
                                                                [color c]
                                                                [posX (send (list-ref (list-ref board (add1 posDown)) (+ posDown2 5)) get-pos-x)]
                                                                [posY (send (list-ref (list-ref board (add1 posDown)) (+ posDown2 5)) get-pos-y)]))
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
                                                                                     [posX (send (list-ref (list-ref board pos) (sub1 pos2)) get-pos-x)]
                                                                                     [posY (send (list-ref (list-ref board pos) (sub1 pos2)) get-pos-y)])
                                                                                (new cCheckersBox%
                                                                                     [color "white"]
                                                                                     [posX (send (list-ref (list-ref board pos) pos2) get-pos-x)]
                                                                                     [posY (send (list-ref (list-ref board pos) pos2) get-pos-y)]))
                                (drop (list-ref board pos) (add1 pos2))))
                  (drop board (add1 pos)))
          (if (= pos2 1)
              '()
              (if ( equal? (send (list-ref (list-ref board pos) (- pos2 2)) get-color) "white")
                  (append (take board pos)
                          (list (append (take (list-ref board pos) (- pos2 2)) (list (new cCheckersBox%
                                                                                     [color c]
                                                                                     [posX (send (list-ref (list-ref board pos) (- pos2 2)) get-pos-x)]
                                                                                     [posY (send (list-ref (list-ref board pos) (- pos2 2)) get-pos-y)])
                                                                                (list-ref (list-ref board pos) (sub1 pos2))
                                                                                (new cCheckersBox%
                                                                                     [color "white"]
                                                                                     [posX (send (list-ref (list-ref board pos) pos2) get-pos-x)]
                                                                                     [posY (send (list-ref (list-ref board pos) pos2) get-pos-y)]))
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
                                                                                     [posX (send (list-ref (list-ref board pos) pos2) get-pos-x)]
                                                                                     [posY (send (list-ref (list-ref board pos) pos2) get-pos-y)])
                                                                                (new cCheckersBox%
                                                                                     [color c]
                                                                                     [posX (send (list-ref (list-ref board pos) (add1 pos2)) get-pos-x)]
                                                                                     [posY (send (list-ref (list-ref board pos) (add1 pos2)) get-pos-y)]))
                                (drop (list-ref board pos) (+ pos2 2))))
                  (drop board (add1 pos)))
          (if (= pos2 (- (length (list-ref board pos)) 2))
              '()
              (if ( equal? (send (list-ref (list-ref board pos) (+ pos2 2)) get-color) "white")
                  (append (take board pos)
                          (list (append (take (list-ref board pos) pos2) (list (new cCheckersBox%
                                                                                     [color "white"]
                                                                                     [posX (send (list-ref (list-ref board pos) pos2) get-pos-x)]
                                                                                     [posY (send (list-ref (list-ref board pos) pos2) get-pos-y)])
                                                                                (list-ref (list-ref board pos) (add1 pos2))
                                                                                (new cCheckersBox%
                                                                                     [color c]
                                                                                     [posX (send (list-ref (list-ref board pos) (+ pos2 2)) get-pos-x)]
                                                                                     [posY (send (list-ref (list-ref board pos) (+ pos2 2)) get-pos-y)]))
                                (drop (list-ref board pos) (+ pos2 3))))
                  (drop board (add1 pos)))
                  '())))))

(define lst (list 1 2 3 4 5))



; Muestra la ventana
(send frame show #t)
; Hace grande la ventana
(send frame maximize #t)
