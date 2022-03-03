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
        (paint-board (create-board) mc)
        )]))


; Muestra la ventana
(send frame show #t)
; Hace grande la ventana
(send frame maximize #t)
