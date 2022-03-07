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
                [size 20]
                )
    ; Los siguientes 5 métodos son getters y setters
    (define/public (get-color) color)
    (define/public (get-posX) posX)
    (define/public (get-posY) posY)
    (define/public (get-circle) circle)
    (define/public (set-color c)
      (set! color c)
      )
    #|
    is-posX-in se usa para saber si una posición
    "x" se encuentra dentro del circulo.
    Parámetro:
    - x: posición "x" a validar
    Salida:
    - Booleano
    |#
    (define/public (is-posX-close x)
      (if (and (<= posX x) (>= (+ posX size) x))
          #t
          #f
          )
      )
    #|
    is-posY-in se usa para saber si una posición
    "y" se encuentra dentro del circulo.
    Parámetro:
    - y: posición "y" a validar
    Salida:
    - Booleano
    |#
    (define/public (is-posY-in y)
      (if (and (<= posY y) (>= (+ posY size) y))
          #t
          #f
          )
      )
    #|
    draw-circle se usa para crear la forma del circulo
    y dibujarlo en un canvas dado.
    Parámetro:
    - mc: el canvas en el que se dibuja el circulo
    Salida:
    - Ninguna
    |#
    (define/public (draw-circle mc)
      (send circle arc posX posY size size 0 (* 2 pi))
      (send this paint-circle mc)
      )
    #|
    paint-circle se usa para dibujar el circulo
    en un canvas dado.
    Parámetro:
    - mc: el canvas en el que se dibuja el circulo
    Salida:
    - Ninguna
    |#
    (define/public (paint-circle mc)
      (send (send mc get-dc) set-brush color 'solid)
      (send (send mc get-dc) draw-path circle)
      )
    )
)

(provide cCheckersBox%)