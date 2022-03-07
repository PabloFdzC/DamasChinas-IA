#lang racket
(require "cCheckersBox.rkt")

(define board%
  (class object%
    (super-new)
    (init-field turn
                [boxSize 20]
                [spaceX 22]
                [spaceY 22]
                [startX 150]
                [startY 10]
                [selectedBox (void)]
                [previousColor null]
                [game null]
                )
    ; Los siguientes son getters y setters
    (define/public (get-turn) turn)
    (define/public (get-selectedBox) selectedBox)
    (define/public (get-previousColor) previousColor)
    (define/public (get-current-game) game)
    (define/public (set-selectedBox b)
      (set! selectedBox b)
      )
    (define/public (set-previousColor c)
      (set! previousColor c)
      )
    #|
      new-game establece el valor del atributo game
      como un nuevo tablero en su estado inicial
      Parámetros:
      - Ninguno
      Salida:
      - Ninguna
    |#
    (define/public (new-game)
      (set! game (send this create-board))
      )
    #|
      is-move-possible, dadas dos casillas del tablero
      indica si el movimiento es válido
      Parámetros:
      - box1: objeto cCheckersBox
      - box2: objeto cCheckersBox
      Salida:
      - Booleano
    |#
    (define/public (is-move-possible box1 box2)
      (if (and ;revisa que se pueda mover a un espacio que esté inmediatamente al lado
            (<= (abs (- (send box1 get-posX) (send box2 get-posX))) (ceiling (/ spaceX 2)))
            (<= (abs (- (send box1 get-posY) (send box2 get-posY))) (+ spaceY (/ spaceY 2)))
            )
          #t
          (if (or
                (and ;revisa que se intente mover dando un salto en diagonal
                  (<= (abs (- (send box1 get-posY) (send box2 get-posY))) (* spaceY 2))
                  (<= (abs (- (send box1 get-posX) (send box2 get-posX))) spaceX)
                  (not (equal? (send box1 get-posX) (send box2 get-posX)))
                  )
                (and ;revisa que se intente mover dando un salto en horizontal
                  (<= (abs (- (send box1 get-posX) (send box2 get-posX))) (* spaceX 2))
                  (equal?  (send box1 get-posY) (send box2 get-posY))
                  )
                )
                (if (equal? ;revisa si el que se intenta saltar es un espacio en blanco
                        "white"
                        (send (send this find-box (if (< (- (send box1 get-posX) (send box2 get-posX)) 0) (+ (send box1 get-posX) (ceiling (/ spaceX 2))) (- (send box1 get-posX) (ceiling (/ spaceX 2)))) (if (< (- (send box1 get-posY) (send box2 get-posY)) 0) (+ (send box1 get-posY) spaceY) (- (send box1 get-posY) spaceY))) get-color))
                    #f
                    #t
                    )
                #f
              )
          )
      )
    #|
      select-box selecciona una casilla del tablero
      y le cambia el color ya sea para saber cuál pieza
      se mueve o para mover una pieza
      Parámetros:
      - posX: posición "x" de la pieza
      - posY: posición "y" de la pieza
      - mc: canvas
      - pColor: color del jugador
      Salida:
      - Ninguna
    |#
    (define/public (select-box posX posY mc pColor)
      (let ([b (find-box posX posY)])
        (if (void? selectedBox)
            (void)
            ((lambda (selectedBox previousColor)
               (display selectedBox)
               (display (void? selectedBox))
               (send selectedBox set-color previousColor)
               (send selectedBox paint-circle mc)
              ) selectedBox previousColor)
            )
        (if (void? b)
            (void)
            (if (equal? pColor (send b get-color))
                ((lambda (b g)
                   (send g set-previousColor (send b get-color))
                   (send b set-color "yellow")
                   (send b paint-circle mc)
                   (send g set-selectedBox b)
                   ) b this)
                (if (and (equal? (send b get-color) "white") (send this is-move-possible selectedBox b))
                    ((lambda (b g)
                       (send b set-color (send g get-previousColor))
                       (send b paint-circle mc)
                       (send (send g get-selectedBox) set-color "white")
                       (send (send g get-selectedBox) paint-circle mc)
                       (send g set-previousColor null)
                       (send g set-selectedBox (void))
                       ;Aquí se debería llamar para el siguiente movimiento
                       ) b this)
                       (void)
                 )
                )
            )
        
        )
      )
    #|
      find-box busca una casilla en el tablero según
      la posición "x" e "y" suministradas
      Entradas:
      - posX: posición "x" de la casilla
      - posY: posición "y" de la casilla
      Salida:
      - Ninguna
    |#
    (define/public (find-box posX posY)
      (define (find-box-aux lst posX posY)
        (if (not(empty? lst))
            (if (send (caar lst) is-posY-in posY)
                (find-box-aux2 (car lst) posX posY)
                (find-box-aux (cdr lst) posX posY)
                )
            (void)
            )
        )
      (define (find-box-aux2 lst posX posY)
        (if (not(empty? lst))
            (if (send (car lst) is-posX-close posX)
                (car lst)
                (find-box-aux2 (cdr lst) posX posY)
                )
            (void)
            )
        )
      (find-box-aux game posX posY)
      )
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
    (define/public (create-board)
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
      Salida:
      - Devuelve una lista objetos cCheckersBox
      Ejemplo:
      (create-board-aux '() 0 3 "green" 10 10 20)
      Devuelve:
      (list (object:cCheckersBox% ...) (object:cCheckersBox% ...) (object:cCheckersBox% ...) (object:cCheckersBox% ...))
      |#
      (define (create-board-aux lst curr end c x y );spaceX)
        (if (> curr end)
            lst
            (create-board-aux
             (append lst (list (new cCheckersBox%
                                    [color c]
                                    [posX x]
                                    [posY y])))
             (add1 curr) end c (+ x spaceX) y )))

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
      (define (create-board-aux2 lst curr end c x y  rev)
        (if (> curr end)
            lst
            (create-board-aux2 (append lst (list (create-board-aux '() 0 curr c x y)))
                               (add1 curr) end c (- x (ceiling (/ spaceX 2))) (if rev (- y spaceX) (+ y spaceY)) rev)))
                                            ;startX: 150, startY: 10
      (append (create-board-aux2 '() 0 3 "green" startX startY #f)
              (reverse (create-board-aux2 '() 8 12 "white" (- startX (* 4 spaceY)) (+ startY (* 8 spaceY)) #t))
              (create-board-aux2 '() 9 12 "white" (- startX (* 4 spaceY) (ceiling (/ spaceX 2))) (+ startY (* 9 spaceY)) #f)
              (reverse (create-board-aux2 '() 0 3 "red" startX (+ startY (* 16 spaceY)) #t))
              )
      )
    )
  )

(provide board%)