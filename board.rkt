#lang racket
(require "cCheckersBox.rkt")
(require "minimax.rkt")

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
                [firstSelected (void)]
                [lastMoved null]
                [preLastMoved null]
                [previousColor null]
                [game null]
                [playWithAI #t]
                [redWon #f]
                [greenWon #f]
                [moveN 0]
                )
    ; Los siguientes son getters y setters
    (define/public (get-turn) turn)
    (define/public (get-selectedBox) selectedBox)
    (define/public (get-firstSelected) firstSelected)
    (define/public (get-previousColor) previousColor)
    (define/public (get-lastMoved) lastMoved)
    (define/public (get-preLastMoved) preLastMoved)
    (define/public (get-current-game) game)
    (define/public (set-turn t)
      (set! turn t)
      )
    (define/public (set-redWon rw)
      (set! redWon rw)
      )
    (define/public (set-greenWon gw)
      (set! greenWon gw)
      )
    (define/public (set-current-game g)
      (set! game g)
      )
    (define/public (set-selectedBox b)
      (set! selectedBox b)
      )
    (define/public (set-firstSelected b)
      (set! firstSelected b)
      )
    (define/public (set-previousColor c)
      (set! previousColor c)
      )
    (define/public (set-lastMoved lm)
      (set! lastMoved lm)
      )
    (define/public (set-preLastMoved plm)
      (set! preLastMoved plm)
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
      (send this reset-variables)
      (set! moveN 0)
      (set! game (send this create-board))
      )
    
    #|
      is-move-single indica si 2 casillas que
      representan un movimiento se encuentran 
      una junto a la otra, esto significa que
      se movió solo una casilla
      Parámetros:
      - box1: objeto cCheckersBox
      - box2: objeto cCheckersBox
      Salida:
      - booleano
    |#
    (define/public (is-move-single box1 box2)
      (or
        (and ;revisa que se pueda mover a un espacio que esté inmediatamente al lado vertical
          (<= (abs (- (send box1 get-posX) (send box2 get-posX))) (ceiling (/ spaceX 2)))
          (<= (abs (- (send box1 get-posY) (send box2 get-posY))) (+ spaceY (/ spaceY 2)))
          )
        (and ;revisa que se pueda mover a un espacio que esté inmediatamente al lado horizontal
          (equal? (send box1 get-posY) (send box2 get-posY))
          (<= (abs (- (send box1 get-posX) (send box2 get-posX))) spaceX)
          )
        )
      )

    #|
      is-move-jump indica si 2 casillas que
      representan un movimiento se encuentran 
      a un salto de la otra, esto significa que
      se movió con salto
      Parámetros:
      - box1: objeto cCheckersBox
      - box2: objeto cCheckersBox
      Salida:
      - booleano
    |#
    (define/public (is-move-jump box1 box2)
      (or
        (and ;revisa que se intente mover dando un salto en diagonal
          (<= (abs (- (send box1 get-posY) (send box2 get-posY))) (* spaceY 2))
          (> (abs (- (send box1 get-posY) (send box2 get-posY))) spaceY)
          (<= (abs (- (send box1 get-posX) (send box2 get-posX))) spaceX)
          (not (equal? (send box1 get-posX) (send box2 get-posX)))
          )
        (and ;revisa que se intente mover dando un salto en horizontal
          (<= (abs (- (send box1 get-posX) (send box2 get-posX))) (* spaceX 2))
          (> (abs (- (send box1 get-posX) (send box2 get-posX))) spaceX)
          (equal?  (send box1 get-posY) (send box2 get-posY))
          )
        )
      )
    #|
      is-move-possible, dadas dos casillas del tablero
      indica si el movimiento es válido
      Parámetros:
      - box1: objeto cCheckersBox
      - box2: objeto cCheckersBox
        saltar
      Salida:
      - Booleano
    |#
    (define/public (is-move-possible box1 box2)
      (if (send this is-move-single box1 box2)
          #t
          (if (send this is-move-jump box1 box2)
                (if (equal? ;revisa si el que se intenta saltar es un espacio en blanco
                        "white"
                        (send (send this find-box (if (< (- (send box1 get-posX) (send box2 get-posX)) 0)
                                                    (+ (send box1 get-posX) (if (equal? (send box1 get-posY) (send box2 get-posY)) spaceX (ceiling (/ spaceX 2))))
                                                    (- (send box1 get-posX) (if (equal? (send box1 get-posY) (send box2 get-posY)) spaceX (ceiling (/ spaceX 2)))))
                                                  (if (< (- (send box1 get-posY) (send box2 get-posY)) 0)
                                                    (+ (send box1 get-posY) spaceY)
                                                    (if (equal? (send box1 get-posY) (send box2 get-posY)) (send box1 get-posY) (- (send box1 get-posY) spaceY))))
                              get-color))
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
      se mueve o para mover una pieza. También si es el
      caso permite seleccionar los botones de "Nuevo Juego"
      y "Confirmar Movimiento"
      Parámetros:
      - posX: posición "x" de la pieza
      - posY: posición "y" de la pieza
      - mc: canvas
      - pColor: color del jugador
      Salida:
      - Ninguna
    |#
    (define/public (select-box posX posY mc pColor)
      (cond 
        [(and (>= posX 490) (>= posY 10) (<= posX 600) (<= posY 40))
          ((lambda ()
            (send this new-game)
            (send this paint-board (send this get-current-game) mc)
            (send this paint-message mc "Turno de:" "Jugador" "red")
            (set! redWon #f)
            (set! greenWon #f)
            (send this reset-variables)))]
        [(and (>= posX 490) (>= posY 300) (<= posX 600) (<= posY 345))
          (if (void? selectedBox)
            (send this move-done mc)
            ((lambda ()
              (send selectedBox set-color (send this get-previousColor))
              (send selectedBox paint-circle mc)
              (send this move-done mc)
              )))]
        [else (if (or redWon greenWon)
          (void)
          (let ([b (find-box posX posY)])
            (if (void? selectedBox)
                (void)
                ((lambda ()
                  (send selectedBox set-color previousColor)
                  (send selectedBox paint-circle mc)
                  ))
                )
            (if (void? b)
                (void)
                (if (equal? pColor (send b get-color))
                    ((lambda ()
                      (send this set-previousColor (send b get-color))
                      (send b set-color "yellow")
                      (send b paint-circle mc)
                      (send this set-selectedBox b)
                      ))
                    (if (void? selectedBox)
                      (void)
                      (if (and (equal? (send b get-color) "white") (send this is-move-possible selectedBox b))
                        (let 
                            ([changeColors (lambda (b g)
                              (send b set-color (send g get-previousColor))
                              (send b paint-circle mc)
                              (send (send g get-selectedBox) set-color "white")
                              (send (send g get-selectedBox) paint-circle mc)
                              (send g set-preLastMoved (send g get-selectedBox))
                              (send g set-previousColor null)
                              (send g set-lastMoved b)
                              (if (void? (send g get-firstSelected))
                                (send g set-firstSelected (send g get-selectedBox))
                                (if (and (equal? (send (send g get-firstSelected) get-posX) (send b get-posX)) (equal? (send (send g get-firstSelected) get-posY) (send b get-posY)))
                                  (send g reset-variables)
                                  (void)))
                              (send g set-selectedBox (void))
                              )])
                            (if (null? (send this get-lastMoved))
                              (changeColors b this)
                              ((lambda () (if (and
                                    (equal? (send lastMoved get-posX) (send selectedBox get-posX)) ;verifica que sea la misma ficha movida antes
                                    (equal? (send lastMoved get-posY) (send selectedBox get-posY)) ;verifica que sea la misma ficha movida antes
                                    (or (and 
                                          (not (send this is-move-single preLastMoved lastMoved)) ;movimiento anterior no sea de paso único
                                          (send this is-move-jump selectedBox b);movimiento actual es salto
                                          )
                                        (and ;vuelve al movimiento pasado
                                          (equal? (send preLastMoved get-posX) (send b get-posX))
                                          (equal? (send preLastMoved get-posY) (send b get-posY)))))
                                (changeColors b this)
                                (void)
                                )
                                ))
                              ))
                        (void)
                        )
                      )
                    )
                )
            )
          )])
      )
    #|
      find-box busca una casilla en el tablero según
      la posición "x" e "y" suministradas
      Entradas:
      - posX: posición "x" de la casilla
      - posY: posición "y" de la casilla
      Salida:
      - objeto cCheckersBox o nada
    |#
    (define/public (find-box posX posY)
      #|
        find-box-aux permite iterar la el tablero
        para buscar la casilla correspondiente
        Entradas:
        - posX: posición "x" de la casilla
        - posY: posición "y" de la casilla
        Salida:
        - objeto cCheckersBox o nada (lo que
          devuelve find-box-aux2)
      |#
      (define (find-box-aux lst posX posY)
        (if (not(empty? lst))
            (if (send (caar lst) is-posY-in posY)
                (find-box-aux2 (car lst) posX posY)
                (find-box-aux (cdr lst) posX posY)
                )
            (void)
            )
        )
      #|
        find-box-aux2 devuelve el objeto cCheckersBox
        si lo encuentra, sino devuelve void
        Entradas:
        - posX: posición "x" de la casilla
        - posY: posición "y" de la casilla
        Salida:
        - objeto cCheckersBox o nada (lo que
          devuelve find-box-aux2)
      |#
      (define (find-box-aux2 lst posX posY)
        (if (not(empty? lst))
            (if (send (car lst) is-posX-close posX)
                (car lst)
                (find-box-aux2 (cdr lst) posX posY)
                )
            (void)
            )
        )
      (find-box-aux (send this get-current-game) posX posY)
      )

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
    (define/public (paint-board board mc)
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
        ((lambda (board mc g)
          (paint-board-aux (first board) mc)
          (send g paint-board (rest board) mc))
        board mc this)
        (void)
        )
      )
    
    (define/public (paint-board-only-changes board boardChanged mc)
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
      (define (paint-board-only-changes-aux board boardChanged mc)
        (if (not (empty? board))
            ((lambda (board mc)
              (if (equal? (send (first board) get-color) (send (first boardChanged) get-color))
                (void)
                ((lambda () (send (first board) set-color (send (first boardChanged) get-color)) (send (first board) paint-circle mc))))
              (paint-board-only-changes-aux (rest board) (rest boardChanged) mc))
            board mc)
            (void)
            )
        )
      (if (not (empty? board))
        ((lambda (board boardChanged mc g)
          (paint-board-only-changes-aux (first board) (first boardChanged) mc)
          (send g paint-board-only-changes (rest board) (rest boardChanged) mc))
        board boardChanged mc this)
        (void)
        )
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
    #|
    move-done termina el turno del jugador y settea las
    variables de movimientos en su estado inicial
    Parámetros:
    - mc: es el canvas del juego
    Salida:
    - Ninguna
    |#
    (define/public (move-done mc)
      (if (null? lastMoved)
        (void)
        ((lambda ()
          (send this reset-variables)
          (if (won-red game)
            ((lambda () (send this paint-message mc "Ganador:" "Jugador" "yellow") (send this set-redWon #t)))
            (if playWithAI
              ((lambda ()
                (send this paint-message mc "Turno de:" "IA" "green")
                (send this make-ai-move mc)
                (if (won-green game)
                  ((lambda () (send this paint-message mc "Ganador:" "IA" "yellow") (send this set-greenWon #t)))
                  (send this paint-message mc "Turno de:" "Jugador" "red"))
                ))
              (send this set-turn (not (send this get-turn)))
              )
            )
          ))
        )
      )    
    #|
    reset-variables devuelve los atributos selectedBox
    lastMoved y previousColor a su valor inicial
    Parámetros:
    - Ninguna
    Salida:
    - Ninguna
    |#
    (define/public (reset-variables)
      (send this set-selectedBox (void))
      (send this set-lastMoved null)
      (send this set-previousColor null)
      (send this set-firstSelected (void))
      )
    #|
    paint-buttons dibuja los botones de nuevo juego
    y confirmar movimiento
    Parámetros:
    - mc: es el canvas del juego
    Salida:
    - Ninguna
    |#
    (define/public (paint-buttons mc)
      (let ([dc (send mc get-dc)])
        (send dc set-text-foreground "black")
        (send dc set-brush "white" 'solid)
        (send dc draw-rounded-rectangle 490 10 110 30)
        (send dc draw-text "Nuevo Juego" 500 15)
        (send dc set-text-foreground "white")
        (send dc set-brush "blue" 'solid)
        (send dc draw-rounded-rectangle 490 300 110 45)
        (send dc draw-text "Confirmar" 510 305)
        (send dc draw-text "Movimiento" 505 320)
        )
      )
    #|
    paint-message pinta en el canvas en una caja el turno
    del jugador actual
    Parámetros:
    - mc: es el canvas del juego
    - player: string con el nombre del jugador actual
    - color: color que se va a pintar la caja
    Salida:
    - Ninguna
    |#
    (define/public (paint-message mc title player color)
      (let ([dc (send mc get-dc)])
        (send dc set-text-foreground "black")
        (send dc set-brush color 'solid)
        (send dc draw-rounded-rectangle 490 50 110 45)
        (send dc draw-text title 510 55)
        (send dc draw-text player 515 70)
        )
      )
    #|
    make-ai-move hace el movimiento de la IA
    Parámetros:
    - mc: es el canvas del juego
    Salida:
    - Ninguna
    |#  
    (define/public (make-ai-move mc)
      (set! moveN (add1 moveN))
      ((lambda (lst)
        (let ([totTime (/ (third lst) 1000.0)])
          (display (string-append (number->string moveN) ") tiempo:\t" (number->string totTime) "s\t" (number->string (third lst)) "ms\n")))
        (send this paint-board-only-changes (send this get-current-game) (caar lst) mc))
      (call-with-values (lambda () (time-apply alpha-beta-search (list (send this get-current-game) "green" "red"))) list))
      )
    )

  )

(provide board%)