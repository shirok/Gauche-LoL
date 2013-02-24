(use srfi-42)
 
(load "./dice_of_doom_v2.scm")
(load "./webserver.scm")
(load "./svg.scm")

(define *board-width* 900)
(define *board-height* 500)
(define *board-scale* 64)
(define *top-offset* 3)
(define *dice-scale* 40)
(define *dot-size* 0.05)

(define (draw-die-svg x y col)
  (define (calc-pt pt)
    (cons (+ x (* *dice-scale* (car pt)))
          (+ y (* *dice-scale* (cdr pt)))))
  (define (f pol col)
    (polygon (map calc-pt pol) col))
  (f '((0 . -1) (-0.6 . -0.75) (0 . -0.5) (0.6 . -0.75))
     (brightness col 40))
  (f '((0 . -0.5) (-0.6 . -0.75) (-0.6 . 0) (0 . 0.25))
     col)
  (f '((0 . -0.5) (0.6 . -0.75) (0.6 . 0) (0 . 0.25))
     (brightness col -40))
  (for-each (^[x y]
              (polygon (map (^[xx yy]
                              (calc-pt (cons (+ x (* xx *dot-size*))
                                             (+ y (* yy *dot-size*)))))
                            '(-1 -1 1 1)
                            '(-1 1 1 -1))
                       '(255 255 255)))
            '(-0.05 0.125 0.3 -0.3 -0.125 0.05 0.2 0.2 0.45 0.45 -0.45 -0.2)
            '(-0.875 -0.80 -0.725 -0.775 -0.70 -0.625
                     -0.35 -0.05 -0.45 -0.15 -0.45 -0.05)))

(define (draw-tile-svg x y pos hex xx yy col chosen-tile)
  (do-ec (: z 2)
         (polygon (map (^[pt]
                         (cons (+ xx (* *board-scale* (car pt)))
                               (+ yy (* *board-scale* 
                                        (+ (cdr pt) (* (- 1 z) 0.1))))))
                       '((-1 . -0.2) (0 . -0.5) (1 . -0.2) 
                         (1 . 0.2) (0 . 0.5) (-1 . 0.2)))
                  (if (eqv? pos chosen-tile)
                    (brightness col 100)
                    col)))
  (do-ec (: z (second hex))
         (draw-die-svg (+ xx
                          (* *dice-scale* 
                             0.3
                             (if (odd? (+ x y z))
                               -0.3
                               0.3)))
                       (- yy (* *dice-scale* z 0.8)) col)))

(define *die-colors* '((255 63 63) (63 63 255)))

(define (draw-board-svg board chosen-tile legal-tiles)
  (do-ec (: y *board-size*)
         (: x *board-size*)
         (let* ([pos (+ x (* *board-size* y))]
                [hex (~ board pos)]
                [xx (* *board-scale* (+ (* 2 x) (- *board-size* y)))]
                [yy (* *board-scale* (+ (* y 0.7) *top-offset*))]
                [col (brightness (~ *die-colors* (first hex))
                                 (* -15 (- *board-size* y)))])
           (if (or (memv pos legal-tiles) (eqv? pos chosen-tile))
             (tag g ()
                  (tag a ("xlink:href" (make-game-link pos))
                       (draw-tile-svg x y pos hex xx yy col chosen-tile)))
             (draw-tile-svg x y pos hex xx yy col chosen-tile)))))

(define (make-game-link pos)
  (format "/game.html?chosen=~a" pos))

(define *cur-game-tree* #f)
(define *from-tile* #f)

(define (dod-request-handler path header params)
  (if (equal? path "game.html")
    (begin (display "<!doctype html>")
           (tag center ()
                (display "Welcome to DICE OF DOOM!")
                (tag br ())
                (let1 chosen (assq 'chosen params)
                  (when (or (not *cur-game-tree*) (not chosen))
                    (set! chosen #f)
                    (web-initialize))
                  (cond [(stream-null? (caddr *cur-game-tree*)) 
                         (web-announce-winner (cadr *cur-game-tree*))]
                        [(zero? (car *cur-game-tree*)) 
                         (web-handle-human 
                          (if chosen
                            (read-from-string (cdr chosen))
                            #f))]
                        [else (web-handle-computer)]))
                (tag br ())
                (draw-dod-page *cur-game-tree* *from-tile*)))
    (display "Sorry... I don't know that page.")))

(define (web-initialize)
  (set! *from-tile* #f)
  (set! *cur-game-tree* (game-tree (gen-board) 0 0 #t)))

(define (web-announce-winner board)
  (newline)
  (let1 w (winners board)
    (if (> (length w) 1)
      (format #t "The game is a tie between ~a" (map player-letter w))
      (format #t "The winner is ~a" (player-letter (car w)))))
  (tag a (href "game.html")
       (display " play again")))

(define (web-handle-human pos)
  (cond [(or (not pos) (eq? pos 'nil))
         (display "Please choose a hex to move from:")]
        [(eq? pos 'pass)
         (set! *cur-game-tree* (cadr (stream-car (caddr *cur-game-tree*))))
         (display "Your reinforcements have been placed.")
         (tag a (href (make-game-link 'nil))
              (display "continue"))]
        [(not *from-tile*) (set! *from-tile* pos)
                           (display "Now choose a destination:")]
        [(eq? pos *from-tile*) (set! *from-tile* #f)
                               (display "Move cancelled.")]
        [else (set! *cur-game-tree*
                    (cadr (stream-find (^[move]
                                         (equal? (car move) 
                                                 (list *from-tile* pos)))
                                       (caddr *cur-game-tree*))))
              (set! *from-tile* #f)
              (display "You may now ")
              (tag a (href (make-game-link 'pass))
                   (display "pass"))
              (display " or make another move:")]))

(define (web-handle-computer)
  (set! *cur-game-tree* (handle-computer *cur-game-tree*))
  (display "The computer has moved. ")
  (tag script ()
       (display
        "window.setTimeout('window.location=\"game.html?chosen=nil\"',5000)")))

(define (draw-dod-page tree selected-tile)
  (svg *board-width* 
       *board-height*
       (draw-board-svg (cadr tree) 
                       selected-tile 
                       (stream->list
                        (if selected-tile
                          (stream-map (^[move]
                                        (if (and (car move)
                                                 (eqv? (caar move)
                                                       selected-tile))
                                          (cadar move)
                                          #f))
                                      (caddr tree))
                          (stream-map (^[move] (and (car move) (caar move)))
                                      (caddr tree)))))))

#|
;; To begin the game...
(serve dod-request-handler)
|#
