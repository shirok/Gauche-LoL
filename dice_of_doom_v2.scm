(use srfi-42)

(load "./dice_of_doom_v1.scm")
(load "./lazy.scm")

(define *board-size* 4)
(define *board-hexnum* (* *board-size* *board-size*))

(define (add-passing-move board player spare-dice first-move moves)
  (if first-move 
    moves
    (stream-cons (list #f
                       (game-tree (add-new-dice board player 
                                                (- spare-dice 1))
                                  (mod (+ player 1) *num-players*)
                                  0
                                  #t))
                 moves)))

(define (attacking-moves board cur-player spare-dice)
  (define (player pos) (car (~ board pos)))
  (define (dice pos) (cadr (~ board pos)))
  (stream-concatenate
   (stream-map
    (^[src]
      (if (eq? (player src) cur-player)
        (stream-concatenate
         (stream-map
          (^[dst]
            (if (and (not (eq? (player dst)
                               cur-player)) 
                     (> (dice src) (dice dst)))
              (list->stream
               (list (list (list src dst)
                           (game-tree (board-attack board
                                                    cur-player
                                                    src
                                                    dst
                                                    (dice src))
                                      cur-player
                                      (+ spare-dice (dice dst))
                                      #f))))
              stream-null))
          (list->stream (neighbors src))))
        stream-null))
    (list->stream (iota *board-hexnum*)))))

(define (handle-human tree)
  (newline)
  (display "choose your move:")
  (let1 moves (caddr tree)
    (define (print-moves moves n)
      (unless (stream-null? moves)
        (let* ([move (stream-car moves)]
               [action (car move)])
          (newline)
          (format #t "~a. " n)
          (if action
            (format #t "~a -> ~a" (car action) (cadr action))
            (display "end turn")))
        (print-moves (stream-cdr moves) (+ n 1))))
    (print-moves moves 1)
    (newline)
    (cadr (stream-ref moves (- (read) 1)))))

(define (play-vs-human tree)
  (print-info tree)
  (if (not (stream-null? (caddr tree)))
    (play-vs-human (handle-human tree))
    (announce-winner (cadr tree))))

#|
;At this point, you can play a game vs a human with:
(play-vs-human (game-tree (gen-board) 0 0 #t))
|#

;Now we start writing improvements for the AI...

(define (limit-tree-depth tree depth)
  (list (car tree) 
	(cadr tree) 
	(if (zero? depth)
          stream-null
	  (stream-map (^[move]
                        (list (car move) 
                              (limit-tree-depth (cadr move) (- depth 1))))
                      (caddr tree)))))

(define *ai-level* 4)

(define (handle-computer tree)
  (let1 ratings (get-ratings (limit-tree-depth tree *ai-level*)
                             (car tree))
    (cadr (stream-ref (caddr tree)
                      (list-index (cute eq? (apply max ratings) <>) ratings)))))

(define (play-vs-computer tree)
  (print-info tree)
  (cond [(stream-null? (caddr tree)) (announce-winner (cadr tree))]
        [(zero? (car tree)) (play-vs-computer (handle-human tree))]
        [else (play-vs-computer (handle-computer tree))]))


(define (score-board board player)
  (sum-ec (: hex (index pos) board)
          (if (eq? (car hex) player)
            (if (threatened pos board)
              1
              2)
            -1)))

(define (threatened pos board)
  (let* ([hex (~ board pos)]
         [player (car hex)]
         [dice (cadr hex)])
    (any?-ec (: n (neighbors pos))
             (let* ([nhex (~ board n)]
                    [nplayer (car nhex)]
                    [ndice (cadr nhex)])
               (and (not (eq? player nplayer)) (> ndice dice))))))

(define (get-ratings tree player)
  (stream->list (stream-map (^[move] (rate-position (cadr move) player))
                            (caddr tree))))

(define (rate-position tree player)
  (let1 moves (caddr tree)
    (if (not (stream-null? moves))
      (apply (if (eq? (car tree) player)
               max
               min)
             (get-ratings tree player))
      (score-board (cadr tree) player))))

#|
;You can now play a game against the computer AI:
;
(play-vs-computer (game-tree (gen-board) 0 0 #t))
|#

;The rest of this file implements ab pruning.

(define (ab-get-ratings-max tree player upper-limit lower-limit)
  (define (f moves lower-limit)
    (if (stream-null? moves)
      '()
      (let1 x (ab-rate-position (cadr (stream-car moves))
                                player
                                upper-limit
                                lower-limit)
        (if (>= x upper-limit)
          (list x)
          (cons x (f (stream-cdr moves) (max x lower-limit)))))))
  (f (caddr tree) lower-limit))

(define (ab-get-ratings-min tree player upper-limit lower-limit)
  (define (f moves upper-limit)
    (if (stream-null? moves)
      '()
      (let1 x (ab-rate-position (cadr (stream-car moves))
                                player
                                upper-limit
                                lower-limit)
        (if (<= x lower-limit)
          (list x)
          (cons x (f (stream-cdr moves) (min x upper-limit)))))))
  (f (caddr tree) upper-limit))

(define (ab-rate-position tree player upper-limit lower-limit)
  (let1 moves (caddr tree)
    (if (not (stream-null? moves))
      (if (eq? (car tree) player)
        (apply max (ab-get-ratings-max tree
                                       player
                                       upper-limit
                                       lower-limit))
        (apply min (ab-get-ratings-min tree
                                       player
                                       upper-limit
                                       lower-limit)))
      (score-board (cadr tree) player))))

(define (handle-computer tree)
  (let1 ratings (ab-get-ratings-max (limit-tree-depth tree *ai-level*)
                                    (car tree)
                                    (greatest-fixnum)
                                    (least-fixnum))
    (cadr (stream-ref (caddr tree)
                      (list-index (cute eq? (apply max ratings) <>) ratings)))))

(define *board-size* 5)
(define *board-hexnum* (* *board-size* *board-size*))

#|
(play-vs-computer (game-tree (gen-board) 0 0 #t))
|#