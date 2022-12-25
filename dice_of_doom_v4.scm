(use srfi.27)
(use srfi.42)

(load "./dice_of_doom_v3.scm")

(define *num-players* 4)
(define *die-colors* '((255 63 63) (63 63 255) (63 255 63) (255 63 255)))

(define *max-dice* 5)
(define *ai-level* 2)

(define (attacking-moves board cur-player spare-dice)
  (define (player pos) (car (~ board pos)))
  (define (dice pos) (cadr (~ board pos)))
  (stream-concatenate
   (stream-map (^[src]
                 (if (eq? (player src) cur-player)
                   (stream-concatenate
                    (stream-map
                     (^[dst]
                       (if (and (not (eq? (player dst) cur-player))
                                (> (dice src) 1))
                        (list->stream (list (list (list src dst)
                                                  (game-tree (board-attack board cur-player src dst (dice src))
                                                             cur-player
                                                             (+ spare-dice (dice dst))
                                                             #f)
                                                  (game-tree (board-attack-fail board cur-player src dst (dice src))
                                                             cur-player
                                                             (+ spare-dice (dice dst))
                                                             #f))))
                        stream-null))
                     (list->stream (neighbors src))))
                   stream-null))
               (stream-iota *board-hexnum*))))

(define (board-attack-fail board player src dst dice)
  (board-array (list-ec (: hex (index pos) board)
                        (if (eq? pos src)
                          (list player 1)
                          hex))))

(define (roll-dice dice-num)
  (rlet1 total (sum-ec (: k dice-num) (+ (random-integer 6) 1))
    (newline)
    (format #t "On ~a dice rolled ~a. " dice-num total)))

(define (roll-against src-dice dst-dice)
  (> (roll-dice src-dice) (roll-dice dst-dice)))

(define (pick-chance-branch board move)
  (define (dice pos) (cadr (~ board pos)))
  (let1 path (car move)
    (if (or (not path) (roll-against (dice (car path))
                                     (dice (cadr path))))
      (cadr move)
      (caddr move))))

(define (handle-human tree)
  (newline)
  (display "choose your move:")
  (let1 moves (caddr tree)
    (define (print-moves moves n)
      (unless (stream-null? moves)
        (let* ([move (stream-car moves)]
               [action (and move (car move))])
          (newline)
          (format #t "~a. " n)
          (if action
            (format #t "~a -> ~a" (car action) (cadr action))
            (display "end turn")))
        (print-moves (stream-cdr moves) (+ n 1))))
    (print-moves moves 1)
    (newline)
    (pick-chance-branch (cadr tree) (stream-ref moves (- (read) 1)))))

(define (handle-computer tree)
  (let1 ratings (get-ratings (limit-tree-depth tree *ai-level*) (car tree))
    (pick-chance-branch
     (cadr tree)
     (stream-ref (caddr tree)
                 (list-index (cute eq? (apply max ratings) <>) ratings)))))

(define *dice-odds* #(#(0.84 0.97 1.0 1.0)
                      #(0.44 0.78 0.94 0.99)
                      #(0.15 0.45 0.74 0.91)
                      #(0.04 0.19 0.46 0.72)
                      #(0.01 0.06 0.22 0.46)))

(define (get-ratings tree player)
  (let1 board (cadr tree)
    (define (dice pos) (cadr (~ board pos)))
    (stream->list (stream-map
                   (^[move]
                     (let1 path (car move)
                       (if path
                         (let* ([src (car path)]
                                [dst (cadr path)]
                                [odds (~ *dice-odds*
                                         (- (dice dst) 1)
                                         (- (dice src) 2))])
                           (+ (* odds (rate-position (cadr move) player))
                              (* (- 1 odds) (rate-position (caddr move)
                                                           player))))
                         (rate-position (cadr move) player))))
                   (caddr tree)))))

(define (limit-tree-depth tree depth)
  (list (car tree)
        (cadr tree)
        (if (zero? depth)
          stream-null
          (stream-map (^[move]
                        (cons (car move)
                              (map (^x (limit-tree-depth x (- depth 1)))
                                   (cdr move))))
                      (caddr tree)))))

(define (get-connected board player pos)
  (define (check-pos pos visited)
    (if (and (eq? (car (~ board pos)) player)
             (not (memv pos visited)))
      (check-neighbors (neighbors pos) (cons pos visited))
      visited))
  (define (check-neighbors lst visited)
    (if (not (null? lst))
      (check-neighbors (cdr lst) (check-pos (car lst) visited))
      visited))
  (check-pos pos '()))


(define (largest-cluster-size board player)
  (define (f pos visited best)
    (if (< pos *board-hexnum*)
      (if (and (eq? (car (~ board pos)) player)
               (not (memv pos visited)))
        (let* ([cluster (get-connected board player pos)]
               [size (length cluster)])
          (if (> size best)
            (f (+ pos 1) (append cluster visited) size)
            (f (+ pos 1) (append cluster visited) best)))
        (f (+ pos 1) visited best))
      best))
  (f 0 '() 0))

(define (add-new-dice board player spare-dice)
  (define (f lst n)
    (cond [(zero? n) lst]
          [(null? lst) '()]
          [else (let ([cur-player (caar lst)]
                      [cur-dice (cadar lst)])
                  (if (and (eq? cur-player player) (< cur-dice *max-dice*))
                    (cons (list cur-player (+ cur-dice 1))
                          (f (cdr lst) (- n 1)))
                    (cons (car lst) (f (cdr lst) n))))]))
  (board-array (f (vector->list board)
                  (largest-cluster-size board player))))

#|
;; To begin the game...
(serve dod-request-handler)
|#
