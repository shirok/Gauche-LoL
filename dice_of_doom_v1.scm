(use gauche.collection) ; find-max
(use scheme.list)       ; first, second, etc.
(use srfi.27)           ; random-integer
(use srfi.42)           ; list-ec, do-ec


(define *num-players* 2)
(define *max-dice* 3)
(define *board-size* 2)
(define *board-hexnum* (* *board-size* *board-size*))

(define (board-array lst)
  (list->vector lst))

(define (gen-board)
  (board-array (list-ec (: n *board-hexnum*)
                        (list (random-integer *num-players*)
                              (+ 1 (random-integer *max-dice*))))))

(define (player-letter n)
  (integer->digit n))

(define (draw-board board)
  (do-ec (: y *board-size*)
         (begin
           (newline)
           (do-ec (: k (- *board-size* y))
                  (display "  "))
           (do-ec (: x *board-size*)
                  (:let hex (~ board (+ x (* *board-size* y))))
                  (format #t "~a-~a "
                          (player-letter (first hex))
                          (second hex))))))

(define (game-tree board player spare-dice first-move)
  (list player
        board
        (add-passing-move board
                          player
                          spare-dice
                          first-move
                          (attacking-moves board player spare-dice))))

(define (add-passing-move board player spare-dice first-move moves)
  (if first-move
    moves
    (cons (list #f
                (game-tree (add-new-dice board player (- spare-dice 1))
                           (mod (+ player 1) *num-players*)
                           0
                           #t))
          moves)))

(define (attacking-moves board cur-player spare-dice)
  (define (player pos)
    (car (~ board pos)))
  (define (dice pos)
    (cadr (~ board pos)))
  (append-ec (: src *board-hexnum*)
             (if (eq? (player src) cur-player)
               (append-ec (: dst (neighbors src))
                          (if (and (not (eq? (player dst) cur-player))
                                   (> (dice src) (dice dst)))
                            `(((,src ,dst)
                               ,(game-tree (board-attack board cur-player src dst (dice src))
                                           cur-player
                                           (+ spare-dice (dice dst))
                                           #f)))
                            '()))
               '())))

(define (neighbors pos)
  (let ([up (- pos *board-size*)]
        [down (+ pos *board-size*)])
    (list-ec (: p (cond-list [#t @ (list up down)]
                             [(not (zero? (mod pos *board-size*)))
                              @ (list (- up 1) (- pos 1))]
                             [(not (zero? (mod (+ pos 1) *board-size*)))
                              @ (list (+ pos 1) (+ down 1))]))
             (if (and (>= p 0) (< p *board-hexnum*)))
             p)))

(define (board-attack board player src dst dice)
  (board-array (list-ec (: hex (index pos) board)
                        (cond [(eq? pos src) (list player 1)]
                              [(eq? pos dst) (list player (- dice 1))]
                              [else hex]))))

(define (add-new-dice board player spare-dice)
  (define (f lst n)
    (cond [(null? lst) '()]
          [(zero? n) lst]
          [else (let ([cur-player (caar lst)]
                      [cur-dice (cadar lst)])
                  (if (and (eq? cur-player player) (< cur-dice *max-dice*))
                    (cons (list cur-player (+ cur-dice 1))
                          (f (cdr lst) (- n 1)))
                    (cons (car lst) (f (cdr lst) n))))]))
  (board-array (f (vector->list board) spare-dice)))

(define (play-vs-human tree)
  (print-info tree)
  (if (not (null? (caddr tree)))
    (play-vs-human (handle-human tree))
    (announce-winner (cadr tree))))

(define (print-info tree)
  (newline)
  (format #t "current player = ~a" (player-letter (car tree)))
  (draw-board (cadr tree)))

(define (handle-human tree)
  (newline)
  (display "choose your move:")
  (flush)
  (let1 moves (caddr tree)
    (do-ec (: move (index n) moves)
           (let1 action (car move)
             (newline)
             (format #t "~a. " (+ n 1))
             (if action
               (format #t "~a -> ~a" (car action) (cadr action))
               (display "end turn"))))
    (newline)
    (cadr (~ moves (- (read) 1)))))

(define (winners board)
  (let* ([tally (list-ec (: hex board) (car hex))]
         [totals (map (^[player]
                        (cons player (count (cut eq? player <>) tally)))
                      (delete-duplicates tally))]
         [best (apply max (map cdr totals))])
    (map car (remove (^x (not (eq? (cdr x) best))) totals))))

(define (announce-winner board)
  (newline)
  (let1 w (winners board)
    (if (> (length w) 1)
      (format #t "The game is a tie between ~a" (map player-letter w))
      (format #t "The winner is ~a" (player-letter (car w))))))

#|
;To play against a human:
(play-vs-human (game-tree (gen-board) 0 0 #t))
|#

;The code below adds the AI player

(define (rate-position tree player)
  (let1 moves (caddr tree)
    (if (not (null? moves))
      (apply (if (eq? (car tree) player)
               max
               min)
             (get-ratings tree player))
      (let1 w (winners (cadr tree))
        (if (memv player w)
          (/ 1 (length w))
          0)))))

(define (get-ratings tree player)
  (map (^[move] (rate-position (cadr move) player))
       (caddr tree)))

(define (handle-computer tree)
  (let1 ratings (get-ratings tree (car tree))
    (cadr (~ (caddr tree)
             (list-index (cute eqv? (apply max ratings) <>) ratings)))))

(define (play-vs-computer tree)
  (print-info tree)
  (cond [(null? (caddr tree)) (announce-winner (cadr tree))]
        [(zero? (car tree)) (play-vs-computer (handle-human tree))]
        [else (play-vs-computer (handle-computer tree))]))

#|
;To play against the computer:
(play-vs-computer (game-tree (gen-board) 0 0 #t))
|#

;The code below optimizes the game and allows play on a 3x3 board

(define *board-size* 3)
(define *board-hexnum* (* *board-size* *board-size*))

(let ([old-neighbors neighbors]
      [cache (make-hash-table 'eqv?)])
  (set! neighbors
        (^[pos]
          (or (hash-table-get cache pos #f)
              (rlet1 v (old-neighbors pos)
                (hash-table-put! cache pos v))))))

(let ([old-game-tree game-tree]
      [cache (make-hash-table 'equal?)])
  (set! game-tree
        (^ rest
          (or (hash-table-get cache rest #f)
              (rlet1 v (apply old-game-tree rest)
                (hash-table-put! cache rest v))))))

(let ([old-rate-position rate-position]
      [cache (make-hash-table 'eqv?)])
  (set! rate-position
        (^[tree player]
          (let1 tab (hash-table-get cache player #f)
            (unless tab
              (set! tab (make-hash-table 'eqv?))
              (hash-table-put! cache player tab))
            (or (hash-table-get tab tree #f)
                (rlet1 v (old-rate-position tree player)
                  (hash-table-put! tab tree v)))))))

(define (add-new-dice board player spare-dice)
  (define (f lst n acc)
    (cond [(zero? n) (append (reverse acc) lst)]
          [(null? lst) (reverse acc)]
          [else (let ([cur-player (caar lst)]
                      [cur-dice (cadar lst)])
                  (if (and (eq? cur-player player)
                           (< cur-dice *max-dice*))
                    (f (cdr lst)
                       (- n 1)
                       (cons (list cur-player (+ cur-dice 1)) acc))
                    (f (cdr lst) n (cons (car lst) acc))))]))
  (board-array (f (vector->list board) spare-dice '())))
