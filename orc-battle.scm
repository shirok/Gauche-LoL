(use gauche.record)
(use gauche.sequence)
(use srfi.27) ; random-integer
(use srfi.42) ; vector-ec
(use srfi.43) ; vector-every

(define *player-health* 0)
(define *player-agility* 0)
(define *player-strength* 0)
(define *monsters* '())
(define *monster-builders* '())
(define *monster-num* 12)

(define (orc-battle)
  (init-monsters)
  (init-player)
  (game-loop)
  (when (player-dead)
    (print "You have been killed. Game Over."))
  (when (monsters-dead)
    (print "Congratulations! You have vanquished all of your foes.")))

(define (game-loop)
  (unless (or (player-dead) (monsters-dead))
    (show-player)
    (dotimes [k (+ 1 (truncate (/ (max 0 *player-agility*) 15)))]
      (unless (monsters-dead)
        (show-monsters)
        (player-attack)))
    (newline)
    (for-each (^m (or (monster-dead m) (monster-attack m))) *monsters*)
    (game-loop)))

(define (init-player)
  (set! *player-health* 30)
  (set! *player-agility* 30)
  (set! *player-strength* 30))

(define (player-dead)
  (<= *player-health* 0))

(define (show-player)
  (newline)
  (display "You are a valiant knight with a health of ")
  (display *player-health*)
  (display ", an agility of ")
  (display *player-agility*)
  (display ", and a strength of ")
  (display *player-strength*))

(define (player-attack)
  (newline)
  (display "Attack style: [s]tab [d]ouble swing [r]oundhouse:")
  (flush)
  (case (read)
    [(s) (monster-hit (pick-monster)
                      (+ 2 (randval (ash *player-strength* -1))))]
    [(d) (let1 x (randval (truncate (/ *player-strength* 6)))
           (display "Your double swing has a strength of ")
           (display x)
           (newline)
           (monster-hit (pick-monster) x)
           (unless (monsters-dead)
             (monster-hit (pick-monster) x)))]
    [else (dotimes [x (+ 1 (randval (truncate (/ *player-strength* 3))))]
            (unless (monsters-dead)
              (monster-hit (random-monster) 1)))]))

(define (randval n)
  (+ 1 (random-integer (max 1 n))))

(define (random-monster)
  (let1 m (~ *monsters* (random-integer (size-of *monsters*)))
    (if (monster-dead m)
      (random-monster)
      m)))

(define (pick-monster)
  (newline)
  (display "Monster #:")
  (let1 x (read)
    (if (not (and (integer? x) (>= x 1) (<= x *monster-num*)))
      (begin (display "That is not a valid monster number.")
             (pick-monster))
      (let1 m (~ *monsters* (- x 1))
        (if (monster-dead m)
          (begin (display "That monster is alread dead.")
                 (pick-monster))
          m)))))

(define (init-monsters)
  (set! *monsters*
        (vector-ec (: i *monster-num*)
                   ((~ *monster-builders*
                       (random-integer (size-of *monster-builders*)))))))

(define (monster-dead m)
  (<= (monster-health m) 0))

(define (monsters-dead)
  (vector-every monster-dead *monsters*))

(define (show-monsters)
  (newline)
  (display "Your foes:")
  (for-each-with-index (^[x m]
                         (newline)
                         (display "   ")
                         (display (+ x 1))
                         (display ". ")
                         (if (monster-dead m)
                           (display "**dead**")
                           (begin (display "(Health=")
                                  (display (monster-health m))
                                  (display ") ")
                                  (monster-show m))))
                       *monsters*))

(define-record-type monster #t #t (health))

(define-method monster-hit (m x)
  (dec! (monster-health m) x)
  (if (monster-dead m)
    (begin (display "You killed the ")
           (display (class-name (class-of m)))
           (display "! "))
    (begin (display "You hit the ")
           (display (class-name (class-of m)))
           (display ", knocking off ")
           (display x)
           (display " health points! "))))

(define-method monster-show (m)
  (display "A fierce ")
  (display (class-name (class-of m))))

(define-method monster-attack (m))

(define-record-type (orc monster) #t #t (club-level))

(push! *monster-builders* (^[] (make-orc (randval 10) (randval 8))))

(define-method monster-show ((m orc))
  (display "A wicked orc with a level ")
  (display (orc-club-level m))
  (display " club"))

(define-method monster-attack ((m orc))
  (let1 x (randval (orc-club-level m))
    (display "An orc swings his club at you and knocks off ")
    (display x)
    (display " of your health points. ")
    (dec! *player-health* x)))

(define-record-type (hydra monster) #t #t)

(push! *monster-builders* (^[] (make-hydra (randval 10))))

(define-method monster-show ((m hydra))
  (display "A malicious hydra with ")
  (display (monster-health m))
  (display " heads."))

(define-method monster-hit ((m hydra) x)
  (dec! (monster-health m) x)
  (if (monster-dead m)
    (display "The corpse of the fully decapitated and decapacitated hydra falls to the floor!")
    (begin (display "You lop off ")
           (display x)
           (display " of the hydra's heads! "))))

(define-method monster-attack ((m hydra))
  (let1 x (randval (ash (monster-health m) -1))
    (display "A hydra attacks you with ")
    (display x)
    (display " of its heads! It also grows back one more head! ")
    (inc! (monster-health m))
    (dec! *player-health* x)))

(define-record-type (slime-mold monster) #t #t (sliminess))

(push! *monster-builders* (^[] (make-slime-mold (randval 10) (randval 5))))

(define-method monster-show ((m slime-mold))
  (display "A slime mold with a sliminess of ")
  (display (slime-mold-sliminess m)))

(define-method monster-attack ((m slime-mold))
  (let1 x (randval (slime-mold-sliminess m))
    (display "A slime mold wraps around your legs and decreases your agility
by ")
    (display x)
    (display "! ")
    (dec! *player-agility* x)
    (when (zero? (random-integer 2))
      (display "It also squirts in your face, taking away a health point! ")
      (dec! *player-health*))))

(define-record-type (brigand monster) #t #t)

(push! *monster-builders* (^[] (make-brigand (randval 10))))

(define-method monster-attack ((m brigand))
  (let1 x (max *player-health* *player-agility* *player-strength*)
    (cond [(= x *player-health*)
           (display "A brigand hits you with his slingshot, taking off 2 health points! ")
           (dec! *player-health* 2)]
          [(= x *player-agility*)
           (display "A brigand catches your leg with his whip, taking off 2
agility points! ")
           (dec! *player-agility* 2)]
          [(= x *player-strength*)
           (display "A brigand cuts your arm with his whip, taking off 2 strength points! ")
           (dec! *player-strength* 2)])))
