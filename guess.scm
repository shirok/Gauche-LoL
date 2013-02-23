;; ch2.  let the computer guess your number.

(define *small* 1)
(define *big* 100)

(define (guess-my-number)
  (ash (+ *small* *big*) -1))

(define (smaller)
  (set! *big* (- (guess-my-number) 1))
  (guess-my-number))

(define (bigger)
  (set! *small* (+ (guess-my-number) 1))
  (guess-my-number))

(define (start-over)
  (set! *small* 1)
  (set! *big* 100)
  (guess-my-number))
