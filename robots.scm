;; The original code is very Common-Lispy, (ab)using loop and format.
;; In Gauche we lose the wackiness of the original code to some extent,
;; unfortunately.

(use scheme.list)
(use srfi.27) ; random-integer
(use srfi.42) ; This is sort of 'loop' equivalent in Scheme
(use util.match)

(define (robots)
  (let/cc return
    (let1 directions '((q . -65) (w . -64) (e . -63) (a . -1)
                       (d .   1) (z .  63) (x .  64) (c . 65))
      (do ([pos 544 (begin
                      (format #t "~%qwe/asd/zxc to move, (t)eleport, (l)eave:")
                      (flush)
                      (let* ([c (read)]
                             [d (assq-ref directions c)])
                        (cond [d (+ pos d)]
                              [(eq? 't c) (random-integer 1024)]
                              [(eq? 'l c) (return 'bye)]
                              [else pos])))]
           [monsters (list-ec (: i 10) (random-integer 1024))
                     (list-ec (: mpos monsters)
                              (if (> (count (cut eqv? mpos <>) monsters) 1)
                                mpos
                                (cdar (sort-by
                                       (list-ec (: kd directions)
                                                (match-let1 (k . d) kd
                                                  (let1 new-mpos (+ mpos d)
                                                    (cons (+ (abs (- (mod new-mpos 64)
                                                                     (mod pos 64)))
                                                             (abs (- (ash new-mpos -6)
                                                                     (ash pos -6))))
                                                          new-mpos))))
                                       car <))))])
          [(every (^[mpos] (> (count (cut eqv? mpos <>) monsters) 1)) monsters)
           'player-wins]
        ;; Gah.  Gauche's format doesn't (yet) support ~{ and ~<.
        (dotimes [p 1024]
          (when (zero? (mod p 64)) (display "|"))
          (cond [(memv p monsters)
                 (cond [(= p pos) (return 'player-loses)]
                       [(> (count (cut eqv? p <>) monsters) 1) (display "*")]
                       [else (display "#")])]
                [(= p pos) (display "@")]
                [else (display " ")])
          (when (zero? (mod (+ p 1) 64)) (display "|\n")))))))
