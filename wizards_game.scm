(use srfi-13)  ; string-trim-both

(define *nodes* '((living-room (you are in the living-room.
                                    a wizard is snoring loudly on the couch.))
                  (garden (you are in a beautiful garden.
                               there is a well in front of you.))
                  (attic (you are in the attic.
                              there is a giant welding torch in the corner.))))

(define (describe-location location nodes)
  (cadr (assoc location nodes)))

(define *edges* '((living-room (garden west door)  
                               (attic upstairs ladder))
                  (garden (living-room east door))
                  (attic (living-room downstairs ladder))))

(define (describe-path edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(define (describe-paths location edges)
  (apply append (map describe-path (cdr (assoc location edges)))))

(define *objects* '(whiskey bucket frog chain))

(define *object-locations* '((whiskey living-room)
                             (bucket living-room)
                             (chain garden)
                             (frog garden)))

(define (objects-at loc objs obj-loc)
  (define (is-at obj)
    (eq? (cadr (assoc obj obj-loc)) loc))
  (filter is-at objs))

(define (describe-objects loc objs obj-loc)
  (define (describe-obj obj)
    `(you see a ,obj on the floor.))
  (apply append (map describe-obj (objects-at loc objs obj-loc))))

(define *location* 'living-room)

(define (look)
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

(define (walk direction)
  (define (correct-way edge)
    (eq? (cadr edge) direction))
  (if-let1 next (find correct-way (cdr (assoc *location* *edges*)))
    (begin (set! *location* (car next)) 
           (look))
    '(you cannot go that way.)))

(define (pickup object)
  (cond [(memv object (objects-at *location* *objects* *object-locations*))
         (push! *object-locations* (list object 'body))
         `(you are now carrying the ,object)]
        [else '(you cannot get that.)]))

(define (inventory)
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

(define (have object)
  (memv object (cdr (inventory))))

(define (game-repl)
  (let1 cmd (game-read)
    (unless (eq? (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

(define (game-read)
  ;; Gauche specific - Gauche's REPL reader (up to 0.9.4) doesn't consume
  ;; the newline after the input, so (game-read) immediately after invoking
  ;; (game-repl) reads the empty input, making cmd an empty list.  We just
  ;; loop in that case.  This issue is addressed in 0.9.5.
  (let1 cmd (read-from-string (string-append "(" (read-line) ")"))
    (define (quote-it x)
      (list 'quote x))
    (if (null? cmd)
      (game-read)
      (cons (car cmd) (map quote-it (cdr cmd))))))

(define *allowed-commands* '(look walk pickup inventory))

(define (game-eval sexp)
  (if (memv (car sexp) *allowed-commands*)
    (eval sexp (current-module)) ; Gauche specific
    '(i do not know that command.)))

(define (tweak-text lst caps lit)
  (if (null? lst)
    '()
    (let ([item (car lst)]
          [rest (cdr lst)])
      (cond [(eqv? item #\space) (cons item (tweak-text rest caps lit))]
            [(memv item '(#\! #\? #\.)) (cons item (tweak-text rest #t lit))]
            [(eqv? item #\") (tweak-text rest caps (not lit))]
            [lit (cons item (tweak-text rest #f lit))]
            [caps (cons (char-upcase item) (tweak-text rest #f lit))]
            [else (cons (char-downcase item) (tweak-text rest #f #f))]))))

(define (game-print lst)
  ($ display
     $ list->string
     $ tweak-text (string->list (string-trim-both (write-to-string lst) #[() ]))
                  #t #f)
  (newline))
