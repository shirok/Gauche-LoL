(use srfi-1)   ;lset-intersection etc.
(use srfi-27)  ;random-integer
(use srfi-42)  ;list-ec etc.
(load "./graph-util")


(define *congestion-city-nodes* '())
(define *congestion-city-edges* '())
(define *visited-nodes* '())
(define *node-num* 30)
(define *edge-num* 45)
(define *worm-num* 3)
(define *cop-odds* 15)
(define *player-pos* 1)

(define (random-node)
  (+ 1 (random-integer *node-num*)))

(define (edge-pair a b)
  (if (eqv? a b)
    '()
    (list (cons a b) (cons b a))))

(define (make-edge-list)
  (append-ec (: i *edge-num*) (edge-pair (random-node) (random-node))))

(define (direct-edges node edge-list)
  (filter (^x (eqv? (car x) node)) edge-list))

(define (get-connected node edge-list)
  (rlet1 visited '()
    (define (traverse node)
      (unless (member node visited)
        (push! visited node)
        (for-each ($ traverse $ cdr $) (direct-edges node edge-list))))
    (traverse node)))


(define (connect-with-bridges islands)
  (if (null? (cdr islands))
    '()
    (append (edge-pair (caar islands) (caadr islands))
            (connect-with-bridges (cdr islands)))))

(define (find-islands nodes edge-list)
  (rlet1 islands '()
    (define (find-island nodes)
      (when (pair? nodes)
        (let* ([connected (get-connected (car nodes) edge-list)]
               [unconnected (lset-difference eqv? nodes connected)])
          (push! islands connected)
          (when unconnected
            (find-island unconnected)))))
    (find-island nodes)))

(define (connect-all-islands nodes edge-list)
  (append (connect-with-bridges (find-islands nodes edge-list)) edge-list))

(define (edges-to-alist edge-list)
  (map (^[node1]
         (cons node1
               (map (^[edge] (list (cdr edge)))
                    (delete-duplicates (direct-edges node1 edge-list) equal?))))
       (delete-duplicates (map car edge-list))))

(define (add-cops edge-alist edges-with-cops)
  (map (^x (let ([node1 (car x)]
                 [node1-edges (cdr x)])
             (cons node1
                   (map (^[edge]
                          (let1 node2 (car edge)
                            (if (null? (lset-intersection equal?
                                                          (edge-pair node1 node2)
                                                          edges-with-cops))
                              edge
                              (list node2 'cops))))
                        node1-edges))))
       edge-alist))

(define (make-city-edges)
  (let* ([nodes (iota *node-num* 1)]
         [edge-list (connect-all-islands nodes (make-edge-list))]
         [cops (filter (^_ (zero? (random-integer *cop-odds*))) edge-list)])
    (add-cops (edges-to-alist edge-list) cops)))

(define (neighbors node edge-alist)
  (map car (assv-ref edge-alist node '())))

(define (within-one a b edge-alist)
  (memv b (neighbors a edge-alist)))

(define (within-two a b edge-alist)
  (or (within-one a b edge-alist)
      (any (cut within-one <> b edge-alist)
           (neighbors a edge-alist))))

(define (make-city-nodes edge-alist)
  (let ([wumpus (random-node)]
        [glow-worms (list-ec (: i *worm-num*) (random-node))])
    (list-ec (: n 1 (+ 1 *node-num*))
             (append (list n)
                     (cond [(eqv? n wumpus) '(wumpus)]
                           [(within-two n wumpus edge-alist) '(blood!)]
                           [else '()])
                     (cond [(memv n glow-worms) '(glow-worm)]
                           [(any (cut within-one n <> edge-alist) glow-worms)
                            '(lights!)]
                           [else '()])
                     (if (any (^e (not (null? (cdr e))))
                              (assv-ref edge-alist n '()))
                       '(sirens!)
                       '())))))

#|
;; provisional version.  see below.
(define (new-game)
  (set! *congestion-city-edges* (make-city-edges))
  (set! *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (set! *player-pos* (find-empty-node))
  (set! *visited-nodes* (list *player-pos*))
  (draw-city))
|#

;; The original implementation of find-empty-node is very inefficient when
;; most of nodes are not empty.
;; (define (find-empty-node)
;;   (let1 x (random-node)
;;     (if (assv-ref *congestion-city-nodes* x)
;;       (find-empty-node)
;;       x)))
(define (find-empty-node)
  (let1 empty-nodes (filter (^n (null? (cdr n))) *congestion-city-nodes*)
    (when (null? empty-nodes)
      (error "City is too congested.  Try (new-game) again."))
    (car (ref empty-nodes (random-integer (length empty-nodes))))))

(define (draw-city)
  (ugraph->png "city" *congestion-city-nodes* *congestion-city-edges*))

(define (known-city-nodes)
  (map (^[node]
         (if (memv node *visited-nodes*)
           (let1 n (assv node *congestion-city-nodes*)
             (if (eqv? node *player-pos*)
               (append n '(*))
               n))
           (list node '?)))
       (delete-duplicates 
        (append *visited-nodes*
                (append-map (cut neighbors <> *congestion-city-edges*)
                            *visited-nodes*)))))

(define (known-city-edges)
  (map (^[node]
         (cons node (map (^x (if (memv (car x) *visited-nodes*)
                               x
                               (list (car x))))
                         (cdr (assv node *congestion-city-edges*)))))
       *visited-nodes*))

(define (draw-known-city)
  (ugraph->png "known-city" (known-city-nodes) (known-city-edges)))

(define (new-game)
  (set! *congestion-city-edges* (make-city-edges))
  (set! *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (set! *player-pos* (find-empty-node))
  (set! *visited-nodes* (list *player-pos*))
  (draw-city)
  (draw-known-city))

(define (walk pos)
  (handle-direction pos #f))

(define (charge pos)
  (handle-direction pos #t))

(define (handle-direction pos charging)
  (if-let1 edge (assv pos (cdr (assv *player-pos* *congestion-city-edges*)))
    (handle-new-place edge pos charging)
    (display "That location does not exist!")))

(define (handle-new-place edge pos charging)
  (let* ([node (assv pos *congestion-city-nodes*)]
         [has-worm (and (memv 'glow-worm node)
                        (not (memv pos *visited-nodes*)))])
    (unless (memv pos *visited-nodes*) (push! *visited-nodes* pos))
    (set! *player-pos* pos)
    (draw-known-city)
    (cond [(memq 'cops edge) (display "You ran into the cops. Game Over.")]
          [(memq 'wumpus node) (if charging
                                 (display "You found the Wumpus!")
                                 (display "You ran into the Wumpus"))]
          [charging (display "You wasted your last bullet. Game Over.")]
          [has-worm (let1 new-pos (random-node)
                      (display "You ran into a Glow Worm Gang! You're now at ")
                      (display new-pos)
                      (handle-new-place '() new-pos #f))])))

