(use gauche.sequence)
(use gauche.process)
(use srfi-1)

(define *wizard-nodes* '((living-room
                          (you are in the living-room.
                           a wizard is snoring loudly on the couch.))
                         (garden
                          (you are in a beautiful garden.
                           there is a well in front of you.))
                         (attic
                          (you are in the attic.
                           there is a giant welding torch in the corner.))))

(define *wizard-edges* '((living-room (garden west door)  
                                      (attic upstairs ladder))
                         (garden (living-room east door))
                         (attic (living-room downstairs ladder))))

(define (dot-name exp)
  (map-to <string>
          (^c (if (or (char-alphabetic? c) (char-numeric? c)) c #\_))
          (write-to-string exp display)))

(define *max-label-length* 30)

(define (dot-label exp)
  (if exp
    (let1 s (write-to-string exp)
      (if (> (string-length s) *max-label-length*)
        (string-append (subseq s 0 (- *max-label-length* 3)) "...")
        s))
    ""))

(define (nodes->dot nodes)
  (dolist [node nodes]
    (newline)
    (display (dot-name (car node)))
    (display "[label=\"")
    (display (dot-label node))
    (display "\"];")))

(define (edges->dot edges)
  (dolist [node edges]
    (dolist [edge (cdr node)]
      (newline)
      (display (dot-name (car node)))
      (display "->")
      (display (dot-name (car edge)))
      (display "[label=\"")
      (display (dot-label (cdr edge)))
      (display "\"];"))))

(define (graph->dot nodes edges)
  (display "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (display "}"))

(define (uedges->dot edges)
  (pair-for-each (^[lst]
                   (dolist [edge (cdar lst)]
                     (unless (assv (car edge) (cdr lst))
                       (newline)
                       (display (dot-name (caar lst)))
                       (display "--")
                       (display (dot-name (car edge)))
                       (display "[label=\"")
                       (display (dot-label (cdr edge)))
                       (display "\"];"))))
                 edges))
  
(define (ugraph->dot nodes edges)
  (display "graph{")
  (nodes->dot nodes)
  (uedges->dot edges)
  (display "}"))

(define (dot->png fname thunk)
  (let1 fn #`",|fname|.dot"
    (with-output-to-file fn thunk :if-exists :supersede)
    (run-process `("dot" "-Tpng" "-O" ,fn) :wait #t)))

(define (dgraph->png fname nodes edges)
  (dot->png fname (cut graph->dot nodes edges)))

(define (ugraph->png fname nodes edges)
  (dot->png fname (cut ugraph->dot nodes edges)))
