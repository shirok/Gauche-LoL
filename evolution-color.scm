;; This is a modification of evolution simulation, to display gene difference
;; by ANSI terminal color.

(use gauche.sequence)

(load "./evolution")

;; override draw-world
(define (draw-world)
  (dotimes [y *height*]
    (newline)
    (display "|")
    (dotimes [x *width*]
      (display (cond [(find (^[animal] (and (= (animal-x animal) x)
                                           (= (animal-y animal) y)))
                            *animals*)
                      => (^a (with-color #\M (animal-genes a)))]
                     [(hash-table-get *plants* (cons x y) #f) #\*]
                     [else #\space])))
    (display "|")))

(define *hue-vecs*
  '((255 0 0)
    (255 191 0)
    (128 255 0)
    (0 255 64)
    (0 255 255)
    (0 64 255)
    (128 0 255)
    (255 0 191)))

;; this is based on xterm colors (see http://en.wikipedia.org/wiki/ANSI_escape_code)
(define *terminal-colors*
  '(((0 0 0)   "30")
    ((205 0 0) "31")
    ((0 205 0) "32")
    ((205 205 0) "33")
    ((0 0 238)  "34")
    ((205 0 205) "35")
    ((0 205 205) "36")
    ((229 229 229) "37")
    ((127 127 127) "30;1")
    ((255 0 0) "31;1")
    ((0 255 0) "32;1")
    ((255 255 0) "33;1")
    ((92 92 255) "34;1")
    ((255 0 255) "35;1")
    ((0 255 255) "36;1")
    ((255 255 255) "37;1")))

(define (gene-color gene)
  (let1 factor (/. (apply + gene))
    (define (col picker)
      (clamp (reduce + 0 (map (^[c g] (*. (picker c) g factor))
                              *hue-vecs* gene))
             0 255))
    (list (col car) (col cadr) (col caddr))))

(define (find-closest-terminal-color color)
  (define (distance c1 c2)
    (apply + (map (^p (expt (- (p c1) (p c2)) 2)) (list car cadr caddr))))
  (find-min *terminal-colors* :key (^e (distance (car e) color))))

(define (with-color char gene)
  (let1 entry (find-closest-terminal-color (gene-color gene))
    (format "\u001b[~am~a\u001b[0m" (cadr entry) char)))

;; small spice; generate different patterns for every execution.
(random-source-randomize! default-random-source)
