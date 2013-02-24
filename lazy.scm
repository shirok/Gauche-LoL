;; The features implemented in lazy.lisp is available in Gauche as
;; util.stream.  Here we only show the correspondence.

(use util.stream)

;; macro: lazy expr ...
;; == stream-delay (begin expr ...)

;; macro: lazy-cons a d
;; == stream-cons a d

;; function: lazy-car x
;; == stream-car x

;; function: lazy-cdr x
;; == stream-cdr x

;; function: lazy-nil
;; == (^[] stream-null)

;; function: lazy-null x
;; == stream-null? x

;; function: make-lazy lst
;; == list->stream lst

;; function: take n s
;; == stream->list (stream-take s n)

;; function: take-all s
;; == stream->list s

;; function: lazy-mapcar fun s
;; == stream-map fun s

;; function: lazy-mapcan fun s
;; == stream-concatenate (stream-map fun s)

;; function: lazy-find-if fun s
;; == stream-find fun s

;; function: lazy-nth n s
;; == stream-ref s n
