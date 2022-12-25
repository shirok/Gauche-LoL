;; This one isn't based on the code in Land of Lisp; instead, it is a
;; variation of the simple http server in "Programming Gauche".
;; Gauche already has various support libraries to handle basic stuff.
;;
;; For more complete embedded http server, see Gauche-makiki
;;  (https://github.com/shirok/Gauche-makiki )

(use gauche.net)
(use rfc.822)     ; rfc822-header->list
(use rfc.uri)     ; uri-parse
(use srfi.13)     ; string-trim
(use text.parse)  ; read-string
(use util.match)
(use www.cgi)     ; cgi-parse-parameters

(define (serve request-handler)
  (let1 server-sock (make-server-socket 'inet 8077 :reuse-addr? #t)
    (unwind-protect
        (let loop ([client (socket-accept server-sock)])
          (unwind-protect
              (if-let1 q (parse-request (socket-input-port client))
                (with-output-to-port (socket-output-port client)
                  (cut apply request-handler q)))
            (socket-close client))
          (loop (socket-accept server-sock)))
      (socket-close server-sock))))

;; Returns (path header params) or #f
(define (parse-request iport)
  (rxmatch-case (read-line iport)
    [#/^(GET|POST)\s+(\S+)\s+HTTP\/\d+\.\d+$/ (_ meth abs-path)
     (receive (scheme info host port path query frag) (uri-parse abs-path)
       (let* ([hdrs (map (^p (cons (string->symbol (car p))
                                   (x->string (cadr p))))
                         (rfc822-header->list iport))]
              [body (if-let1 p (assq-ref hdrs 'content-length)
                      (string-trim-both (read-string (x->integer p) iport))
                      "")]
              [params (map (^p (cons (string->symbol (car p))
                                     (x->string (cadr p))))
                           (cgi-parse-parameters :query-string (or query body)))])
         (list (string-trim path #[/]) ;; drop leading '/'
               hdrs params)))]
    [else #f]))

#|
;; sample
(define (hello-request-handler path header params)
  (if (equal? path "greeting")
    (if-let1 name (assoc 'name params)
      (format #t "Nice to meet you, ~a!" (cdr name))
      (print "<html><form>What is your name?<input name='name' /></form></html>"))
    (print "Sorry... I don't know that page.")))

(serve hello-request-handler)
|#
