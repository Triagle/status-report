(require-extension medea shell udp6 socket)
(use medea shell udp6 socket)

(define (get-json path)
  (with-input-from-file path
    (lambda ()
      (read-json (current-input-port)))))
(define (make-status test)
  (cons (car test) (string-trim-both (capture ,(cdr test)))))
(define (make-status-report tests)
  (map make-status tests))
(define (alist->json alist)
  (with-output-to-string (lambda ()
                           (write-json alist))))
(define (count-bytes string)
  (string-length string))
(define (bind-port! port)
  (let ((s (udp-open-socket 'inet6)))
    (udp-bind! s "::" port)
    s))
(define (server-start status-file)
  (let ((s (bind-port! 6060)))
    ;; No receive timeout
    (socket-receive-timeout #f)
    ;; Setup named let
    (let loop ((cur-status ""))
      ;; Wait on data from client
      (receive (len str host port) (udp-recvfrom s 8)
        ;; Run case on resulting string
        (cond 
         ;; If command is STATUS, prepare status report and return bytes
         ((equal? str "STATUS") (let ((status (alist->json (make-status-report (get-json status-file)))))
                                  (udp-sendto s host port (number->string (count-bytes status)))
                                  (loop status)))
         ;; If client is read, send json data
         ((equal? str "READY") (udp-sendto s host port cur-status))
         (#t (print "Unexpected bytes from " host " " str)))
        ;; else loop if timeout is reached
        (loop cur-status)))
    (udp-close-socket s)))
(define (main args)
  (if (not (= (length args) 1))
      (print "Usage: server.scm [config file path]")
      (server-start (car args))))
