(require-extension medea udp6 socket fmt fmt-color)
(use medea udp6 socket fmt fmt-color)
(define (format-status status)
  (fmt #f "- " (dsp (symbol->string (car status))) " :: "  ((if (equal? (cdr status) "UP")
                                                                fmt-green
                                                                fmt-red) (dsp (cdr status)))))
(define (format-statuses statuses)
  (string-join (map format-status statuses) "\n"))
(define (up? status)
  (equal? (cdr status) "UP"))
(define (all-up? statuses)
  (not (any (lambda (x) (not (up? x))) statuses)))
(define (print-report ip statuses)
  (fmt #t
       ".: Status report for "
       (fmt-bold (dsp ip))
       " :."
       nl nl
       (dsp (format-statuses statuses))
       nl nl
       (if (all-up? statuses)
           (dsp "Looks like all checks have passed!")
           (dsp "It appears something may have gone wrong!"))
       nl))
(define (connect-port! ip port)
  (let ((s (udp-open-socket 'inet)))
    (udp-connect! s ip port)
    s))
(define (start-client ip)
  (let* ((port 1337) (s (connect-port! ip port)))
    ;; Setup named let
    (let loop ((bytes #f))
      (udp-sendto s ip port (if bytes "READY" "STATUS"))
      ;; Wait on data from client
      (receive (len str host port) (udp-recvfrom s (or bytes 10))
        ;; Run case on resulting string
        (cond 
         ;; If string is a valid number, assume it's the byte length being sent to us
         ((string->number str) (loop (string->number str)))
         ;; If client is read, send json data
         ((read-json str) (begin (print-report ip (read-json str))
                                 (exit)))
         (#t (print "Unexpected bytes from " host " " str)))
        ;; else loop if timeout is reached
        (loop cur-status)))
    (udp-close-socket s)))

(define (main args)
  (if (not (= (length args) 1))
      (print "Usage: client.scm [ip]")
      (start-client (car args))))
