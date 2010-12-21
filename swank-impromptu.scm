(sys:clear-log-view)

;;; Utility Functions
(define hex-digit-values '(0 1 2 3 4 5 6 7 8 9 a b c d e f))

(define hexstring-conversion-accum 
   (lambda (val index digits) 
      (if (null? digits)
         (real->integer val)
        (let ((digit-val (list-position (string->atom (string (car digits))) hex-digit-values)))
           (hexstring-conversion-accum (+ val (* digit-val (expt 16 index))) 
                                       (+ index 1)
                                       (cdr digits)))))) 

(define (hexstring->number hexstring) 
   (hexstring-conversion-accum 0 0 (reverse (string->list hexstring))))

(define (objc:nsdata->string data)
   (objc:nsstring->string (objc:make "NSString" "initWithData:encoding:" data 4)))

;; Swank
(define *swank:connections* '()) 

(define (swank:serve socket)
   (print 'connection-on-socket socket)
   (let ((streams (io:tcp:get-streams-from-socket socket)))
      (set! *swank:connections* (cons streams *swank:connections*))))

(print *swank:connections*)

(equal? (car (string->sexpr "(:emacs-rex (swank:connection-info))")) ':emacs-rex)

(define (swank:connection-info)
   "")

(equal? "" "")

(help cond #t)

(define swank:listen (lambda () ))
(define (swank:listen)
   (for-each (lambda (connection)
                (if (io:tcp:data-available? connection)
                    (let* ((bytes-to-read-data (io:tcp:read-from-stream connection 6))
                           (bytes-to-read (hexstring->number (objc:nsdata->string bytes-to-read-data)))
                           (event-data (io:tcp:read-from-stream connection bytes-to-read)) 
                           (event (string->sexpr (objc:nsdata->string event-data))))
                       (print "bytes to read" bytes-to-read)
                       (print "event" event)
                       (cond ((equal? event "") 
                              (print "doing nothing"))
                             ((equal? (car event) ':emacs-rex)
                              (print "emacs-rex"))
                             ))))
            *swank:connections*)
   (callback (+ (now) 5000) 'swank:listen))

(swank:listen)

(define (swank:start)
   (if (io:tcp:start-server 4005 (ipc:get-process-name) "swank:serve")
       (swank:listen)
      (print "Starting swank failed")))   

(swank:start)
