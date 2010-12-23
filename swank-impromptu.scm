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

(define (number->hexstring num)
   (objc:nsstring->string (objc:string:with-format "%06x" num)))

(define (objc:nsdata->string data)
  (objc:nsstring->string (objc:make "NSString" "initWithData:encoding:" data 4)))

(define (objc:string->nsdata str)
   (objc:call (objc:string->nsstring str)
              "dataUsingEncoding:" 4))

(define (string-empty? str)
   (= 0 (string-length str)))

(objc:call (objc:string->nsdata "abc") "length")

;; Swank
(define *swank:connections* '())

(define (swank:serve socket)
  (print 'connection-on-socket socket)
  (let ((streams (io:tcp:get-streams-from-socket socket)))
    (set! *swank:connections* (cons streams *swank:connections*))))

(define (swank:connection-info)
  '(:pid 1000
    :package (:name "impromptu" :prompt "impromptu>")
    :lisp-implementation (:type "scheme" :name "Impromptu" :version "2.5")))

(define (swank:return-ok-result result id)
  `(:return (:ok ,result) ,id))

(define (swank:handle-event event)
  (print "event" event)
  (unless (null? event)
      (let ((event-type (car event)))
        (cond ((equal? event-type ':emacs-rex)
               (swank:return-ok-result
                (eval (cadr event))
                (list-ref event 4)))))))

(define (swank:read-next-event connection)
  (let* ((bytes-to-read-data (io:tcp:read-from-stream connection 6))
         (bytes-to-read (hexstring->number (objc:nsdata->string bytes-to-read-data)))
         (event-data (io:tcp:read-from-stream connection bytes-to-read))
         (event-str (objc:nsdata->string event-data)))
    (if (string-empty? event-str)
        '()
        (string->sexpr event-str))))

(define (swank:write-result connection result-sexpr)
  (let* ((result-str (sexpr->string result-sexpr))
         (result-length (number->hexstring (string-length result-str))))
    (io:tcp:write-to-stream connection
                            (objc:string->nsdata
                             (string-append result-length result-str)))))
(define (swank:listen))
(define (swank:listen)
  (for-each (lambda (connection)
              (if (io:tcp:data-available? connection)
                  (swank:write-result
                   connection
                   (swank:handle-event (swank:read-next-event connection)))))
            *swank:connections*)
  (callback (+ (now) 5000) 'swank:listen))

(swank:listen)

(define (swank:start)
  (if (io:tcp:start-server 4005 (ipc:get-process-name) "swank:serve")
      (swank:listen)
      (print "Starting swank failed")))

(swank:start)
