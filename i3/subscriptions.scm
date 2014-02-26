(define-module (i3 subscriptions)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (i3 socket)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 optargs))

(define-record-type <i3-subscription>
  (new-i3-subscription h-thread)
  subscription?
  (h-thread i3-event-thread set-i3-event-thread!))

(define-public (i3-subscribe payload handler)
  (new-i3-subscription
   (call-with-new-thread
    (lambda ()
      (parameterize ((current-i3-connection (i3-connect)))
        (i3-send (pack 'subscribe payload))
        (let handler-loop ()
          ; TODO: check for succesfull subscription
          ; before passing off to handler
          (define payload (i3-receive))
          (handler payload)
          (handler-loop)))))))
