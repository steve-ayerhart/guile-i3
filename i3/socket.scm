(define-module (i3 socket)
  #:use-module (json)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 popen))

(define %i3-header-length 14)
(define %i3-magic-string "i3-ipc")

(define %i3-message-types '((command . 0)
                            (get-workspaces . 1)
                            (subscribe . 2)
                            (get-outputs . 3)
                            (get-tree . 4)
                            (get-marks . 5)
                            (get-bar-config . 6)
                            (get-version . 7)))

(define %i3-event-types '((workspace 0)
                          (output 1)))

(define-public current-i3-connection
  (make-parameter #f))

(define-public (get-i3-socket-path)
  "Returns the I3 socket path"
  (define port (open-input-pipe "i3 --get-socketpath"))
  (define path (read-line port))
  (close port)
  path)

(define*-public (make-i3-connection #:optional (path (get-i3-socket-path)))
  (define connection (i3-connect path))
  (current-i3-connection connection)
  #t)
  
(define*-public (i3-connect #:optional (path (get-i3-socket-path)))
  ; TODO: error checking
  (define sock (socket PF_UNIX SOCK_STREAM 0))

  (connect sock AF_UNIX path)
  sock)


(define-public (pack type payload)
  (define command-bv (make-bytevector 4))
  (define length-bv (make-bytevector 4))
  (define payload-bv (string->utf8 payload))

  (define payload-length (bytevector-length payload-bv))

  (bytevector-u32-set! length-bv 0 payload-length (native-endianness))
  (bytevector-u32-set! command-bv 0 (assoc-ref %i3-message-types type) (native-endianness))

  (call-with-values (lambda ()
                      (open-bytevector-output-port))
    (lambda (port get-message)
      (display %i3-magic-string port)
      (put-bytevector port length-bv)
      (put-bytevector port command-bv)
      (put-bytevector port payload-bv)
      (get-message))))

(define-public (unpack-header bv)
  (define magic-string (utf8->string (u8-list->bytevector (take (bytevector->u8-list bv) 6))))
  (define len (bytevector-u32-ref bv 6 (native-endianness)))
  (define type (bytevector-u32-ref bv 10 (native-endianness)))

  (values magic-string len type))

(define*-public (i3-send bv #:optional (port (current-i3-connection)))
  "Send a message to I3."
  (put-bytevector port bv))

(define*-public (i3-receive #:optional (port (current-i3-connection)))
  "Receive a message from the I3 socket.  Returns a JSON encoded bytevector"
  (define header (get-bytevector-n port %i3-header-length))

  (receive (magic-string payload-len type)
      (unpack-header header)
    (get-bytevector-n port payload-len)))

(define*-public (i3-msg type #:optional (payload "") (port (current-i3-connection)))
  "Convenience function to send and receive a message from I3.  Returns a JSON encoded bytevector"
  (i3-send (pack type payload) port)
  (i3-receive port))