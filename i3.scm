(define-module (i3)
  #:use-module (i3 socket)
  #:use-module (i3 subscriptions))

(define-syntax re-export-modules
  (syntax-rules ()
    ((_ (mod ...) ...)
     (begin
       (module-use! (module-public-interface (current-module))
                    (resolve-interface '(mod ...)))
       ...))))

(re-export-modules (i3 socket)
                   (i3 subscriptions))
