(define-module (i3 types)
  #:use-module (json)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu))

(define-syntax define-i3-type
  (lambda (stx)
    (define (build-name name . fields)
      (datum->syntax name
                     (string->symbol (apply string-append
                                            (map (lambda (f)
                                                   (if (string? f)
                                                       f
                                                       (string->symbol (syntax->datum f))))
                                                 fields)))))
    (syntax-case stx ()
      ((_ name field ...)
       (with-syntax ((type (build-name #'name "<" #'name ">"))
                     (cntr (build-name "make-" #'name))
                     (pred (build-name #'name #'name "?"))
                     ((accessor ...)
                      (map (lambda (f)
                             (build-name f #'name "-" f))
                           #'(field ...))))
         #'(begin
             (define-public type
               (make-record-type (symbol->string (syntax->datum #'name)) '(field ...)))
             (define-public pred
               (record-predicate type))
             (define-public cntr
               (record-constructor type '(field ...)))
             (define-public accessor
               (record-accessor type 'field))
             ...))))))

(define-i3-type i3-version major minor patch human)
(define-i3-type i3-rec x y width height)
(define-i3-type i3-workspace num name visible focused urgent rect output)
(define-i3-type i3-outputs name active current-workspace rect)
(define-i3-type i3-tree id name border current-border-width layout orientation percent rec window-rect geometry window urgent focused)
(define-i3-type i3-bar-config id mode position status-command font workspace-buttons binding-mode-indicator verbose colors)
