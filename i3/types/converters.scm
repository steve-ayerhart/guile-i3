(define-module (i3 types converters)
  #:use-module (ice-9 regex)
  #:use-module (i3 types))

(define-public (success-reply->scm reply)
  (define success-hash (car reply))
  (define success (hash-ref success-hash "success"))
  (hash-remove! success-hash "success")
  (if success 
      (values #t '())
      (values #f (hash-map->list 
                  (lambda (k v)
                    (cons 
                     (regexp-substitute/global #f "_" k 'pre "-" 'post)
                     v))
                  success-hash))))
                              

(define-public (workspace-reply->scm reply)
  "stub")
(define-public (outputs-reply->scm reply)
  "stub")
(define-public (tree-reply->scm reply)
  "stub")
(define-public (marks-reply->scm reply)
  "stub")
(define-public (bar-config-reply->scm reply)
  "stub")
(define-public (version-reply->scm reply)
  "stub")
