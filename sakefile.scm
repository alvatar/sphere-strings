(define modules
  '(char
    format
    string))

(define-task compile ()
  (for-each (lambda (m) (sake#compile-c-to-o (sake#compile-to-c m)))
            modules))

(define-task clean ()
  (sake#default-clean))

(define-task install ()
  (for-each sake#install-compiled-module modules))

(define-task all (compile install)
  'all)
