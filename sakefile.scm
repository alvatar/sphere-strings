(define modules
  '(char
    format
    string))

(define-task compile ()
  (for-each sake#compile-module modules))

(define-task clean ()
  (sake#default-clean))

(define-task install ()
  (for-each sake#install-compiled-module modules))

(define-task force-install ()
  (sake#install-sphere-to-system))

(define-task all (compile install)
  'all)
