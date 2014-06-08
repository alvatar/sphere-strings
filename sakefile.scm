(define modules
  '(char
    format
    string
    u8))

(define-task compile ()
  (sake#parallel-for-each
   (lambda (m)
     (sake#compile-module m cond-expand-features: '(optimize)))
   modules))

(define-task post-compile ()
  (for-each (lambda (m) (sake#make-module-available m)) modules))

(define-task install ()
  (sake#install-sphere-to-system))

(define-task test ()
  (sake#test-all))

(define-task clean ()
  (sake#default-clean))

(define-task all (compile post-compile)
  'all)
