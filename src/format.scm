;;!!! String formatting

(declare (standard-bindings) (extended-bindings) (block))

;;!! SRFI-28 Basic Format Strings
;; Copyright (C) Scott G. Miller (2002). All Rights Reserved.

;;! srfi-28/format
(define srfi-28/format
  (lambda (format-string . objects)
    (let ((buffer (open-output-string)))
      (let loop ((format-list (string->list format-string))
                 (objects objects))
        (cond ((null? format-list) (get-output-string buffer))
              ((char=? (car format-list) #\~)
               (if (null? (cdr format-list))
                   (error 'format "Incomplete escape sequence")
                   (case (cadr format-list)
                     ((#\a)
                      (if (null? objects)
                          (error 'format "No value for escape sequence")
                          (begin
                            (display (car objects) buffer)
                            (loop (cddr format-list) (cdr objects)))))
	             ((#\s)
                      (if (null? objects)
                          (error 'format "No value for escape sequence")
                          (begin
                            (write (car objects) buffer)
                            (loop (cddr format-list) (cdr objects)))))
                     ((#\%)
                      (newline buffer)
                      (loop (cddr format-list) objects))
                     ((#\~)
                      (write-char #\~ buffer)
                      (loop (cddr format-list) objects))
                     (else
                      (error 'format "Unrecognized escape sequence")))))
              (else (write-char (car format-list) buffer)
                    (loop (cdr format-list) objects)))))))


;;!! SRFI-48 Intermediate Format Strings

;;; IMPLEMENTS:   Format function {Scheme} -- see documentation below.
;;; AUTHOR:       Ken Dickey
;;; COPYRIGHT (c) 1988..2005 by Kenneth Alan Dickey
;;;
;;;Permission is hereby granted, free of charge, to any person
;;;obtaining a copy of this software and associated documentation
;;;files (the "Software"), to deal in the Software without
;;;restriction, including without limitation the rights to use,
;;;copy, modify, merge, publish, distribute, sublicense, and/or
;;;sell copies of the Software, and to permit persons to whom
;;;the Software is furnished to do so, subject to the following
;;;conditions:
;;;
;;;The above copyright notice and this permission notice shall
;;;be included in all copies or substantial portions of the Software.
;;;
;;;THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;;HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;;WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;OTHER DEALINGS IN THE SOFTWARE.
;;;  ========
;;;  FUNCTION: (FORMAT <port> <format-string> . <args>)
;;;  ========
;;;  RESULT: returns #!void or a string; has side effect of
;;;  printing according to <format-string>.  If <port> is #t the output is
;;;  to the current output port.  If <port> is #f, a formatted string is
;;;  returned as the result of the call.  Otherwise <port> must be an
;;;  output port.  <format-string> must be a string.  Characters are output
;;;  as if the string were output by the DISPLAY function with the
;;;  exception of those prefixed by a tilde (~) as follows [note that options
;;;  which take arguments remove them from the argument list (they are said to
;;;  be `consumed')]:
;;;
;;; OPTION  [MNEMONIC]      DESCRIPTION     -- Implementation Assumes ASCII Text Encoding
;;; ~H      [Help]          output this text
;;; ~A      [Any]           (display arg) for humans
;;; ~S      [Slashified]    (write arg) for parsers
;;; ~W      [WriteCircular] like ~s but outputs circular and recursive data structures
;;; ~~      [tilde]         output a tilde
;;; ~T      [Tab]           output a tab character
;;; ~%      [Newline]       output a newline character
;;; ~&      [Freshline]     output a newline character if the previous output was not a newline
;;; ~D      [Decimal]       the arg is a number which is output in decimal radix
;;; ~X      [heXadecimal]   the arg is a number which is output in hexdecimal radix
;;; ~O      [Octal]         the arg is a number which is output in octal radix
;;; ~B      [Binary]        the arg is a number which is output in binary radix
;;; ~w,dF   [Fixed]         the arg is a string or number which has width w and d digits after the decimal
;;; ~C      [Character]     charater arg is output by write-char
;;; ~_      [Space]         a single space character is output
;;; ~Y      [Yuppify]       the list arg is pretty-printed to the output
;;; ~?      [Indirection]   recursive format: next 2 args are format-string and list of arguments
;;; ~K      [Indirection]   same as ~?

;;! srfi-48/format
(define srfi-48/format
  (lambda args
    (define ascii-tab   (integer->char  9)) ;; NB: assumes ASCII encoding
    (define ascii-ff    (integer->char 12))
    (define (freshline port) (if (not (= 1 (output-port-column port))) (newline port)))
    (cond
     ((null? args)
      (error "FORMAT: required format-string argument is missing"))
     ((string? (car args))
      (apply format (cons #f args)))
     ((< (length args) 2)
      (error (format #f "FORMAT: too few arguments ~s" (cons 'format args))))
     (else
      (let ((output-port (car  args))
            (format-string (cadr args))
            (args (cddr args)))
        (letrec ((port
                  (cond ((output-port? output-port) output-port)
                        ((eq? output-port #t) (current-output-port)) 
                        ((eq? output-port #f) (open-output-string)) 
                        (else (error
                               (format #f "FORMAT: bad output-port argument: ~s"
                                       output-port)))))
                 (return-value 
                  (if (eq? output-port #f) ;; if format into a string 
                      (lambda () (get-output-string port)) ;; then return the string
                      ;; else do something harmless [Gambit]
                      void)))
          (define (string-index str c)
            (let ((len (string-length str)))
              (let loop ((i 0))
                (cond ((= i len) #f)
                      ((eqv? c (string-ref str i)) i)
                      (else (loop (+ i 1)))))))
          (define (string-grow str len char)
            (let ((off (- len (string-length str))))
              (if (positive? off)
                  (string-append (make-string off char) str)
                  str)))
          (define (compose-with-digits digits pre-str frac-str exp-str)
            (let ((frac-len (string-length frac-str)))
              (cond
               ((< frac-len digits) ;; grow frac part, pad with zeros
                (string-append pre-str "."
                               frac-str (make-string (- digits frac-len) #\0)
                               exp-str))
               ((= frac-len digits) ;; frac-part is exactly the right size
                (string-append pre-str "."
                               frac-str
                               exp-str))
               (else ;; must round to shrink it
                (let* ((first-part (substring frac-str 0 digits))
                       (last-part  (substring frac-str digits frac-len))
                       (temp-str
                        (number->string
                         (round (string->number
                                 (string-append first-part "." last-part)))))
                       (dot-pos (string-index  temp-str #\.))
                       (carry?
                        (and (> dot-pos digits)
                             (> (round (string->number
                                        (string-append "0." frac-str)))
                                0)))
                       (new-frac
                        (let* ( (frac (substring temp-str 0 dot-pos))
                                (frac-len (string-length frac)))
                          (if (< frac-len digits)
                              (string-append (make-string (- digits frac-len) #\0)
                                             frac)
                              (substring frac 0 digits)))))
                  (string-append
                   (if carry? (number->string (+ 1 (string->number pre-str))) pre-str)
                   "."
                   new-frac
                   exp-str))))))


          (define (format-fixed number-or-string width digits) ; returns a string
            (cond
             ((string? number-or-string)
              (string-grow number-or-string width #\space))
             ((number? number-or-string)
              (let ((real (real-part number-or-string))
                    (imag (imag-part number-or-string)))
                (cond
                 ((not (zero? imag))
                  (string-grow
                   (string-append (format-fixed real 0 digits)
                                  (if (negative? imag) "" "+")
                                  (format-fixed imag 0 digits)
                                  "i")
                   width
                   #\space))
                 (digits
                  (let* ((num (exact->inexact real))
                         (small? (< (abs num) 1))
                         (nega?  (negative? num))
                         ;; want to display digits around the decimal
                         (num-str
                          (number->string
                           (if small? ((if nega? - +) num 1) num)))
                         (dot-index (string-index  num-str #\.))
                         (exp-index (string-index  num-str #\e))
                         (length    (string-length num-str))
                         (pre-string
                          (cond
                           (exp-index
                            (if dot-index
                                (substring num-str 0 dot-index)
                                (substring num-str 0 exp-index)))
                           (dot-index
                            (substring num-str 0 dot-index))
                           (else
                            num-str)))
                         (exp-string
                          (if exp-index (substring num-str exp-index length) ""))
                         (frac-string
                          (cond
                           (dot-index
                            (if exp-index
                                (substring num-str (+ dot-index 1) exp-index)
                                (substring num-str (+ dot-index 1) length)))
                           (else ""))))
                    (if (zero? (string-length pre-string))
                        (set! pre-string "0"))
                    (if small? (string-set! pre-string
                                            (- (string-length pre-string) 1)
                                            #\0))
                    (string-grow
                     (if dot-index
                         (compose-with-digits digits
                                              pre-string
                                              frac-string
                                              exp-string)
                         (string-append pre-string exp-string))
                     width
                     #\space)
                    ))
                 (else ;; no digits
                  (string-grow (number->string real) width #\space)))))
             (else
              (error
               (format "FORMAT: ~F requires a number or a string, got ~s" number-or-string)))))
          (define documentation-string
            "(format [<port>] <format-string> [<arg>...]) -- <port> is #t, #f or an output-port
OPTION  [MNEMONIC]      DESCRIPTION     -- Implementation Assumes ASCII Text Encoding
~H      [Help]          output this text
~A      [Any]           (display arg) for humans
~S      [Slashified]    (write arg) for parsers
~W      [WriteCircular] like ~s but outputs circular and recursive data structures
~~      [tilde]         output a tilde
~T      [Tab]           output a tab character
~%      [Newline]       output a newline character
~&      [Freshline]     output a newline character if the previous output was not a newline
~D      [Decimal]       the arg is a number which is output in decimal radix
~X      [heXadecimal]   the arg is a number which is output in hexdecimal radix
~O      [Octal]         the arg is a number which is output in octal radix
~B      [Binary]        the arg is a number which is output in binary radix
~w,dF   [Fixed]         the arg is a string or number which has width w and d digits after the decimal
~C      [Character]     charater arg is output by write-char
~_      [Space]         a single space character is output
~Y      [Yuppify]       the list arg is pretty-printed to the output
~?      [Indirection]   recursive format: next 2 args are format-string and list of arguments
~K      [Indirection]   same as ~?
"
            )

          (define (require-an-arg args)
            (if (null? args)
                (error "FORMAT: too few arguments" )))
          (define (display-possible-list possible-list port)
            (cond
             ((null? possible-list)
              (display "()" port))
             ((not (pair? possible-list))
              (display possible-list port))
             (else
              (display #\( port)
              (let loop ((list possible-list) (first-time? #t))
                (cond
                 ((null? list)
                  (display #\) port))
                 ((not (or (null? (cdr list)) (pair? (cdr list))))
                  ;; improper list
                  (if (not first-time?)
                      (display #\space port))
                  (display-possible-list (car list) port)
                  (display " . " port)
                  (display-possible-list (cdr list) port)
                  (display #\) port))
                 (else
                  (if (not first-time?)
                      (display #\space port))
                  (display-possible-list (car list) port)
                  (loop (cdr list) #f)))))))
          (define (format-help format-strg arglist)
            (letrec ((length-of-format-string (string-length format-strg))
                     (anychar-dispatch       
                      (lambda (pos arglist) 
                        (if (>= pos length-of-format-string) 
                            arglist     ; return unused args 
                            (let ( (char (string-ref format-strg pos)) ) 
                              (cond            
                               ((eqv? char #\~)   
                                (tilde-dispatch (+ pos 1) arglist)) 
                               (else                   
                                (write-char char port)     
                                (anychar-dispatch (+ pos 1) arglist))))))) ; end anychar-dispatch
                     (tilde-dispatch          
                      (lambda (pos arglist)     
                        (cond           
                         ((>= pos length-of-format-string)   
                          (write-char #\~ port) ; tilde at end of string is just output
                          arglist)      ; return unused args     
                         (else      
                          (case (char-upcase (string-ref format-strg pos)) 
                            ((#\A)      ; Any -- for humans
                             (require-an-arg arglist)
                             (let ((whatever (car arglist)))
                               (display-possible-list whatever port)
                               (anychar-dispatch (+ pos 1) (cdr arglist))))
                            ((#\S)      ; Slashified -- for parsers
                             (require-an-arg arglist)
                             (let ((whatever (car arglist)))
                               (write whatever port)     
                               (anychar-dispatch (+ pos 1) (cdr arglist))))
                            ((#\W)
                             (require-an-arg arglist)
                             (let* ((whatever (car arglist))
                                    (readtable (output-port-readtable port))
                                    (sharing-allowed? (readtable-sharing-allowed? readtable)))
                               ;;(write-with-shared-structure whatever port)  ;; srfi-38
                               (dynamic-wind
                                   (lambda ()
                                     (output-port-readtable-set!
                                      port
                                      (readtable-sharing-allowed?-set readtable 'serialize)))
                                   (lambda () (write whatever port))
                                   (lambda ()
                                     (output-port-readtable-set!
                                      port
                                      (readtable-sharing-allowed?-set readtable sharing-allowed?))))
                               (anychar-dispatch (+ pos 1) (cdr arglist))))                           
                            ((#\D)      ; Decimal
                             (require-an-arg arglist)
                             (display (number->string (car arglist) 10) port)  
                             (anychar-dispatch (+ pos 1) (cdr arglist)))            
                            ((#\X)      ; HeXadecimal
                             (require-an-arg arglist)
                             (display (number->string (car arglist) 16) port)
                             (anychar-dispatch (+ pos 1) (cdr arglist)))             
                            ((#\O)      ; Octal
                             (require-an-arg arglist)
                             (display (number->string (car arglist)  8) port) 
                             (anychar-dispatch (+ pos 1) (cdr arglist)))       
                            ((#\B)      ; Binary
                             (require-an-arg arglist)
                             (display (number->string (car arglist)  2) port)
                             (anychar-dispatch (+ pos 1) (cdr arglist)))           
                            ((#\C)      ; Character
                             (require-an-arg arglist)
                             (write-char (car arglist) port) 
                             (anychar-dispatch (+ pos 1) (cdr arglist)))          
                            ((#\~)      ; Tilde  
                             (write-char #\~ port)   
                             (anychar-dispatch (+ pos 1) arglist))            
                            ((#\%)      ; Newline   
                             (newline port) 
                             (anychar-dispatch (+ pos 1) arglist))
                            ((#\&)      ; Freshline
                             (freshline port)
                             (anychar-dispatch (+ pos 1) arglist))
                            ((#\_)      ; Space 
                             (write-char #\space port)   
                             (anychar-dispatch (+ pos 1) arglist))             
                            ((#\T) ; Tab -- IMPLEMENTATION DEPENDENT ENCODING    
                             (write-char ascii-tab port)          
                             (anychar-dispatch (+ pos 1) arglist))             
                            ((#\Y)      ; Pretty-print
                                        ;(pretty-print (car arglist) port)  ;; IMPLEMENTATION DEPENDENT
                             (pp (car arglist) port)
                             (anychar-dispatch (+ pos 1) (cdr arglist)))              
                            ((#\F)
                             (require-an-arg arglist)
                             (display (format-fixed (car arglist) 0 #f) port)
                             (anychar-dispatch (+ pos 1) (cdr arglist)))
                            ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) ;; gather "~w[,d]F" w and d digits
                             (let loop ( (index (+ pos 1))
                                         (w-digits (list (string-ref format-strg pos)))
                                         (d-digits '())
                                         (in-width? #t))
                               (if (>= index length-of-format-string)
                                   (error
                                    (format "FORMAT: improper numeric format directive in ~s" format-strg))
                                   (let ( (next-char (string-ref format-strg index)) )
                                     (cond
                                      ((char-numeric? next-char)
                                       (if in-width?
                                           (loop (+ index 1)
                                                 (cons next-char w-digits)
                                                 d-digits
                                                 in-width?)
                                           (loop (+ index 1)
                                                 w-digits
                                                 (cons next-char d-digits)
                                                 in-width?)))
                                      ((char=? next-char #\F)
                                       (let ((width (string->number (list->string (reverse w-digits))))
                                             (digits (if (zero? (length d-digits))
                                                         #f
                                                         (string->number (list->string (reverse d-digits))))))
                                         (display (format-fixed (car arglist) width digits) port)
                                         (anychar-dispatch (+ index 1) (cdr arglist))))
                                      ((char=? next-char #\,)
                                       (if in-width?
                                           (loop (+ index 1)
                                                 w-digits
                                                 d-digits
                                                 #f)
                                           (error
                                            (format "FORMAT: too many commas in directive ~s" format-strg))))
                                      (else
                                       (error (format "FORMAT: ~~w.dF directive ill-formed in ~s" format-strg))))))))
                            ((#\? #\K) ; indirection -- take next arg as format string
                             (cond ;  and following arg as list of format args
                              ((< (length arglist) 2)
                               (error
                                (format "FORMAT: less arguments than specified for ~~?: ~s" arglist)))
                              ((not (string? (car arglist)))
                               (error
                                (format "FORMAT: ~~? requires a string: ~s" (car arglist))))
                              (else
                               (format-help (car arglist) (cadr arglist))
                               (anychar-dispatch (+ pos 1) (cddr arglist)))))
                            ((#\H)      ; Help
                             (display documentation-string port)
                             (anychar-dispatch (+ pos 1) arglist))
                            (else                
                             (error (format "FORMAT: unknown tilde escape: ~s"
                                            (string-ref format-strg pos))))))))))
              ;; format-help main
              (anychar-dispatch 0 arglist)))
          (let ((unused-args (format-help format-string args)))
            (force-output port)
            (if (not (null? unused-args))
                (error (format "FORMAT: unused arguments ~s" unused-args))
                (return-value)))))))))

(define format srfi-48/format)


;;!! SRFI-54 Formatting

;;! cat
(letrec-syntax
    ;; macros borrowed from SRFI-86
    ((alet-cat*
      (syntax-rules ()
        ((alet-cat* z (a . e) bd ...)
         (let ((y z))
           (%alet-cat* y (a . e) bd ...)))))
     (%alet-cat*
      (syntax-rules ()
        ((%alet-cat* z ((n d t ...)) bd ...)
         (let ((n (if (null? z)
                      d
                      (if (null? (cdr z))
                          (wow-cat-end z n t ...)
                          (error "cat: too many arguments" (cdr z))))))
           bd ...))
        ((%alet-cat* z ((n d t ...) . e) bd ...)
         (let ((n (if (null? z)
                      d
                      (wow-cat! z n d t ...))))
           (%alet-cat* z e bd ...)))
        ((%alet-cat* z e bd ...)
         (let ((e z)) bd ...))))
     (wow-cat-end
      (syntax-rules ()
        ((wow-cat-end z n)
         (car z))
        ((wow-cat-end z n t)
         (let ((n (car z)))
           (if t n (error "cat: too many argument" z))))
        ((wow-cat-end z n t ts)
         (let ((n (car z)))
           (if t ts (error "cat: too many argument" z))))
        ((wow-cat-end z n t ts fs)
         (let ((n (car z)))
           (if t ts fs)))))
     (wow-cat!
      (syntax-rules ()
        ((wow-cat! z n d)
         (let ((n (car z)))
           (set! z (cdr z))
           n))
        ((wow-cat! z n d t)
         (let ((n (car z)))
           (if t
               (begin (set! z (cdr z)) n)
               (let lp ((head (list n)) (tail (cdr z)))
                 (if (null? tail)
                     d
                     (let ((n (car tail)))
                       (if t
                           (begin (set! z (append (reverse head) (cdr tail))) n)
                           (lp (cons n head) (cdr tail)))))))))
        ((wow-cat! z n d t ts)
         (let ((n (car z)))
           (if t
               (begin (set! z (cdr z)) ts)
               (let lp ((head (list n)) (tail (cdr z)))
                 (if (null? tail)
                     d
                     (let ((n (car tail)))
                       (if t
                           (begin (set! z (append (reverse head) (cdr tail))) ts)
                           (lp (cons n head) (cdr tail)))))))))
        ((wow-cat! z n d t ts fs)
         (let ((n (car z)))
           (if t
               (begin (set! z (cdr z)) ts)
               (begin (set! z (cdr z)) fs)))))))
  (define (cat object . rest)
    (define (str-index str char)
      (let ((len (string-length str)))
        (let lp ((n 0))
          (and (< n len)
               (if (char=? char (string-ref str n))
                   n
                   (lp (+ n 1)))))))
    (define (part pred ls)
      (let lp ((ls ls) (true '()) (false '()))
        (cond
         ((null? ls) (cons (reverse true) (reverse false)))
         ((pred (car ls)) (lp (cdr ls) (cons (car ls) true) false))
         (else (lp (cdr ls) true (cons (car ls) false))))))
    (define (e-mold num pre)
      (let* ((str (number->string (exact->inexact num)))
             (e-index (str-index str #\e)))
        (if e-index
            (string-append (mold (substring str 0 e-index) pre)
                           (substring str e-index (string-length str)))
            (mold str pre))))
    (define (mold str pre)
      (let ((ind (str-index str #\.)))
        (if ind
            (let ((d-len (- (string-length str) (+ ind 1))))
              (cond
               ((= d-len pre) str)
               ((< d-len pre) (string-append str (make-string (- pre d-len) #\0)))
               ;;((char<? #\4 (string-ref str (+ 1 ind pre)))
               ;;(let ((com (expt 10 pre)))
               ;;  (number->string (/ (round (* (string->number str) com)) com))))
               ((or (char<? #\5 (string-ref str (+ 1 ind pre)))
                    (and (char=? #\5 (string-ref str (+ 1 ind pre)))
                         (or (< (+ 1 pre) d-len)
                             (memv (string-ref str (+ ind (if (= 0 pre) -1 pre)))
                                   '(#\1 #\3 #\5 #\7 #\9)))))
                (apply
                 string
                 (let* ((minus (char=? #\- (string-ref str 0)))
                        (str (substring str (if minus 1 0) (+ 1 ind pre)))
                        (char-list
                         (reverse
                          (let lp ((index (- (string-length str) 1))
                                   (raise #t))
                            (if (= -1 index)
                                (if raise '(#\1) '())
                                (let ((chr (string-ref str index)))
                                  (if (char=? #\. chr)
                                      (cons chr (lp (- index 1) raise))
                                      (if raise
                                          (if (char=? #\9 chr)
                                              (cons #\0 (lp (- index 1) raise))
                                              (cons (integer->char
                                                     (+ 1 (char->integer chr)))
                                                    (lp (- index 1) #f)))
                                          (cons chr (lp (- index 1) raise))))))))))
                   (if minus (cons #\- char-list) char-list))))
               (else
                (substring str 0 (+ 1 ind pre)))))
            (string-append str "." (make-string pre #\0)))))
    (define (separate str sep num opt)
      (let* ((len (string-length str))
             (pos (if opt
                      (let ((pos (remainder (if (eq? opt 'minus) (- len 1) len)
                                            num)))
                        (if (= 0 pos) num pos))
                      num)))
        (apply string-append
               (let loop ((ini 0)
                          (pos (if (eq? opt 'minus) (+ pos 1) pos)))
                 (if (< pos len)
                     (cons (substring str ini pos)
                           (cons sep (loop pos (+ pos num))))
                     (list (substring str ini len)))))))
    (define (every? pred ls)
      (let lp ((ls ls))
        (or (null? ls)
            (and (pred (car ls))
                 (lp (cdr ls))))))
    (let* ((str-rest (part string? rest))
           (str-list (car str-rest))
           (rest-list (cdr str-rest)))
      (if (null? rest-list)
          (apply string-append
                 (cond
                  ((number? object) (number->string object))
                  ((string? object) object)
                  ((char? object) (string object))
                  ((boolean? object) (if object "#t" "#f"))
                  ((symbol? object) (symbol->string object))
                  (else
                   (get-output-string
                    (let ((str-port (open-output-string)))
                      (write object str-port)
                      str-port))))
                 str-list)
          (alet-cat*
           rest-list
           ((width 0 (and (integer? width) (exact? width)))
            (port #f (or (boolean? port) (output-port? port))
                  (if (eq? port #t) (current-output-port) port))
            (char #\space (char? char))
            (converter #f (and (pair? converter)
                               (procedure? (car converter))
                               (procedure? (cdr converter))))
            (precision #f (and (integer? precision)
                               (inexact? precision)))
            (sign #f (eq? 'sign sign))
            (radix 'decimal
                   (memq radix '(decimal octal binary hexadecimal)))
            (exactness #f (memq exactness '(exact inexact)))
            (separator #f (and (list? separator)
                               (< 0 (length separator) 3)
                               (char? (car separator))
                               (or (null? (cdr separator))
                                   (let ((n (cadr separator)))
                                     (and (integer? n) (exact? n)
                                          (< 0 n))))))
            (writer #f (procedure? writer))
            (pipe #f (and (list? pipe)
                          (not (null? pipe))
                          (every? procedure? pipe)))
            (take #f (and (list? take)
                          (< 0 (length take) 3)
                          (every? (lambda (x)
                                    (and (integer? x) (exact? x)))
                                  take))))
           (let* ((str
                   (cond
                    ((and converter
                          ((car converter) object))
                     (let* ((str ((cdr converter) object))
                            (pad (- (abs width) (string-length str))))
                       (cond
                        ((<= pad 0) str)
                        ((< 0 width) (string-append (make-string pad char) str))
                        (else (string-append str (make-string pad char))))))
                    ((number? object)
                     (and (not (eq? radix 'decimal)) precision
                          (error "cat: non-decimal cannot have a decimal point"))
                     (and precision (< precision 0) (eq? exactness 'exact)
                          (error "cat: exact number cannot have a decimal point without exact sign"))
                     (let* ((exact-sign (and precision
                                             (<= 0 precision)
                                             (or (eq? exactness 'exact)
                                                 (and (exact? object)
                                                      (not (eq? exactness
                                                                'inexact))))
                                             "#e"))
                            (inexact-sign (and (not (eq? radix 'decimal))
                                               (or (and (inexact? object)
                                                        (not (eq? exactness
                                                                  'exact)))
                                                   (eq? exactness 'inexact))
                                               "#i"))
                            (radix-sign (cdr (assq radix
                                                   '((decimal . #f)
                                                     (octal . "#o")
                                                     (binary . "#b")
                                                     (hexadecimal . "#x")))))
                            (plus-sign (and sign (< 0 (real-part object)) "+"))
                            (exactness-sign (or exact-sign inexact-sign))
                            (str
                             (if precision
                                 (let ((precision (inexact->exact
                                                   (abs precision)))
                                       (imag (imag-part object)))
                                   (if (= 0 imag)
                                       (e-mold object precision)
                                       (string-append
                                        (e-mold (real-part object) precision)
                                        (if (< 0 imag) "+" "")
                                        (e-mold imag precision)
                                        "i")))
                                 (number->string
                                  (cond
                                   (inexact-sign (inexact->exact object))
                                   (exactness
                                    (if (eq? exactness 'exact)
                                        (inexact->exact object)
                                        (exact->inexact object)))
                                   (else object))
                                  (cdr (assq radix '((decimal . 10)
                                                     (octal . 8)
                                                     (binary . 2)
                                                     (hexadecimal . 16)))))))
                            (str
                             (if (and separator
                                      (not (or (and (eq? radix 'decimal)
                                                    (str-index str #\e))
                                               (str-index str #\i)
                                               (str-index str #\/))))
                                 (let ((sep (string (car separator)))
                                       (num (if (null? (cdr separator))
                                                3 (cadr separator)))
                                       (dot-index (str-index str #\.)))
                                   (if dot-index
                                       (string-append
                                        (separate (substring str 0 dot-index)
                                                  sep num (if (< object 0)
                                                              'minus #t))
                                        "."
                                        (separate (substring
                                                   str (+ 1 dot-index)
                                                   (string-length str))
                                                  sep num #f))
                                       (separate str sep num (if (< object 0)
                                                                 'minus #t))))
                                 str))
                            (pad (- (abs width)
                                    (+ (string-length str)
                                       (if exactness-sign 2 0)
                                       (if radix-sign 2 0)
                                       (if plus-sign 1 0))))
                            (pad (if (< 0 pad) pad 0)))
                       (if (< 0 width)
                           (if (char-numeric? char)
                               (if (< (real-part object) 0)
                                   (string-append (or exactness-sign "")
                                                  (or radix-sign "")
                                                  "-"
                                                  (make-string pad char)
                                                  (substring str 1
                                                             (string-length
                                                              str)))
                                   (string-append (or exactness-sign "")
                                                  (or radix-sign "")
                                                  (or plus-sign "")
                                                  (make-string pad char)
                                                  str))
                               (string-append (make-string pad char)
                                              (or exactness-sign "")
                                              (or radix-sign "")
                                              (or plus-sign "")
                                              str))
                           (string-append (or exactness-sign "")
                                          (or radix-sign "")
                                          (or plus-sign "")
                                          str
                                          (make-string pad char)))))
                    (else
                     (let* ((str (cond
                                  (writer (get-output-string
                                           (let ((str-port
                                                  (open-output-string)))
                                             (writer object str-port)
                                             str-port)))
                                  ((string? object) object)
                                  ((char? object) (string object))
                                  ((boolean? object) (if object "#t" "#f"))
                                  ((symbol? object) (symbol->string object))
                                  (else (get-output-string
                                         (let ((str-port (open-output-string)))
                                           (write object str-port)
                                           str-port)))))
                            (str (if pipe
                                     (let loop ((str ((car pipe) str))
                                                (fns (cdr pipe)))
                                       (if (null? fns)
                                           str
                                           (loop ((car fns) str)
                                                 (cdr fns))))
                                     str))
                            (str
                             (if take
                                 (let ((left (car take))
                                       (right (if (null? (cdr take))
                                                  0 (cadr take)))
                                       (len (string-length str)))
                                   (define (substr str beg end)
                                     (let ((end (cond
                                                 ((< end 0) 0)
                                                 ((< len end) len)
                                                 (else end)))
                                           (beg (cond
                                                 ((< beg 0) 0)
                                                 ((< len beg) len)
                                                 (else beg))))
                                       (if (and (= beg 0) (= end len))
                                           str
                                           (substring str beg end))))
                                   (string-append
                                    (if (< left 0)
                                        (substr str (abs left) len)
                                        (substr str 0 left))
                                    (if (< right 0)
                                        (substr str 0 (+ len right))
                                        (substr str (- len right) len))))
                                 str))
                            (pad (- (abs width) (string-length str))))
                       (cond
                        ((<= pad 0) str)
                        ((< 0 width) (string-append (make-string pad char) str))
                        (else (string-append str (make-string pad char))))))))
                  (str (apply string-append str str-list)))
             (and port (display str port))
             str))))))

