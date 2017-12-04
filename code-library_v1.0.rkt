;"C:\Program Files\Racket\raco" exe --gui C:\Users\joe\Desktop\programming\scheme\code_library\code-library.rkt
;#lang racket

(define file "library_v1.0.txt")
(define num 0)
(define lib '())

(define (incr)
  (set! num (+ num 1)) num)

(define (create-lib)
  (let ((port (open-input-file file)))
    (let loop((ls '()) (char (read-char port)))
      (cond ((eof-object? char)
	  (begin
	    (close-input-port port)  
	    (set! lib (reverse ls))))
            ((char=? char #\%)
                 (loop(append '(#\space #\- #\space)(reverse(string->list(number->string(incr)))) ls)(read-char port))            )
	  (else (loop (cons char ls) (read-char port)))))))

(define (toc-next-line ls)
  (let loop((ls1 ls))
    (if (not(null? ls1))
        (if (and (not (and (char-numeric? (car ls1))
                (eqv? (cadr ls1) #\space)
                (eqv? (caddr ls1) #\-)
                (eqv? (cadddr ls1) #\space)))
                (not (and (char-numeric? (car ls1))
                (char-numeric? (cadr ls1))          
                (eqv? (caddr ls1) #\space)
                (eqv? (cadddr ls1) #\-)
                (eqv? (cadddr (cdr ls1)) #\space))))
            (loop (cdr ls1))
            (toc ls1))
        (values))))

(define (toc ls)
  (let loop((line ls)) 
  (if (not(null? line))
      (if (not (eqv? (car line) #\return))
          (begin
            (display (car line))
            (loop (cdr line)))
          (begin
           (newline)
           (toc-next-line (cdr line))))
      (values))))

(define (display-entry ls)
  (let loop((entry ls))
    (if (not(null? entry))
        (if (not (and (eqv? (car entry) #\return)
                      (eqv? (cadr entry) #\newline)
                      (eqv? (caddr entry) #\return)
                      (eqv? (cadddr entry) #\newline)
                      (eqv? (cadddr (cdr entry)) #\return)))
            (begin
              (display (car entry))
              (loop (cdr entry)))
            (values))
        (values))))

(define (get-entry input)
  (let loop((ls lib))
  (if (not(null? ls))
    (if (or (and (string->number(string (car ls)))
             (= input (string->number(string (car ls))))
             (eqv? (cadr ls) #\space)
             (eqv? (caddr ls) #\-)
             (eqv? (cadddr ls) #\space))
            (and (string->number(string (car ls)))
             (string->number(string (cadr ls)))
             (= input (string->number (list->string (list (car ls)(cadr ls)))))
             (eqv? (caddr ls) #\space)
             (eqv? (cadddr ls) #\-)
             (eqv? (cadddr (cdr ls)) #\space)))
        (begin
          (newline)
          (display-entry ls))
        (loop (cdr ls)))
    (values))))

(define (check-input input)
  (cond((eqv? input 'toc)
        (begin
          (display "\n----CODE LIBRARY SCHEME----\n")
          (toc lib)
          (main)))
    ((not (number? input))
        (begin
          (display "Type in a number or 'toc'.")
          (newline)
          (check-input(read))))
       (else
        (begin
          (get-entry input)
          (newline)
          (main)))))

(define (main)
  (cond((null? lib)
        (begin
          (create-lib)
          (display "----CODE LIBRARY SCHEME----\n")
          (toc lib)
          (main)))
       (else
        (begin
          (newline)
          (display "What would you like to read? ")
          (check-input(read))
          (main)))))

(main)