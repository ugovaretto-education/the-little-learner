#lang racket

(provide symbol->function
         scalar?
         fformat
         left-pad)

(define symbol->function
  (lambda (f)
    (if (symbol? f)
        (eval f)
        f)))

(define scalar? number?)

(define fformat
  (lambda (f decimal-digits)
    (let ((xs (string-split (number->string f) ".")))
      (if (empty? (cdr xs))
          (car xs)
          (format "~a.~a"
                  (car xs)
                  (list->string
                   (take (string->list (cadr xs)) decimal-digits)))))))

(define num-digits
  (lambda (n)
    (if (< n 10)
        1
        (+ 1 (num-digits (/ n 10))))))

(define left-pad
  (lambda (num c n)
    (if (>= (num-digits num) n)
        (number->string num)
        (do ((i 0 (+ 1 i))
             (d (num-digits num))
             (padded (number->string num) (format "~a~a" c padded)))
            ((= i (- n d)) padded)))))
