#lang racket

(provide symbol->function
         scalar?
         fformat)

(define symbol->function
  (lambda (f)
    (if (symbol? f)
        (eval f)
        f)))

(define scalar? number?)

(define fformat
  (lambda (f decimal-digits)
    (let* ((xs (string-split (number->string f) "."))
      (if (empty? (cdr xs))
          (car xs)
          (format "~a.~a"
                  (car xs)
                  (list->string
                   (take (string->list (cadr xs)) decimal-digits)))))))
