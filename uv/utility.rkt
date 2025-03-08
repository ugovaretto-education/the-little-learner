#lang racket

(provide symbol->function
         scalar?
         fformat
         left-pad
         sample-sequence
         sample-vectors)

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

(define sample-sequence
  (lambda (size max-index)
    (set->list (sampled-sequence size max-index (set)))))

(define sampled-sequence
  (lambda (size max-index acc)
    (if (= (set-count acc) size)
        acc
        (sampled-sequence size max-index (set-add acc (random max-index))))))


(define extract-sub-vectors
  (lambda (indices xs)
    (extracted indices xs (make-list (length xs) (vector))))) ;;xs is a list of vector

(define extracted
  (lambda (indices vs acc)
    (if (empty? indices)
        acc
        (let* ((i (car indices))
              (a (map (lambda (xs ys)
                        (vector-append ys
                                       (vector
                                        (vector-ref xs i)))) vs acc)))
            (extracted (cdr indices) vs a)))))

(define sample-vectors
  (lambda (num-samples xs)
    (let ((idx (sample-sequence num-samples (vector-length (car xs)))))
      (extract-sub-vectors idx ))))
