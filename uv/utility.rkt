#lang racket

;;; This module provides various utility functions, including type checking,
;;; string formatting, and random sampling.

(provide symbol->function
         scalar?
         fformat
         left-pad
         sample-sequence
         sample-vectors)

;; Takes a symbol or a function. If it's a symbol, evaluates it to get
;; the corresponding function. Otherwise, returns the function itself.
(define symbol->function
  (lambda (f)
    (if (symbol? f)
        (eval f)
        f)))

;; Checks if a value is a scalar (i.e., a number).
(define scalar? number?)

;; Formats a floating-point number `f` to a string with a specified
;; number of `decimal-digits`.
(define fformat
  (lambda (f decimal-digits)
    (let ((xs (string-split (number->string f) ".")))
      (if (empty? (cdr xs))
          (car xs)
          (format "~a.~a"
                  (car xs)
                  (list->string
                   (take (string->list (cadr xs)) decimal-digits)))))))

;; Helper function to count the number of digits in an integer `n`.
(define num-digits
  (lambda (n)
    (if (< n 10)
        1
        (+ 1 (num-digits (/ n 10))))))

;; Pads a number `num` with a character `c` on the left until it reaches
;; a total width of `n` characters. Returns the padded string.
(define left-pad
  (lambda (num c n)
    (if (>= (num-digits num) n)
        (number->string num)
        (do ((i 0 (+ i 1))
             (d (num-digits num))
             (padded (number->string num) (format "~a~a" c padded)))
            ((= i (- n d)) padded)))))

;; Generates a list of `size` unique random integers, where each integer
;; is less than `max-index`.
(define sample-sequence
  (lambda (size max-index)
    (set->list (sampled-sequence size max-index (set)))))

;; Helper function for `sample-sequence`. Recursively builds a set of
;; unique random numbers.
(define sampled-sequence
  (lambda (size max-index acc)
    (if (= (set-count acc) size)
        acc
        (sampled-sequence size max-index (set-add acc (random max-index))))))


;; Extracts sub-vectors from a list of vectors `xs` based on a list of `indices`.
;; `xs` is a list of vectors, e.g., '( #(x1 x2 x3) #(y1 y2 y3) ).
;; `indices` is a list of indices, e.g., '(0 2).
;; Returns a new list of vectors with the extracted elements, e.g., '( #(x1 x3) #(y1 y3) ).
(define extract-sub-vectors
  (lambda (indices xs)
    (extracted indices xs (make-list (length xs) (vector))))) ;;xs is a list of vector

;; Recursive helper for `extract-sub-vectors`.
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

;; Creates a random sample of corresponding elements from a list of vectors.
;; `num-samples`: The number of samples to draw.
;; `xs`: A list of vectors of the same length.
;; Returns a new list of vectors, where each vector is a random subsample
;; of the corresponding input vector.
(define sample-vectors
  (lambda (num-samples xs)
    (let ((idx (sample-sequence num-samples (vector-length (car xs)))))
      (extract-sub-vectors idx xs))))
