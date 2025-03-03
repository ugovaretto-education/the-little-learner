(provide uv/revise
         uv/grad-descent
         uv/line-loss
         uv/grad-descent-line)

(require "calculus.rkt")
(require "vector.rkt")

(define uv/revise
  (lambda (f revisions p)
    (if (zero? revisions)
        p
        (uv/revise f (- revisions 1) (f p)))))

(define uv/grad-descent
  (lambda (f revisions p a)
    (uv/revise
     (lambda (v)
       (v- v (v* a (uv/gradient f v)))
       revisions
       p))))
  ;; (lambda (f revisions p a)
  ;;   (if (zero? revisions)
  ;;       p
  ;;       (uv/grad-descent
  ;;        revisions
  ;;        (v- p (v* a (uv/gradient f p)))
  ;;        a))))

(define uv/line-loss
  (lambda (xs ys)
    (((uv/l2-loss uv/line) xs ys))))

(define grad-descent-line
  (lambda (xs ys revisions p a)
    (uv/grad-descent (uv/line-loss xs ys) revisions p a)))
