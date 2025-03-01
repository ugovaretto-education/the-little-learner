#lang racket

(provide scatter-plot
         line-plot)

(require "vector.rkt" "calculus.rkt")

(require plot)


(define scatter-plot
  (lambda (xs ys color)
    (let
        (
         (xm (- (vmin xs) 0.1))
         (xM (+ (vmax xs) 0.1))
         (ym (- (vmin ys) 0.1))
         (yM (+ (vmax ys) 0.1))
         )
    (plot
     (points
      (vector-map vector xs ys)
      #:color color
      #:x-min xm
      #:x-max xM
      #:y-min ym
      #:y-max yM
      )))))

(define line-plot
  (lambda (xs theta color)
    (let ((xmin (- (vmin xs) 0.1))
          (xmax (+ (vmax xs) 0.1)))
      (plot (function
             (uv/line-eq theta)
             xmin
             xmax
             #:color color)))))


(define line-scatter-plot
  (lambda (xs ys theta color-p color-l)
    (let ((xm (- (vmin xs) 0.1))
          (xM (+ (vmax xs) 0.1))
          (ym (- (vmin ys) 0.1))
          (yM (+ (vmax ys) 0.1)))
      (plot (list
       (points
        (vector-map vector xs ys)
        #:color color-p
        #:x-min xm
        #:x-max xM
        #:y-min ym
        #:y-max yM)
       (function
        (uv/line-eq theta)
        xm
        xM
        #:color color-l))))))

;; (require plot)
;; (plot (function (line-eq '(0.5 -1)) (- 2) 2 #:label "y = line-eq(x)"))
;; applicaiton x -> y: (vector-map (line-eq 0.5 1) #(1 2 3 4))
;; Theta: θ 003B8

;; (require plot)
;; (define xs '(0 1 2 3 4 5))
;; (define ys '(0 1 4 9 16 25))
;; (plot (points (map vector xs ys) #:color 'red))
;; use vector-map if xs ys are vectors
