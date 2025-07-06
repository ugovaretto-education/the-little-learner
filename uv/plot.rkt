#lang racket

;;; This module provides functions for creating various kinds of plots
;;; using the `plot` library. It is specialized for visualizing
;;; data points and linear models from this project.

(provide scatter-plot
         line-plot
         line-scatter-plot
         line-scatter-plot-file)

(require "vector.rkt" "calculus.rkt")
(require plot)


;; Creates a scatter plot of (x, y) data points.
;; `xs`: A vector of x-coordinates.
;; `ys`: A vector of y-coordinates.
;; `color`: The color of the points.
;; The plot boundaries are automatically determined with a small margin.
(define scatter-plot
  (lambda (xs ys (color 'red))
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

;; Plots a line based on the linear equation parameters `theta`.
;; `xs`: A vector of x-values used to determine the plot's x-range.
;; `theta`: A list or vector containing the slope (m) and intercept (c) of the line.
;; `color`: The color of the line.
(define line-plot
  (lambda (xs theta (color 'red))
    (let ((xmin (- (vmin xs) 0.1))
          (xmax (+ (vmax xs) 0.1)))
      (plot (function
             (uv/line-eq theta)
             xmin
             xmax
             #:color color)))))


;; Creates a plot that combines a scatter plot of data points and a line plot.
;; `xs`, `ys`: Vectors for the scatter plot data.
;; `theta`: Parameters for the line equation.
;; `color-p`: Color for the points.
;; `color-l`: Color for the line.
;; `label`: An optional label for the line.
;; `filename`: An optional filename to save the plot. If empty, displays the plot.
(define line-scatter-plot
  (lambda (xs ys theta color-p color-l (label "") (filename #f))
    (let ((xm (- (vmin xs) 0.1))
          (xM (+ (vmax xs) 0.1))
          (ym (- (vmin ys) 0.1))
          (yM (+ (vmax ys) 0.1)))
      (plot
       (list
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
         #:color color-l
         #:label label))
       #:out-file filename))))

;; Saves a combined scatter and line plot to a JPEG file.
;; `xs`, `ys`: Vectors for the scatter plot data.
;; `theta`: Parameters for the line equation.
;; `color-p`: Color for the points.
;; `color-l`: Color for the line.
;; `filename`: The path to save the output JPEG file.
;; `label`: An optional label for the line.
(define line-scatter-plot-file
  (lambda (xs ys theta color-p color-l filename (label ""))
    (let ((xm (- (vmin xs) 0.1))
          (xM (+ (vmax xs) 0.1))
          (ym (- (vmin ys) 0.1))
          (yM (+ (vmax ys) 0.1)))
      (plot-file
       (list
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
         #:color color-l
         #:label label))
       filename
       'jpeg))))
