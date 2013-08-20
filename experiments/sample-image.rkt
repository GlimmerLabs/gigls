#lang racket

;;; gigls/experiments/sample-image
;;;   A quick sample image

(require gigls/unsafe)

(define image (image-show (image-new 200 200)))
(context-set-fgcolor! "blue")
(image-draw-line! image 0 0 100 100)
(image-select-ellipse! image REPLACE 10 20 30 40)
(context-set-fgcolor! "red")
(image-fill! image)
(image-select-nothing! image)
