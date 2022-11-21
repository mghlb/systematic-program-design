;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname decreasing-image) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;  PROBLEM:
;  
;  Design a function called decreasing-image that consumes a Natural n and produces an image of all the numbers 
;  from n to 0 side by side. 
;  
;  So (decreasing-image 3) should produce .


(define TEXT-SIZE 30)
(define TEXT-COLOR "blue")
(define SPACING (text " " TEXT-SIZE TEXT-COLOR))

;; Natural -> Image
;; consume natural and produce descending list of images of naturals down to 0
(check-expect (decreasing-image 0) (text "0" 30 "blue"))
(check-expect (decreasing-image 3) (beside (text "3" 30 "blue") SPACING
                                           (text "2" 30 "blue") SPACING
                                           (text "1" 30 "blue") SPACING
                                           (text "0" 30 "blue")))

;(define (decreasing-image n) empty-scene)    ;stub

(define (decreasing-image n)
  (cond [(zero? n) (text "0" TEXT-SIZE TEXT-COLOR)]
        [else
         (beside (text (number->string n) TEXT-SIZE TEXT-COLOR)
                 SPACING
                 (decreasing-image (sub1 n)))]))
