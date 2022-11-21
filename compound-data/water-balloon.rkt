;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname water-balloon-s) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; water-balloon-starter.rkt

; PROBLEM:
; 
; In this problem, we will design an animation of throwing a water balloon.  
; When the program starts the water balloon should appear on the left side 
; of the screen, half-way up.  Since the balloon was thrown, it should 
; fly across the screen, rotating in a clockwise fashion. Pressing the 
; space key should cause the program to start over with the water balloon
; back at the left side of the screen. 
; 
; NOTE: Please include your domain analysis at the top in a comment box. 
; 
; Use the following images to assist you with your domain analysis:
; 
; 
; 1)
; 2).
; .
; 3)
; .
; 4)
; 
; .
;     
; 
; Here is an image of the water balloon:
; (define WATER-BALLOON.)
; 
; 
; 
; NOTE: The rotate function wants an angle in degrees as its first 
; argument. By that it means Number[0, 360). As time goes by your balloon 
; may end up spinning more than once, for example, you may get to a point 
; where it has spun 362 degrees, which rotate won't accept. 
; 
; The solution to that is to use the modulo function as follows:
; 
; (rotate (modulo ... 360) (text "hello" 30 "black"))
; 
; where ... should be replaced by the number of degrees to rotate.
; 
; NOTE: It is possible to design this program with simple atomic data, 
; but we would like you to use compound data.


(require 2htdp/image)
(require 2htdp/universe)

;; Throwing water balloon

;; =================
;; Constants:

(define WATER-BALLOON.)

(define WIDTH 400)
(define HEIGHT 200)

(define CTR-Y (/ HEIGHT 2))

(define MTS (empty-scene WIDTH HEIGHT))

;; =================
;; Data definitions:
(define-struct balloon (x a))
;; Balloon is (make-balloon Number Number)
;; state of thrown balloon where:
;;     x is its coordinate
;;     a is its angle
(define B1 (make-balloon 20 5))
(define B2 (make-balloon 10 2))

#;
(define (fn-for-balloon b)
         (... (b-x b)
              (b-a b)))

;; Template Rules Used:
;; Compound: 2 fields

;; =================
;; Functions:

;; Balloon -> Balloon
;; start the world with (main (make-balloon 0 0))
(define (main b)
  (big-bang b                      ; Balloon
            (on-tick next-b)       ; Balloon -> Balloon
            (to-draw render-b)     ; Balloon -> Image
            (on-key  reset-b)))    ; Balloon KeyEvent -> Balloon

;; Balloon -> balloon
;; advance balloon
(check-expect (next-b (make-balloon 1 10)) (make-balloon (+ 1 1) (+ 3 10)))

;(define (next-b b) b)     ;stub

;<template from balloon>

(define (next-b b)
  (make-balloon (+ (balloon-x b) 1)
                (+ (balloon-a b) 3)))


;; Balloon -> Image
;; render image of rotated balloon
(check-expect (render-b (make-balloon 1 10)) (place-image (rotate 10 WATER-BALLOON) 1 CTR-Y MTS))
(check-expect (render-b (make-balloon 20 361)) (place-image (rotate 1 WATER-BALLOON) 20 CTR-Y MTS))

;(define (render-b b) MTS)     ;stub

;<template from balloon>

(define (render-b b)
         (place-image (rotate (modulo (balloon-a b) 360) WATER-BALLOON) (balloon-x b) CTR-Y MTS))

;; Balloon KeyEvent -> Balloon
;; reset balloon position when spacebar is pressed
(check-expect (reset-b (make-balloon WIDTH 30) " ") (make-balloon 0 0))
(check-expect (reset-b (make-balloon (/ WIDTH 2) 0) " ") (make-balloon 0 0))
(check-expect (reset-b (make-balloon WIDTH 30) "f") (make-balloon WIDTH 30))

;(define (reset-b b ke) b)     ;stub

;<template from balloon>

(define (reset-b b ke)
        (cond [(key=? " " ke) (make-balloon 0 0)]
              [else b]))