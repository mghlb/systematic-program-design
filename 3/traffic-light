;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname traffic-light) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; traffic-light-starter.rkt

; 
; PROBLEM:
; 
; Design an animation of a traffic light. 
; 
; Your program should show a traffic light that is red, then green, 
; then yellow, then red etc. For this program, your changing world 
; state data definition should be an enumeration.
; 
; Here is what your program might look like if the initial world 
; state was the red traffic light:
; .
; Next:
; .
; Next:
; .
; Next is red, and so on.
; 
; To make your lights change at a reasonable speed, you can use the 
; rate option to on-tick. If you say, for example, (on-tick next-color 1) 
; then big-bang will wait 1 second between calls to next-color.
; 
; Remember to follow the HtDW recipe! Be sure to do a proper domain 
; analysis before starting to work on the code file.
; 
; Note: If you want to design a slightly simpler version of the program,
; you can modify it to display a single circle that changes color, rather
; than three stacked circles. 
; 


(require 2htdp/image)
(require 2htdp/universe)

;; Traffic lights

;; =================
;; Constants:

(define RADIUS 20)    ;size of light ball
(define SPACING 5)    ;space between lights

(define BACKGROUND (rectangle (+ (* 2 SPACING) (* 2 RADIUS))
                              (+ (* 4 SPACING) (* 6 RADIUS))
                              "solid" "black"))
                              
(define SPACE (square SPACING "solid" "black"))

(define RED
  (overlay (above SPACE
                  (circle RADIUS "solid" "red")
                  SPACE
                  (circle RADIUS "outline" "yellow")
                  SPACE
                  (circle RADIUS "outline" "green")
                  SPACE)
           BACKGROUND))

(define YELLOW
  (overlay (above SPACE
                  (circle RADIUS "outline" "red")
                  SPACE
                  (circle RADIUS "solid" "yellow")
                  SPACE
                  (circle RADIUS "outline" "green")
                  SPACE)
           BACKGROUND))

(define GREEN
  (overlay (above SPACE
                  (circle RADIUS "outline" "red")
                  SPACE
                  (circle RADIUS "outline" "yellow")
                  SPACE
                  (circle RADIUS "solid" "green")
                  SPACE)
           BACKGROUND))

;; =================
;; Data definitions:

;; Light is one of:
;; - "red"
;; - "yellow"
;; - "green"

;; interp. color of traffic light

#;
(define (fn-for-lights l)
  (cond [(string=? l "red") (...)]
        [(string=? l "yellow") (...)]
        [(string=? l "green") (...)]))

;;Template Rules Used:
;; one of: 3 cases
;;  - atomic distinct: "red"
;;  - atomic distinct: "yellow"
;;  - atomic distinct: "green"
  
;; =================
;; Functions:

;; Light -> Light
;; start the world with ...
;; 
(define (main l)
  (big-bang l                          ; Light
            (on-tick next-color 1)     ; Light -> Light
            (to-draw render-color)))   ; Light -> Image
         

;; Light -> Light
;; produce the next traffic color light
(check-expect (next-color "red") "green")
(check-expect (next-color "green") "yellow")

;(define (next-color l) "")     ;stub

;<template from Light>
(define (next-color l)
  (cond [(string=? l "red") "green"]
        [(string=? l "yellow") "red"]
        [(string=? l "green") "yellow"]))

;; Light -> Image
;; render traffic light
(check-expect (render-color "red") RED)
(check-expect (render-color "green") GREEN)

;(define (render-color l) BACKGROUND)     ;stub

;<template from Light>
(define (render-color l)
  (cond [(string=? l "red") RED]
        [(string=? l "yellow") YELLOW]
        [(string=? l "green") GREEN]))