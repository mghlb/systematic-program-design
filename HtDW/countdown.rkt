;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname countdown) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; countdown-animation starter.rkt

; 
; PROBLEM:
; 
; Design an animation of a simple countdown. 
; 
; Your program should display a simple countdown, that starts at ten, and
; decreases by one each clock tick until it reaches zero, and stays there.
; 
; To make your countdown progress at a reasonable speed, you can use the 
; rate option to on-tick. If you say, for example, 
; (on-tick advance-countdown 1) then big-bang will wait 1 second between 
; calls to advance-countdown.
; 
; Remember to follow the HtDW recipe! Be sure to do a proper domain 
; analysis before starting to work on the code file.
; 
; Once you are finished the simple version of the program, you can improve
; it by reseting the countdown to ten when you press the spacebar.
; 


;; A countdown animation
;; ==============
;; Constants:

(define HEIGHT 100)
(define WIDTH 100)

(define CTR-X (/ HEIGHT 2))
(define CTR-Y (/ WIDTH 2))

(define MTS (empty-scene WIDTH HEIGHT))

(define TEXT-COLOR "black")
(define TEXT-SIZE 20)

;; ==============
;; Data defintion:

;; Countdown is a Natural
;; interp. seconds to launch
(define CD1 10)   ;countdown not started
(define CD2 5)    ;countdown in middle

#;
(define (fn-for-countdown cd)
  (... cd))

;; Template Rules Used:
;; - atomic non-distinct: Natural

;; =============
;; Functions:

;; Countdown -> Countdown

(define (main cd)
  (big-bang cd                       ; Countdown
    (on-tick advance-countdown 1)    ; Countdown -> Countdown
    (to-draw render-countdown)       ; Countdown -> Image
    (on-key handle-key)))            ; Countdown KeyEvent -> Countdown


;; Countdown -> Countdown
;; produce the next Natural
(check-expect (advance-countdown 10) 9)
(check-expect (advance-countdown 0) 0)

;(define (advance-countdown cd) )       ;stub

;<template from Countdown>

(define (advance-countdown cd)
  (cond [(= cd 0) 0]
        [else (- cd 1)]))

;; Countdown -> Image
;; produce image of countdown
(check-expect (render-countdown 10) (place-image (text "10" TEXT-SIZE TEXT-COLOR)
                                                 CTR-X CTR-Y MTS))
(check-expect (render-countdown 0) (place-image (text "0" TEXT-SIZE TEXT-COLOR)
                                                 CTR-X CTR-Y MTS))

;(define (render-countdown cd) MTS)           ;stub

;<template from Countdown>

(define (render-countdown cd)
  (place-image (text (number->string cd) TEXT-SIZE TEXT-COLOR)
                     CTR-X CTR-Y MTS))

;; Countdown KeyEvent -> Countdown
;; reset countdown when spacebar is pressed
(check-expect (handle-key 0 " ") 10)
(check-expect (handle-key 10 " ") 10)
(check-expect (handle-key 5 "x") 5)

;(define (handle-key cd ke) 0)      ;stub

;<template from Countdown>

(define (handle-key cd ke)
  (cond [(key=? ke " ") 10]
        [else cd]))