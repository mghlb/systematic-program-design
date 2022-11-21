;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HtDDQuiz) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; HtDD Design Quiz

;; Age is Natural
;; interp. the age of a person in years
(define A0 18)
(define A1 25)

#;
(define (fn-for-age a)
  (... a))

;; Template rules used:
;; - atomic non-distinct: Natural


; Problem 1:
; 
; Consider the above data definition for the age of a person.
; 
; Design a function called teenager? that determines whether a person
; of a particular age is a teenager (i.e., between the ages of 13 and 19).


;; Age -> Boolean
;; Return true if 13 >= age <= 19
(check-expect (teenager? 15) true)
(check-expect (teenager? 20) false)

;(define (teenager? 12) false)   ;stub

;<Template from Age>

(define (teenager? a)
  (<= 13 a 19))


; Problem 2:
; 
; Design a data definition called MonthAge to represent a person's age
; in months.


;; MonthAge is Natural
;; interp. the age of a person in months
(define MA0 (* 18 12))
(define MA1 (* 25 12))

#;
(define (fn-for-months-age ma)
  (... ma))

; Template Rules Used:
; atomic distinct: Natural


; Problem 3:
; 
; Design a function called months-old that takes a person's age in years 
; and yields that person's age in months.
; 


;; Age -> MonthAge
;; return age in months from years
(check-expect (months-old 10) 120)
(check-expect (months-old 3) 36)

;(define (months-old a) ma)   ;stub

;<Template from MonthsAge>

(define (months-old ma)
  (* 12 ma))


; Problem 4:
; 
; Consider a video game where you need to represent the health of your
; character. The only thing that matters about their health is:
; 
;   - if they are dead (which is shockingly poor health)
;   - if they are alive then they can have 0 or more extra lives
; 
; Design a data definition called Health to represent the health of your
; character.
; 
; Design a function called increase-health that allows you to increase the
; lives of a character.  The function should only increase the lives
; of the character if the character is not dead, otherwise the character
; remains dead.



;; Data Definition

;; Health is one of:
;; - "dead"
;; - Natural
;; interp. "dead" means dead player, a number represents the players lives
(define H1 "dead")
(define H2 2)

#;
(define (fn-for-health h)
  (cond [(string? h) (...)]
        [else (... h)]))

;; Template Rules Used:
;; one of: 2 cases
;; - atomic non-distinct: "dead"
;; - atomic non-distinct: Natural


;; Function Defintion

;; Health -> Health
;; increase lives of a character if not dead
(check-expect (increase-health "dead") "dead")
(check-expect (increase-health 0) 1)

;(define (increase-health h) 0)    ;stub

;<Template from Health>

(define (increase-health h)
  (cond [(string? h) "dead"]
        [else (+ 1 h)]))