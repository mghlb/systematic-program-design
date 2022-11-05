;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname rocket) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; rocket-starter.rkt

;; =================
;; Data definitions:

; 
; PROBLEM A:
; 
; You are designing a program to track a rocket's journey as it descends 
; 100 kilometers to Earth. You are only interested in the descent from 
; 100 kilometers to touchdown. Once the rocket has landed it is done.
; 
; Design a data definition to represent the rocket's remaining descent. 
; Call it RocketDescent.
; 


;; RocketDescent is one of:
;; - false
;; - Natural[0, 100]
;; interp. descending from 100km & false if rocket has already descended

(define RD1 100)
(define RD2 50)
(define RD3 1)
(define RD4 false)

#;
(define (fn-for-rocket-descent rd)
  (cond [(and (number? rd)
              (< 0 rd)
              (<= rd 100))
               (... rd)]
        [else (...)]))

;; Template rules used:
;; one of: 2 cases
;; - atomic non distinct: false
;; - atomic distinct: Natural[0, 100]

;; =================
;; Functions:

; 
; PROBLEM B:
; 
; Design a function that will output the rocket's remaining descent distance 
; in a short string that can be broadcast on Twitter. 
; When the descent is over, the message should be "The rocket has landed!".
; Call your function rocket-descent-to-msg.
; 


;; RocketDescent -> String
;; print remaining distance to descent & when descent is over print message "The rocket has landed!"
(check-expect (rocket-descent-to-msg 50) (number->string 50))
(check-expect (rocket-descent-to-msg 0) "The rocket has landed")
(check-expect (rocket-descent-to-msg 80) (number->string 80))

;(define (rocket-descent-to-msg rd) "")   ;stub

;<Template from RocketDescent>

(define (rocket-descent-to-msg rd)
  (cond [(and (number? rd)
              (< 0 rd)
              (<= rd 100))
               (number->string rd)]
        [else "The rocket has landed"]))
