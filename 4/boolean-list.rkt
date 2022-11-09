;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname boolean-list) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; boolean-list-starter.rkt

;; =================
;; Data definitions:

; 
; PROBLEM A:
; 
; Design a data definition to represent a list of booleans. Call it ListOfBoolean. 
; 


;; ListOfBoolean is one of:
;; - empty
;; - cons (Boolean ListOfBoolean)
;; interp. a list of true & false values
(define LOB1 empty)
(define LOB2 (cons true (cons false empty)))
(define LOB3 (cons true (cons true (cons true empty))))

#;
(define (fn-for-lob lob)
  (cond [(empty? lob) (...)]
        [else
         (... (first lob)
             (fn-for-lob (rest lob)))]))

;; Template rules used:
;; one of: 2 cases
;; - empty
;; - compound: cons (Boolean ListOfBooleans)
;; - self-refernce: (rest lob) is ListOfBooleans

;; =================
;; Functions:

; 
; PROBLEM B:
; 
; Design a function that consumes a list of boolean values and produces true 
; if every value in the list is true. If the list is empty, your function 
; should also produce true. Call it all-true?
; 


;; ListOfBooleans -> Booleans
;; produce true if all values in list are true or list is empty
(check-expect (all-true? LOB1) true)
(check-expect (all-true? LOB2) false)
(check-expect (all-true? LOB3) true)

;(define (all-true? lob) true)       ;stub

(define (all-true? lob)
  (cond [(empty? lob) true]
        [else
         (and (first lob)
              (all-true? (rest lob)))]))