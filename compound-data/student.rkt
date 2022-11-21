;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname student) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; student-starter.rkt

;; =================
;; Data definitions:

; 
; PROBLEM A:
; 
; Design a data definition to help a teacher organize their next field trip. 
; On the trip, lunch must be provided for all students. For each student, track 
; their name, their grade (from 1 to 12), and whether or not they have allergies.
; 


(define-struct student (name grade allergy))
;; Student is (make-student String Natural[1,12] Boolean)
;; interp. a student with a name, in grade[1,12] and has an allergy or not
(define S1 (make-student "Adam" 5 true))
(define S2 (make-student "Max" 11 false))

#;
(define (fn-for-student s)
  (... (student-name s)
       (student-grade s)
       (student-allergy s)))

;; Template Rules Used:
;; - Compound: 3 fields

;; =================
;; Functions:

; 
; PROBLEM B:
; 
; To plan for the field trip, if students are in grade 6 or below, the teacher 
; is responsible for keeping track of their allergies. If a student has allergies, 
; and is in a qualifying grade, their name should be added to a special list. 
; Design a function to produce true if a student name should be added to this list.
; 


;; Student -> Boolean
;; produce name of student if in grade <= 6 and has an allergy
(check-expect (add-to-list? S1) true)
(check-expect (add-to-list? S2) false)

;(define (add-to-list? s) false)         ;stub

;<template from student>

(define (add-to-list? s)
  (and (<= (student-grade s) 6)
           (student-allergy s)))

  