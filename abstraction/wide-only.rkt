;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname wide-only) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; 
; PROBLEM:
; 
; Use the built in version of filter to design a function called wide-only 
; that consumes a list of images and produces a list containing only those 
; images that are wider than they are tall.
; 


(check-expect (wide-only empty) empty)
(check-expect (wide-only (list (rectangle 40 20 "solid" "white")
                                     (rectangle 20 20 "solid" "white")
                                     (rectangle 10 20 "solid" "white")
                                     (rectangle 10 5 "solid" "white"))) (list (rectangle 40 20 "solid" "white")
                                                                             (rectangle 10 5 "solid" "white")))
                                                                               
; (listof Image) -> (listof Image)
; consume loi and produce loi of wider images only

(define (wide-only loe)
  (local [(define (wider? i) (> (image-width i) (image-height i)))]
    (filter wider? loe)))