;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname create-image) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(above (rectangle 400 80 "solid" "red")
       (overlay (star 40 "solid" "gray")
        (rectangle 400 80 "solid" "white"))
       (rectangle 400 80 "solid" "black"))