;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname foo-evaluation) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(if (string=? (substring "abc" 0 1) "a")
    (string-append "abc" "a")
    "abc")

(if (string=? "a" "a")
    (string-append "abc" "a")
    "abc")

(if true
    (string-append "abc" "a")
    "abc")

(string-append "abc" "a")

"abca"