;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname fold-dir) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)


; 
; In this exercise you will be need to remember the following DDs 
; for an image organizer.
; 


;; =================
;; Data definitions:

(define-struct dir (name sub-dirs images))
;; Dir is (make-dir String ListOfDir ListOfImage)
;; interp. A directory in the organizer, with a name, a list
;;         of sub-dirs and a list of images.

;; ListOfDir is one of:
;;  - empty
;;  - (cons Dir ListOfDir)
;; interp. A list of directories, this represents the sub-directories of
;;         a directory.

#;
(define (fn-for-lod lod)
  (cond [(empty? lod) (...)]
        [else
         (... (fn-for-dir (first lod))
              (fn-for-lod (rest lod)))]))

;; ListOfImage is one of:
;;  - empty
;;  - (cons Image ListOfImage)
;; interp. a list of images, this represents the sub-images of a directory.
;; NOTE: Image is a primitive type, but ListOfImage is not.

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-img (first loi))
              (fn-for-loi (rest loi)))]))

(define I1 (square 10 "solid" "red"))
(define I2 (square 12 "solid" "green"))
(define I3 (rectangle 13 14 "solid" "blue"))
(define D4 (make-dir "D4" empty (list I1 I2)))
(define D5 (make-dir "D5" empty (list I3)))
(define D6 (make-dir "D6" (list D4 D5) empty))

#;
(define (fn-for-dir dir)
  (... (dir-name dir)
       (fn-for-lod ...)
       (fn-for-loi ...)))

;; =================
;; Functions:

; 
; PROBLEM A:
; 
; Design an abstract fold function for Dir called fold-dir. 
; 


; (String Y Z -> X) (X Y -> Y) (Image Z -> Z) Y Z Dir -> X
; Fold function for dir
(check-expect (fold-dir make-dir cons cons empty empty D4) D4)

(define (fold-dir c1 c2 c3 b1 b2 dir)
  (local [(define (fn-for-dir dir)
            (c1 (dir-name dir)                          ; Dir -> X
                (fn-for-lod (dir-sub-dirs dir))
                (fn-for-loi (dir-images dir))))
          
          (define (fn-for-lod lod)                      ; (listof Dir) -> Y
            (cond [(empty? lod) b1]
                  [else
                   (c2 (fn-for-dir (first lod))
                       (fn-for-lod (rest lod)))]))

          (define (fn-for-loi loi)                      ; (listof Image) -> Z
            (cond [(empty? loi) b2]
                  [else
                   (c3 (first loi)
                       (fn-for-loi (rest loi)))]))]
    (fn-for-dir dir)))          


; 
; PROBLEM B:
; 
; Design a function that consumes a Dir and produces the number of 
; images in the directory and its sub-directories. 
; Use the fold-dir abstract function.
; 


; Dir -> Number
; consumes dir and produce number of all images in in dir & its subdirs
(check-expect (num-of-imgs D4) 2)
(check-expect (num-of-imgs D6) 3)

(define (num-of-imgs dir)
  (local [(define (c1 n rlod rloi) (+ rlod rloi))
          (define (c2 rdir rlod) (+ rdir rlod))
          (define (c3 img rloi) (+ 1 rloi))]
    (fold-dir c1 c2 c3 0 0 dir)))


; 
; PROBLEM C:
; 
; Design a function that consumes a Dir and a String. The function looks in
; dir and all its sub-directories for a directory with the given name. If it
; finds such a directory it should produce true, if not it should produce false. 
; Use the fold-dir abstract function.
; 


; Dir String -> Boolean
; consumes a dir and string and produce true if a dir with string name is found
(check-expect (find-dir? D4 "D4") true)
(check-expect (find-dir? D5 "D4") false)
(check-expect (find-dir? D6 "D4") true)

(define (find-dir? dir str)
  (local [(define (c1 n rlod rloi) (or (string=? n str)
                                       rlod))
          (define (c2 rdir rlod) (or rdir rlod))
          (define (c3 img rloi) false)]
    (fold-dir c1 c2 c3 false false dir)))

; 
; PROBLEM D:
; 
; Is fold-dir really the best way to code the function from part C? Why or 
; why not?
; 



; No. fold-dir will traverse the whole tree even if it found the desired dir.