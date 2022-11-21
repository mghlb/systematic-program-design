;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname spinning-bears) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/image)
(require 2htdp/universe)

; PROBLEM:
; 
; In this problem you will design another world program. In this program the changing 
; information will be more complex - your type definitions will involve arbitrary 
; sized data as well as the reference rule and compound data. But by doing your 
; design in two phases you will be able to manage this complexity. As a whole, this problem 
; will represent an excellent summary of the material covered so far in the course, and world 
; programs in particular.
; 
; This world is about spinning bears. The world will start with an empty screen. Clicking
; anywhere on the screen will cause a bear to appear at that spot. The bear starts out upright,
; but then rotates counterclockwise at a constant speed. Each time the mouse is clicked on the 
; screen, a new upright bear appears and starts spinning.
; 
; So each bear has its own x and y position, as well as its angle of rotation. And there are an
; arbitrary amount of bears.
; 
; To start, design a world that has only one spinning bear. Initially, the world will start
; with one bear spinning in the center at the screen. Clicking the mouse at a spot on the
; world will replace the old bear with a new bear at the new spot. You can do this part 
; with only material up through compound. 
; 
; Once this is working you should expand the program to include an arbitrary number of bears.
; 
; Here is an image of a bear for you to use: .


;; ==================
;; Constants:

(define WIDTH 400)
(define HEIGHT 400)
(define SPEED 2)

(define BEAR-IMG .)
(define MTS (empty-scene WIDTH HEIGHT))

;; ==================
;; Data Definitions:

(define-struct bear (x y a))
;; Bear is (make-bear Number[0, WIDTH] Number[0, HEIGHT] Integer)
;; interp. a bear (x) & (y) coordinates and rotating angle (a)
(define BR1 (make-bear 0 0 0))                        ;bear at upper left
(define BR2 (make-bear (/ WIDTH 2) (/ HEIGHT 2) 0))   ;bear upright at middle of screen

#;
(define (fn-for-bear br)
  (... (bear-x br)
       (bear-y br)
       (bear-a br)))

;; Template rules used:
;; - compound: 3 fields

;; ListOfBears is one of:
;; - empty
;; - (cons Bear ListOfBears)
;; interp. a list of bears
(define LOB1 empty)
(define LOB2 (cons BR1 (cons BR2 empty)))

#;
(define (fn-for-lob lob)
  (cond [(empty? lob) (...)]
        [else
         (... (fn-for-bear (first lob))
              (fn-for-lob  (rest lob)))]))

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Bear ListOfBears)
;; - reference: (first lob) is Bear
;; - self-reference: (rest lob) is ListOfBears

;; ==================
;; Functions:

;; ListOfBears -> ListOfBears
;; start the world with (main empty)

(define (main lob)
  (big-bang lob                ;ListOfBears
    (on-tick spin-bears)       ;ListOfBears -> ListOfBears
    (to-draw render-bears)     ;ListOfBears -> Image
    (on-mouse handle-mouse)))  ;ListOfBears Integer Integer MouseEvent -> ListOfBears

;; ListOfBears -> ListOfBears
;; spin bears by degree SPEED
(check-expect (spin-bears LOB1) empty)
(check-expect (spin-bears LOB2) (cons (make-bear 0 0 (+ 0 SPEED)) (cons (make-bear (/ WIDTH 2) (/ HEIGHT 2) (+ 0 SPEED)) empty)))

;(define (spin-bears lob) empty)          ;stub

(define (spin-bears lob)
  (cond [(empty? lob) empty]
        [else
         (cons (spin-bear  (first lob))
               (spin-bears (rest lob)))]))

;; ListOfBears -> Image
;; produce image of bears
(check-expect (render-bears LOB1) MTS)
(check-expect (render-bears LOB2) (place-image (rotate 0 BEAR-IMG) 0 0
                                               (place-image (rotate (bear-a BR2) BEAR-IMG) (/ WIDTH 2) (/ HEIGHT 2) MTS)))

;(define (render-bears lob) MTS)          ;stub

(define (render-bears lob)
  (cond [(empty? lob) MTS]
        [else
              (render-bear  (first lob)
              (render-bears (rest lob)))]))

;; Bear -> Bear
;; spin a bear counterclockwise
(check-expect (spin-bear (make-bear 15 30 0)) (make-bear 15 30 (+ 0 SPEED)))
(check-expect (spin-bear (make-bear 15 30 90)) (make-bear 15 30 (+ 90 SPEED)))

;(define (spin-bear br) br)            ;stub

(define (spin-bear br)
  (make-bear (bear-x br)
             (bear-y br)
             (+ SPEED (bear-a br))))

;; Bear -> Image
;; render the image of a bear
(check-expect (render-bear BR1 MTS) (place-image BEAR-IMG 0 0 MTS))
(check-expect (render-bear BR2 MTS) (place-image BEAR-IMG 200 200 MTS))

;(define (render-bear br) MTS)               ;stub

(define (render-bear br img)
  (place-image (rotate (modulo (bear-a br) 360) BEAR-IMG) (bear-x br) (bear-y br) img))

;; Bear Integer Integer Mousevent -> Bear
;; produce bear at x & y coordinates
(check-expect (handle-mouse empty 5 4 "button-down") (cons (make-bear 5 4 0) empty))
(check-expect (handle-mouse empty 5 4 "move") empty)

;(define (handle-mouse lob x y me) empty)

(define (handle-mouse lob x y me)
  (cond [(mouse=? me "button-down") (cons (make-bear x y 0) lob)]
        [else lob]))