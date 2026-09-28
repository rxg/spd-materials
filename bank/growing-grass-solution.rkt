;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname growing-grass-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require spd/tags)

(@assignment bank/compound-p5)
(@cwl ???)

(@problem 1)
;; Design a world program as follows:
;; 
;; The world starts off with a piece of grass waiting to grow. As time passes, 
;; the grass grows upwards. Pressing any key cuts the current strand of 
;; grass to 0, allowing a new piece to grow to the right of it.
;;
;; Starting display:
;;
;; https://cs110.students.cs.ubc.ca/bank/grass-strt.png
;;
;; After a few seconds:
;; 
;; https://cs110.students.cs.ubc.ca/bank/grass-sec.png
;;
;; After a few more seconds:
;;
;; https://cs110.students.cs.ubc.ca/bank/grass-sec2.png
;;
;; Immediately after pressing any key:
;;
;; https://cs110.students.cs.ubc.ca/bank/grass-key.png
;;
;; A few more seconds after pressing any key:
;;
;; https://cs110.students.cs.ubc.ca/bank/grass-sec3.png
;;
;; NOTE 1: Remember to follow the HtDW recipe! Be sure to do a proper domain 
;; analysis before starting to work on the code file.


;; Growing and replanting grass

(@htdw Grass)

;; =================
;; Constants:

(define WIDTH 600)
(define HEIGHT 400)

(define GRASS-COLOR "green")
(define GRASS-WIDTH 10)
(define GRASS-Y (sub1 HEIGHT))
(define GRASS-SPACING 10)

(define SUN (overlay (circle 45 "solid" "yellow")
                     (radial-star 20 70 50 "solid" "orange")))
(define MTS (overlay/align "left" 
                           "top"
                           SUN
                           (rectangle WIDTH HEIGHT "solid" "sky blue")))

(define GROW-SPEED 1)



;; =================
;; Data definitions:

(@htdd Grass)
(define-struct grass (x h))
;; Grass is (make-grass Number Number)
;; interp. a grass at some x-position, in pixels, that is grass-h pixels tall
(define G1 (make-grass 0 0))
(define G2 (make-grass 10 4))

(@dd-template-rules compound) ;2 fields

#;
(define (fn-for-grass g)
  (... (grass-x g)       ;Number
       (grass-h g)))     ;Number



;; =================
;; Functions:

(@htdf main)
(@signature Grass -> Grass)
;; starts the world call (main (make-grass 10 0))
;; no tests for main function

(@template-origin htdw-main)

(define (main g)
  (big-bang g                         ; Grass
    (on-tick grow-grass)      ; Grass -> Grass
    (to-draw place-grass)     ; Grass -> Image
    (on-key replant-grass)))  ; Grass KeyEvent -> Grass


(@htdf grow-grass)
(@signature Grass -> Grass)
;; increase g's height by GROW-SPEED
(check-expect (grow-grass (make-grass 0 1))
              (make-grass 0 (+ 1 GROW-SPEED)))

;(define (grow-grass g) G2)    ;stub

(@template-origin Grass)

(@template
 (define (grow-grass g)
   (... (grass-x g)
        (grass-h g))))

(define (grow-grass g)
  (make-grass (grass-x g)
              (+ GROW-SPEED (grass-h g))))


(@htdf place-grass)
(@signature Grass -> Image)
;; place the grass at grass-x, GRASS-Y with height grass-h
(check-expect (place-grass (make-grass 1 20))
              (place-image/align (rectangle GRASS-WIDTH 20 "solid" GRASS-COLOR)
                                 1 GRASS-Y
                                 "center" "bottom"
                                 MTS))

;(define (place-grass g) MTS)   ;stub

(@template-origin Grass)

(@template
 (define (place-grass g)
   (... (grass-x g)
        (grass-h g))))

(define (place-grass g)
  (place-image/align (rectangle GRASS-WIDTH
                                (grass-h g)
                                "solid"
                                GRASS-COLOR)
                     (grass-x g) GRASS-Y
                     "center" "bottom"
                     MTS))


(@htdf replant-grass)
(@signature Grass KeyEvent -> Grass)
;; replant new grass at (grass-x + GRASS-SPACING)
(check-expect (replant-grass (make-grass 30 40) " ") (make-grass 40 0))

;(define (replant-grass g ke) G1)    ;stub

(@template-origin Grass)

(@template
 (define (replant-grass g ke)
   (... (grass-x g)
        (grass-h g)
        ke)))

(define (replant-grass g ke)
  (make-grass (+ (grass-x g) GRASS-SPACING) 0))
