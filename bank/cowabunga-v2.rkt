;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cowabunga-v2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require spd/tags)

(@assignment bank/compound-l3)
(@cwl ???)

;;   cowabunga-starter.rkt  problem statement
;;   cowabunga-v0.rkt       has constants
;;   cowabunga-v1.rkt       has data definition
;; > cowabunga-v2.rkt       has main function, wish list entries
;;   cowabunga-v3.rkt       has next-cow
;;   cowabunga-v4.rkt       has render-cow
;;   cowabunga-v5.rkt       has handle-key
;;   cowabunga-v6.rkt       waddles

(@problem 1)

;; A cow, meandering back and forth across the screen.

(@htdw Cow)

;; =================
;; Constants:

(define WIDTH  400)
(define HEIGHT 200)


(define CTR-Y (/ HEIGHT 2)) 


(define RCOW
  (bitmap/url
   "https://cs110.students.cs.ubc.ca/bank/rcow.png"))
(define LCOW
  (bitmap/url
   "https://cs110.students.cs.ubc.ca/bank/lcow.png"))


(define MTS (empty-scene WIDTH HEIGHT))



;; =================
;; Data definitions:

(@htdd Cow)
(define-struct cow (x dx))
;; Cow is (make-cow Natural Integer)
;; interp. (make-cow x dx) is a cow with x coordinate x and x velocity dx
;;         the x is the center of the cow
;;         x  is in screen coordinates (pixels)
;;         dx is in pixels per tick
;;         CONSTRAINT: x is in the interval [0, WIDTH]
;;         Note that since x is the center of the cow, the cow is allowed
;;         to PARIALLY leave the screen
;;
(define C1 (make-cow 10  3)) ; at 10, moving left -> right
(define C2 (make-cow 20 -4)) ; at 20, moving left <- right

(@dd-template-rules compound) ;2 fields

#;
(define (fn-for-cow c)
  (... (cow-x c)    ;Natural
       (cow-dx c))) ;Integer



;; =================
;; Functions:

(@htdf main)
(@signature Cow -> Cow)
;; called to make the cow go for a walk; start with (main (make-cow 0 3))
;; no tests for main function

(@template-origin htdw-main)

(define (main c)
  (big-bang c
    (on-tick next-cow)       ; Cow -> Cow
    (to-draw render-cow)     ; Cow -> Image
    (on-key  handle-key)))   ; Cow KeyEvent -> Cow


(@htdf next-cow)
(@signature Cow -> Cow)
;; increase cow x by dx; bounce off edges
;; !!!
(define (next-cow c) c)      ;stub


(@htdf render-cow)
(@signature Cow -> Image)
;; place appropriate cow image on MTS at (cow-x c) and CTR-Y
;; !!!
(define (render-cow c) MTS)  ;stub  


(@htdf handle-key)
(@signature Cow KeyEvent -> Cow)
;; reverse direction of cow travel when space bar is pressed
;; !!!
(define (handle-key c ke) c) ;stub

