;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname m04-raining-eggs-v2-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require 2htdp/image)
(require 2htdp/universe)
(require spd/tags)

(@assignment lectures/m04-raining-eggs)

(@cwl ???)

;;  Yoshi Eggs
(@htdw ListOfEgg)

;; Constants:
(@problem 1)
(define WIDTH 400)
(define HEIGHT 600)

(define MARIO
  (bitmap/url "https://cs110.students.cs.ubc.ca/lectures/m04-mario.png"))

(define MTS
  (place-image MARIO
               (/ WIDTH 2)
               (- HEIGHT (/ (image-height MARIO) 2) 5)
               (empty-scene WIDTH HEIGHT)))

(define YOSHI-EGG 
  (bitmap/url "https://cs110.students.cs.ubc.ca/lectures/m04-egg.png"))

(define FALL-SPEED 5) ;pixels  per tick
(define SPIN-SPEED 5) ;degrees per tick


;; Data Definitions:

(@htdd Egg)
(define-struct egg (x y r))
;; Egg is (make-egg Number Number Number)
;; interp. the x, y position of an egg in screen coordinates (pixels),
;;         and rotation angle in degrees

(define E1 (make-egg 100 50 23))
(define E2 (make-egg 20 30 50))

(@dd-template-rules compound) ;3 fields

(define (fn-for-egg e)
  (... (egg-x e)    ;Number
       (egg-y e)    ;Number
       (egg-r e)))  ;Number


(@htdd ListOfEgg)
;; ListOfEgg is one of:
;; - empty
;; - (cons Egg ListOfEgg)
;; interp. a list of eggs
(define LOE1 empty)
(define LOE2 (cons E1 empty))
(define LOE3 (cons E1 (cons E2 empty)))

(@dd-template-rules one-of           ;2 cases
                    atomic-distinct  ;empty
                    compound         ;cons
                    ref              ;(first loe) is Egg
                    self-ref)        ;(rest loe) is ListOfEgg

(define (fn-for-loe loe)
  (cond [(empty? loe) (...)]
        [else
         (... (fn-for-egg (first loe))
              (fn-for-loe (rest loe)))]))


;; =================
;; Functions:

(@htdf main)
(@signature ListOfEgg -> ListOfEgg)
;; start the world with ...
;; No tests for main functions

(@template-origin htdw-main)

(define (main loe)
  (big-bang loe          ; ListOfEgg
    (on-tick   tock)     ; ListOfEgg -> ListOfEgg
    (to-draw   render)   ; ListOfEgg -> Image
    (on-mouse  ...)      ; ListOfEgg Integer Integer MouseEvent -> ListOfEgg
    (on-key    ...)))    ; ListOfEgg KeyEvent -> ListOfEgg

(@htdf tock)
(@signature ListOfEgg -> ListOfEgg)
;; produce the next eggs at appropriate locations and angles
;!!!
(define (tock loe) ...)


(@htdf render)
(@signature ListOfEgg -> Image)
;; place YOSHI-EGG at appropriate location/rotation on MTS for each egg in loe
;!!!
(define (render loe) ...)
