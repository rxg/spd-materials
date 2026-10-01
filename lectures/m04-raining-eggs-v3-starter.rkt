;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname m04-raining-eggs-v3-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
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


;; ===================================================

;; Functions:

(@htdf main)
(@signature ListOfEgg -> ListOfEgg)
;; start the world with (main empty)

(@template-origin htdw-main)

(define (main loe)
  (big-bang loe            ;ListOfEgg
    (state    true)
    (on-tick  next-eggs)   ;ListOfEgg -> ListOfEgg
    (to-draw  render-eggs) ;ListOfEgg -> Image
    (on-mouse handle-mouse);ListOfEgg Integer Integer MouseEvent -> ListOfEgg
    (on-key   handle-key)));ListOfEgg KeyEvent -> ListOfEgg

(@htdf next-eggs)
(@signature ListOfEgg -> ListOfEgg)
;; produce the next eggs at appropriate locations and angles
(check-expect (next-eggs empty) empty)
(check-expect (next-eggs (cons (make-egg 10 20 30) empty))
              (cons (make-egg 10 (+ 20 FALL-SPEED) (+ 30 SPIN-SPEED)) empty))

;(define (next-eggs loe) loe) ;stub

(@template-origin ListOfEgg)

(@template
 (define (next-eggs loe)
   (cond [(empty? loe) (...)]
         [else
          (... (fn-for-egg (first loe))
               (next-eggs (rest loe)))])))

(define (next-eggs loe)
  (cond [(empty? loe) empty]
        [else
         (cons (next-egg (first loe))
               (next-eggs (rest loe)))]))

(@htdf next-egg)
(@signature Egg -> Egg)
;; produce the next egg and increase y by FALL-SPEED, r by SPIN-SPEED
(check-expect (next-egg (make-egg 10 20 30))
              (make-egg 10 (+ 20 FALL-SPEED) (+ 30 SPIN-SPEED)))
(check-expect (next-egg (make-egg 11 21 31))
              (make-egg 11 (+ 21 FALL-SPEED) (+ 31 SPIN-SPEED)))

;(define (next-egg e) e) ;stub

(@template-origin Egg)

(@template
 (define (next-egg e)
   (... (egg-x e)
        (egg-y e)
        (egg-r e))))

(define (next-egg e)
  (make-egg (egg-x e)
            (+ (egg-y e) FALL-SPEED)
            (+ (egg-r e) SPIN-SPEED)))


(@htdf render-eggs)
(@signature ListOfEgg -> Image)
;; place YOSHI-EGG at appropriate location/rotation on MTS for each egg in loe
;!!!
(define (render-eggs loe) MTS) 



(@htdf handle-mouse)
(@signature ListOfEgg Integer Integer MouseEvent -> ListOfEgg)
;; add and egg at x, y with rotation 0 when the mouse is clicked
(check-expect (handle-mouse empty 10 40 "button-down")
              (cons (make-egg 10 40 0) empty))
(check-expect (handle-mouse (cons (make-egg 3 4 5) empty) 90 100 "drag")
              (cons (make-egg 3 4 5) empty))

;(define (handle-mouse loe x y me) loe) ;stub

(@template-origin MouseEvent)

(@template
 (define (handle-mouse loe x y me)
   (cond [(mouse=? me "button-down") (... loe x y)]
         [else    
          (... loe x y)])))

(define (handle-mouse loe x y me)
  (cond [(mouse=? me "button-down") (cons (make-egg x y 0) loe)] 
        [else loe]))

(@htdf handle-key)
(@signature ListOfEgg KeyEvent -> ListOfEgg)
;; on space reset to zero eggs
(check-expect (handle-key (cons E1 (cons E2 empty)) " ") empty)
(check-expect (handle-key (cons E1 empty) "a") (cons E1 empty))

;(define (handle-key loe ke) loe) ;stub

(@template-origin KeyEvent)

(@template   
 (define (handle-key loe ke)
   (cond [(key=? ke " ") (... loe)]
         [else  
          (... loe)])))

(define (handle-key loe ke)
  (cond [(key=? ke " ") empty]   
        [else loe]))
