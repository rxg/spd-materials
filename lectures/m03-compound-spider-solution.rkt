;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname m03-compound-spider-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)
(require 2htdp/universe)

(@assignment lectures/m03-compound-spider)

;; Spider, goes down screen with thread, changes dir on space and at top/bottom

(@htdw Spider)

;; =================
;; Constants:

(define WIDTH 400)
(define HEIGHT 600)

(define CTR-X (/ WIDTH 2))

(define SPIDER-RADIUS 10)

(define TOP (+ 0        SPIDER-RADIUS))
(define BOT (- HEIGHT 1 SPIDER-RADIUS))

(define MID (/ HEIGHT 2))

(define SPIDER-IMAGE (circle SPIDER-RADIUS "solid" "black"))

(define MTS (empty-scene WIDTH HEIGHT))


;; =================
;; Data definitions:
(@problem 1)
(@htdd Spider)
(define-struct spider (y dy))
;; Spider is (make-spider Number Number)
;; interp. y is spider's vertical position in screen coordinates (pixels)
;;         dy is velocity in pixels per tick, + is down, - is up
;; CONSTRAINT: to be visible, must be in
;;             [TOP, BOT] which is [SPIDER-RADIUS, HEIGHT - 1 - SPIDER-RADIUS]
(define S-TOP-D (make-spider TOP  3))   ;top going down
(define S-MID-D (make-spider MID  2))   ;middle going down
(define S-MID-U (make-spider MID -3))   ;middle going up
(define S-BOT-U (make-spider BOT -3))   ;bottom going up


(@dd-template-rules compound);2 fields

(define (fn-for-spider s)
  (... (spider-y s)
       (spider-dy s)))

;; =================
;; Functions:

(@htdf main)
(@signature Spider -> Spider)
;; start the world with (main S-TOP-D)
;; no tests for htdw-main template

(@template-origin htdw-main)

;; no @template for htdw-main template

(define (main s)
  (big-bang s                    ; Spider
    (on-tick   tock)             ; Spider -> Spider
    (to-draw   render)           ; Spider -> Image
    (on-key    reverse-spider))) ; Spider KeyEvent -> Spider


(@htdf tock)
(@signature Spider -> Spider)
;; move spider by dy, except change direction at bottom and top edges

;; top edge: doesn't touch TOP, just touches TOP, tries to go past TOP
(check-expect (tock (make-spider (+ TOP 3) -2)) (make-spider (+ TOP 1) -2))
(check-expect (tock (make-spider (+ TOP 3) -3)) (make-spider    TOP     3))
(check-expect (tock (make-spider (+ TOP 3) -4)) (make-spider    TOP     4))

;; bottom edge: doesn't touch BOT, just touches BOT, tries to go past BOT
(check-expect (tock (make-spider (- BOT 3) 2))  (make-spider (- BOT 1)  2))
(check-expect (tock (make-spider (- BOT 3) 3))  (make-spider    BOT    -3))
(check-expect (tock (make-spider (- BOT 3) 4))  (make-spider    BOT    -4))

;(define (tock s) s) ;stub

(@template-origin Spider)

(@template
 (define (tock s)
   (... (spider-y s)
        (spider-dy s))))

(define (tock s)
  (cond [(<= (+ (spider-y s) (spider-dy s)) TOP)
         (make-spider TOP (- (spider-dy s)))]
        [(>= (+ (spider-y s) (spider-dy s)) BOT)
         (make-spider BOT (- (spider-dy s)))]
        [else
         (make-spider (+ (spider-y s) (spider-dy s))
                      (spider-dy s))]))


(@htdf render)
(@signature Spider -> Image)
;; place SPIDER-IMAGE and thread image on MTS, at spider's y coordinate
(check-expect (render S-MID-D)
              (add-line (place-image SPIDER-IMAGE CTR-X (spider-y S-MID-D) MTS)
                        CTR-X 0
                        CTR-X (spider-y S-MID-D)
                        "black"))

(check-expect (render (make-spider 36 -10))
              (add-line (place-image SPIDER-IMAGE CTR-X 36 MTS)
                        CTR-X 0
                        CTR-X 36
                        "black"))

;(define (render s) MTS) ;stub

(@template-origin Spider)

(@template
 (define (render s)
   (... (spider-y s)
        (spider-dy s))))

(define (render s)
  (add-line (place-image SPIDER-IMAGE
                         CTR-X
                         (spider-y s)
                         MTS)
            CTR-X 0
            CTR-X (spider-y s)
            "black"))


(@htdf reverse-spider)
(@signature Spider KeyEvent -> Spider)
;; change direction of spider on space; remain unchanged for other keys
(check-expect (reverse-spider (make-spider 100  2) " ") (make-spider 100 -2))
(check-expect (reverse-spider (make-spider 200 -3) " ") (make-spider 200  3))
(check-expect (reverse-spider (make-spider 100  2) "a") (make-spider 100  2))

;(define (reverse-spider s ke) s) ;stub

(@template-origin KeyEvent ;using large enumeration rule
                  Spider)  ;accessing spider fields

(@template
 (define (reverse-spider s ke)
   (cond [(key=? ke " ") (... (spider-y s) (spider-dy s))]
         [else
          (... (spider-y s) (spider-dy s) ke)])))


(define (reverse-spider s ke)
  (cond [(key=? ke " ") (make-spider (spider-y s) (- (spider-dy s)))]
        [else s]))
