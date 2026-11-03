;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname m09-search-maze-2w-v2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(@assignment lectures/m09-search-maze-2w)

(@cwl ???) ;replace ??? with your cwl

;; maze-2w-v2.rkt

;; Solve simple square mazes

;; Data definitions:
(@problem 1)
(@htdd Maze)
;; Maze is (listof Boolean)
;; interp. a square maze
;;         each side length is (sqrt (length <maze>))
;;         true  (aka #t) means open, can move through this
;;         false (aka #f) means a wall, cannot move into or through a wall
;; CONSTRAINT: maze is square, so (sqrt (length <maze>)) is a Natural
;;
(define O true)  ;Open
(define W false) ;Wall

(define M0
  (list O W W W
        W W W W
        W W W W
        W W W W))

(define M1
  (list O W W W W
        O O W O O
        W O W W W 
        O O W W W
        O O O O O))

(define M2
  (list O O O O O
        O W W W O
        O W W W O
        O W W W O
        O W W W O))

(define M3             ;forces backtracking in this solver
  (list O O O O O
        O W W W W
        O W W W W
        O W W W W 
        O O O O O))

(define M4
  (list O O O O O
        O W W W O
        O W O O O
        O W O W W
        W W O O O))

;; <template omitted intentionally>

(@htdd Position)

(define-struct pos (x y))
;; Position is (make-pos Natural Natural)
;; interp. an x, y position in the maze.
;;         0, 0 is upper left.
;;         a position is only valid for a given maze if:
;;            - (<= 0 x (sub1 <size>))
;;            - (<= 0 y (sub1 <size>))
;;            - there is a true in the given cell
;; 
(define P0 (make-pos 0 0)) ;upper left  in 4x4 maze
(define P1 (make-pos 3 0)) ;upper right  "  "   "
(define P2 (make-pos 0 3)) ;lower left   "  "   "
(define P3 (make-pos 3 3)) ;lower right  "  "   "

(define (fn-for-pos p)
  (... (pos-x p)
       (pos-y p)))

;; Functions:
(@problem 2)
(@htdf solvable?)
(@signature Maze -> Boolean)
;; produce true if maze is solvable, false otherwise
;; CONSTRAINT: maze has an empty square at least in the upper left
(check-expect (solvable? M1) #t)
(check-expect (solvable? M2) #t)
(check-expect (solvable? M3) #t) 
(check-expect (solvable? M4) #f)

(define (solvable? m) #f)


(@htdf mref)
(@signature Maze Position -> Boolean)
;; produce contents of given square in given maze
(check-expect (mref (list #t #f #f #f) (make-pos 0 0)) #t)
(check-expect (mref (list #t #t #f #f) (make-pos 0 1)) #f)

(@template-origin Position)

(define (mref m p)
  (local [(define s (sqrt (length m))) ;each side length
          (define x (pos-x p))
          (define y (pos-y p))]
    (cond [(not (<= 0 x (sub1 s)))
           (error 'mref "x value " x " must be within [0, " (sub1 s) "].")]
          [(not (<= 0 y (sub1 s)))
           (error 'mref "y value " y " must be within [0, " (sub1 s) "].")]
          [else
           (list-ref m (+ x (* y s)))])))



(require 2htdp/image)

(define SQUARE-SZ 20)
(define GOAL-SZ 8)
(define DOT-SZ 6)

(define DOT (circle DOT-SZ "solid" "black"))
(define RED (circle DOT-SZ "solid" "red"))

(define OS (square SQUARE-SZ "outline" "white"))
(define WS (square SQUARE-SZ "solid" "black"))

(@htdf render-maze-w/path)
(@signature Maze -> Image)
;; produce simple rendering of MAZE using above constants
(check-expect
 (render-maze-w/path (list O W O O)
                     (list (make-pos 0 0) (make-pos 0 1)))
 (place-image
  RED (* .5 SQUARE-SZ) (* .5 SQUARE-SZ)
  (place-image
   OS (* .5 SQUARE-SZ) (* .5 SQUARE-SZ)
   (place-image
    WS (* 1.5 SQUARE-SZ) (* 0.5 SQUARE-SZ)
    (place-image
     RED (* 0.5 SQUARE-SZ) (* 1.5 SQUARE-SZ)
     (place-image
      OS (* 0.5 SQUARE-SZ) (* 1.5 SQUARE-SZ)
      (place-image
       OS (* 1.5 SQUARE-SZ) (* 1.5 SQUARE-SZ)
       (square (* 2 SQUARE-SZ) "outline" "black"))))))))
                           

(define (render-maze-w/path m path)
  (local [(define rank (sqrt (length m)))

          (define bkgrd (square (* rank SQUARE-SZ) "outline" "black"))

          ;; foldr w/ extra accumulator
          ;; i is Integer; index number of (first lov) in original m
          (define (fold lov i img)
            (cond [(empty? lov) img]
                  [else
                   ;; be prepared to put RED over wall because the path
                   ;; might be buggy
                   (place-image (if (member (make-pos (remainder i rank)
                                                      (quotient i rank))
                                            path)
                                    (overlay RED (if (first lov) OS WS))
                                    (if (first lov) OS WS))
                                (i->x i)
                                (i->y i)
                                (fold (rest lov) (add1 i) img))]))

          (define (i->x i)
            (floor (+ (* (remainder i rank) SQUARE-SZ) (/ SQUARE-SZ 2))))
          (define (i->y i)
            (floor (+ (* (quotient  i rank) SQUARE-SZ) (/ SQUARE-SZ 2))))]
    
    (fold m 0 bkgrd)))
