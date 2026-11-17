;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname m09-search-triangle-solitaire-v2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(@assignment lectures/m09-search-triangle-solitaire)

(@cwl ???) ;replace ??? with your cwl

;; m09-search-triangle-solitaire-v2.rkt


;; ================================
;; Data definitions and constants
(@problem 1)
(@htdd Board)
;; Board is (listof Boolean) of length 15
;; interp.
;;   true  means a peg is in the hole
;;   false means the hole is empty

;; <examples down below>
;; <template intentionally omitted>

(@htdd Position)
;; Position is Natural
;; interp. 0 based indexes into a board
;; CONSTRAINT: is in [0, 14]

;; <examples and templates intentionally omitted>

(@htdd Jump)
(define-struct jump (from over to))
;; Jump is (make-jump Position Position Position)
;; interp.
;;  a jump on the board, with the from, to and
;;  jumped over positions

;; <examples intentionally omitted>
(define (fn-for-jump j)
  (... (jump-from j)
       (jump-over j)
       (jump-to j)))

(define POSITIONS
  ;; (list         0
  ;;              1 2
  ;;            3  4  5
  ;;          6  7  8  9
  ;;        10 11 12 13 14))
  (list 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14))

(define MTB  (build-list 15 (lambda (n) false)))


(define BD1  (build-list 15 (lambda (p) (not (= p 0))))) ;top spot open
(define BD2  (build-list 15 (lambda (p) (not (= p 4))))) ;center spot open

(define BD3  (build-list 15                              ;almost solved
                         (lambda (p)  
                           (or (= p 13)
                               (= p 14)))))

(define BD4  (build-list 15                              ;0 and 3 have pegs
                         (lambda (p)                     ;this board is
                           (or (= p 0)                   ;unsolvable
                               (= p 3)))))



(define BD1a (build-list 15                              ;possible next board
                         (lambda (p)                     ;after BD1
                           (not (or (= p 1)  
                                    (= p 3))))))

(define BD1b (build-list 15                              ;possible next board
                         (lambda (p)                     ;after BD1
                           (not (or (= p 2)  
                                    (= p 5))))))

(define BD1s (build-list 15 (lambda (p) (= p 12))))
(define BD2s (build-list 15 (lambda (p) (= p 12))))
(define BD3s (build-list 15 (lambda (p) (= p 12))))

;; The raw possible jumps given the shape of the board. 
;; For a given board these are only valid if the from and
;; over are full and the to is empty.
(define JUMPS
  (local [(define mj make-jump)]
    (list
     (mj  0  1  3)  (mj  0  2  5)
     (mj  1  3  6)  (mj  1  4  8)
     (mj  2  4  7)  (mj  2  5  9)
     (mj  3  1  0)  (mj  3  4  5)  (mj  3  6 10)  (mj  3  7 12)
     (mj  4  7 11)  (mj  4  8 13)
     (mj  5  2  0)  (mj  5  4  3)  (mj  5  8 12)  (mj  5  9 14)
     (mj  6  3  1)  (mj  6  7  8)
     (mj  7  4  2)  (mj  7  8  9)
     (mj  8  4  1)  (mj  8  7  6)
     (mj  9  5  2)  (mj  9  8  7)
     (mj 10  6  3)  (mj 10 11 12)
     (mj 11  7  4)  (mj 11 12 13)
     (mj 12  7  3)  (mj 12  8  5)  (mj 12 11 10)  (mj 12 13 14)
     (mj 13  8  4)  (mj 13 12 11)
     (mj 14  9  5)  (mj 14 13 12))))


;; ==========
;; Functions:
(@problem 2)
(@htdf solve)
(@signature Board -> (listof Board) or false)
;; If bd is solvable, produce list of boards to solution.
(check-expect (solve MTB) false)
(check-expect (solve BD4) false)
(check-expect (solve BD3s) (list  BD3s))
(check-expect (solve BD3)  (list BD3 BD3s))
(check-expect (solve BD1)
              (local [(define T true)
                      (define F false)]
                (list
                 (list F T T T T T T T T T T T T T T)
                 (list T F T F T T T T T T T T T T T)
                 (list T F T T F F T T T T T T T T T)
                 (list F F F T F T T T T T T T T T T)
                 (list F T F F F T F T T T T T T T T)
                 (list F T T F F F F T T F T T T T T)
                 (list F T T F T F F F T F T F T T T)
                 (list F T T F T T F F F F T F F T T)
                 (list F F T F F T F F T F T F F T T)
                 (list F F F F F F F F T T T F F T T)
                 (list F F F F F T F F T F T F F T F)
                 (list F F F F F F F F F F T F T T F)
                 (list F F F F F F F F F F T T F F F)
                 (list F F F F F F F F F F F F T F F))))

(define (solve bd) false) ;stub





(@htdf get-cell)
(@signature Board Position -> Boolean)
;; produce contents of position p on bd
(check-expect (get-cell BD1 0) false)
(check-expect (get-cell BD1 3) true)

(@template-origin 2-one-of)

(define (get-cell bd p)
  ;;     bd               empty    (cons Boolean Board)
  ;; p
  ;; 
  ;; 0                     N/A      (first bd)
  ;; 
  ;; (add1 Position)       N/A      (get-cell (rest bd) (sub1 p))
  (cond [(zero? p) (first bd)]
        [else
         (get-cell (rest bd) (sub1 p))]))


(@htdf set-cell)
(@signature Board Position Boolean -> Board)
;; produce new board with v at position p
(check-expect (set-cell BD1 3 false)
              (build-list 15 
                          (lambda (p) 
                            (not (or (= p 0)  
                                     (= p 3))))))
(check-expect (set-cell BD1 5 false)
              (build-list 15 
                          (lambda (p) 
                            (not (or (= p 0)  
                                     (= p 5))))))
(check-expect (set-cell BD1 0 true)
              (build-list 15 
                          (lambda (p) true)))


(check-expect (set-cell MTB 5 true) 
              (build-list 15 (lambda (p) (= p 5))))
(check-expect (set-cell MTB 5 false) 
              (build-list 15 (lambda (p) false)))
(check-expect (set-cell BD2 12 false) 
              (build-list 15 (lambda (p) (not (or (= p 4)
                                                  (= p 12))))))

(@template-origin 2-one-of)

(define (set-cell bd p v)
  ;;     bd               empty    (cons Boolean Board)
  ;; p
  ;; 
  ;; 0                     N/A      (cons v (rest bd))
  ;; 
  ;; (add1 Position)       N/A      (cons (first bd)
  ;;                                      (set-cell (rest bd) (sub1 p) v))
  (cond [(zero? p) (cons v (rest bd))]
        [else
         (cons (first bd)
               (set-cell (rest bd) (sub1 p) v))]))





(require 2htdp/image)

(define RENDER-MAP
  (list (list      0)
        (list     1  2)
        (list    3  4  5)
        (list   6  7  8  9)
        (list 10 11 12 13 14)))

(define HOLE (circle 10 "outline" "black"))
(define PEG  (overlay (circle 8 "solid" "blue") HOLE))

(@htdf render-board)
(@signature  Board -> Image)
;; render an image of the given board
(check-expect (render-board BD1)
              (above
               HOLE
               (beside PEG PEG)
               (beside PEG PEG PEG)
               (beside PEG PEG PEG PEG)
               (beside PEG PEG PEG PEG PEG)))

(check-expect (render-board BD2)
              (above
               PEG
               (beside PEG PEG)
               (beside PEG HOLE PEG)
               (beside PEG PEG PEG PEG)
               (beside PEG PEG PEG PEG PEG)))

(@template-origin fn-composition use-abstract-fn)

(define (render-board bd)
  (foldr above
         empty-image
         (map (lambda (row)
                (foldr (lambda (p img)
                         (beside (if (get-cell bd p) PEG HOLE)
                                 img))
                       empty-image
                       row))
              RENDER-MAP)))

