;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname m10-sequencep-alt-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(@assignment lectures/m10-sequencep)


(@problem 1)

(@htdf sequence?)

(@signature (listof Natural) -> Boolean)
;; produces true if every element of lon is 1 greater than prior element
(check-expect (sequence? (list))       true)
(check-expect (sequence? (list 2))     true)
(check-expect (sequence? (list 2 3 4)) true)
(check-expect (sequence? (list 3 5 6)) false)
(check-expect (sequence? (list 2 3 4 7 5)) false)

(@template-origin (listof Natural) accumulator)

(define (sequence? lon0)
  ;; prev is MaybeNat
  ;; invariant: the element of lon0 immediately before (first lon),
  ;; or false if none
  ;; (sequence? (list 2 3 4 7 5))
  
  ;; (sequence? (list   3 4 7 5) 2)  
  ;; (sequence? (list     4 7 5) 3)
  ;; (sequence? (list       7 5) 4) ==> false
  (local [(@htdd MaybeNat)
          ;; MaybeNat is one of:
          ;; - Natural
          ;; - false
          ;; interp. an optional natural number, or false if there is none.

          (@template-origin MaybeNat)
          (@signature Natural MaybeNat -> Boolean)
          ;; produce true if prev is false or one less than n
          (define (prev-checks? n prev)
            (cond [(number? prev) (= n (+ 1 prev))]
                  [else true]))
          
          (define (sequence? lon prev)
            (cond [(empty? lon) true]     
                  [else           
                   (if (prev-checks? (first lon) prev) ;exploit (use)
                       (sequence? (rest lon)
                                  (first lon))    ;preserve
                       false)]))]

    (sequence? lon0 false)))  ;initialize

