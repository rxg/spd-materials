;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname m08-from-examples-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(@assignment lectures/m08-from-examples)

(@cwl ???) ;replace ??? with your cwl

;; 
;; One of the most important ideas in software is abstraction. What abstraction
;; means is that we:
;;   - recognize that some functionality seems to be repeated/common
;;   - package that common behaviour in a reusable way
;;   - so that future (and current) uses of that common behaviour
;;     can be easier to develop, easier to understand, more reliable etc.
;; 
;; So abstraction is a verb:
;;   - to recognize the common functionality and package it
;; 
;; and a noun:
;;   - the packaged up common behaviour.
;; 
;; 
;; We have already seen one form of abstraction - templates.
;; 
;; (@template-origin Region ListOfRegion encapsulated)
;; (define (fn-for-region r)
;;   (local [(define (fn-for-region r)
;;             (cond [(leaf? r)
;;                    (... (leaf-label r)
;;                         (leaf-weight r)
;;                         (leaf-color r))]
;;                   [else
;;                    (... (inner-color r)
;;                         (fn-for-lor (inner-subs r)))]))
;; 
;;           (define (fn-for-lor lor)
;;             (cond [(empty? lor) (...)]
;;                   [else
;;                    (... (fn-for-region (first lor))
;;                         (fn-for-lor (rest lor)))]))]
;; 
;;     (fn-for-region r)))
;; 
;; When we see that, we know that it captures the common behaviour
;; of traversing a Region tree and that we can reuse that by copying
;; it and editing.
;; 
;; We can also combine the template based abstractions:
;; 
;; 
;; (@template-origin encapsulated Region ListOfRegion try-catch)
;; 
;; (define (fn-for-region r)
;;   (local [(define (fn-for-region r)
;;             (cond [(leaf? r)
;;                    (... (leaf-label r)
;;                         (leaf-weight r)
;;                         (leaf-color r))]
;;                   [else
;;                    (... (inner-color r)
;;                         (fn-for-lor (inner-subs r)))]))
;; 
;;           (define (fn-for-lor lor)
;;             (cond [(empty? lor) (...)]
;;                   [else
;;                    (local [(define try (fn-for-region (first lor)))]
;;                      (if (not (false? try))
;;                          (... try)
;;                          (fn-for-lor (rest lor))))]))]
;; 
;;     (fn-for-region r)))
;; 
;; 
;; That's very powerful.  As we have said before, expert thinking is 
;; partly characterized by being able to think about programs in terms
;; of abstractions like these that are larger than individual program
;; constructs.
;; 
;; 
;; There's another way to go about it, which works in fewer cases, but
;; provides much greater abstraction when it does work.  The idea is to
;; desing a function that captures the reusable behaviour, with special
;; arguments corresponding to each of the ... in the template.
;; 
;; Again, abstractions packaged as functions are very powerful when they
;; work for a problem; but they don't work for all problems. So after 
;; module 8 we will use both kinds of abstraction as appropriate.
;; 



(@htdd ListOfNumber)
;; ListOfNumber is one of:
;;  - empty
;;  - (cons Number ListOfNumber)
;; interp. a list of numbers


(@problem 1)
#|
Develop a FUNCTION DEFINITION for an abstract function to simplify the 
following two functions. Complete your work by re-defining the original
functions to use the new abstract function. You do not need to do the
signature, purpose or tests for the new abstract function yet.
|#

(@htdf all-greater?)
(@signature ListOfNumber Number -> Boolean)
;; produce true if every number in lon is greater than x.
(check-expect (all-greater? empty 0) true)
(check-expect (all-greater? (list 2 -3 -4) -6) true)
(check-expect (all-greater? (list -2 -3 -4) -3) false)

(@template-origin ListOfNumber)

(define (all-greater? lon x)
  (cond [(empty? lon) true]
        [else
         (and (> (first lon) x)
              (all-greater? (rest lon) x))]))


(@htdf all-positive?)
(@signature ListOfNumber -> Boolean) ;<< Change ListOfNumber to (listof Number)
;; produce true if every number in lon is positive?
(check-expect (all-positive? empty) true)
(check-expect (all-positive? (list 2 3 -4)) false)
(check-expect (all-positive? (list 2 3  4)) true)

(@template-origin ListOfNumber)

(define (all-positive? lon)
  (cond [(empty? lon) true]
        [else
         (and (positive? (first lon))
              (all-positive? (rest lon)))]))


(@problem 2)
#|
Now complete the design of the andmap2 abstract function.

First move the definition from above below here.  Then work backwards
through the recipe to add @template-origin, check-expects, purpose,
@signature, and @htdf tag.  Use type inference to produce the signature.
Be sure to use the (list <Type>) form for every list type from now on.
|#












(@problem 3)
#|
Complete the design of the filter2 abstract function with signature,
purpose and tests.
|#

(@htdf positive-only)
(@signature ListOfNumber -> ListOfNumber)
;; produce list with only postive? elements of lon
(check-expect (positive-only empty) empty)
(check-expect (positive-only (list 1 -2 3 -4)) (list 1 3))

;(define (positive-only lon) empty)   ;stub

(@template-origin use-abstract-fn)

(define (positive-only lon)
  (filter2 positive? lon))


(@htdf negative-only)
(@signature ListOfNumber -> ListOfNumber)
;; produce list with only negative? elements of lon
(check-expect (negative-only empty) empty)
(check-expect (negative-only (list 1 -2 3 -4)) (list -2 -4))

;(define (negative-only lon) empty)   ;stub

(define (negative-only lon)
  (filter2 negative? lon))




(@htdf filter2)

(@template-origin ListOfNumber)

(define (filter2 p lon)
  (cond [(empty? lon) empty]
        [else 
         (if (p (first lon))
             (cons (first lon) 
                   (filter2 p (rest lon)))
             (filter2 p (rest lon)))]))




(@problem 4)
;;
;; Complete the design of the map2 abstract function with signature.
;;

(@htdf squares)
(@signature (listof Number) -> (listof Number))
;; produce list of sqr of every number in lon
(check-expect (squares empty) empty)
(check-expect (squares (list 3 4)) (list 9 16))

(define (squares lon) (map2 sqr lon))

(@htdf square-roots)
(@signature (listof Number) -> (listof Number))
;; produce list of sqrt of every number in lon
(check-expect (square-roots empty) empty)
(check-expect (square-roots (list 9 16)) (list 3 4))

(define (square-roots lon) (map2 sqrt lon))


(@htdf map2)
;; <SIGNATURE IS MISSING>
;; given fn and (list n0 n1 ...) produce (list (fn n0) (fn n1) ...)
(check-expect (map2 sqr empty) empty)
(check-expect (map2 sqr (list 2 4)) (list 4 16))
(check-expect (map2 sqrt (list 16 9)) (list 4 3))
(check-expect (map2 abs (list 2 -3 4)) (list 2 3 4))

(@template-origin ListOfNumber)

(define (map2 fn lon)
  (cond [(empty? lon) empty]
        [else
         (cons (fn (first lon))
               (map2 fn (rest lon)))]))




