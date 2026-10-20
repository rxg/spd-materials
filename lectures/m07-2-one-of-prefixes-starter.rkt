;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname m07-2-one-of-prefixes-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)

(@assignment lectures/m07-2-one-of-prefixes)

(@cwl ???) ;replace ??? with your cwl

(@problem 1)
#|
Design a function that consumes two lists of strings and produces
true if the first list is a prefix of the second, meaning that 
every element of the first list appears, in order, at the front
of the second list.

For example:

  (prefix=? (list "a" "b") (list "a" "b" "c")) --> true
  (prefix=? (list "a" "b") (list "b" "c"))     --> false
  (prefix=? (list "a" "b") (list "a" "b"))     --> true
  (prefix=? (list "a" "b") (list "a"))         --> false

  
As a reminder, here is a data definition for a list of strings. To 
save space later we are calling it LOS instead of ListOfString.
|#

;; Data Definitions:
;;
;; NOTE: IN 2-ONE-OF PROBLEMS ONLY, WE USE LOS FOR ListOfString.
;;

(@htdd LOS)
;; LOS is one of:
;;  - empty
;;  - (cons String LOS)
;; interp. a list of strings
(define LOS1 empty)
(define LOS2 (cons "a" (cons "b" empty)))

(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else
         (... (first los)
              (fn-for-los (rest los)))]))

;; Function:
