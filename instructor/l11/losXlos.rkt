;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname losXlos) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)

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

(define (fn-for-losxlos los1 los2)
  (cond [(and (empty? los1) (empty? los2)) (...)]
        [(empty? los2) ;los1 is a cons
         (... (first los1)
              (fn-for-losxlos (rest los1) empty))]
        [(empty? los1) ;los2 is a cons
         (... (first los2)
              (fn-for-losxlos empty (rest los2)))]
        [else ;los1 and los2 are both cons
         (... (first los1)
              (first los2)
              (fn-for-losxlos los1 (rest los2))
              (fn-for-losxlos (rest los1) los2)
              (fn-for-losxlos (rest los1) (rest los2)))]))
