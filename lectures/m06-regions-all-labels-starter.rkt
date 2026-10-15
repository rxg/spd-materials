;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname m06-regions-all-labels-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)

(@assignment lectures/m06-regions-all-labels)

(@cwl ???) ;replace ??? with your cwl

(@problem 1)
;;
;; Region and ListOfRegion data definitions provided. 
;; 
(@htdd Region ListOfRegion)
(define-struct leaf (label weight color))
(define-struct inner (color subs))
;; Region is one of:
;;  - (make-leaf String Natural Color)
;;  - (make-inner Color ListOfRegion)
;; interp.
;;  an arbitrary-arity tree of regions
;;  leaf regions have label, weight and color
;;  inners have a color and a list of sub-regions
;;
;;  weight is a unitless number indicating how much weight
;;  the given leaf region contributes to whole tree

;; ListOfRegion is one of:
;;  - empty
;;  - (cons Region ListOfRegion)
;; interp. a list of regions

;; All the Ls and Is are Regions
(define L1 (make-leaf "one" 20 "red"))
(define L2 (make-leaf "two" 40 "blue"))
(define L3 (make-leaf "three" 60 "orange"))
(define L4 (make-leaf "four" 30 "black"))
(define L5 (make-leaf "five" 50 "purple"))
(define L6 (make-leaf "six" 80 "yellow"))

(define I1 (make-inner "red"  (list L1 L2 L3)))
(define I2 (make-inner "blue" (list I1 L4)))
(define I3 (make-inner "orange" (list L5 L6)))
(define I4 (make-inner "black" (list I2 I3)))

(define LORE empty)
(define LOR123 (list L1 L2 L3))

(@template-origin Region)

(define (fn-for-region r)
  (cond [(leaf? r)
         (... (leaf-label r)
              (leaf-weight r)
              (leaf-color r))]
        [else
         (... (inner-color r)
              (fn-for-lor (inner-subs r)))]))

(@template-origin ListOfRegion)

(define (fn-for-lor lor)
  (cond [(empty? lor) (...)]
        [else
         (... (fn-for-region (first lor))
              (fn-for-lor (rest lor)))]))



(@htdd ListOfString)
;; NOTE: ListOfString data definition omitted intentionally

;; Design a function that consumes a region and
;; produces a list of all the labels in the region.  Be sure to
;; follow the structure above, where the @htdf tag names both
;; functions, the signatures are grouped, leaf purpose, tests
;; are grouped, and each function has its own template tag.
