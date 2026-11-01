;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname m08-abstract-fold-usages-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; DO NOT PUT ANYTHING PERSONALLY IDENTIFYING BEYOND YOUR CWL IN THIS FILE.
;; YOUR CWLs WILL BE SUFFICIENT TO IDENTIFY YOU AND, IF YOU HAVE ONE, YOUR 
;; PARTNER.
;;
(require spd/tags)
(@assignment lectures/m08-abstract-fold-usages)

(@problem 1)
(@htdd Course)
(define-struct course (number credits dependents))
;; Course is (make-course Natural Natural (listof Course))
;; interp. a course with a course number,
;;         the number of credits the course is worth,
;;         a list of courses that have this course as a pre-requisite

(define LOC0 empty)
(define C322 (make-course 322 3 LOC0))
(define C320 (make-course 320 3 LOC0))
(define C319 (make-course 319 4 LOC0))
(define C317 (make-course 317 3 LOC0))
(define C314 (make-course 314 3 LOC0))
(define C313 (make-course 313 3 LOC0))
(define C312 (make-course 312 3 LOC0))
(define C311 (make-course 311 3 LOC0))
(define LOC1 (list C319))
(define C310 (make-course 310 4 LOC1))
(define C304 (make-course 304 3 LOC0))
(define C302 (make-course 302 3 LOC0))
(define C303 (make-course 303 3 LOC0))
(define LOC2 (list C304 C313 C314 C317 C320 C322))
(define C221 (make-course 221 4 LOC2))
(define LOC3 (list C313 C317))
(define C213 (make-course 213 4 LOC3))
(define LOC4 (list C213 C221 C310 C311 C312))
(define C210 (make-course 210 4 LOC4))
(define C203 (make-course 203 3 LOC0))
(define C189 (make-course 189 1 LOC0))
(define LOC5 (list C189 C203 C210 C302 C303))
(define C110 (make-course 110 4 LOC5))
(define C100 (make-course 100 3 LOC0))

(@template-origin Course (listof Course) encapsulated)

(define (fn-for-course c0)
  (local [(define (fn-for-course c)
            (... (course-number c)
                 (course-credits c)
                 (fn-for-loc (course-dependents c))))

          (define (fn-for-loc loc)
            (cond [(empty? loc) (...)]
                  [else
                   (... (fn-for-course (first loc))
                        (fn-for-loc (rest loc)))]))]

    (fn-for-course c0)))




(@htdf fold-course)
(@signature (Natural Natural Y -> X) (X Y -> Y) Y Course -> X)
;; abstract fold for Course
(check-expect (fold-course make-course cons empty C110) C110)
(check-expect (local [(define (c1 n cr rmr) (add1 rmr))
                      (define (c2 rmr rnr) (+ rmr rnr))
                      (define b1 0)]
                (fold-course c1 c2 b1 C110))
              20)

(@template-origin Course (listof Course) encapsulated)

(define (fold-course c1 c2 b1 c0)
  (local [(define (fn-for-course c)  ;--> X
            (c1 (course-number c)
                (course-credits c)
                (fn-for-loc (course-dependents c))))

          (define (fn-for-loc loc)   ;--> Y
            (cond [(empty? loc) b1]
                  [else
                   (c2 (fn-for-course (first loc))
                       (fn-for-loc (rest loc)))]))]

    (fn-for-course c0)))



;;
;; Complete the definition of this function to use fold-course.
;;

(@htdf all-course-numbers)
(@signature Course -> (listof Natural))
;; produce a list of all course numbers in given tree
(check-expect (all-course-numbers C100) (list 100))
(check-expect (all-course-numbers C213) (list 213 313 317))
(check-expect (all-course-numbers C210)
              (list 210 213 313 317 221 304 313
                    314 317 320 322 310 319 311 312))

(@template-origin use-abstract-fn)

(define (all-course-numbers c)
  (local [(define (num-and-rmr num credits rmr) (cons num rmr))] 
    (fold-course num-and-rmr append empty c))) 


(@problem 2)

;;
;; Complete the design of this function using fold-course.
;;

(@htdf total-credits)
(@signature Course -> Natural)
;; produce the total number of credits in a course tree
(check-expect (total-credits C100) 3)
(check-expect (total-credits C110) 64)

(@template-origin use-abstract-fn)


(define (total-credits c)
  (local [(define (c1 number credits rmr) (+ credits rmr))
          (define (c2 rmr rnr) (+ rmr rnr))  
          (define b1 0)]   
    (fold-course c1 c2 b1 c)))

;;
;; Note how similar the two solutions in this file are:
;;
;; (define (all-course-numbers c)
;;   (local [(define (num-and-rmr num credits rmr) (cons num rmr))]
;;     (fold-course num-and-rmr append empty c)))
;;
