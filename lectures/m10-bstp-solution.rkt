;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname m10-bstp-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(@assignment lectures/m10-bstp)
;;
;; PROBLEM:
;;
;; Given the following data definition for binary trees, design a function that
;; consumes a binary tree and produces true if the invariants for the tree being
;; a binary search tree are satisfied.
;;
(@problem 1)
(@htdd BinaryTree)

(define-struct node (k v l r))
;; BinaryTree is one of:
;;  - false
;;  - (make-node Integer String BinaryTree BinaryTree)
;; interp.
;;   a binary tree where each node has a key, value and two sub-nodes

(define (fn-for-bt bt)
  (cond [(false? bt) (...)]
        [else
         (... (node-k bt)
              (node-v bt)
              (fn-for-bt (node-l bt))
              (fn-for-bt (node-r bt)))]))

(define BT1 (make-node 100 "a"
                       (make-node 50 "b"
                                  (make-node 25 "c"
                                             (make-node 10 "d" false false)
                                             (make-node 30 "e" false false))
                                  (make-node 75 "c"
                                             (make-node 60 "d" false false)
                                             (make-node 80 "e" false false)))
                       (make-node 200 "f" false false)))

(define BT2 (make-node 100 "a"   ;violates left branch rule
                       (make-node 50 "b"
                                  (make-node 51 "c"
                                             (make-node 10 "d" false false)
                                             (make-node 30 "e" false false))
                                  (make-node 75 "c"
                                             (make-node 60 "d" false false)
                                             (make-node 80 "e" false false)))
                       (make-node 200 "f" false false)))

(define BT3 (make-node 100 "a"   ;violates right branch rule
                       (make-node 50 "b"
                                  (make-node 25 "c"
                                             (make-node 10 "d" false false)
                                             (make-node 30 "e" false false))
                                  (make-node 49 "c"
                                             (make-node 60 "d" false false)
                                             (make-node 80 "e" false false)))
                       (make-node 200 "f" false false)))

(define BT4 (make-node 100 "a"
                       (make-node 50 "b"
                                  (make-node 25 "c"
                                             (make-node 10 "d" false false)
                                             (make-node 30 "e" false false))
                                  (make-node 101 "c"
                                             (make-node 60 "d" false false)
                                             (make-node 80 "e" false false)))
                       (make-node 200 "f" false false)))

(define BT5 (make-node 100 "a"
                       (make-node 50 "b"
                                  (make-node 25 "c"
                                             (make-node 10 "d" false false)
                                             (make-node 30 "e" false false))
                                  (make-node 75 "c"
                                             (make-node 49 "d" false false)
                                             (make-node 80 "e" false false)))
                       (make-node 200 "f" false false)))

 
(@htdf bst?) 
(@signature BinaryTree -> Boolean)
;; produce true if bt satisfies binary search tree invariants
(check-expect (bst? BT1) true)
(check-expect (bst? BT2) false)
(check-expect (bst? BT3) false)    
(check-expect (bst? BT4) false)
(check-expect (bst? BT5) false)

(@template-origin BinaryTree accumulator)

(define (bst? bt0)
  ;; lower is Integer
  ;;   lower bound of key at current node
  ;;   initially -inf.0 is reset on recursions down a right branch
  ;; upper is Integer
  ;;   upper bound of key at current node
  ;;   initially +inf.0 is reset on recursions down a left branch
  ;; 
  ;; <see diagram for example progression>
  (local [(define (bst? bt lower upper)
            (cond [(false? bt) true]     
                  [else        
                   (and (< lower (node-k bt) upper)                  ;exploit
                        (bst? (node-l bt)  lower        (node-k bt)) ;preserve
                        (bst? (node-r bt)  (node-k bt)  upper))]))]  ;preserve
    
    (bst? bt0 -inf.0 +inf.0)));                                      ;initialize
;;                            ;NOTE that we would never expect you to have
;;                            ;     already known about these two constants!
