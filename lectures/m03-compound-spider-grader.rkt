#lang racket
(require spd-grader/grader
         spd-grader/check-template
         spd-grader/templates)

(provide grader)

(define Spider (compound (Number Number) make-spider spider? (spider-y spider-dy)))

(define grader  
  (lambda ()
    (grade-submission

      (define %%MID (/ HEIGHT 2))

      (define (%%spider? x)
        (and (spider? x)
             (number? (spider-y x))
             (number? (spider-dy x))
             (<= TOP (spider-y x) BOT)))
      
      (define (%%tock s)
        (cond [(<= (+ (spider-y s) (spider-dy s)) TOP) (make-spider TOP (- (spider-dy s)))]
              [(>= (+ (spider-y s) (spider-dy s)) BOT) (make-spider BOT (- (spider-dy s)))]
              [else
               (make-spider (+ (spider-y s) (spider-dy s))
                            (spider-dy s))]))
      
      (define (%%reverse-spider s ke)
        (cond [(key=? ke " ") (make-spider (spider-y s) (- (spider-dy s)))]
              [else s]))
      
      (grade-problem 1
        (weights (*)
          (grade-htdd Spider
            (grade-dd-rules-and-template 1 Spider))
          (grade-htdf main                
            (grade-signature (Spider -> Spider)))

          (grade-bb-handler (on-tick tick-handler-name)
            (weights (*)
              
              (grade-signature (Spider -> Spider))

              (grade-tests-validity (sp) r
                (%%spider? sp)
                (equal? r (%%tock sp)))
              
              (grade-argument-thoroughness
                  (per-args (sp)
                    (= (+ (spider-y sp) (spider-dy sp)) (+ TOP 1))
                    (= (+ (spider-y sp) (spider-dy sp)) (- TOP 1))
                    (= (+ (spider-y sp) (spider-dy sp)) TOP)
                    (= (+ (spider-y sp) (spider-dy sp)) (+ 1 BOT))
                    (= (+ (spider-y sp) (spider-dy sp)) (- BOT 1))
                    (= (+ (spider-y sp) (spider-dy sp)) BOT)))
              
              (grade-thoroughness-by-faulty-functions 1
                (define (,tick-handler-name s)
                  (cond [(< (+ (spider-y s) (spider-dy s)) TOP)
                         (make-spider TOP
                                      (- (spider-dy s)))]
                        [(>= (+ (spider-y s) (spider-dy s)) BOT)
                         (make-spider BOT
                                      (- (spider-dy s)))]
                        [else
                         (make-spider (+ (spider-y s) (spider-dy s))
                                      (spider-dy s))]))
                (define (,tick-handler-name s)
                  (cond [(<= (+ (spider-y s) (spider-dy s)) TOP)
                         (make-spider TOP
                                      (- (spider-dy s)))]
                        [(>= (spider-y s) BOT)
                         (make-spider BOT
                                      (- (spider-dy s)))]
                        [else
                         (make-spider (+ (spider-y s) (spider-dy s))
                                      (spider-dy s))])))
              
              (grade-template-origin (Spider))
              (grade-template         Spider)
              (grade-submitted-tests)
              (grade-additional-tests 1
                (check-expect (,tick-handler-name (make-spider   %%MID    3)) (%%tock (make-spider   %%MID    3)))
                (check-expect (,tick-handler-name (make-spider   %%MID   -3)) (%%tock (make-spider   %%MID   -3)))
                (check-expect (,tick-handler-name (make-spider (+ TOP 3) -2)) (%%tock (make-spider (+ TOP 3) -2)))
                (check-expect (,tick-handler-name (make-spider (+ TOP 3) -3)) (%%tock (make-spider (+ TOP 3) -3)))
                (check-expect (,tick-handler-name (make-spider (+ TOP 3) -4)) (%%tock (make-spider (+ TOP 3) -4)))
                (check-expect (,tick-handler-name (make-spider (- BOT 3)  2)) (%%tock (make-spider (- BOT 3)  2)))
                (check-expect (,tick-handler-name (make-spider (- BOT 3)  3)) (%%tock (make-spider (- BOT 3)  3)))
                (check-expect (,tick-handler-name (make-spider (- BOT 3)  4)) (%%tock (make-spider (- BOT 3)  4))))))

          (grade-bb-handler (to-draw draw-handler-name)
            (weights (*)
              (grade-signature (Spider -> Image))
              (grade-template-origin (Spider))
              (grade-template         Spider)
              (grade-submitted-tests)))

          (grade-bb-handler (on-key key-handler-name key-handler-defn)
            (weights (*)
              
              (grade-signature (Spider KeyEvent -> Spider))

              (grade-tests-validity (s ke) r
                (%%spider? s)
                (key-event? ke)
                (equal? r (%%reverse-spider s ke)))
              
              (grade-argument-thoroughness
                  (per-args (s ke)
                    (key=? ke " ")
                    (not (key=? ke " "))))
              
              (grade-thoroughness-by-faulty-functions 1
                (define (,key-handler-name s ke)
                  (make-spider (spider-y s) (- (spider-dy s))))
                (define (,key-handler-name s ke)
                  s))
              
              (grade-template-origin (KeyEvent Spider))
              (grade-template/body (s ke)
                  (cond [(key=? ke " ") (... (spider-y s) (spider-dy s))]
                        [else (... (spider-y s) (spider-dy s) ke)]))

              (grade-questions-intact/body key-handler-defn (s ke)
                  (cond [(key=? ke " ") (...)]
                        [else (...)]))
              
              (grade-submitted-tests)
              (grade-additional-tests 1
                (check-expect (,key-handler-name (make-spider 100  2) " ") (%%reverse-spider (make-spider 100  2) " "))
                (check-expect (,key-handler-name (make-spider 200 -3) " ") (%%reverse-spider (make-spider 200 -3) " "))
                (check-expect (,key-handler-name (make-spider 100  2) "a") (%%reverse-spider (make-spider 100  2) "a"))))))))))


