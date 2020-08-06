#lang racket
(require racket/engine)

(define (fib n)
  (let fib ([i n])
    (cond
      [(= i 0) 0]
      [(= i 1) 1]
      [else
       (+ (fib (- i 1))
          (fib (- i 2)))])))
(define eng
  (engine (lambda (x) (fib 20))))


;; should probably bein interface which guarantees it implements 'tick'

(define logger% (class object%
                  (super-new)
                  (define/public (tick)
                    (println "* "))))

(define l (new logger%))
