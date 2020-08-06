#lang racket/gui

(provide clock% logger%)

(define handle-tick-interface (interface () tick))

(define logger% (class* object% (handle-tick-interface)
                  (super-new)
                  (define/public (tick)
                    (println "* "))))

(define clock% (class object%
                (super-new)
                (init clock-interval)
                (define interval clock-interval)
                (init-field (clock (new timer% (notify-callback (lambda () (tick))))))
                (init-field (clients '()))
                (define/public (register client)
                   (set! clients (cons client clients)))
                 (define/public (start)
                   (send clock start interval))
                 (define/public (stop)
                   (send clock stop))
                 (define/public (get-clients)
                   clients)
                (define/public (tick)
                  (for-each (lambda (client)
                              (send client tick))
                            clients))))

;(define timer (new clock% [clock-interval 1000]))
;(send timer register l)


