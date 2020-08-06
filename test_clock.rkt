#lang racket

(require "clock.rkt")

(define timer (new clock% [clock-interval 1000]))
(define l (new logger%))
(send timer register l)