#lang racket
(define (s a b c)
  (/ (+ a b c) 2))

(define (area a b c)
  (sqrt (* (- (s a b c) a)
           (- (s a b c) b)
           (- (s a b c) c))))

(define (areah a b c)
  (let ((s (/ (+ a b c) 2)))
    (sqrt (* s (- s a) (- s b) (- s c)))))
