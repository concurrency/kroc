(require (lib "string.ss"))

(define num.points 181)
(define radius 40)
(define-struct point (x y))
(define displacement.x 0.0)
(define displacement.y 8.0)
(define PI 3.14159265534897)

(define (iota n)
  (define (iota* n)
    (if (zero? n)
        '()
        (cons n (iota* (sub1 n)))))
  (reverse (map (lambda (n) (sub1 n)) (iota* n))))

(define array
  (map (lambda (d)
         (map (lambda (i)
		(let ([angle (/ (* PI i) num.points)])
		  (make-point (+ (* (cos angle) d) displacement.x)
			      (+ (* (sin angle) d) displacement.y)))
                ) (iota num.points))
         ) (iota radius)))

(define (list-intersperse ls obj)
  (cond
    [(null? ls) '()]
    [(null? (cdr ls)) (list (car ls))]
    [else
     (cons (car ls)
           (cons
            obj
            (list-intersperse (cdr ls) obj)))]))

(define (show-array arr)
  (let ([str
         (apply string-append
                (list-intersperse
                 (map (lambda (arc)
                        (format "[~a]"
                                (apply string-append
                                       (list-intersperse
                                        (map (lambda (pt)
                                               (format "[~a,~a]"
(point-x pt)   (point-y pt)))
                                               arc) (format ",~n")))))
arr) (format ",~n~n")))
         ])
    (string-uppercase! str)
    str))

(display (format "VAL [][]POINT points IS [~a]:~n~n" (show-array array)))
(exit)
