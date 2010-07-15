;; The chipmap for the 1280 was generated from the following
;; comma-separated list. Our hope is that this list is correct.
#|
;; 1
PG5, PE0, PE1, PE2, PE3, PE4, PE5, PE6
;; 9
PE7, XXX, XXX, PH0, PH1, PH2, PH3, PH4
;; 17
PH5, PH6, PB0, PB1, PB2, PB3, PB4, PB5
;; 25
PB6, PB7, PH7, PG3, PG4, XXX, XXX, XXX
;; 33
XXX, XXX, PL0, PL1, PL2, PL3, PL4, PL5
;; 41
PL6, PL7, PD0, PD1, PD2, PD3, PD4, PD5
;; 49
PD6, PD7, PG0, PG1, PC0, PC1, PC2, PC3
;; 57
PC4, PC5, PC6, PC7, XXX, XXX, PJ0, PJ1
;; 65
PJ2, PJ3, PJ4, PJ5, PJ6, PG2, PA7, PA6
;; 73
PA5, PA4, PA3, PA2, PA1, PA0, PJ7, XXX
;; 81
XXX, PK7, PK6, PK5, PK4, PK3, PK2, PK1
;; 89
PK0, PF7, PF6, PF5, PF4, PF3, PF2, PF1
;; 97
PF0, XXX, XXX, XXX
|#

;; This list was turned into a few Scheme lists, and mangled
;; into the occam-pi arrays that were used in chipmap.module.
;; We should not need to run this again.

(define pins '(PG5 PE0 PE1 PE2 PE3 PE4 PE5 PE6 PE7 XXX XXX PH0 PH1 PH2 PH3 PH4 PH5 PH6 PB0 PB1 PB2 PB3 PB4 PB5 PB6 PB7 PH7 PG3 PG4 XXX XXX XXX XXX XXX PL0 PL1 PL2 PL3 PL4 PL5 PL6 PL7 PD0 PD1 PD2 PD3 PD4 PD5 PD6 PD7 PG0 PG1 PC0 PC1 PC2 PC3 PC4 PC5 PC6 PC7 XXX XXX PJ0 PJ1
PJ2 PJ3 PJ4 PJ5 PJ6 PG2 PA7 PA6 PA5 PA4 PA3 PA2 PA1 PA0 PJ7 XXX XXX PK7 PK6 PK5 PK4 PK3 PK2 PK1 PK0 PF7 PF6 PF5 PF4 PF3 PF2 PF1 PF0 XXX XXX XXX))


(define (port p)
  (let ([letter (string-ref p 1)])
    (format "PORT~a" letter)))

(define (reg s)
  (lambda (p)
    (let ([letter (string-ref p 1)])
      (format "~a~a" s letter))))

(define (bit)
  (lambda (p)
    (let ([letter (string-ref p 1)]
          [bit (string-ref p 2)])
      (if (equal? letter #\X)
          "-1"
          (format "~a" bit)
          ))))

(define (list-intersperse ls obj)
  (cond
    [(null? (rest ls)) (list (first ls))]
    [else
     (cons (first ls)
           (cons obj (list-intersperse (rest ls) obj)))]))

(map (lambda (ls)
       (printf "[~a]~n~n" 
               (apply string-append (list-intersperse ls ", "))))
     (map (lambda (f)
            (map (lambda (p)
                   (f (symbol->string p)))
                 pins))
          (list (reg "PORT") (reg "PIN") (reg "DDR") (bit))))
       

