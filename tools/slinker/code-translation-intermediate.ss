#cs
(module code-translation-intermediate mzscheme
  (require 
   "helpers.ss"
   "constants.ss"
   "version.ss"
   (lib "plt-match.ss"))
  (provide (all-defined))
  
  (define (make-stack) '())
  (define *stack* (make-stack))
  
  (define (push obj)
    (set! *stack* (cons obj *stack*)))
  
  (define (pop)
    (if (null? *stack*)
        (error 'CTI "Tried to pop an empty stack.")
        (let ([top (car *stack*)])
          (set! *stack* (cdr *stack*))
          top)))
  
  (define loc->symbolic-instruction
    (lambda (p)
      (let* ([table-loc (car p)]
	     [inst (cdr p)]
	     [meta (Instruction-meta inst)]
	     [src-loc 
	      (begin
		(hash-table-get meta 'line (lambda () 'x)))]
	     [abbrev (hash-table-get meta 'abbreviation (lambda () 'xxx))]
	     [operand (hash-table-get meta 'operand (lambda () ""))]
	     [comment (hash-table-get meta 'comment (lambda () ""))])
	(let ([binary-decode ""])
	  (cond
            [(and (binary? inst) 
                  (not (null?  (binary-value inst))))
             (let-values ([(fn op sp p)
                           (decode-ins (mapl integer->char (binary-value inst)) 0)])
               (let ([primary? (lambda (op) (< op 15))]
                     [secondary? (lambda (x) #t)])
                 (cond
                   [(primary? fn)
                    (if (> op #x80000000)
                        (set! op (* -1 (add1 (bitwise-mattnot op)))))
                    (set! binary-decode 
                          (format "~a ~a" (list-ref primaries fn) (hex op)))]
                   [(secondary? op)
                    (let ([list-index (quotient op 16)]
                          [abbrv-index (remainder op 16)])
                      (set! binary-decode 
                            (format "~a" 
                                    (list-ref (list-ref secondaries list-index)
                                              abbrv-index))))])))]
            [else
             (set! binary-decode "")])
          (if (not (equal? operand ""))
              (list abbrev operand)
              (list abbrev))
              ))))
  
  (define (just-dump-stuff pass)
    (let* ([meta (pass-meta pass)]
           [bytes (pass-bytes pass)]
           [bytes-sorted (hash->list bytes)])
      (for-each (lambda (b)
                  (printf "~a~n" b))
                bytes-sorted)
      pass))
  

  ;; This compresses the list of operations for output to 
  ;; efficient C. However, I'm not going to be doing that right now,
  ;; so I'll leave it as a stub to be filled in more, later.
  ;; Instead, I'll work on the other bit... 
  (define (op-compression ls)
    (match ls
      ;; OUT and IN
      [`((ldlp ,ldlp) (ldl ,ldl) (ldc ,ldc) 
                      ;; This is the match against (OUT) or (IN)
                      (,(? (lambda (op)
                             (member op '(in out))) op))
                      ;; And the rest of the list...
                      ,rest ...)
        (let ([label (gensym (format "~a_label" op))]
              [magicop (string->symbol (format "magic-~a" op))])
          (cons `(,magicop ,ldlp ,ldl ,ldc ,label)
                (cons
                 `(label ,label)
                 (op-compression rest))))]
      ;; If no patterns match, simply keep things as they are.
      [otherwise
       (cond
         [(null? otherwise) '()]
         [else
          (cons (car otherwise) (op-compression (cdr otherwise)))])]
      ))
  
  
  (define (->n v)
    (cond
      [(number? v) v]
      [(symbol? v) v]
      [(string? v) (string->number v 16)]))
  
  (define (is-primary? sym)
    (member sym primaries))
  
  (define (is-secondary? sym)
    (ormap (lambda (ls)
             (member sym ls)) secondaries))
  
  (define (is-load? sym)
    (member sym '(ldc ldlp ldl ldnlp ldnl)))
  
  (define (o2c-load pair)
    (if (is-load? (car pair))
        (push pair)
        (error 'o2c-load "Tried to push non-load.")))
 
  (define (is-store? sym)
    (member sym '(stl stnl)))
 
  (define (o2c-store pair)
    (if (is-store? (car pair))
        (case (car pair)
          [(stl) `(store-local ,(->n (cadr pair)) ,(pop))]
          [(stnl) `(store-non-local ,(->n (cadr pair)) ,(pop))])
        (error 'o2c-store "Tried to pop non-store.")))

  (define (o2c-sec pair) 'dummy)
	  
  (define (o2c ls)
    (cond
      [(null? ls) '()]
      ;; Push loads
      [(and (list? (car ls))
            (is-load? (caar ls)))
       (o2c-load (car ls))
       ;; Don't build a list here
       (o2c (cdr ls))]
      ;; Pop stores; these are top-level,
      ;; so they'll become full assignment statements
      [(and (list? (car ls))
            (is-store? (caar ls)))
       ;; Build a list
       (cons (o2c-store (car ls))
             (o2c (cdr ls)))]
      ;; All other operations; eventually, these
      ;; will pop some things...
      [else 
       (let ([result (o2c-sec (car ls))])
         (if result
             (cons result (o2c (cdr ls)))
             (o2c (cdr ls))))]
      ))
       
       
      
       
  
 
  (define (output-efficient-c pass)
    (let* ([meta (pass-meta pass)]
           [bytes (pass-bytes pass)]
           [instruction-pairs (map loc->symbolic-instruction (hash->list bytes))])
      (define (get-jump-targets ls)
        (cond
          [(null? ls) '()]
          [(equal? (caar ls) 'j)
           (cons (cadar ls) (get-jump-targets (cdr ls)))]
          [else
           (get-jump-targets (cdr ls))]))
      
      (define (insert-labels ls lbls cnt)
        (cond
          [(null? ls) '()]
          [(member cnt lbls)
           (cons (list 'label cnt) 
                 (cons (car ls)
                       (insert-labels (cdr ls) lbls (add1 cnt))))]
          [else
           (cons (car ls)
                 (insert-labels (cdr ls) lbls (add1 cnt)))]))
      
      ;;This takes the dotted pair form, and inserts 
      ;; dotted pair labels; this way, I know where to drop C labels.
      ;; Oddly, this is just reinserting information that was optimized out several passes ago.
      (set! instruction-pairs (insert-labels instruction-pairs (get-jump-targets instruction-pairs) 0))
      
      ;; Do the nifty replacements
      ;;(set! instruction-pairs (op-compression instruction-pairs))
      
      ;; Do naive transformation to C. This converts the list from a 
      ;; list of (abbr op) pairs to a list of strings.
      (set! instruction-pairs (o2c instruction-pairs))
      
      (for-each (lambda (b) 
                  (if (string? b)
                      (display b)
                      (printf "~s~n" b))) instruction-pairs)
  
      pass))
  
  
  )