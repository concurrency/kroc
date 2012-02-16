#cs
(module code-translation mzscheme
  (require 
   "helpers.ss"
   "constants.ss"
   "version.ss"
   (lib "plt-match.ss"))
  (provide (all-defined))
  
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
  
  (define (is-rhs? sym)
    (member sym '(ldlp ldnl ldc ldnlp ldl)))
  
  (define (is-load? sym)
    (member sym '(ldc ldlp ldl ldnlp ldnl)))
  
  ;; (ldlp ldnl ldc ldnlp ldl adc eqc stl stnl)
  (define (o2c-rhs pair)
    (match pair
      [`(ldc ,ldc)     (format "~a" (->n ldc))]
      [`(ldl ,ldl)     (format "wptr[~a]" (->n ldl))]
      ;; WARNING: Matt-C
      ;; /* areg = *(((WORD *)areg) + oreg); */
      [`(ldnl ,ldnl)   (format "*(((int*)areg) + ~a" (->n ldnl))]
      ;; WARNING: Matt-C
      ;; /* *(((WORD *)areg) + oreg) = breg; */
      [`(ldnlp ,ldnlp) (format "*(((int*)areg) + ~a" (->n ldnlp))]
      [`(ldlp ,ldlp)   (format "(int) (wptr + ~a)" (->n ldlp))]

      [else (format "/* o2c-rhs error: ~a */~n" pair)]))
    
  (define (is-lhs? sym)
    (member sym '(stl stnl)))
  
  ;; (stnl "1")
  ;; (stl "2")
  
  ;; Tricky; I've doubled up reg to mean
  ;; either a register for stl, or 'pop for 
  ;; stnl
  (define (o2c-lhs pair reg)
    (match pair
        ;; MATT-C
      [`(stl ,stl)
        ;; Explicit pop?
        (format 
         (string-append
          "/* ~a */~n"
          "wptr[~a] = ~a;~n"
          ;; Not sure when I should pop...
          (if (equal? reg 'areg)
              (format
               (string-append
                "/* POP */~n"
                "areg = breg;~n"
                "breg = creg;~n")) "")) 
         `(stl ,stl) (->n stl) reg)]
      
      ;; MATT-C
      [`(stnl ,stnl)
        ;; Explicit pop (twice)?
        (format 
         (string-append
          "/* ~a */~n"
          "*(((int*)areg) + ~a = breg;~n"
          (if (equal? reg 'pop)
              (format 
               (string-append
                "/* POP x 2 */~n"
                "areg = creg;~n"))
              ""))
          `(stnl ,stnl) (->n stnl))]
      [else "/* o2c-lhs error: ~a */~n" pair]))
  
  (define o2c-atomics '(ret ajw j  adc eqc))
  (define (o2c-atomic pair)
    (match pair
      [`(j ,j) (format "goto ~a;~n" j)]
      ;; RET
      ;; wptr = wptr + 4;
      ;; goto *wptr[-4];
      [`(ret) "wptr = wptr + 4;~ngoto *wptr[4];~n" ]
      
      ;; AJW
      ;; AJW -1 =>
      ;; wptr = wptr - 1;
      [`(ajw ,ajw)
        (let* ([n (->n ajw)]
               [abs (lambda (n) (if (< n 0) (* -1 n) n))])
          (format "wptr = wptr ~a ~a;~n" 
                  (if (> n 0) "+" "-")
                  (abs n))
          )]
      [else (format "/* o2c-atomic error: ~a */~n" pair)]))
      
  
  (define (o2c ls)
    (match ls
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; CLUSTERS
      
      ;; First, let's catch all of the sequences of three 
      ;; primary instructions, and load them into the 
      ;; three registers. Hopefully, that will be a reasonable optimization.
      
      ;; CALL with three arguments
      [`((,(? is-rhs? op1) ,v1) 
         (,(? is-rhs? op2) ,v2)
         (,(? is-rhs? op3) ,v3)
         (call ,tgt)
         ,rest ...)
        (let ([call-label (gensym (format "label_call"))])
          (cons
           (format 
            (string-append
             "/* ~a ~a ~a ~a */~n" 
             "wptr[-4] = (int) &&~a;~n"
             "wptr[-3] = ~a;~n"
             "wptr[-2] = ~a;~n"
             "wptr[-1] = ~a;~n" 
             "wptr = wptr - 4;~n"
             "goto L~a;~n" 
             "~a:~n")
            op1 op2 op3 'call3
            call-label
            (o2c-rhs `(,op1 ,v1))
            (o2c-rhs `(,op2 ,v2))
            (o2c-rhs `(,op3 ,v3))
            tgt
            call-label)
           (o2c rest)))]
      
      ;; CALL with 2 arguments
      [`((,(? is-rhs? op1) ,v1) 
         (,(? is-rhs? op2) ,v2)
         (call ,tgt)
         ,rest ...)
        (let ([call-label (gensym (format "label_call"))])
          (cons
           (format 
            (string-append
             "/* ~a ~a ~a */~n" 
             "wptr[-4] = (int) &&~a;~n"
             "wptr[-3] = ~a;~n"
             "wptr[-2] = ~a;~n"
             ;; WARNING MCJ
             ;; I'm assuming I have to fill this in.
             "wptr[-1] = areg;~n"
             "wptr = wptr - 4;~n"
             "goto L~a;~n" 
             "~a:~n")
            op1 op2 'call2
            call-label
            (o2c-rhs `(,op1 ,v1))
            (o2c-rhs `(,op2 ,v2))
            tgt
            call-label)
           (o2c rest)))]
      
      ;; CALL with 1 arguments (?) I haven't seen this yet.
      [`((,(? is-rhs? op1) ,v1) 
         (call ,tgt)
         ,rest ...)
        (let ([call-label (gensym (format "label_call"))])
          (cons
           (format 
            (string-append
             "/* ~a ~a */~n" 
             "wptr[-4] = (int) &&~a;~n"
             "wptr[-3] = ~a;~n"
             ;; WARNING MCJ
             ;; I assume these must fill in from
             ;; the stack, or CALL will die.
             "wptr[-2] = breg;~n"
             "wptr[-1] = areg;~n"
             "wptr = wptr - 4;~n"
             "goto L~a;~n" 
             "~a:~n")
            op1 'call1
            call-label
            (o2c-rhs `(,op1 ,v1))
            (->n tgt)
            call-label)
           (o2c rest)))]
 
      
      ;; IN/OUT
      [`((,(? is-rhs? op1) ,v1) 
         (,(? is-rhs? op2) ,v2)
         (,(? is-rhs? op3) ,v3)
         (,(? (lambda (s) (member s '(in out))) dir)) ,rest ...)
        (let ([label (gensym (format "label_~a" dir))])
          (cons
           (format 
            (string-append
             "creg = ~a;~n"
             "breg = ~a;~n"
             "areg = ~a;~n" 
             "iptr = &&~a;~n"
             "~a()~n"
             "goto *iptr;~n" 
             "~a:~n") 
            (o2c-rhs `(,op1 ,v1))
            (o2c-rhs `(,op2 ,v2))
            (o2c-rhs `(,op3 ,v3))
            label dir label)
           (o2c rest)))]
      
      ;; Groups of stores
      [`((stnl ,stnl) (stl ,stl) ,rest ...)
        (cons
         (format 
          (string-append
           "/* stnl stl */~n"
           "~a"
           "~a")
           (o2c-lhs `(stnl ,stnl) 'nopop)
           (o2c-lhs `(stl ,stl) 'creg))
         (o2c rest))]
      
      ;; Groups of loads
      ;; Seems like I should be able to groups of 
      ;; instructions together.
      [`((,(? is-rhs? op1) ,v1)
         (,(? is-rhs? op2) ,v2)
         (,(? is-rhs? op3) ,v3)
         ,rest ...)
        (cons
         (format
          (string-append
           "/* ~a ~a ~a ~a */~n"
           "creg = ~a;~n"
           "breg = ~a;~n" 
           "areg = ~a;~n")
          op1 op2 op3 'rhs3
          (o2c-rhs `(,op1 ,v1))
          (o2c-rhs `(,op2 ,v2))
          (o2c-rhs `(,op3 ,v3)))
         (o2c rest))]
      
      ;; two
      [`((,(? is-rhs? op1) ,v1)
         (,(? is-rhs? op2) ,v2)
         ,rest ...)
        (cons
         (format
          (string-append
           "/* ~a ~a ~a */~n"
           "creg = breg;~n"
           "breg = ~a;~n" 
           "areg = ~a;~n")
          op1 op2 'rhs2
          (o2c-rhs `(,op1 ,v1))
          (o2c-rhs `(,op2 ,v2)))
         (o2c rest))]

      ;; one
      [`((,(? is-rhs? op1) ,v1)
         ,rest ...)
        (cons
         (format
          (string-append
           "/* ~a ~a */~n"
           "creg = breg;~n"
           "breg = areg;~n"
           "areg = ~a;~n")
          op1 'rhs1
          (o2c-rhs `(,op1 ,v1)))
         (o2c rest))]

      ;; There's room to do things with groups of loads and stores
      ;; at this point, as really we're just short-circuiting
      ;; the stack. However, because 'stnl' pops the stack twice,
      ;; and 'stl' only pops it once, it seems like each
      ;; combo needs to be handled uniquely. Perhaps.
      ;; Either way, we can spend a lot of time optimizing 
      ;; groups of loads and stores, which will improve our 
      ;; performance overall with a one-time investment of 
      ;; work.
      
         
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Atomics
      
      [`((,(? (lambda (op)
                (member op o2c-atomics)) op)
           ,v) ,rest ...)
        (cons (format "~a" (o2c-atomic `(,op ,v)))
              (o2c rest))]
      
      ;; I suspect I need to handle my atomic stores separately.
      [`((,(? is-lhs? op) ,v) ,rest ...)
        (case op
          [(stl) (cons (format "~a" (o2c-lhs `(,op ,v) 'areg))
                       (o2c rest))]
          [(stnl) (cons (format "~a" (o2c-lhs `(,op ,v) 'pop))
                        (o2c rest))])]
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Secondaries
      ;;
      ;; I think these are just function calls.
      [`((,(? is-secondary? op)) ,rest ...)
        (cons
         (format "~a();~n" op)
         (o2c rest))]
      
      ;; If no patterns match, simply keep things as they are.
      [otherwise
       (cond
         [(null? otherwise) '()]
         [else
          (cons (car otherwise) (o2c (cdr otherwise)))])]
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