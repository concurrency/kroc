#|
slinker - helpers.ss
A Scheme linker for Transputer bytecodes targeting the Transterpreter
Copyright (C) 2004-2008 Matthew C. Jadud, Christian L. Jacobsen

 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2 of the License, or
 (at your option) any later version.
 
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
|#
#cs
(module helpers mzscheme
  (require 
   (prefix list: (lib "list.ss"))
   (prefix srfi: (lib "1.ss" "srfi"))
   (lib "pregexp.ss")
   "types.ss"
   "constants.ss"
   "version.ss"
   (lib "defmacro.ss"))
  
  (provide (all-defined)
           (all-from "types.ss")
           (all-from "constants.ss")
           )
  
  ;; Add this files revision to the version information
  (put-revision "$Revision: 3119 $")
  
  ;; We may get a list of characters or ints.
  ;; Do the conversion correctly.
  (define (list-of-nums->string ls)
    (cond
      [(andmap byte? ls)
       ;;(printf "Byte case.~n")
       (bytes->string/locale (list->bytes ls))
       ;;(list->string (map integer->char ls))
       ]
      [(andmap char? ls)
       ;;(printf "Char case~n")
       (list->string ls)
       ]
      [else
       (error 'list-of-nums->string "How did you get this? ~a~n" ls)]))
  
  
  ;; FIXME: I moved this into constants. Is this a good thing? perhaps not
  ;; but I dont care, I need to debug a function in constants and this
  ;; does the trick.
  #|
     (define-macro debug
                   (lambda (level . bodies)
                     `(if (member ,level *debug-modes*)
                          (parameterize ([current-output-port (current-error-port)])
                            (begin ,@bodies)))))
     |#
  
  (define mapl
    (lambda (f ls)
      (let ([v (list->vector ls)])
        (let loop ([i 0])
          (unless (>= i (length ls))
            (vector-set! v i (f (vector-ref v i)))
            (loop (add1 i))))
        (vector->list v))))
  
  #|(define-syntax debug
       (lambda (x)
         (syntax-case x ()
           [(_ bodies ...)
            (if #f
                (syntax (begin 
                          (let ([cop (current-output-port)])
                            (current-output-port (current-error-port))
                            bodies ...
                            (current-output-port cop)
                            )))
                (syntax (begin (void))))]
           )))|#
  
  
  
  (define decode-num
    (lambda (code pos)
      ;;(printf "Decoding number position: ~a~n" pos)
      (let ([building #t]
            [startpos pos]
            [num 0])
        (let loop ()
          (unless (not building)
            ;; In V300, these should all be integers
            ;; Bind it to the identity function.
            (let* ([char->integer (lambda (x) x)]
                   [byte (char->integer (if (list? code)
                                            (list-ref code pos)
                                            (vector-ref code pos))
                                        )]
                   [fn (>> byte 4)])
              (set! pos (add1 pos))
              (set! num (bitwise-ior num (bitwise-and byte 15)))
              #|
	    ;;(printf "byte: ~s~nnum: ~s~nfn: ~s~npos: ~s~n" byte num fn pos)
	    |#
              (cond
                [(= fn *PFIX*) (set! num (<< num 4))]
                [(= fn *NFIX*) (set! num (<< (bitwise-mattnot num) 4))]
                [(= fn *LDC*) 
                 (set! building #f)]
                [else (error (format "Bad TCE Number @ ~a~n" startpos))]))
            (loop)))
        (values num pos))))
  
  (define decode-ins
    (lambda (code pos)
      ;;(printf "Decoding instruction position: ~a~n" pos)
      (let ([building #t]
            [startpos pos]
            [op 0]
            [fn 0])
        (let loop ()
          (unless (not building)
            ;; In V300, these should all be integers
            ;; Bind it to the identity function
            (let* ([char->integer (lambda (x) x)]
                   [byte (char->integer (if (list? code)
                                            (list-ref code pos)
                                            (vector-ref code pos))
                                        )])
              ;;(debug (fprintf (current-error-port) "DEC: ~a FN: ~a OP: ~a~n" (hex byte) (hex fn) (hex op)))
              (set! fn (>> byte 4))
              (set! pos (add1 pos))
              (set! op (bitwise-ior op (bitwise-and byte 15)))
              (cond
                [(= fn *PFIX*) (set! op (<< op 4))]
                ;; V300 : Will bitwise-mattnot be a problem?
                [(= fn *NFIX*) (set! op (<< (bitwise-mattnot op) 4))]
                [else 
                 (set! building #f)]))
            (loop)
            ))
        
        (values fn op startpos pos)
        )))
  
  
  
  
  (define copy-data
    (lambda (ls start end)
      (map
       (lambda (n)
         (if (list? ls)
             (list-ref ls n)
             (vector-ref ls n))
         )
       (map (lambda (n)
              (+ start n))
            (reverse 
             (map sub1 (make-lon (- end start))))))))
  
  (define make-lon
    (lambda (n)
      (if (zero? n) '() (cons n (make-lon (sub1 n))))))
  
  (define hex
    (lambda (n)
      (let ([num (cond 
                   [(char? n) (char->integer n)]
                   [(integer? n) n]
                   [else n])])
        (if (< num 16)
            (format "0~a" (number->string num  16))
            (number->string num 16)))))
  
  #|
     (define dump-print-hash
       (lambda (pass)
         
         (let* ([h (pass-bytes pass)]
                [keys (list:quicksort
                      (hash-table-map
                       h (lambda (k v) k)) <)])
           (for-each
            (lambda (k)
              (let* ([struct 
                      (hash-table-get
                       h k (lambda () 'foo))]
                     [vec
                      (struct->vector struct)])
                (if (spec? struct)
                    (printf "~a\t<~a>\t~a~n"
                            k 
                            (hex (vector-ref vec 1))
                            (map (lambda (c)
                                   (hex (char->integer c)))
                                 (vector-ref vec 2)))
                    (printf ;;"~a\t<~a>\t\t~a~n"
                     "~a\t~a~n"
                     k 
                     ;;(hex (vector-ref vec 1))
                     (hex (vector-ref vec 2)))
                    )))
            keys))
         
         pass))
     
     
     (define dump-print-hash
       (lambda (pass)
         
         (let* ([h (pass-bytes pass)]
                [keys (list:quicksort
                      (hash-table-map
                       h (lambda (k v) k)) <)])
           (for-each
            (lambda (k)
              (let* ([struct 
                      (hash-table-get
                       h k (lambda () 'foo))]
                     [vec
                      (struct->vector struct)])
                (if (spec? struct)
                    (printf "~a\t<~a>\t~a~n"
                            k 
                            (hex (vector-ref vec 1))
                            (map (lambda (c)
                                   (hex (char->integer c)))
                                 (vector-ref vec 2)))
                    (printf ;;"~a\t<~a>\t\t~a~n"
                     "~a\t~a~n"
                     k 
                     ;;(hex (vector-ref vec 1))
                     (let ([v (vector-ref vec 2)])
                       (cond
                         [(and (list? v)
                               (andmap char? v))
                          (map char->integer v)]
                         [(and (list? v)
                               (andmap integer? v))
                          v]
                         [else
                          (hex v)]))
                    ))))
            keys))
         
         pass))
     |#
  
  ;; Right
  (define >> (lambda (n m) (arithmetic-shift n (* -1 m))))
  ;; Left
  (define << (lambda (n m) (bitwise-and (arithmetic-shift n m) *MAXWORD*)))
  
  #|
     (define bitwise-mattnot
       (let ()
         (define pad
           (lambda (ls leng)
             ;;(printf "ls: ~a~n" ls)
             (if (= (* 8 *WORDSIZE*) (length ls))
                 ls
                 (let ([diff (abs (- leng (length ls)))])
                   ;;(printf "Diff: ~a~n" diff)
                   (append (srfi:make-list diff #\0) ls)))))
         (lambda (n)
           (string->number
            (list-of-nums->string
             (map (lambda (c)
                    (if (equal? #\0 c)
                        #\1 #\0))
                  
                  (pad (string->list 
                        (number->string n 2)) 
                       (* 8 *WORDSIZE*))
                  
                  )) 2))))
     |#
  ;; Rewrite of bitwise mattnot which is actually efficient.
  ;; The magical numbers are (2^(bits-in-word)) -1 -x
  (define bitwise-mattnot  ;;actually cljnot
    (lambda (x)
      (cond
        [(= *WORDSIZE* 1) (- 255 x)]
        [(= *WORDSIZE* 2) (- 65535 x)]
        [(= *WORDSIZE* 4) (- 4294967295 x)]
        [(= *WORDSIZE* 8) (- 18446744073709551615 x)]
        )))
  ;; Bytestrings are just... well, they should just be lists of 
  ;; ints or something like that. So... I'll rewrite this to use
  ;; bytestrings. Or, perhaps not... perhaps this should just work?
  #|(define bitwise-mattnot
       (let ()
         (define pad
           (lambda (ls leng)
             ;;(printf "ls: ~a~n" ls)
             (if (= (* 8 *WORDSIZE*) (length ls))
                 ls
                 (let ([diff (abs (- leng (length ls)))])
                   ;;(printf "Diff: ~a~n" diff)
                   (append (srfi:make-list diff #\0) ls)))))
         (lambda (n)
           (let ([partial
                  (map (lambda (c)
                         (if (equal? #\0 c)
                             #\1 #\0))
                       
                       (pad (string->list 
                             (number->string n 2)) 
                            (* 8 *WORDSIZE*))
                  
                       )])
             ;; This doesn't seem to be a problem under V300
             ;;(printf "Partial: ~s~n" partial)
             (string->number (list->string partial) 2)
             )))) |#
  
  ;; FIXME: Does not care about das wordsize of anything 
  ;; Turn what we consider to be an unsigned number into a signed one. Ie
  ;; currently decode-num always returns unsigned stuff (in schemes eyes) but
  ;; sometimes we wanna interpret it as signed...
  (define (->signed n)
    (if (> (bitwise-and #x80000000 n) 0)
        (- n 4294967296)
        n))
  
  ;; V300
  (define (bytes->symbol ls)
    (string->symbol
     (bytes->string/locale
      (list->bytes ls))))
  (define (symbol->bytes sym)
    (string->bytes/locale
     (symbol->string sym)))
  
  ;;insert-instruction
  ;; We should use this whenever inserting new instructions into the hash.
  ;; We should use this whenever inserting new instructions into the hash.
  ;; If there is already an instruction at that location, throw an error
  ;; and die.
  (define insert-instruction
    (case-lambda 
      [(hash loc ins)
       (let ()
         (if (hash-table-get hash loc (lambda () #f))
             (error 
              (format "Tried to insert instruction ~a into ~a."
                      ins loc))
             (hash-table-put! hash loc ins))
         hash)]
      [(oldhash newhash loc ins)
       (let ()
         (if (hash-table-get oldhash loc (lambda () #f))
             (error 'insert-instruction (format "Tried to insert instruction ~a into ~a."
                                                ins loc))
             (hash-table-put! newhash loc ins))
         newhash)]))
  
  
  
  ;; next-key takes the location of a setlab, and finds
  ;; the next valid table entry after it. This way,
  ;; we don't point to an instruction that is falling out
  ;; in this pass.
  (define get-next
    (lambda (ls key)
      (call/cc 
       (lambda (break)
         (for-each
          (lambda (v)
            (if (< key v)
                (break v)))
          ls)
         #f))))
  
  (define next-key
    (lambda (h)
      (let ([keys (list:quicksort 
                   (hash-table-map
                    h (lambda (k v) k)) <)])
        (lambda (key)
          (get-next keys key)))))
  
  #|
(define next-key
  (lambda (h key)
    ;; Grab the size of the table in terms of the
    ;; largest numbered instruction in it.
    (let ([h-size (car
		   (reverse
		    (list:quicksort
		     (hash-table-map h (lambda (k v) k)) <)))])
      ;; Loop from the current key position to the 
      ;; next valid jump point.
      (let loop ([n key])
	(let ([val (hash-table-get h n (lambda () #f))])
	  ;;(fprintf (current-error-port) "Checking ~a (~a)~n" n val)
	  (cond
	   ;; If we run off the end, that's no good. Die.
	   [(> n h-size) 
            (error 
             (format "Ran off table in next-key; looking for ~a in ~a~n"
                     key h))]
	   ;; If we found a value, and it isn't a setlab,
	   ;; go ahead and jump to it.
	   [(and val (not (or (ulabel? val)
			      (procentry? val)
			      (globalname? val)
			      ))) n]
	   ;; Otherwise, keep looking.
	   [else (loop (add1 n))])
	  )))
    ))
|#
  
  
  (define adjust
    (lambda (num)
      (let ([max (expt 2 (sub1 (* 8 *WORDSIZE*)))])
        (if (> num max)
            (begin
              #|
	    (fprintf (current-error-port) 
		     "~a -> ~a~n" num 
		     (- num (expt 2 (* 8 *WORDSIZE*))))
	    |#
              (- num (expt 2 (* 8 *WORDSIZE*)))
              )
            num))))
  
  (define prefix
    (lambda (fn op)
      (let ([op (adjust op)])
        (cond
          [(and (>= op 0) (< op 16)) (list (bitwise-ior (<< fn 4) op))]
          [(<  op 0)
           (let* ([twos-compliment (add1 (bitwise-mattnot (abs op)))]
                  [myop (prefix *NFIX* (>> (bitwise-mattnot twos-compliment) 4))])
             (append myop 
                     (list (bitwise-ior (<< fn 4) (bitwise-and twos-compliment #xF)))))]
          [(>= op 16)
           (let ([myop (prefix *PFIX* (>> op 4))])
             (append myop 
                     (list (bitwise-ior (<< fn 4) (bitwise-and op #xF)))))]
          [else (error (format "prefix failed.~nfn: ~s~nop: ~s~n" 
                               fn op))]))))
  
  
  
  (define list-intersperse
    (lambda (obj ls)
      (cond
        [(null? ls) '()]
        [(null? (cdr ls)) (list (car ls))]
        [else (cons (car ls) 
                    (cons obj
                          (list-intersperse obj (cdr ls))))]
        ))) 
  
  ;; Fixes by clj3 20060112
  ;; - Now record the min key, soo when we walk the bigv, we can 
  ;;   start at 'min'
  ;; - Elements are stored in the vector, used to be (index val)
  ;;   pairs, I think it is more correct that it should be
  ;;   (old-key val) pairs, which it now is.
  (define hash->list
    (lambda (h)
      (let ([max 0]
            [min -1]
            [actual 0])
        
        (hash-table-for-each
         h (lambda (k v)
             (if (> k max)
                 (set! max k))
             (if (= min -1) (set! min k)
                 (begin
                   (if (< k min) (set! min k))))
             (set! actual (add1 actual))))
        
        (let ([bigv (make-vector (add1 max) 'bogus)])
          
          (hash-table-for-each
           h (lambda (k v)
               (vector-set! bigv k v)))
          
          (let ([loop-end (vector-length bigv)]
                [actualv (make-vector actual)])
            (let loop ([n min]
                       [counter 0])
              (unless (>= n loop-end)
                (let ([val (vector-ref bigv n)])
                  (if (equal? 'bogus val)
                      (loop (add1 n) counter)
                      (begin
                        (vector-set! actualv counter (cons n val))
                        (loop (add1 n) (add1 counter)))))))
            (vector->list actualv))))))
  
  
  (define hash->list-orig
    (lambda (h)
      (list:quicksort
       (hash-table-map h (lambda (k v) (cons k v)))
       (lambda (p1 p2)
         (< (car p1) (car p2))))))
  
  ;; This function only works when the hash table it is used on has keys
  ;; strictly increasing from 0..n-1 where n is the size of the hash table.
  (define hash->vector
    (lambda (h)
      (let ([vec (make-vector (hash-table-count h))])
        (hash-table-for-each h
                             (lambda (k v)
                               (vector-set! vec k v)))
        vec)))
  
  
  (define pprint-inst
    (lambda (h)
      (let* (
             [inst (hash-table-map h (lambda (k v) (list k v)))]
             [sortfun ;;(if (number? (car inst))
              (lambda (subls1 subls2)
                (< (car subls1) (car subls2)))
              ;;(error "What?")
              ]
             [sinst (list:quicksort inst sortfun)]
             [ready (map (lambda (ls)
                           (list (car ls)
                                 (struct->vector (cadr ls)))) sinst)])
        (for-each
         (lambda (sls)
           (fprintf (current-error-port) "~a" (car sls))
           (for-each
            (let ([tabs "\t"])
              (lambda (e)
                (if (symbol? e)
                    (let* ([sym (pregexp-split ":" (symbol->string e))]
                           [sym (if (= (length sym) 1)
                                    (car sym)
                                    (cadr sym))])
                      (cond
                        [(< (string-length sym) 8) 
                         (set! tabs "\t\t")]
                        [(and (> (string-length sym) 8)
                              (< (string-length sym) 16))
                         (set! tabs "\t")]
                        [else
                         (set! tabs "")])
                      (fprintf (current-error-port) "\t~a~a" sym tabs))
                    (fprintf (current-error-port) "\t~a"
                             (cond
                               [(number? e) (hex e)]
                               [(list? e)
                                (map (lambda (o)
                                       (cond
                                         [(number? o) (hex o)]
                                         [else o])) e)]
                               [else e]))
                    ))) ;;end let
            (vector->list (cadr sls)))
           (fprintf (current-error-port) "~n")) ready)
        )))
  
  
  (define build-get-distance-cache
    (lambda (h)
      (build-get-distance-cache-partial (hash->vector h) 0 (hash-table-count h))))
  
  
  ;; This thing expects a vector instead of a hash, as if this function is
  ;; going to do the conversion itself, it becomes somewhat expensive if it
  ;; is called in partial mode, where it might (for a given file) be called n
  ;; times (where n is reasonably large) before the entire file has been
  ;; calculated
  
  ;; Does this only ever get called after renumber so we can use 
  ;; hash->vector? which might be faster. Thoug it does not give us
  ;; a vector-for-each, but I am sure that Matt could teach me how
  ;; to define a syntax so I can write that myself :)
  (define build-get-distance-cache-partial
    (lambda (inst-vect start end)
      (let ([dist (if (= start 0) 0 (vector-ref get-distance-cache (sub1 start)))]
            [last-dist 0])
        ;; Set up the get distance cache, a vector
        (if (= start 0)
            (set-get-distance-cache! (make-vector (vector-length inst-vect))))
        ;; Calculate cumulative distances for each instruction
        (let loop ([i start])
          (unless (>= i end)
            (let ([obj (vector-ref inst-vect i)])
              (cond
                [(binary? obj)
                 (set! dist (+ dist (length (binary-value obj))))]
                [(load-label? obj)
                 ;; Less than optimal
                 (set! dist (+ 2 (+ dist (* 2 *WORDSIZE*)))) ]
                [(or (load-label-difference? obj)
                     (ujump? obj))
                 ;; Less than optimal
                 (set! dist (+ dist (* 2 *WORDSIZE*))) ]
                [(stubname? obj) (set! dist (+ dist (* 2 *WORDSIZE*)))]
                ;; For the future: This, technically, limits the number of
                ;; entries we can have in the ffi table. Millions (on 32-bit)
                ;; or thousands (16-bit), but still. Just sayin'.
                [(ffi-stubname? obj) (set! dist (+ dist (* 2 *WORDSIZE*)))]
                
                ;; WARNING 20061228 MCJ
                ;; Not sure how we should handle this at all.
                [(i64toreal? obj) (set! dist (+ dist (* 2 *WORDSIZE*)))]
                
                [else
                 (error 'build-get-distance-cache
                        (string-append
                         "failed mostly horrifically.~n"
                         "Cannot deal with record of type: ~a~n") obj)])
              (debug 'get-distance
                     (begin
                       (printf "~a:\t~a\t~a\t~a\t~a  ~a ~a~n" 
                               i 
                               (hash-table-get (Instruction-meta obj) 'line (lambda () "-"))
                               dist
                               (- dist last-dist)
                               (cond
                                 [(binary? obj)                "bin "]
                                 [(load-label? obj)            "ll  "]
                                 [(load-label-difference? obj) "lld "]
                                 [(ujump? obj)                 "ujmp"]
                                 [(stubname? obj)              "stub"]
                                 [(ffi-stubname? obj)          "ffis"])
                               (hash-table-get (Instruction-meta obj) 
                                               'abbreviation 
                                               (lambda () "---")) 
                               (hash-table-get (Instruction-meta obj) 
                                               'operand 
                                               (lambda () "")) 
                               )
                       (set! last-dist dist)))
              (vector-set! get-distance-cache i dist))
            (loop (add1 i)))))
      ))
  
  
  (define get-distance
    (lambda (h start end)
      ;; Sanity check
      (if (not (equal? (vector-length get-distance-cache) (hash-table-count h)))
          (begin
            (debug 'get-distance
                   (begin
                     (printf "gdcs: ~a\thtc: ~a~n" 
                             (vector-length get-distance-cache)
                             (hash-table-count h))
                     (printf "gdc: ~n~a~n" get-distance-cache)
                     (printf "ht: ~n~a~n" h)))
            (error 'get-distance "The cache is stale!")))
      (let* ([dist (abs (- 
                         (vector-ref get-distance-cache start) 
                         (if (= end 0) 0 (vector-ref get-distance-cache (sub1 end)))))]
             [dist 	(if (= start end)
                            (*  1 (* 2 *WORDSIZE*))
                            dist)])
        
        (debug 'get-distance
               (printf "Start: ~a End: ~a Dist: ~a~n" start end dist))
        dist)))
  
  #|
	   (debug 'get-distance
		  (let ([olddist (get-distance-old h start end)])
		    (printf "old: ~a, new: ~a~n" olddist dist)
                    (if (not (equal? dist olddist)) (error 'get-distance "olddist != dist"))))
  |#   
  
  ;;(define get-distance-old (void))
  (define get-distance-old
    (lambda (h s e)
      (let ([inc-op (if (< s e) 
                        add1 
                        sub1 
                        )]
            [dec-op (if (< s e) sub1 add1)]
            [test-op (if (< s e) > <)]
            [dist 0])
        ;;(printf "Start: ~a~nEnd: ~a~ninc: ~a~ntest: ~a~n" (inc-op s) (dec-op e) inc-op test-op)
        (let loop ([n (if (< s e)
                          (inc-op s)
                          s)])
          (unless (test-op n (if (< s e)
                                 (dec-op e)
                                 e))
            ;;(printf "Dist is : ~a~n" dist)
            (let ([obj (hash-table-get 
                        h n (lambda ()
                              (error 'helper-get-distance "get-distance: failed horrifically (aiming for ~a, died at ~a)." e n)))])
              (cond
                [(binary? obj) (set! dist 
                                     (+ dist
                                        (length (binary-value obj))))]
                [(load-label? obj)
                 
                 ;; Less than optimal
                 (set! dist (+ 2 (+ dist (* 2 *WORDSIZE*))))
                 
                 ]
                ;;(set! dist (+ dist (* 2 *WORDSIZE*)))]
                
                [(or (load-label-difference? obj)
                     (ujump? obj)
                     (stubname? obj))
                 ;; Less than optimal
                 (set! dist (+ dist (* 2 *WORDSIZE*)))
                 
                 ]
                
                [(stubname? obj) (set! dist (+ dist (* 2 *WORDSIZE*)))]
                
                [else
                 (printf "Location: ~a ~n" n)
                 (hash-table-for-each
                  h (lambda (k v)
                      (printf "K ~a\tV ~a~n" k v)))
                 (error 'helper-get-distance 
                        (string-append
                         "failed mostly horrifically.~n"
                         "Cannot deal with record of type: ~a~n") obj)]
                ))
            (loop (inc-op n))))
        dist)))
  
  
  ;; Pads an instruction out with (no-op) prefixes. If no pad amount is
  ;; specified, instructions are padded to be the correct length for the
  ;; current WORDSIZE setting. Otherwise pad-amount (no-op) prefix
  ;; instructions are inserted. pad-amount is in bytes/prefixing instrucitons
  ;; (ie a prefixing instruciton is one byte long)
  (define pad
    (case-lambda 
      [(ls)
       (pad ls (* 2 *WORDSIZE*))]
      [(ls pad-amount)
       (append (srfi:make-list (- pad-amount (length ls)) (<< *PFIX* 4))
               ls)]))
  
  
  (define-syntax while
    (lambda (x)
      (syntax-case x ()
        [(_ test bodies ...)
         (syntax 
          (let loop ()
            (if test
                (begin
                  bodies ...
                  (loop)))))])))
  
  ;; clj3 14/07/05 -- I have abandoned this for now, so it probably does not
  ;; work, especially considering that I have no clue how to make a syntax...
  ;; oh well... not even sure it needs to be a syntax... anyways, I was just
  ;; messing around
  (define-syntax vector-for-each
    (syntax-rules ()
      ((vector-for-each vec lam)
       (let ([vec-len (vector-length vec)])
         (let loop (i 0)
           (if (< i vec-len) 
               (begin
                 (lam i (vector-ref vec i))
                 (loop (add1 i)))))))))
  
  (define list->symbol
    (lambda (ls)
      (string->symbol (list-of-nums->string ls))))
  
  
  ;; Stole these from skroc - clj
  (define stripext
    (lambda (f)
      (pregexp-replace "\\.[^\\.]*$" f "")))
  
  (define ->tvmdbg
    (lambda (f)
      (string-append (stripext f) ".tvmdbg")))
  
  (define ->ffi
    (lambda (f)
      (string-append (stripext f) ".ffi")))
  
  )

