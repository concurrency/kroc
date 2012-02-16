#|
slinker - code-output.ss
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
(module code-output mzscheme
  (require 
   (prefix srfi: (lib "1.ss" "srfi"))
   (prefix list: (lib "list.ss"))
   (lib "date.ss")
   (lib "pregexp.ss")
   (lib "string.ss")
   "helpers.ss"
   "constants.ss"
   "version.ss"
   "code-translation-intermediate.ss"
   ;; For printing in binary2scheme
   (lib "pconvert.ss")
   (lib "pretty.ss")
   (prefix database: "c-code.scm")
   )
  (provide (all-defined)
           (all-from "code-translation-intermediate.ss"))
  
  ;; Add this files revision to the version information
  (put-revision "$Revision: 3148 $")
  
    ;; Ugh. This was replicated several times in this file.
  (define (byte-write ls op)
    (cond
      [(null? ls)]
      [else
       (write-byte (car ls) op)
       (byte-write (cdr ls) op)]))
  

  
  ;; SLOWDOWN
  ;; This whole process is slow.
  ;; ANTISLOWDOWN
  ;; This whole process has been made not slow by clj3
  (define (write-debugging-info h port)
   (let ()
      ;; What follows are a bunch of internal defines
      ;; They're all just little helper procedures for outputting
      ;; 32-bit words of all of the values we want to dump.
      ;; This way, we don't have to use format strings in C,
      ;; which would probably break...
      (define (write-dbg-info op str) (begin
					(debug 'tvmdbg (printf "tvmdbg file: ~a\n" str)) 
					(fprintf op "~a" str)))
      
      (define (int->charls n)
	(map (lambda (x) (bitwise-and 255 x))
	     (list (arithmetic-shift n -24)
		   (arithmetic-shift n -16)
		   (arithmetic-shift n -8)
		   n)))

      (define (write-dbg-words op offset linenum)
        (let ([offls (int->charls offset)]
              [linels (int->charls linenum)])
         
	  (debug 'tvmdbg (printf "tvmdbg offset: ~a\n" offls))
          (for-each 
           (lambda (offc) (write-byte offc op)) 
	   offls)
          
	  (debug 'tvmdbg (printf "tvmdbg line: ~a\n" linels))
          (for-each
           (lambda (linec) (write-byte linec op))
	   linels)
          ))
      
      
      ;; For those of you following along at home, everything above were helper procedures.
      ;; This is where the pass "actually begins"
      ;;(lambda
      ;;    (pass)
        (let* ([meta (pass-meta h)]
               [bytes (pass-bytes h)]
               [op port])
          
          ;; Format: filename[\n][offset][value]...
          ;; where each offset and value is a single word.
          
          ;; Drop the actual line number pairs, (offset, linenuber). Only the
          ;; first offset with a new number is dropped, except for the last
          ;; entry, which which is the ending offset and a line number value
          ;; of zero.
          (let* ([list-bytes (hash->list bytes)]
                 [offset (caar list-bytes)]
                 [last-line 0]
                 [last-file #f])
            ;; For each binary (main contain more than one bytecode byte)
            (for-each (lambda (pair)
                        ;; Get the length of this binary structure, ie how
                        ;; many bytecodes does it contain.
                        (let* ([len (length (binary-value (cdr pair)))]
                               [key (car pair)]
                               [inst-meta (Instruction-meta (cdr pair))]
                               [line (hash-table-get inst-meta 'line (lambda () 0))]
                               [proc (hash-table-get inst-meta 'proc (lambda () 'empty))]
                               [file (hash-table-get inst-meta 'file (lambda () 'empty))])
                          (if (not (equal? file #f))
                              (begin
                                ;; Write the filename if it is different from
                                ;; the one we saw on the last instruction. We
                                ;; also ignore the case when the instruction
                                ;; had no filename in its metadata
                                (if (and (not (equal? file last-file)) (not (equal? file 'empty)))
                                    (begin
                                      ;; Indicate the end of a file, and the beginning of a new one (or the
                                      ;; end of file) a line number value of zero. The offset is the last
                                      ;; offset in this file, so we can determine where the last line ends
                                      ;; in the bytecode stream.
                                      ;;
                                      ;; We do not subtract one from the offset, even though it is
                                      ;; further ahead than the last byet of the last line in the file.
                                      ;; This is cos we compaire offsets in pairs, we initially load the
                                      ;; first offset as the current offset. (loopstart) we then load the 
                                      ;; next offset, and check if: 
                                      ;;   current_offset <= iptr_offset < next_offset
                                      ;; It is so that the second test will work unchanged that we do not
                                      ;; subtract one from the offset here, as we would then have to
                                      ;; change the test in that case from < to <=.
                                      (if (not (equal? last-file #f))
                                          ;; We dont want an end of file marker as the
                                          ;; very first thing
                                          (write-dbg-words op offset 0))
                                      (write-dbg-info op 
                                                      ;; V300 Was list->string
                                                      (list->bytes file))
                                      (newline op)
                                      (set! last-file file)))
                                ))
                          ;; Line number zero is invalid. Our line numbers
                          ;; are one indexed. Also unless we have had a
                          ;; filename, we will not dump line number info.
                          ;;(printf "~a ~a~n" key line)
                          (if (and (not (zero? line)) (not (equal? #f last-file)))
                              ;; If the last line is not equal to the current
                              ;; line, then drop a new (offset, linenum) pair
                              (if (not (= last-line line))
                                  (begin
                                    (write-dbg-words op offset line)
                                    (set! last-line line))))
                          ;; All the bytes in one binary structure all
                          ;; have the same debug info so we dont have
                          ;; to worry about the debug info changeing
                          ;; suddenly within a binary structure.
                          ;; But we do have to remember to increment the
                          ;; offset according to the number of bytes in
                          ;; the binary struycture.
                          ;; We must do this regardless of wether we
                          ;; dropped debugging info or not.
                          (set! offset (+ offset len))
                          ))
                      list-bytes)
            
            
            ;; The very end of the file
            (write-dbg-words op offset 0)
            (close-output-port op)
	    )
          
          h)));)

  (define (dump-debugging-info h)
    (write-debugging-info h (open-output-file (->tvmdbg (get-output-filename)) 'replace)))
  
  (define renumbering-start 0)
  
  (define calculate-renumbering-point
    (lambda (pass)
      (let ([meta (pass-meta pass)]
            [bytes (pass-bytes pass)])
        (set! renumbering-start 
              (apply + (map
                        (lambda (lib-pass)
                          (let ([libmeta (pass-meta pass)])
                            (hash-table-get libmeta 'pass-length)))
                        *precompiled-libraries*))))
      pass))
  
  
  (define min-dump 0)
  (define max-dump 10000000)
  
  (define dump-range1
    (lambda (pass)
      (set! min-dump 0)
      (set! max-dump 101)
      pass))
  
  (define dump-range2
    (lambda (pass)
      (set! min-dump 0)
      (set! max-dump 10)
      pass))
  
  (define check-abbreviation
    (lambda (pass)
      (let ([meta (pass-meta pass)]
	    [bytes (pass-bytes pass)])
	(hash-table-for-each
	 bytes (lambda (k v)
		 (if (binary? v)
		     (let ([h (Instruction-meta v)])
		       (if (not (hash-table-get h 'abbreviation (lambda () #f)))
			   (error 'no-abbrev "~a ~a~n" k v)))))))
      pass))
  
  
  (define debug-ip
    (lambda (pass)
      (let ([meta (pass-meta pass)]
	    [bytes (pass-bytes pass)]
	    [n 0])
	(debug 'debug-ip
               (printf "Cleng\tSloc\tSym\tBin~n")
               (for-each
                (lambda (p)
                  (let ([k (car p)]
                        [v (cdr p)])
		    (let ([h (Instruction-meta v)]
			  [line (lambda (h)
				  (let ([s (hash-table-get  h 'line (lambda () #f))])
				    (if s s "")))]
			  [sym (lambda (h)
				 (let ([s (hash-table-get h 'abbreviation (lambda () #f))])
				   (if s s "")))])
		      
		      (printf "~a\t~a\t~a\t~a~n"
			      n
			      (line h)
			      (sym h)
			      (map hex (binary-value v))
			      ))
		    (set! n (+ (length (binary-value v)) n)))
                  )
                (hash->list bytes)))
	pass)))
  
  
  (define instruction-list
    (lambda (pass)
      (let ([meta (pass-meta pass)]
	    [bytes (pass-bytes pass)]
	    [ls '()])
	(define uniq
	  (lambda (ls)
	    (cond
              [(null? ls) '()]
              [(member (car ls) (cdr ls))
               (uniq (cdr ls))]
              [else
               (cons (car ls)
                     (uniq (cdr ls)))])))
	(debug 'instruction-list 
	       (begin
		 (hash-table-for-each
		  bytes
		  (lambda (k v)
		    (let ([h (void)]
			  [str ""])
		      (if (binary? v)
			  (set! h (Instruction-meta v)))
		      (if (hash-table? h)
			  (let ([abb (hash-table-get h 'abbreviation (lambda () #f))])
			    (if abb
				(let ([abb (if (list? abb) (car abb) abb)])
				  (if (not (member abb ls))
				      (set! ls (cons (symbol->string abb) ls))))))))))
                 
		 (for-each
		  (lambda (s)
		    (printf "~a~n" s))
		  (uniq 
		   (list:quicksort ls (lambda (a b)
					(string<? a b)))))))
	pass)))
  
  (define dump-annotations
    (case-lambda
      [(pass)
       (debug 'dump-annotations
              (let ([meta (pass-meta pass)]
                    [bytes (pass-bytes pass)])
                (let ([bytes-sorted
                       (hash->list bytes)])
                  (printf "~a ~a ~a ~a ~a~n"
                          (->justified-string "Hash" 5 'right)
                          (->justified-string "SrcLn" 5 'right)
                          (->justified-string "Mnemn" 10 'left )
                          (->justified-string "Operand" 10 'left)
                          (->justified-string "Decoded" 10 'left))
                  
                  (for-each
                   (lambda (p)
                     (if (and (>= (car p) min-dump)
                              (<= (car p) max-dump))
                         (output-instruction p)))
                   bytes-sorted))))
       pass]))
  
  
  
  
  (define ->justified-string
    (lambda (v size loc)
      (let ([blanksize (- size (string-length (format "~a" v)))])
	(if (> 0 blanksize)
	    (set! blanksize 0))
	(let ([str (make-string blanksize #\space)])
	  (if (equal? loc 'right)
	      (format "~a~a" str (format "~a" v))
	      (format "~a~a" (format "~a" v) str))))))
  
  
  (define output-instruction
    (lambda (p)
      (let* ([table-loc (car p)]
	     [inst (cdr p)]
	     [meta (Instruction-meta inst)]
	     [src-loc 
	      (begin
		(hash-table-get meta 'line (lambda () 'x)))]
	     [abbrev (hash-table-get meta 'abbreviation (lambda () 'xxx))]
	     [operand (hash-table-get meta 'operand (lambda () 'x))]
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
          
          
          
	  (printf "~a ~a ~a ~a ~a~n"
		  (->justified-string table-loc 5 'right)
		  (->justified-string src-loc 5 'right)
		  (->justified-string abbrev  10 'left)
		  (->justified-string operand 10 'left)
		  (->justified-string binary-decode 20 'left)
		  ;;(if comment (->justified-string comment 10 'left) "")
		  )
          ))))
  
  
  (define instruction-string
    (lambda (p)
      (let* ([table-loc (car p)]
	     [inst (cdr p)]
	     [meta (Instruction-meta inst)]
	     [src-loc 
	      (begin
		;;(hash-table-for-each meta (lambda (k v) (printf "OI: ~a ~a~n" k v)))
		(hash-table-get meta 'line (lambda () 'x)))]
	     [abbrev (hash-table-get meta 'abbreviation (lambda () 'xxx))]
	     [operand (hash-table-get meta 'operand (lambda () 'x))]
	     [comment (hash-table-get meta 'comment (lambda () ""))])
	(let ([binary-decode ""])
	  
	  (if (not (null?  (binary-value inst)))
	      (let-values ([(fn op sp p)
			    (decode-ins (mapl integer->char (binary-value inst)) 0)])
		(let ([primary? (lambda (op) (< op 15))]
		      [secondary? (lambda (x) #t)])
		  (cond
                    [(primary? fn)
                     (if (> op #x80000000)
                         (set! op (* -1 (add1 (bitwise-mattnot op)))))
                     (set! binary-decode 
                           (format "~a" 
                                   (list-ref primaries fn) 
                                   ;; (hex op)
                                   ))]
                    [(secondary? op)
                     (let ([list-index (quotient op 16)]
                           [abbrv-index (remainder op 16)])
                       (set! binary-decode 
                             (format "~a" 
                                     (list-ref (list-ref secondaries list-index)
                                               abbrv-index))))]))))
          binary-decode
          ))))
  
  
  
  
  ;; When loading this hash table, it will need to be mapped
  ;; over with 'make-binary' applied to all the values in the table.
  ;; This is because I'd like to be able to read in this 'let',
  ;; and 'eval' it, and 'make-binary' isn't in the top-level environment.
  (define binary2scheme
    (lambda (pass)
      (let ([meta  (pass-meta pass)]
            [bytes (pass-bytes pass)])
        (let* ([op (open-output-file (get-output-filename) 'replace)]
               ;; This is a list of pairs; hash-key-number and the instruction
               [ilist (hash->list bytes)]
               ;; 20060502 MCJ
               ;; Adding 'abbreviation and 'operand to this list,
               ;; which will probably expand .precomp files significantly.
               ;; This can be dealt with in due time... but this is
               ;; the easy way of getting more data for the bytecode->C
               ;; conversion path; perhaps I'll shorten "abbreviation" throughout
               ;; the entire toolchain... and "operand", while I'm at it...
               [allowed-metadata '(abbreviation operand line proc file)]
               [scheme-structure
                ;; When reading in, we'll use let-values, and get
                ;; the globalnames and instructions in two separate 
                ;; hashes.
                `(let ([h (make-hash-table)]
                       [globals (make-hash-table)]
                       [precomp-meta (make-hash-table)]
                       [dynlib (make-hash-table)]
		       [p hash-table-put!]
		       [l list])
                   ,@(hash-table-map
                      globalnames
                      (lambda (k v)
                        `(hash-table-put! globals (quote ,k) ,v)))
                   ,@(hash-table-map
                      spragma-dynlibs
                      (lambda (k v)
                        `(hash-table-put! dynlib (quote ,(string->symbol k)) (quote ,v))))
                   
                   ;; HERE IS SOME NEW STUFFS
                   ;; This is a sequence of things that get crammed into the precompiled
                   ;; metadata.
                   ;; This way, we can track things like when the library was compiled,
                   ;; the library version, and other stuffs.
                   
                   ;; Library version
                   (hash-table-put! precomp-meta 'library-version ,*precompiled-library-version*)
                   
                   ;; When the library was compiled
                   (hash-table-put! precomp-meta 'compiled-date
                                    ,(date->string
                                      (seconds->date
                                       (current-seconds))
                                      #t))
                   
                   ;; This is ugly, but works.
                   ;; This tells us the stubnames that are contained
                   ;; within this library. We use this to calculate which libraries
                   ;; we still need to import to completely resolve the dependancies
                   ;; at compile time.
                   ,(let ([stubnames '()])
                      (define (stubname->symbol v)
                        (cond
                          [(list? (stubname-value v))
                           (list->symbol (stubname-value v))]
                          [(string? (stubname-value v))
                           (string->symbol (stubname-value v))]
                          [else
                           (error 
                            'output-library
                            "Cannot mangle stubname; is neither a list of characters nor a string: ~a~n" 
                            (stubname-value v))]))
                      (map 
                       (lambda (p)
                         (cond
                           [(stubname? (cdr p))
                            (set! stubnames (cons (stubname->symbol (cdr p)) stubnames))]))
                       ilist)
                      `(hash-table-put! precomp-meta 'required-stubnames (quote ,stubnames)))
                               
                   
                   ;; This is the bytecode.
                   ,@(map (lambda (p)
			    (cond
                              [(binary? (cdr p))
                               `(p 
                                 h ,(car p)
                                 ;; Original version
                                 ;; (quote ,(binary-value (cdr p)))
                                 
                                 ;; This is a start at including annotation
                                 ;; in the library output.
                                 ,@(if (member 'library-debug *debug-modes*)
                                       `((list
                                          (quote ,(cons 'b (binary-value (cdr p))))
                                          '(,@(hash-table-map
                                               (Instruction-meta (cdr p))
                                               (lambda (k v)
                                                 `(,k ,v))))))
                                       ;;`((l (quote ,(cons 'b (binary-value (cdr p)))) '())))
                                       `((l (quote ,(cons 'b (binary-value (cdr p))))
					    '(,@(srfi:filter list? (hash-table-map
                                                                    (Instruction-meta (cdr p))
                                                                    (lambda (k v)
                                                                      (if (member k allowed-metadata)
                                                                          `(,k ,v) #f))))))))
                                 )]
                              [(stubname? (cdr p))
                               `(p 
                                 h ,(car p)
                                 ,@(if (member 'library-debug *debug-modes*)
                                       `((list 
                                          (quote ,(cons 'stub (stubname-value (cdr p))))
                                          '(,@(hash-table-map
                                               (Instruction-meta (cdr p))
                                               (lambda (k v)
                                                 `(,k ,v))))))
                                       ;;`((l (quote ,(cons 'stub (stubname-value (cdr p)))) '())))
                                       `((l (quote ,(cons 'stub (stubname-value (cdr p))))
					    '(,@(srfi:filter list? (hash-table-map
                                                                    (Instruction-meta (cdr p))
                                                                    (lambda (k v)
                                                                      (if (member k allowed-metadata)
                                                                          `(,k ,v) #f))))))))
                                 )]
                              [(ffi-stubname? (cdr p))
                               `(p 
                                 h ,(car p)
                                 ,@(if (member 'library-debug *debug-modes*)
                                       `((list 
                                          (quote ,(cons 'ffi (ffi-stubname-value (cdr p))))
                                          '(,@(hash-table-map
                                               (Instruction-meta (cdr p))
                                               (lambda (k v)
                                                 `(,k ,v))))))
                                       ;;`((l (quote ,(cons 'ffi (ffi-stubname-value (cdr p)))) '())))
                                       `((l (quote ,(cons 'ffi (ffi-stubname-value (cdr p))))
					    '(,@(srfi:filter list? (hash-table-map
                                                                    (Instruction-meta (cdr p))
                                                                    (lambda (k v)
                                                                      (if (member k allowed-metadata)
                                                                          `(,k ,v) #f))))))))
                                 
                                 )]
                              
                              ))
                          ilist)
                   
                   (values globals dynlib precomp-meta h))])
          ;;(pretty-print (cons 'begin (print-convert scheme-structure)) op)
	  (if (get-use-compact-libraries)
	    ;; We are using compact, ie MzScheme bytecompiled libraries, so
	    ;; compile and save.
	    (write (compile scheme-structure) op)
	    ;; We are just saving a load of (pretty) scheme statements
	    (pretty-print scheme-structure op))
          (close-output-port op))
        pass
        )))
  
  
  
  
  
  (define boolinvert2binary
    (lambda (h)
      (let ([meta (pass-meta h)]
            [h (pass-bytes h)])
        (hash-table-for-each
         h (lambda (k v)
             (if (boolinvert? v)
                 (hash-table-put! 
                  h k 
                  ;; WARNING
                  ;; This is hacked for the moment.
                  (make-binary 
		   (Instruction-meta v)
                   (prefix *OP* *BOOLINVERT*) ))
                 )))
        (make-pass meta h))))
  
  (define widenshort2binary
    (lambda (h)
      (let ([meta (pass-meta h)]
            [h (pass-bytes h)])
        (hash-table-for-each
         h (lambda (k v)
             (if (widenshort? v)
                 (hash-table-put! 
                  h k 
                  ;; WARNING
                  ;; This is hacked for the moment.
                  (make-binary 
		   (Instruction-meta v)
                   (prefix *OP* *WIDENSHORT*) ))
                 )))
        (make-pass meta h))))
  
  (define burp
    (lambda (x)
      (printf "~nHELLO~n")
      x))
  
  (define renumber2
    (case-lambda
      [(h)
       ;; We call this when renumbering code that is being
       ;; compiled.
       (renumber2 h 0)]
      ;; But this signature lets me pass in a starting point
      ;; for renumbering; useful for precompiled libraries
      ;; that need to be renumbered.
      [(h renumber-start)
       (let ([meta (pass-meta h)]
             [h    (pass-bytes h)])
         (let* ([code     (hash->list h)]
                [map-h    (make-hash-table)]
                [new-code (make-hash-table)]
                [lookup 
                 (let ([falseproc (lambda () #f)])
                   (lambda (old)
                     (let ([this (hash-table-get map-h old falseproc)]
                           [next old])
                       (if this
                           this
                           (begin
                             (let loop ([old next])
                               (unless (hash-table-get map-h old falseproc)
                                 (set! next old)
                                 (loop (add1 old))))
                             (hash-table-get map-h (add1 next))
                             )))))])
           
           ;; This provides the old-to-new mapping
           ;; Purely side-effecting on map-h
           ;; WARNING ... now using renumbering-start...
           ;; may break lots of things.
           (let ([c renumber-start])
             (for-each 
              (lambda (i)
                ;;(printf "Mapping ~a to ~a~n" i c)
                (hash-table-put! map-h (car i) c)
                (set! c (add1 c))) code))
           
           #|           
           ;;Renumber the globalnames table
	   (if (equal? (hash-table-get 
                        meta 'extension (lambda () #f)) "precomp")
               (let ([new (make-hash-table)]
                     [globalnames (hash-table-get meta 'globalnames)])
		 (debug 'output (printf "Handling precomp globalnames~n"))
                 (hash-table-for-each
                  globalnames
                  (lambda (k v)
                    (debug 'output (printf "Globalname mapping ~a (~a -> ~a)~n" k v (lookup v)))
                    (hash-table-put! new k (lookup v))))
                 (hash-table-put! 
                  meta 'globalnames globalnames)))
|#
           
           (let ([c renumber-start])
             (for-each
              (lambda (i)
                ;;(printf "About to handle ~a~n" i)
                (let* ([loc (car i)]
                       [ins (cdr i)]
                       [res
                        (cond
                          [(ujump? ins)
                           (hash-table-put! 
                            new-code
                            c (make-ujump
			       (Instruction-meta ins)
                               (lookup (ujump-value ins))
                               (ujump-fn ins)
			       ))]
                          [(load-label? ins)
                           (hash-table-put!
                            new-code
                            c (make-load-label
			       (Instruction-meta ins)
                               (lookup (load-label-value ins))
			       ))]
                          [(load-label-difference? ins)
                           (hash-table-put! 
                            new-code
                            c (make-load-label-difference
			       (Instruction-meta ins)
                               (lookup (load-label-difference-start ins))
                               (lookup (load-label-difference-end ins))
			       ))]
                          [else (hash-table-put! new-code c ins)])])
                  (set! c (add1 c))
                  res))
              code))
           
           (make-pass meta new-code)
           ))]))
  
  
  (define renumber3
    (case-lambda
      [(h)
       ;; We call this when renumbering code that is being
       ;; compiled.
       (renumber3 h 0)]
      ;; But this signature lets me pass in a starting point
      ;; for renumbering; useful for precompiled libraries
      ;; that need to be renumbered.
      [(h renumber-start)
       (let ([meta (pass-meta h)]
             [h    (pass-bytes h)])
         (let* ([code     (hash->list h)]
                [map-h    (make-hash-table)]
                [new-code (make-hash-table)]
                [lookup 
                 (let ([falseproc (lambda () #f)])
                   (lambda (old)
                     (let ([this (hash-table-get map-h old falseproc)]
                           [next old])
                       (if this
                           this
                           (begin
                             (let loop ([old next])
                               (unless (hash-table-get map-h old falseproc)
                                 (set! next old)
                                 (loop (add1 old))))
                             (hash-table-get map-h (add1 next))
                             )))))])
           
           ;; This provides the old-to-new mapping
           ;; Purely side-effecting on map-h
           ;; WARNING ... now using renumbering-start...
           ;; may break lots of things.
           (let ([c renumber-start])
             (for-each 
              (lambda (i)
                ;;(printf "Mapping ~a to ~a~n" i c)
                (hash-table-put! map-h (car i) c)
                (set! c (add1 c))) code))
           
           ;;Renumber the globalnames table
	   (let ([new (make-hash-table)]
		 ;;[globalnames (hash-table-get meta 'globalnames)]
		 )
	     (debug 'output (printf "Handling precomp globalnames~n"))
	     (hash-table-for-each
	      globalnames
	      (lambda (k v)
		(debug 'output (printf "Globalname mapping ~a (~a -> ~a)~n" k v (lookup v)))
		(hash-table-put! new k (lookup v))))
	     ;;(hash-table-put! meta 'globalnames globalnames)
	     (set-globalnames! new)
	     )
           
           (let ([c renumber-start])
             (for-each
              (lambda (i)
                ;;(printf "About to handle ~a~n" i)
                (let* ([loc (car i)]
                       [ins (cdr i)]
                       [res
                        (cond
                          [(ujump? ins)
                           (hash-table-put! 
                            new-code
                            c (make-ujump
			       (Instruction-meta ins)
                               (lookup (ujump-value ins))
                               (ujump-fn ins)
			       ))]
                          [(load-label? ins)
                           (hash-table-put!
                            new-code
                            c (make-load-label
			       (Instruction-meta ins)
                               (lookup (load-label-value ins))
			       ))]
                          [(load-label-difference? ins)
                           (hash-table-put! 
                            new-code
                            c (make-load-label-difference
			       (Instruction-meta ins)
                               (lookup (load-label-difference-start ins))
                               (lookup (load-label-difference-end ins))
			       ))]
                          [else (hash-table-put! new-code c ins)])])
                  (set! c (add1 c))
                  res))
              code))
           
           (make-pass meta new-code)
           ))]))
  
  
  (define renumber4
    (lambda (h)
      (let* ([meta (pass-meta h)]
             [bytes (pass-bytes h)]
             ;; Hash table for storing the new renumbered instruction stream
             [newh (make-hash-table)]
             [labelmap (make-hash-table)]
             [->key (lambda (o) 
                      (cond
                        [(list? o) 
                         ;; V300
                         ;; No longer dealing with lists of chars.
                         (bytes->symbol o)]
                        [(string? o) 
                         ;; Probably shouldn't be here.
                         (error 'renumber4:->key "Probably shouldn't be here: ~a~n" o)
                         ;;(string->symbol o)
                         ]
                        [else o]))]
             [lookup
              (lambda (k)
                (hash-table-get labelmap (->key k)
                                (lambda ()
                                  (error 'renumber4 
                                         (format 
					  "You should *never* be here, got ~a."
					  k)))))])
        
	;; This renumbers all the keys in the hash table
	(define renumber-keys
	  (lambda ()
	    (let ([newkey 0])
	      (for-each 
	       (lambda (instr)
		 (let ([inst (cdr instr)]
		       [oldkey  (car instr)])
		   (if (ulabel? inst)
                       (begin
                         (debug 'renumber (printf "LM: ~a -> ~a~n" (->key (ulabel-value inst)) newkey))
                         (hash-table-put! labelmap (->key (ulabel-value inst)) newkey))
                       (begin
                         (hash-table-put! newh newkey inst)
                         (set! newkey (add1 newkey))))))
	       (hash->list bytes)))))
        
        
	(define renumber-instructions
	  (lambda ()
	    (hash-table-for-each 
             newh 
             (lambda (key inst)
               (cond
                 ;; 060430 MCJ
                 ;; I want the metadata to reflect the new target.
                 ;; Why? Why not. Actually, it is because it will be much
                 ;; easier to use the metadata for converting to C 
                 ;; than having to decode this from the binary again, later.
                 [(ujump? inst)
                  (let ([tgt (lookup (ujump-value inst))])
                    (debug 'renumber (printf "jmp: ~a -> ~a~n" (->key (ujump-value inst)) tgt)) 
                    (let ([newm (Instruction-meta inst)])
                      (hash-table-put! newm 'operand tgt)
                      (hash-table-put! 
                       newh
                       key (make-ujump
                            ;; (Instruction-meta inst)
                            newm
                            tgt
                            (ujump-fn inst)
                            ))))]
                 [(load-label? inst)
                  (hash-table-put!
                   newh
                   key (make-load-label
                        (Instruction-meta inst)
                        (lookup (load-label-value inst))
                        ))]
                 [(load-label-difference? inst)
                  (hash-table-put! 
                   newh
                   key (make-load-label-difference
                        (Instruction-meta inst)
                        (lookup (load-label-difference-start inst))
                        (lookup (load-label-difference-end inst))
                        ))])))))
        
        
	(define renumber-globalnames
	  (lambda ()
	    (let ([new-globalnames (make-hash-table)])
	      (hash-table-for-each
               globalnames
               (lambda (key value)
                 (debug 'renumber (printf "Renumber globalname ~a: ~a -> " key value)
                        (printf "~a~n" (lookup key)))
                 (hash-table-put! new-globalnames key (lookup key))))
	      (set-globalnames! new-globalnames))))
        
	(define renumber-procentries
	  (lambda ()
	    (let ([new-procentries (make-hash-table)])
	      (hash-table-for-each
               procentries
               (lambda (key value)
                 (debug 'renumber (printf "Renumber procentry ~a: ~a -> " value key)
                        (printf "~a~n" (lookup value)))
                 (hash-table-put! new-procentries key (lookup value))))
	      (set-procentries! new-procentries))))
        
        
	(renumber-keys)
	(renumber-instructions)
	(renumber-globalnames)
	(renumber-procentries)
        
	(make-pass meta newh)
	)))
  
  (define renumber-for-c
    (lambda (h)
      (let* ([meta (pass-meta h)]
             [bytes (pass-bytes h)]
             ;; Hash table for storing the new renumbered instruction stream
             [newh (make-hash-table)]
             [labelmap (make-hash-table)]
             [->key (lambda (o) 
                      (cond
                        ;; V300 now using bytes; need bytes->symbol
                        [(list? o) (bytes->symbol o)]
                        ;; V300 shouldn't be here, now... 
                        [(string? o) 
                         (error 'renumber-for-c:->key "Shouldn't be here?: ~a~n" o)
                         ;;(string->symbol o)
                         ]
                        [else o]))]
             [lookup
              (lambda (k)
                (hash-table-get labelmap (->key k)
                                (lambda ()
                                  (error 'renumber-for-c
                                         (format 
					  "You should *never* be here, got ~a."
					  k)))))])
        
	;; This renumbers all the keys in the hash table
	(define renumber-keys
	  (lambda ()
	    (let ([newkey 0])
	      (for-each 
	       (lambda (instr)
		 (let ([inst (cdr instr)]
		       [oldkey  (car instr)])
                   
		   (if (ulabel? inst)
                       (begin
                         (debug 'renumber (printf "LM: ~a -> ~a~n" (->key (ulabel-value inst)) newkey))
                         (hash-table-put! labelmap (->key (ulabel-value inst)) newkey)
                         (set-ulabel-value! inst newkey)
                         ))
                   
                   ;; DIFFERENCE
                   ;; In this version, we still keep the labels and in the instruction stream.
                   ;; The c-outputter relies on the label instructions still being present.
                   (hash-table-put! newh newkey inst)
                   (set! newkey (add1 newkey))))
	       (hash->list bytes)))))
        
        
	(define renumber-instructions
	  (lambda ()
	    (hash-table-for-each 
             newh 
             (lambda (key inst)
               (cond
                 [(ujump? inst)
                  (let ([tgt (lookup (ujump-value inst))])
                    (debug 'renumber (printf "jmp: ~a -> ~a~n" (->key (ujump-value inst)) tgt)) 
                    (hash-table-put! 
                     newh
                     key (make-ujump
                          (Instruction-meta inst)
                          tgt
                          (ujump-fn inst)
                          )))]
                 [(load-label? inst)
                  (hash-table-put!
                   newh
                   key (make-load-label
                        (Instruction-meta inst)
                        (lookup (load-label-value inst))
                        ))]
                 [(load-label-difference? inst)
                  (hash-table-put! 
                   newh
                   key (make-load-label-difference
                        (Instruction-meta inst)
                        (lookup (load-label-difference-start inst))
                        (lookup (load-label-difference-end inst))
                        ))])))))
        
        
	(define renumber-globalnames
	  (lambda ()
	    (let ([new-globalnames (make-hash-table)])
	      (hash-table-for-each
               globalnames
               (lambda (key value)
                 (debug 'renumber (printf "Renumber globalname ~a: ~a -> " key value)
                        (printf "~a~n" (lookup key)))
                 (hash-table-put! new-globalnames key (lookup key))))
	      (set-globalnames! new-globalnames))))
        
	(renumber-keys)
	(renumber-instructions)
	(renumber-globalnames)
        
	(make-pass meta newh)
	)))
  
  
  ;; Point this at the most recent version of the function.
  (define renumber renumber4)
  
  
  
  ;; PASS ...
  (define inst2binary
    (lambda (h)
      (let ([meta (pass-meta h)]
            [bytes (pass-bytes h)])
        (hash-table-for-each
         bytes (lambda (k v)
                 (cond
                   [(inst? v)
                    (hash-table-put! 
                     bytes k (make-binary 
                              (Instruction-meta v)
                              (prefix (inst-fn v)
                                      (inst-op v))
                              ))]
                   )))
	
        (make-pass meta bytes))))
  
  
  (define dump-meta
    (lambda (i)
      (hash-table-for-each
       (Instruction-meta i)
       (lambda (k v)
	 (printf "K: ~a\tV: ~a~n" k v)))))
  
  ;; PASS ...
  (define data-bytes2binary
    (lambda (h)
      (let ([meta (pass-meta h)]
            [bytes (pass-bytes h)])
        (hash-table-for-each
         bytes (lambda (k v)
		 (if (data-bytes? v)
		     (let ([new-inst 
			    (make-binary
			     (Instruction-meta v)
			     (map (lambda (c)
                                    ;; V300
                                    ;; No longer dealing with characters; they're 
                                    ;; all integers now.
				    ;; was (char->integer c)
                                    c
                                    )
				  (data-bytes-string v)))])
		       (debug 'db2bin (dump-meta new-inst))
		       (debug 'db2bin (printf "DB: ~a~n" (map hex (binary-value new-inst))))
		       (hash-table-put! 
			bytes k new-inst )))))
        (make-pass meta bytes)
	)))
  
  (define binary2occam
    (lambda (h)
      (let ([meta (pass-meta h)]
            [h (pass-bytes h)])
        (let* ([op (open-output-file (get-output-filename) 'replace)]
               [ilist (map cdr (hash->list h))]
               [bytes (map (lambda (b)
                             (binary-value b))
                           ilist)]
               [b->str
                (lambda (n)
                  (if (number? n)
                      (format "#~a")
                      (apply string-append
                             (list-intersperse 
                              (format ", ")
                              (map (lambda (h)
                                     (format " #~a" h))
                                   (map hex n))))))])
          
          (letrec ([byte-print
                    (lambda (ls)
                      (cond
                        [(null? ls) ""]
                        [(null? (cdr ls)) (b->str (car ls))]
			[(equal? '() (car ls)) (byte-print (cdr ls))]
                        [else
                         (format "~a, ~a" 
                                 (b->str (car ls))
                                 (byte-print (cdr ls)))]))])
            
            (fprintf op "~n-- slinker binary2occam output~n")
            (fprintf op "VAL INT ws.size IS ~a:~n" *WS*)
            (fprintf op "VAL INT vs.size IS ~a:~n" *VS*)
            (fprintf op "VAL INT ms.size IS ~a:~n" *MS*)
            (fprintf op "VAL INT inst.size IS ~a:~n" (apply + (map length bytes)))
            (fprintf op "VAL []BYTE transputercode IS [ ")
            (let ([str (byte-print bytes)])
              (string-uppercase! str)
              (set! str (pregexp-replace "\\,\\s+$" str ""))
              (fprintf op "~a " str))
            (fprintf op " ]:~n~n")
            (make-pass (make-hash-table) (make-hash-table))
            )))))
  
  (define binary2c
    (lambda (h)
      (let ([meta (pass-meta h)]
            [h (pass-bytes h)])
        (let* ([op (open-output-file (get-output-filename) 'replace)]
               [ilist (map cdr (hash->list h))]
               [bytes (map (lambda (b)
                             (binary-value b))
                           ilist)]
               [b->str
                (lambda (n)
                  (if (number? n)
                      (format "0x~a") ;; If we ever hit this, it will die. This is wrong.
                      (apply string-append
                             (list-intersperse 
                              (format ",~n")
                              (map (lambda (h)
                                     (format " 0x~a" h))
                                   (map hex n))))))])
          
          (letrec ([byte-print
                    (lambda (ls)
                      (cond
                        [(null? ls) ""]
                        [(null? (cdr ls)) (b->str (car ls))]
			[(equal? '() (car ls)) (byte-print (cdr ls))]
                        [else
                         (format "~a, ~a" 
                                 (b->str (car ls))
                                 (byte-print (cdr ls)))]))])
            
            (fprintf op "~n/* slinker binary2c output */~n")
            (fprintf op "static const int ws_size = ~a;~n" *WS*)
            (fprintf op "static const int vs_size = ~a;~n" *VS*)
            (fprintf op "static const int ms_size = ~a;~n" *MS*)
            (fprintf op "static const int inst_size = ~a;~n" (apply + (map length bytes)))
            (fprintf op "static const unsigned char transputercode[] TVM_WORD_ALIGN = {~n")
            (fprintf op "~a " (byte-print bytes))
            (fprintf op "~n};~n~n")
            (make-pass (make-hash-table) (make-hash-table))
            )))))
  
  
  
  
  ;; This version of the bytecode file contains a header (tvm followed by one
  ;; byte) the bytecode version, and then the bytecode
  ;; 
  ;; [ header  ] [bytecode stream ...]
  ;; 74 76 6d 01 bb bb bb bb
  ;; bb bb bb bb bb bb bb bb
  ;; ...
  (define binary2bytecode-version-1
    ;;(define binary2bytecode
    (lambda (h)
      (let ([meta (pass-meta h)]
            [h (pass-bytes h)])
        (let* ([op (open-output-file (get-output-filename) 'replace)]
               [ilist (map cdr (hash->list h))]
               [bytes (apply append (map (lambda (b) (binary-value b)) ilist))]
               [bytecode-version 1])
          ;; Output the transterpreter bytecode file identifier
          ;; which is the three letters (bytes) 'tvm', and a byte indicating
          ;; the version of the bytecode file.
          (fprintf op "tvm")
          (write-char (integer->char bytecode-version) op)
          ;; FIXME: We should also indicate how much memory is needed! 
          ;; and possibly other things, this can come later in the 
          ;; bytecode format.
          (byte-write bytes op)
          (close-output-port op)
          (make-pass (make-hash-table) (make-hash-table))
          ))))
  
  ;; Writes version 2 bytecode to a port
  ;;
  ;; [ header  ] [ ws req  ]
  ;; 74 76 6d 02 ws ws ws ws
  ;; [ vs req  ] [ ms req  ]
  ;; vs vs vs vs ms ms ms ms
  ;; [bytecode stream ...  ]
  ;; bb bb bb bb bb bb bb bb
  ;; ...
  ;;
  ;; Where 
  ;;   [ws req] = Workspace memory requirements
  ;;   [vs req] = Vectorspace memory requirements
  ;;   [ms req] = Mobilespace memory requirements
  ;;   all in BYTEs
  (define (write-bytecode-version-2 h port)
    (let ([meta (pass-meta h)]
          [h (pass-bytes h)])
      (let* ([op port]
             [ilist (map cdr (hash->list h))]
             [bytes (apply append (map (lambda (b) (binary-value b)) ilist))]
             ;; 060518
             ;; V300 This is probably the source of most of the breakage.
             ;; It looks like, with changes made in earlier passes and some tweaks
             ;; to helpers.ss, that the biggest problem must, necessarily, reside
             ;; in the output pass. It looks like (but is not certain) that 
             ;; the breakage (at this point, for bytecode output) is entirely
             ;; in the last pass. This, right here, would break things. Why?
             ;; Because integers are being turned into unicode characters, and then
             ;; from there written as unicode chars. That, of course, is no good.
             ;;[output-int-list (lambda ch (write-char (integer->char (car ch)) op))]
             [output-int-list (lambda ch (write-byte (car ch) op))]
             [bytecode-version 2])
        ;; 060518 
        ;; V300 There is code duplication here from somewhere else... 
        
        ;; Output the transterpreter bytecode file identifier
        ;; which is the three letters (bytes) 'tvm', and a byte indicating
        ;; the version of the bytecode file.
        (fprintf op "tvm")
        ;; 060518 V300
        (write-byte bytecode-version op)
        ;; FIXME!!!! Is this endian independent????????
        ;; IF NOT (WHICH I WOULD MAKE SENSE) THEN IT NEEDS FIXING!!!
        ;; AND WE NEED TO DECIDE ON AN ENDIAN FOR THE HEADERS
        ;; Output the workspace requirements as a 32bit integer
        (for-each output-int-list (number->bytes *WS* 4))
        ;; Output the vectorspace requirements as a 32bit integer
        (for-each output-int-list (number->bytes *VS* 4))
        ;; Output the mobilespace requirements as a 32bit integer
        (for-each output-int-list (number->bytes *MS* 4))
        ;; Output the bytecode
        (byte-write bytes op)
        (close-output-port op)
        (make-pass (make-hash-table) (make-hash-table))
        )))
  
  
  
  ;; 20080109 MCJ
  ;; This is just like the version above, but we include the length
  ;; of the bytecode stream. This is for uploading to the blackfin.
  ;;
  ;; Someday, we'll have to unify these things.
  (define binary2blackfin
    (lambda (h)
      (write-blackfin-bytecode h (open-output-file (get-output-filename) 'replace))))
  
  (define (write-blackfin-bytecode h port)
    (let ([meta (pass-meta h)]
          [h (pass-bytes h)])
      (let* ([op port]
             [ilist (map cdr (hash->list h))]
             [bytes (apply append (map (lambda (b) (binary-value b)) ilist))]
             [output-int-list (lambda ch (write-byte (car ch) op))]
             [bytecode-version 3])
        (fprintf op "tvm")
        (write-byte bytecode-version op)
        
        ;; Output the workspace requirements as a 32bit integer
        (for-each output-int-list (number->bytes *WS* 4))
        ;; Output the vectorspace requirements as a 32bit integer
        (for-each output-int-list (number->bytes *VS* 4))
        ;; Output the mobilespace requirements as a 32bit integer
        (for-each output-int-list (number->bytes *MS* 4))
        ;; Output the size of the bytecode array as a 32bit integer
        (for-each output-int-list (number->bytes (length bytes) 4))
        ;; Output the bytecode
        (byte-write bytes op)
        (close-output-port op)
        (make-pass (make-hash-table) (make-hash-table))
        )))
  

  ;;(define binary2bytecode-version-2
  (define binary2bytecode
    (lambda (h)
      (write-bytecode-version-2 h (open-output-file (get-output-filename) 'replace))))
  
  ;; dump-unified-output
  ;; Dumps a single file with bytecode, ffi and debugging information. A small
  ;; header is put at the front to be able to find the different bits in the
  ;; file.
  ;;
  ;; TBZ file format:
  ;;   Each character in the format is one byte, tough | is used as a separator
  ;;   (to indicate word groups). Where '.'s are used, these generally indicate
  ;;   something that is going to be variable in length. Anyway, this is a
  ;;   pretty informal description...
  ;;
  ;; Header format:
  ;;        magic "tbz"
  ;;       /  version, currently 0 (byte)
  ;;      /  /  reserved (currently all 0)
  ;;     /  /  /   reserved (currently all 0)
  ;;    /  /  /   /
  ;;   |tbz0|rrrr|rrrr|
  ;;   |bytcode_desc__|
  ;;   |ffi_desc______|
  ;;   |debug_desc____|
  ;;   START OF PAYLOAD
  ;;   |...bytecode...|
  ;;   |..............|
  ;;   |.bytecode end.|
  ;;   |.....ffi......|
  ;;   |..............|
  ;;   |...ffi  end...|
  ;;   |.....debug....|
  ;;   |..............|
  ;;   |..debug  end..|
  ;;   END   OF    FILE
  ;;
  ;; Where desc format is:
  ;;         start offset
  ;;        /    length
  ;;       /    /    flags (unused)
  ;;      /    /    /
  ;;   |ssss|llll|ffff|
  ;;
  ;; Each section in the file has a descriptor: bytecode, ffi and debug section.
  ;; The descriptor describes where in the file the section starts, relative to
  ;; the start of the file (ssss), how long the section is (llll) and any flags
  ;; (ffff) (currently unused).
  ;;
  ;; The sections of the file come right after the payload (though this is
  ;; irrelevant, as the start offset into a section should be used to find it,
  ;; and the actual length of the header or start of the payload does not affect
  ;; the start offset). Currently the files are simply concatenated one after
  ;; the other.
  ;;
  ;; If a section is not present in the file, all its fields in the descriptor
  ;; should be set to 0.
  ;;
  (define (dump-unified-output h)
    (define version 0)
    (define pad-bytes 8) ;; 8 = 64 bit alignment
    (define header-offset (* 12 4)) ;; Twelve words * 4 bytes per word

    (define (write-descriptor start len flags output-file)
        (byte-write (number->bytes start 4) output-file)
        (byte-write (number->bytes len 4) output-file)
        (byte-write (number->bytes flags 4) output-file))

    (let ([debug-port      (open-output-bytes)]
          [ffi-port        (open-output-bytes)]
          [bytecode-port   (open-output-bytes)]
          [output-file     (open-output-file (get-output-filename) 'replace)])
      ;; Generate the output
      (write-debugging-info h debug-port)
      (write-external-ffi-table h ffi-port)
      (write-bytecode-version-2 h bytecode-port)
      (let* (;; The contents of each of the ports
             [debug-bytes     (get-output-bytes debug-port)]
             [ffi-bytes       (get-output-bytes ffi-port)]
             [bytecode-bytes  (get-output-bytes bytecode-port)]
             ;; The length of the content
             [debug-length    (bytes-length debug-bytes)]
             [ffi-length      (bytes-length ffi-bytes)]
             [bytecode-length (bytes-length bytecode-bytes)]
             ;; The pad amount (we could check if (= (% len length) pad-bytes)
             ;; and then set pad bytes to zero. Later though...)
             [debug-pad       (- pad-bytes (modulo debug-length pad-bytes))]
             [ffi-pad         (- pad-bytes (modulo ffi-length pad-bytes))]
             [bytecode-pad    (- pad-bytes (modulo bytecode-length pad-bytes))])
        ;; Write the header
        ;; ----------------
        ;; Write the magic
        (fprintf output-file "tbz")
        (write-byte version output-file)
        ;; Two words which we can later use for flags and such
        (byte-write (number->bytes 0 4) output-file)
        (byte-write (number->bytes 0 4) output-file)
        ;; bytecode start, length, flags
        (let ([offset header-offset]
              [len    bytecode-length]
              [flags  0])
          (write-descriptor offset len flags output-file))
        ;; ffi      start, length, flags
        (let* ([mylen  (bytes-length ffi-bytes)]
               [offset (if (> mylen 0) (+ header-offset bytecode-length bytecode-pad) 0)]
               [len    (if (> mylen 0) mylen 0)]
               [flags 0])
          (write-descriptor offset len flags output-file))
        ;; debug    start, length, flags
        (let* ([mylen  debug-length]
               [offset (if (> mylen 0) (+ header-offset bytecode-length bytecode-pad ffi-length ffi-pad) 0)]
               [len    (if (> mylen 0) mylen 0)]
               [flags 0])
          (write-descriptor offset len flags output-file))
        ;; Done with the header
        ;; Write the actual bites
        (write-bytes bytecode-bytes output-file)
        (byte-write (srfi:make-list bytecode-pad 0) output-file)
        (write-bytes ffi-bytes output-file)
        (byte-write (srfi:make-list ffi-pad 0) output-file)
        (write-bytes debug-bytes output-file)
        (byte-write (srfi:make-list debug-pad 0) output-file)

        ;; Close the output file
        (close-output-port output-file)
      )))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Begin section related to writing SRECords
;                              
;                              
;    ;; ; ;;;;;  ;;;;;;   ;;;; 
;   ;  ;;  ;   ;  ;   ;  ;   ; 
;   ;      ;   ;  ; ;    ;     
;    ;;;   ;   ;  ;;;    ;     
;       ;  ;;;;   ; ;    ;     
;       ;  ;  ;   ;      ;     
;   ;;  ;  ;   ;  ;   ;  ;   ; 
;   ; ;;  ;;;   ;;;;;;;   ;;;  
;                              
 
  #|
The general format of an S-record follows:

    +-------------------//------------------//-----------------------+ 
    | type | count | address |             data           | checksum | 
    +-------------------//------------------//-----------------------+ 

type
    A char[2] field. These characters describe the type of record (S0, S1, S2, S3, S5, S7, S8, or S9). 

count
    A char[2] field. These characters when paired and interpreted as a hexadecimal value, display the count of remaining character pairs in the record. 

address
    A char[4,6, or 8] field. These characters grouped and interpreted as a hexadecimal value, display the address at which the data field is to be loaded into memory. The length of the field depends on the number of bytes necessary to hold the address. A 2-byte address uses 4 characters, a 3-byte address uses 6 characters, and a 4-byte address uses 8 characters. 

data
    A char [0-64] field. These characters when paired and interpreted as hexadecimal values represent the memory loadable data or descriptive information. 

checksum
    A char[2] field. These characters when paired and interpreted as a hexadecimal value display the least significant byte of the ones complement of the sum of the byte values represented by the pairs of characters making up the count, the address, and the data fields.

S0
----
    The type of record is 'S0' (0x5330). The address field is unused and will be filled with zeros (0x0000). The header information within the data field is divided into the following subfields.

        1. mname is char[20] and is the module name. 
        2. ver is char[2] and is the version number. 
        3. rev is char[2] and is the revision number. 
        4. description is char[0-36] and is a text comment.

    Each of the subfields is composed of ASCII bytes whose associated characters, when paired, represent one byte hexadecimal values in the case of the version and revision numbers, or represent the hexadecimal values of the ASCII characters comprising the module name and description. 

S1
----
    The type of record field is 'S1' (0x5331). The address field is interpreted as a 2-byte address. The data field is composed of memory loadable data.

S9
----
    The type of record field is 'S9' (0x5339). The address field contains the starting execution address and is interpreted as a 2-byte address. There is no data field.

EXAMPLE 
|#
  
  
  ;; checksum
  ;; A char[2] field. These characters when paired and interpreted as a hexadecimal value 
  ;; display the least significant byte of the ones complement of the sum
  ;; of the byte values represented by the pairs of characters making up the count, 
  ;; the address, and the data fields.
  (define (srec-checksum lob)
    (define sum (apply + lob))
    (define LSB (bitwise-and sum #xFF))
    (define compliment (- 255 LSB))
    compliment)
  
  (define (string->bytes str)
    (apply append
           (map (lambda (n)
                  (number->bytes n 1))
                (map char->integer (string->list str)))))
  
  (define (byte->readable b)
    (let ([str
           (if (<= b #x0F)
               (format "0~a" (number->string b 16))
               (number->string b 16))])
      (string-uppercase! str)
      str))
  
  (define (srec-write num lob op)
    (define (help lob)
      (cond
        [(null? lob) (void)]
        [else
         (let ([readable (byte->readable (car lob))])
           (fprintf op "~a" readable)
           (help (cdr lob)))]))
    (fprintf op "S~a" num)
    (help lob)
    (fprintf op "~n"))
  
  (define (assemble addr data)
    ;; Should I include the checksum in this count?
    (define count (+ (length addr) (length data) 1))
    (define checksum (srec-checksum (append (list count) addr data)))
    (append (list count) addr data (list checksum)))
   

  ;; Currently, I'm putting the TVM metadata in the S0
  ;; record, but we're not sure when/where in LEGO this
  ;; stuff comes in. Is it a #x45? #x75? Who knows?     
  (define (write-S0 bytecode-version op)
    (let* ([tvm-tag (string->bytes "tvm")]
           ;; Each number is represented by two bytes.
           [ws-req (number->bytes *WS* 2)]
           [vs-req (number->bytes *VS* 2)]
           [ms-req (number->bytes *MS* 2)])
      (define data (apply append (list tvm-tag
                                       (list bytecode-version)
                                       ws-req vs-req ms-req)))
      (srec-write 0 (assemble '(#x00 #x00) data) op)
      ))
  
  (define (split-list-at ls n)
    (define (slah ls acc n)
      (cond
        [(null? ls) (list (reverse acc) '())]
        [(zero? n) (list (reverse acc) ls)]
        [else
         (slah (cdr ls)
               (cons (car ls) acc)
               (sub1 n))]))
    (slah ls '() n))
  
  (define (snoc ls obj)
    (reverse (cons obj (reverse ls))))
  
  ;; This is the length of the S1 field. I chose 13
  ;; because it means the length comes out the same as 
  ;; our S0 field. Really doesn't matter. Arbitrary
  (define S1-length 20)
  (define start-addr #x8000)
  (define addr start-addr)
  ;; Post increment the address, so we start numbering at
  ;; the 'start-addr' and then continue.
  (define (next-addr)
    (let ([next addr])
      (set! addr (+ addr S1-length))
      next))
  
  ;; Chop off a length of bytes, output it, and return the remainder.
  (define (write-S1 lob op)
    (let ([chunks (split-list-at lob S1-length)])
      (define first (car chunks))
      (define rest  (cadr chunks))
      (srec-write 1 (assemble (number->bytes (next-addr) 2) first) op)
      rest))
  
  (define (write-S9 addr op)
    (let ([len (list #x03)])
      (srec-write 9 (assemble addr '()) op)))
 
  ;; 20070601 MCJ
  ;; This is a first cut at an SREC outputter. We'll be using
  ;; this on the new RCX runtime... and actually, we might get
  ;; away with using it elsewhere as well. 
  ;;
  ;; This pass started life as binary2bytecode. So much of this code
  ;; needs cleaning/reorganizing! But, then, that's why slinker2 was 
  ;; started...
  (define binary2srec
    (lambda (h)
      (let ([meta (pass-meta h)]
            [h (pass-bytes h)])
        (let* ([op (open-output-file (get-output-filename) 'replace)]
               [ilist (map cdr (hash->list h))]
               [bytes (apply append (map (lambda (b) (binary-value b)) ilist))]
               [output-int-list (lambda ch (write-byte (car ch) op))]
               [bytecode-version 2]
	       [ws-req (number->bytes *WS* *WORDSIZE*)]
	       [vs-req (number->bytes *VS* *WORDSIZE*)]
	       [ms-req (number->bytes *MS* *WORDSIZE*)]
	       [metadata (append 
			  (number->bytes bytecode-version *WORDSIZE*)
			  (number->bytes (length bytes) *WORDSIZE*)
			  ws-req
			  vs-req
			  ms-req)]
	       [bytes (append metadata bytes)])
	  
          ;; Most of what follows is just fine; our standard bytecode outputter
          ;; will... well, almost work for SREC output. The header can
          ;; certainly go in the S0 field, along with our metadata. That
          ;; way, S1 fields can just contain the program code.
          
          
          ;; ---------------- S0 ----------------
          (write-S0 bytecode-version op)
          ;; Output the bytecode
          ;; This goes out as a sequence of S1 records.
          ;; ---------------- S1 ----------------
          (let loop ([rest (write-S1 bytes op)])
            (unless (null? rest)
              (loop (write-S1 rest op))))            
          
          ;; ---------------- S9 ----------------
          ;; The end of the record.
          ;; Pass the address where execution should start.
          ;; Need to get this in some other way.
          (write-S9 (number->bytes start-addr 2) op)
          
          (close-output-port op)
          (make-pass (make-hash-table) (make-hash-table))
          ))))
  
  
  
  #|
#define HEADER_FIELDS 8       //!< number of header fields stored on disk

typedef struct {
  unsigned short version;     //!< version number                         0
  unsigned short base;        //!< current text segment base address      b000
  unsigned short text_size;   //!< size of read-only segment              0
  unsigned short data_size;   //!< size of initialized data segment       bytecode-length
  unsigned short bss_size;    //!< size of uninitialized data segment     0
  unsigned short stack_size;  //!< stack size                             0
  unsigned short offset;      //!< start offset from text                 0
  unsigned short num_relocs;  //!< number of relocations.                 0
  
  unsigned char  *text;       //!< program text (not stored on disk)
  unsigned short *reloc;      //!< relocations (not stored on disk)
} lx_t;  

 int lx_write(const lx_t *lx,const unsigned char *filename) {

  if(fd<0)
    return fd;

  // write ID
  //
  ASSURED_WRITE(fd,"brickOS",8);
  // write header data in MSB
  //
  for(i=0; i<HEADER_FIELDS; i++) {
    tmp=htons( ((unsigned short*)lx)[i] );
    ASSURED_WRITE(fd,&tmp,2);
  }

  // write program text (is MSB, because H8 is MSB)
  //
  ASSURED_WRITE(fd,lx->text,lx->text_size + lx->data_size);

  // write relocation data in MSB
  //
  for(i=0; i<lx->num_relocs; i++) {
    tmp=htons( lx->reloc[i] );
    ASSURED_WRITE(fd,&tmp,2);
  }

  close(fd);
  return 0;
}


|#
  
  (define binary2lx
    (lambda (h)
      (let ([meta (pass-meta h)]
            [h (pass-bytes h)])
        (let* ([op (open-output-file (get-output-filename) 'replace)]
               [ilist (map cdr (hash->list h))]
               [bytes (apply append (map (lambda (b)
                                           (binary-value b))
                                         ilist))])
          (define SHORT 16)
          (define lon
            (lambda (s e)
              (if (= s e)
                  '()
                  (cons s 
                        (lon (add1 s) e)))))
          
          (define byte->bitvector
            (lambda (n)
              (let ([vec (make-vector SHORT 0)]
                    [loc 0])
                (letrec ([n->b
                          (lambda (n)
                            (if (zero? n)
                                (vector-set! vec loc 0)
                                (begin
                                  
                                  (vector-set! vec loc (remainder n 2))
                                  (set! loc (add1 loc))
                                  (n->b (quotient n 2)))))])
                  (n->b n)
                  vec))))
          
          (define vector->number
            (lambda (v)
              (let ([ls (vector->list v)]
                    [n 0]
                    [power 0])
                (for-each
                 (lambda (bit)
                   (set! n (+ n (* bit (expt 2 power))))
                   (set! power (add1 power)))
                 ls)
                n)))
          
          
          (define charsplit
            (lambda (v)
              (let ([first (make-vector 8 0)]
                    [second (make-vector 8 0)]
                    [v* '()])
                
                (for-each
                 (lambda (n)
                   (vector-set! second (- n 8) 
                                (vector-ref v n)))
                 (lon 8 16))
                
                (for-each
                 (lambda (n)
                   (vector-set! first n (vector-ref v n)))
                 (lon 0 8))
                
                (list
                 (vector->number first)
                 (vector->number second)))))
          
          (define num->charpair 
            (lambda (n)
              (charsplit (byte->bitvector n))))
          
            (byte-write (map char->integer (string->list "brickOS\x00")) op)
            ;;unsigned short version;     //!< version number                         0
            (byte-write (num->charpair 0) op)
            ;;unsigned short base;        //!< current text segment base address   b000
            (byte-write (list #xb0 #x00) op)
            ;;unsigned short text_size;   //!< size of read-only segment              0
            ;;(byte-write (num->charpair 0))
            (byte-write (reverse (num->charpair (+ 6 (length bytes)))) op)
            ;;unsigned short data_size;   //!< size of initialized data segment       bytecode-length
            ;;(byte-write (num->charpair (length bytes)))
            (byte-write (num->charpair 0) op)
            ;;unsigned short bss_size;    //!< size of uninitialized data segment     0
            (byte-write (num->charpair 0) op)
            ;;unsigned short stack_size;  //!< stack size                             0
            (byte-write (reverse (num->charpair #x400)) op)
            ;;unsigned short offset;      //!< start offset from text                 0
            (byte-write (reverse (num->charpair 4)) op)
            ;;unsigned short num_relocs;  //!< number of relocations.                 0
            (byte-write (num->charpair 0) op)
            ;; Write the transterpreter header
            ;; This may be slightly buggered atm...
            (byte-write (list #x74 #x76 ) op) 
            (byte-write (list #x6d #x00 ) op) 
            (byte-write (list #x54 #x70 ) op) 
            ;; Write the actual code
            (byte-write bytes op)
          (close-output-port op)
          (make-pass (make-hash-table) (make-hash-table))
          ))))
  
  
  
  
  (define jumps-and-loads2binary  
    (let* ([inst-len (make-hash-table)]
           [dist     (make-hash-table)]
           [changed #f]
           [iterations 0]
           [get-dist
            (lambda (key)
              (hash-table-get dist key (lambda () 0)))]
           [set-dist!
            (lambda (key val)
              (set! changed #t)
              (hash-table-put! dist key val))]
           [get-inst-len
            (lambda (key)
              (hash-table-get inst-len key (lambda () 0)))]
           [set-inst-len!
            (lambda (key val)
              (set! changed #t)
              (hash-table-put! inst-len key val))])
      
      (define get-distance
        (lambda (h s e)
          (let ([inc-op (if (< s e) 
                            add1 
                            sub1 
                            )]
                [dec-op (if (< s e) sub1 add1)]
                [test-op (if (< s e) > <)]
                [dist 0])
            
            (cond
              ;; We might be jumping to ourselves. If that is the 
              ;; case, we should just jump back two. This is 
              ;; a very tight infinite loop, as demonstrated by
              ;;
              ;; WHILE TRUE
              ;;   SKIP
              ;; 
              [(= s e) (set! dist -2)]
              [else
               ;; Either the start is less than the end-point, or
               ;; the start-point is greater than the end-point.
               (let loop ([n (if (< s e)
                                 (inc-op s)
                                 s)])
                 (unless (test-op n (if (< s e)
                                        (dec-op e)
                                        e))
                   ;;(printf "Dist is : ~a~n" dist)
                   (let ([obj (hash-table-get 
                               h n (lambda ()
                                     (error 
                                      (format 
                                       "~a: ~a trying to get value @ ~a called with s:~a e:~a~n"
                                       "get-distance (jumps-and-loads2binary)"
                                       "failed horrifically"
                                       n s e))))])
                     (cond
                       [(binary? obj) (set! dist 
                                            (+ dist
                                               (length (binary-value obj))))]
                       [(load-label? obj)
                        (set! dist (+ dist (get-inst-len n))) ]
                       
                       [(or (load-label-difference? obj)
                            (ujump? obj))
                        (set! dist (+ dist (get-inst-len n)))]
                       [(align? obj) 
                        (set! dist (+ dist (get-inst-len n)))]
		       [(stubname? obj) (set! dist (+ dist (* 2 *WORDSIZE*)))]
                       [else
                        (error (format
                                (string-append
				 "This is a test~n"
                                 "get-distance: failed mostly horrifically.~n"
                                 "Cannot deal with record of type: ~a (~a)~n") obj (stubname? obj)))]
                       ))
                   (loop (inc-op n))))])
            dist)))
      
      
      
      (define calculate
        (lambda (h)
          ;; First things first; when we enter this function, there's
          ;; no changes that have taken place.
          (set! changed #f)
          (set! iterations (add1 iterations))
	  (debug 'jumps-and-loads2binary 
		 (printf "--------------- Iteration #~a ---------------~n" iterations))
          ;;Then, loop through storing the distances for each jump and load.
          (hash-table-for-each
           h (lambda (k v)
               (cond
                 [(or (ujump? v) (fjump? v))
                  (let* ([distance (get-distance h k (ujump-value v))]
                         [distance
                          (if (<= (ujump-value v) k)
                              (* -1 distance)
                              distance)]
                         ;; The length of the instruction varies with jumps,
                         ;; but fjumps are always a fixed length
                         [len (if (fjump? v)
                                  (fjump-len v)
                                  (length (prefix (ujump-fn v) distance)))])
                    (if (and (= (ujump-fn v) *J*) (= distance 0))
                        ;; We would like to filter out J 0's, as they are redundant
                        ;; and actually used as breakpoint traps. We can do this by
                        ;; setting the length of an instruction to zero, whos distance
                        ;; is also zero
                        (begin 
                          (unless (and (= 0 (get-dist k))
                                       (= 0 (get-inst-len k)))
                            (set-dist! k 0)
                            (set-inst-len! k 0)
			    (debug 'jumps-and-loads2binary 
				   (printf "@~a J0* (d: ~a l: ~a)~n" k (get-dist k) (get-inst-len k)))
			    )
			  ;;(debug 'jumps-and-loads2binary 
                          ;; (printf "@~a J0 (d: ~a l: ~a)~n" k (get-dist k) (get-inst-len k)))
                          )
                        ;; Otherwise we just do the normal thing and update the 
                        ;; new distance and length of instruction calculated
                        (begin
                          (unless (and (= distance (get-dist k))
                                       (= len (get-inst-len k)))
                            (set-dist! k distance)
                            (set-inst-len! k len)
			    (debug 'jumps-and-loads2binary 
				   (printf "@~a J* (d: ~a l: ~a)~n" k (get-dist k) (get-inst-len k)))
			    ))))]
                 
                 
                 [(load-label? v)
                  (let* ([me k]
                         [target (load-label-value v)]
                         [distance (get-distance h me target)]
                         [distance (if (< target me) 
                                       (* -1 distance) 
                                       distance)]
                         [len (length 
                               (append
                                (prefix *LDC* distance)
                                (prefix *OP* *LDPI*)))])
                    (unless (and (= distance (get-dist k))
                                 (= len (get-inst-len k)))
                      (set-dist! k distance)
                      (set-inst-len! k len)
		      (debug 'jumps-and-loads2binary 
			     (printf "@~a LL* (d: ~a l: ~a)~n" k (get-dist k) (get-inst-len k)))
		      ))]
                 
                 [(load-label-difference? v)
                  (let* ([start (load-label-difference-start v)]
                         [end (load-label-difference-end v)]
                         [distance (get-distance h (- start 1) end)]
                         [len (length (prefix *LDC* distance))])
                    (unless (and (= distance (get-dist k))
                                 (= len (get-inst-len k)))
                      (set-dist! k distance)
                      (set-inst-len! k len)
		      (debug 'jumps-and-loads2binary 
			     (printf "@~a LLD* (d: ~a l: ~a)~n" k (get-dist k) (get-inst-len k)))
		      ))]
                 
                 ;; CGR 20070919 - ETC alignment value is a power of 2
                 [(align? v)
                  (let* ([align-amount (expt 2 (align-value v))]
                         ;; Get the distance from the start of the file till
                         ;; the instruction before this one.
                         ;; FIXME: Is this correct? I would assume that would
                         ;; give me the distance to the end of instruction k-1
			 ;; 
			 ;; clj3 14/07/05 There is a special case here, where
			 ;; the align is at the top, ie k = 0, in that case the
			 ;; distance is zero (if we actually do the call to
			 ;; getdistance, it will fail most horifically!!!
			 ;; The question is, fix it here??? or fix it in get
			 ;; distance so it will work for other cases too?
			 ;; Will do it here for now
                         [distance (if (= k 0) 0 (get-distance h 0 (- k 1)))]
			 [len (modulo (- align-amount (remainder distance align-amount))
                                      align-amount)])
                    (debug 'align
                           (printf "align:~n  @: ~a~n  amount: ~a~n  dist: ~a~n  len: ~a~n"
                                   k align-amount distance len))
                    (unless (and (= distance (get-dist k))
                                 (= len (get-inst-len k)))
                      (set-dist! k distance)
                      (set-inst-len! k len)
		      (debug 'jumps-and-loads2binary 
			     (printf "@~a ALIGN* (d: ~a l: ~a)~n" k (get-dist k) (get-inst-len k)))
		      ))]
                 
                 )))))
      
      (define generate-binary
        (lambda (h)
          
          (hash-table-for-each
           h (lambda (k v)
               (cond
		 [(fjump? v)
		  (hash-table-put!
                   h k
                   (make-binary
                    (Instruction-meta v)
                    ;; We pad by the mount specified in the fjump
                    (pad (prefix (ujump-fn v) (get-dist k)) (fjump-len v))
                    ))]
                 [(ujump? v)
		  ;;(if (<= (ujump-value v) k) (printf "Should be negative: ~a~n" (get-dist k))
		  ;;  (printf "Should be positive: ~a~n" (get-dist k)))
                  (if (and (= (ujump-fn v) *J*) (= (get-inst-len k) 0))
                      ;; If we have a straight J and its instruction length
                      ;; is zero, then nuke it, ie we have a J 0
                      ;;(hash-table-remove! h k)
		      ;; Removing stuff seems to be a bad thing at this point,
		      ;; as we are not going to be renumbering anymore...
		      ;; and that is definitely bad for libraries, had some
		      ;; funnyness cos of removing alignment when dealing with
		      ;; libraries, so lets leave them in, but drop empty
		      ;; binaries
		      (hash-table-put!
                       h k
                       (make-binary
                        (Instruction-meta v)
                        '()))
                      ;; Otherwise make a sequence of bytes of the jump
                      (hash-table-put! 
                       h k 
                       (make-binary
			(Instruction-meta v)
                        (prefix (ujump-fn v) (get-dist k))
			)))]
                 [(load-label? v)
                  (hash-table-put! 
                   h k 
                   (make-binary 
		    (Instruction-meta v)
                    (append (prefix *LDC* (get-dist k))
                            (prefix *OP* *LDPI*))
		    ))]
                 [(load-label-difference? v)
                  (hash-table-put! 
                   h k 
                   (make-binary
		    (Instruction-meta v)
                    (prefix *LDC* (get-dist k))
		    ))]
                 [(align? v)   
                  (if (> (get-inst-len k) 0)
                      ;; If the length of the align special is above zero, make it
                      (hash-table-put!
                       h k
                       ;; The align special fills the space it appears with anough
                       ;; zeros to make the byte after it word aligned. The amount
                       ;; of zeros it needs to insert is the value which has been
                       ;; calculated earlier and stored, and now retreived using
                       ;; (get-inst-len k). We make a list of zeros here, then
                       ;; turn it into a binary.
                       (make-binary 
			(Instruction-meta v)
			(srfi:make-list (get-inst-len k) 0)
			))
                      ;; If the length of the align special is 0, remove it completely
		      ;; As with jumps above, lets just drop an empty binary
		      ;; structure rather than removeing it...
                      ;;(hash-table-remove! h k)
                      (hash-table-put!
                       h k
                       (make-binary 
			(Instruction-meta v)
			'()))
		      )]
                 )))
          h))
      
      (define main
        (lambda (h)
          #|
	(fprintf (current-error-port) "Distances~n")
	(fprintf (current-error-port) "~a~n" dist)
	(fprintf (current-error-port) "InstLen~n")
	(fprintf (current-error-port) "~a~n" inst-len)
	|#
          (let loop ()
            ;;(fprintf (current-error-port) "----- ~a -----~n" iterations)
            (calculate h)
            #|
	  (fprintf (current-error-port) "Distances~n")
	  (fprintf (current-error-port) "~a~n" dist)
	  (fprintf (current-error-port) "InstLen~n")
	  (fprintf (current-error-port) "~a~n" inst-len)
	  |#
            
            (if changed
                (if (< iterations *MAX-PREFIXING-ITERATIONS*)
                    (loop)
                    (fprintf 
                     (current-error-port)
                     "HIT MAX ITERATIONS (~a)~n"
                     iterations) )))
          
          (fprintf 
           (current-error-port)
           "Prefixing Iterations: ~a~n" iterations)
          (generate-binary h)))
      
      (lambda (h)
        (let ([meta (pass-meta h)]
              [h (pass-bytes h)])
          (make-pass meta (main h))))
      ))
  
  (print-hash-table #t)      
  
  (define jumps2binary
    (let ([abs (lambda (v) (if (< v 0) (* -1 v) v))])
      
      (lambda (h)
        (let ([meta (pass-meta h)]
              [h (pass-bytes h)])
	  (build-get-distance-cache h)
          (hash-table-for-each
           h (lambda (k v)
               (if (ujump? v)
                   (begin
                     (debug 'jumps2binary (printf "Resloving: ~a\t~a\t(~a -> ~a)\t" 
                                                  (list-ref primaries (ujump-fn v))
                                                  (ujump-value v)
                                                  k
                                                  (ujump-value v)))
                     (let* ([distance (get-distance h k (ujump-value v))]
                            [distance
                             (if (<= (ujump-value v) k)
                                 ;;(* -1 (add1 (+ (* 2 *WORDSIZE*) distance)))
                                 (* -1 distance)
                                 distance)])
                       (debug 'jumps2binary (printf "(dist: ~a)~n" distance))
                       ;;(printf "I'm jumping ~a !!!~n" distance)
                       (hash-table-put! 
                        h k 
                        (make-binary
                         (Instruction-meta v)
                         (pad (prefix (ujump-fn v) distance))
                         ))
                       )))))
          (make-pass meta h)))))
  
  
  
  ;;WARNING 
  ;; Currently, LDPI is hard-coded at size 2... this could change
  ;; if we move the instruction somewhere else in the interpreter 
  ;; tables... probably need to come up with a better solution for this?
  (define load-labels2binary
    (let ([abs (lambda (v) (if (< v 0) (* -1 v) v))])
      (lambda (h)
        (let ([meta (pass-meta h)]
              [h (pass-bytes h)])
          (build-get-distance-cache h)
          (hash-table-for-each
           h (lambda (k v)
               (cond
                 [(load-label? v)
                  (let* ([me k]
                         [target (load-label-value v)]
                         [dist (get-distance h me target)]
                         ;; IF WE ARE GOING BACKWARDS
                         ;; We will need to subtract one from the distance,
                         ;; because we're only half-way through the LDPI 
                         ;; instruction (21 FB). 
                         ;; IF WE ARE GOING FORWARDS
                         ;; we need to add one, to get the rest of the way
                         ;; through the LDPI.
                         ;; Subtle, evil thing.
                         [offset (if (< target me) 
                                     (* -1 dist) 
                                     dist)]
                         [bin
                          (make-binary 
			   (Instruction-meta v)
                           (append (pad (prefix *LDC* offset))
                                   (prefix *OP* *LDPI*))
			   )])
                    ;;(printf "LLB Distance: ~a~n" dist)
                    (hash-table-put! h k bin))]
                 [(load-label-difference? v)
                  (let* ([start (load-label-difference-start v)]
                         [end (load-label-difference-end v)]
                         ;; FIXME: WARNING: I think this should be start - 1, to ensure that
                         ;; we do not count the instruction to which the last label points
                         ;; this may depend on what direction the difference is though?
                         ;; start - 1 assumes of course that the start label, 
                         ;; is actually the end
                         [dist (get-distance h (- start 1) end)]
                         ;; WARNING here is the hardcode again...
                         ;;[offset (- dist 2)]
                         [offset dist]
                         ;; WARNING: Is this an absolute quantity?
                         ;;[dist (if (< end start) (* -1 dist) dist)]
                         [bin
                          (make-binary
			   (Instruction-meta v)
                           (pad (prefix *LDC* offset))
                           #|
		      (append (pad (prefix *LDC* offset))
			      (prefix *OP* *LDPI*))
				  |#
                           )])
		    (debug 'load-label-difference
                      (begin
			(printf "LLD: s ~a, e ~a; l1 ~a, l2 ~a; d ~a; l ~a~n" 
				start 
				end 
				(car (hash-table-get (Instruction-meta v) 
						     'operand (lambda () (list "-" "-"))))
			   (cadr (hash-table-get (Instruction-meta v) 
						 'operand (lambda () (list "-" "-"))))
			   dist 
			   (hash-table-get (Instruction-meta v) 'line (lambda () "-")))))
                    (hash-table-put! h k bin))]
                 )))
          (make-pass meta h)))))
  
  (define prefix-patching
    (lambda (h)
      (let ([meta (pass-meta h)]
	    [h (pass-bytes h)]
	    [things (list 'j 'cj 'call 'stubname 'load-label 'load-label-difference)])
	(hash-table-for-each
           h (lambda (k v)
	       ;; Check to see if we have a binary (most probably)
	       ;; and its one of the things we are looking for
	       ;; j, cj, call, load-label-difference, load-label
		 (if (and (binary? v) (>= (length (binary-value v)) 8))
		   (let ([abbrv (hash-table-get (Instruction-meta v) 'abbreviation)])
		     (if (member abbrv things)
		       (let* ([unadjusted-offset (srfi:fold 
				       ;; 0x20 is 'prefix 0'
				       (lambda (x count) (if (= #x20 x) (+ count 1) count)) 
				       0 (binary-value v))]
			      ;; -1 because iptr already points to next instruction
			      ;; -1 because  the first instruction to holds the jump
			      [offset (- unadjusted-offset 2)] ;; iptr already points to the next ins
			      [adjust (> unadjusted-offset 2)]
			      ;; Only if we are going to jump over at least one
			      ;; prefix change the sequence of bytes
			      [bytes (if adjust
				       (append (prefix *J* offset) (cdr (binary-value v)))
				       'no-change)])
			 (debug 'prefix-patching 
			   (printf "prefix-patching: type ~a, bytes ~a, offset ~a (~a), new-bytes ~a~n" 
				 abbrv (binary-value v) unadjusted-offset offset bytes))
			 (if adjust (set-binary-value! v bytes))
		   ))))))
          (make-pass meta h))))

 
  ;;Takes the output of 'read-bytes' and outputs the list, one
  ;; character per line.
  (define binary2linear-bytes
    (lambda (h)
      (let ([meta (pass-meta h)]
            [h (pass-bytes h)])
        (let* ([op (open-output-file (get-output-filename) 'replace)]
               [ilist (map cdr (hash->list h))]
               [bytes (map (lambda (b)
                             (binary-value b))
                           ilist)]
               [b->str
                (lambda (n)
                  (if (number? n)
                      (format "0x~a")
                      (apply string-append
                             (list-intersperse 
                              (format "~n")
                              (map (lambda (h)
                                     (format " 0x~a" h))
                                   (map hex n))))))])
          
          (letrec ([byte-print
                    (lambda (ls)
                      (cond
                        [(null? ls) ""]
                        [(null? (cdr ls)) (b->str (car ls))]
                        [else
                         (format "~a~n~a" 
                                 (b->str (car ls))
                                 (byte-print (cdr ls)))]))])
            (fprintf op "~a " (byte-print bytes))
            (make-pass (make-hash-table) (make-hash-table))
            )))))
  
  ;; This is for the unoptimised jumps and loads to binary (which is two
  ;; seperate passes. The optimised pass takes care of this, but the
  ;; unoptimised passes does not...
  (define insert-alignment
    (lambda (h)
      (let ([meta (pass-meta h)]
	    [bytes (pass-bytes h)])
	(let ([last-aligned-inst 0]
	      [vec (hash->vector bytes)])
	  (define do-align
            (lambda (inst pos)
	      (if (align? inst)
                  (begin
                    ;;(printf "~a ~a~n" last-aligned-inst pos)
                    (build-get-distance-cache-partial vec last-aligned-inst pos)
                    (let ([to-insert
                           (if (= last-aligned-inst pos)
                               (begin
                                 (debug 'align "We is us!~n")
                                 (make-binary (Instruction-meta inst) '() ))
                               (let (;;[dist (get-distance bytes last-aligned-inst pos)]
                                     ;; This does the same as above, and for new get
                                     ;; distance is no slower, however it allows the
                                     ;; old get distance to check if the values
                                     ;; produced by new get distance are ok
                                     [dist (get-distance bytes 0 pos)]
				     [align-amount (expt 2 (align-value inst))]
                                     [pad-dist 0])
                                 
                                 (set! pad-dist (modulo 
                                                 (- align-amount (remainder dist align-amount))
                                                 align-amount))
                                 (debug 'align 
                                        (printf "Loc: ~a\tbloc: ~a\tpad: ~a~n" pos dist pad-dist))
                                 
                                 (make-binary 
                                  (Instruction-meta inst)
                                  (vector->list (make-vector pad-dist 0))
                                  ))
                               )])
                      ;; We need to update our instructino table 
                      (hash-table-put! 
                       bytes
                       pos
                       to-insert)
                      ;; But also the local vector which is used for 
                      ;; build-get-distance-cache-partial (and other stuff)
                      (vector-set! vec pos to-insert)
                      ;; Update last-aligned
                      (set! last-aligned-inst pos)
                      ))
                  ))) ;;end lambda
	  (let
              ([len (vector-length vec)])
            (let loop ([i 0])
              (unless (>= i len)
                (do-align (vector-ref vec i) i)
                (loop (add1 i)))))
          
	  (make-pass meta bytes)))
      
      ))
  
  ;; This thing takes a number and a wordlength and produces a list of bytes
  ;; which is padded to be the correct wordlength]
  ;; V300 
  ;; I'm sure this could be done better... 
  (define number->bytes
    (lambda (n b)
      
      (define pad-list
        (lambda (ls padding)
          (append (vector->list (make-vector padding)) ls)))
      
      (define make-pairs
        (lambda (ls)
          (cond
            [(null? ls) '()]
            [(null? (cdr ls))
             (list (list (car ls) #\0))]
            [else
             (cons
              (list (car ls)
                    (cadr ls))
              (make-pairs (cddr ls)))])))
      
      (let* ([nls (reverse (string->list (number->string n 16)))]
             [split (map reverse (make-pairs nls))]
             [as-bytes
              (reverse (map (lambda (s)
                              (string->number s 16))
                            (map list->string split)))])
        (if (> b (length as-bytes))
            (pad-list as-bytes (- b (length as-bytes)))
            as-bytes))))

  ;; This function writes the external ffi table to a port. This port could be a
  ;; file, or someting else(!). The code in this function used to be in
  ;; 'dump-external-ffi-table'. It no longer is due to the ability to pass an
  ;; arbitrary port.
  (define (write-external-ffi-table h port)
   (let ([meta (pass-meta h)]
      [bytes (pass-bytes h)]
      [ffi-table (make-hash-table)]
      [sanitize-name (lambda (name)
                       (set! name (bytes->string/locale name))
                       (set! name (pregexp-replace "^CX" name ""))
                       (set! name (pregexp-replace "^C" name ""))
                       (set! name (pregexp-replace* "\\." name "_"))
                       (string->bytes/locale name))])
  (if (> (hash-table-count ffi-names) 0)
      (begin
        ;; The ffi-names hash table was previously
        ;; k=name, v=id, we now need these values the otherway
        ;; around, so we create a new hash table with k=id, v=name
        (debug 'ffi (printf "FFI Table has ~a entries:~n" (hash-table-count ffi-names)))
        (hash-table-for-each
         ffi-names
         (lambda (k v)
           (hash-table-put! ffi-table v k)
           (debug 'ffi (printf "  ~a\t~a~n" v k))))
        (let* ([op port]
               [ffi-table-list (hash->list ffi-table)]
               [ffi-count (hash-table-count ffi-table)]
               ;; Initial distance is the number of ffi's times 2
               ;; (two bytes of storage per ffi record) times 8
               ;; (8 bytes in a word) FIXME: fix for wordsize
               ;; We are adding one to ffi-count so we can put a null
               ;; terminating word pair at the end, signifying that
               ;; there are no more references to be resolved.
               ;; (and that the rest of the file are strings)
               [dist (* (* (+ ffi-count 1) 2) *WORDSIZE*)])
          ;; This comes from the lx outputter, code duplication, BAD!!!
          (let ()
            ;; 060331 Was a letrec; now internal defines
            ;; 060518 V300
            ;; Converting these to use byte strings, and hoping that's
            ;; the correct thing to do. I am expecting that there are
            ;; no Unicode strings left at this point, so... they'd better
            ;; be bytestrings, or something is broken earlier in the slinker.
            (define (string-write str)
              (byte-write (bytes->list str) op))
            (define (string0-write str)
              (byte-write (append (bytes->list str) '(#x00)) op))
            
            ;; Write a header
            (string-write #"ffitvm\0\0")
            (let* ([dynlibs (get-all-dynlibs)]
                   [num-dynlibs (length dynlibs)]
                   [bytes-written 0])
              ;; Write the number of library names we are going to dump
              (byte-write (reverse (number->bytes num-dynlibs *WORDSIZE*)) op)
              (set! bytes-written (+ bytes-written *WORDSIZE*))
              ;; Write out the library names
              (for-each 
               (lambda (alib)
                 (let* ([libname (symbol->bytes alib)]
                        [libnamelen (bytes-length libname)])
                   ;; Is it too long?
                   (if (> libnamelen 254)
                       (raise-user-error 'dump-external-ffi-table
                        (format "Length of dynamic library ~a too long (> 254)~n" alib)))
                   ;; No, write its length, add1 for null terminator
                   (byte-write (number->bytes (add1 libnamelen) 1) op)
                   ;; Write the actual string
                   (string0-write libname)
                   ;; Update with how many bytes we have written
                   ;; one byte for length + one for name + one for null
                   (set! bytes-written (+ 1 libnamelen 1 bytes-written))
                   (debug 'ffi (printf "dynlib: ~a ~a~n" libnamelen libname))))
               dynlibs)
              (let* ([*PADSIZE* (* 2 *WORDSIZE*)]
                     [pad (- *PADSIZE* (modulo bytes-written *PADSIZE*))])
                (debug 'ffi (printf "PADSIZE: ~a written: ~a pad: ~a~n" *PADSIZE* bytes-written pad))
                (if (not (= *PADSIZE* pad))
                    (byte-write (number->bytes 0 pad) op))))
            (for-each 
             (lambda (kv)
               (let ([ffi-name-length 
                      (+ (bytes-length (sanitize-name (cdr kv))) 1)])
                 ;; Write an empty word which will get filled by the TVM
                 (byte-write (number->bytes 0 *WORDSIZE*) op)
                 ;; Write a pointer to the string we are jumping to, also a word
                 ;; Due to endianness we reverse this so that it is little
                 ;; endian, swapping will be needed in the TVM then, sigh
                 (byte-write (reverse (number->bytes dist *WORDSIZE*)) op)
                 ;; Update the distance to start of the next string
                 (set! dist (+ dist ffi-name-length))
                 ))
             ffi-table-list)
            ;; The null terminating pair of entries
            (byte-write (number->bytes 0 *WORDSIZE*) op)
            (byte-write (number->bytes 0 *WORDSIZE*) op)
            ;; Write out the strings
            (for-each 
             (lambda (kv)
               (let ([ffi-name (sanitize-name (cdr kv))])
                 ;; Dump the string which we are referencing
                 ;;(byte-write (append (map char->integer (string->list ffi-name)) '(#x00)))
                 (string0-write ffi-name)
                 ))
             ffi-table-list)
            ))))
  (make-pass meta bytes)))

  ;; This currently dumps the FFI table to  a seperate file, which is not
  ;; what we want eventually, but it will do for now.
  ;; Also we might need to sort this thing out for endianness? or perhaps 
  ;; probably not, as we should perhaps do that when we access the table using
  ;; read and write thingys.
  (define dump-external-ffi-table
    (lambda (h)
      (cond
        [(equal? ffi-type 'static) h]
        [else 
          ;; Technically this check also happens in write-external-ffi-table,
          ;; but if we dont do it here, then an ffi file will be created
          ;; regardless of whether it is needed or not.
          (if (> (hash-table-count ffi-names) 0)
            (write-external-ffi-table h (open-output-file (->ffi (get-output-filename)) 'replace))
            h)])))
  
  
  (define annotation2c
    (lambda (pass)
      (define uberstring
	(format "#define WORDLENGTH 4
#define POOTERS_REAL

#include \"types.h\"
#include \"instructions.h\"
#include \"ins_pri.h\"
#include \"ins_sec.h\"
#include \"ins_chan.h\"
#include \"hook_timer.h\"
#include \"transputer.h\"
#include \"interpreter.h\"
#include \"ext_chan.h\"
#include <stdio.h>
#include <sys/time.h>
/*
#define write_byte(ptr, val) *(ptr) = val
#define read_byte(ptr) *ptr
#define pooter_plus(ptr, inc) ptr + inc
#define pooter_minus(ptr,  inc)  ptr - inc
#define bpooter_plus( ptr,  inc) ptr + inc
#define bpooter_minus( ptr,  inc)  ptr - inc

#define write_mem( ptr, val)\\
 *(ptr) = (WORD)\\
( ((((UWORD) val) >> 24) & 0x000000FF) | ((((UWORD) val) >>  8) & 0x0000FF00) | \\
        ((((UWORD) val) <<  8) & 0x00FF0000) | ((((UWORD) val) << 24) & 0xFF000000) )

#define read_mem(ptr) (WORD)\\
( ((((UWORD)*ptr) >> 24) & 0x000000FF) | ((((UWORD)*ptr) >>  8) & 0x0000FF00) | \\
        ((((UWORD)*ptr) <<  8) & 0x00FF0000) | ((((UWORD)*ptr) << 24) & 0xFF000000) )
*/


int transputermem[~a];

static WORD stiwc_get_time(void)
{
	struct timeval t;

	gettimeofday(&t, 0);
	return (WORD)((t.tv_sec * 1000000) + t.tv_usec);
}

static void ext_chan_scr(WORD count, BPOOTER address)
{

 if(count != 1)
   {
    fprintf(stderr, \"STIWC ERROR: count for SCR channel != 1\\n\");
	   exit(1);
	   }
   if(read_byte(address) == 0xFF)
   {
    fflush(stdout);
	  }
   else
   {
    fprintf(stdout, \"%c\", read_byte(address));
	   }
   }

int main(int argc, char* argv) {~n" (+ 4 *WS*)))
      (define less-uberstring
        (format "int i;
  wptr = (POOTER) &transputermem[~a];
  
  for(i = 0; i < NUM_PRI; i++)
    {
      fptr[i] = (POOTER)NOT_PROCESS_P;
      bptr[i] = (POOTER)NOT_PROCESS_P;
      tptr[i] = (POOTER)NOT_PROCESS_P;
    }

  areg = 0;
  breg = 0;
  creg = 0;
  oreg = 0;

  ext_chan_table[EXT_CHAN_SCR] = ext_chan_scr;
  init_stackframe();
  get_time = stiwc_get_time;
" (sub1 (+ 4 *WS*))))
      
      (define remove-dot
        (lambda (str)
          (pregexp-replace* "\\." str "_")))
      
      
      
      (let ([pass-m (pass-meta pass)]
            [bytes (pass-bytes pass)]
            [print printf]
            [data-bytes (make-hash-table)])
        
        (let ([current-section-label 0])
          (for-each
           (lambda (pair)
             (let* ([k (car pair)]
                    [v (cdr pair)]
                    [meta (Instruction-meta v)]
                    [abr (hash-table-get meta 'abbreviation)]
                    [opr (hash-table-get meta 'operand (lambda () #f))])
               (cond
                 [(equal? abr 'sectionlab)
                  (set! current-section-label opr)]
                 [(equal? 'data-bytes abr)
                  (hash-table-put! data-bytes current-section-label opr)])))
           (hash->list bytes)))
        
        (print uberstring)
        
        ;; Print databytes
        (hash-table-for-each
         data-bytes
         (lambda (k v)
           (print "char label~a[~a] = {~a };~n"
                  k
                  (length v)
                  (apply string-append
                         (list-intersperse 
                          (format ", ")
                          (map (lambda (h)
                                 (format " 0x~a" h))
                               (map hex (map char->integer v))))))))
        
        (print less-uberstring)
        
        (for-each
         (lambda (pair)
           (let ([k (car pair)]
                 [ins (cdr pair)]
                 [current-section-label 0])
             (let* ([meta (Instruction-meta ins)]
                    [abr (hash-table-get meta 'abbreviation)]
                    [opr (hash-table-get meta 'operand (lambda () #f))]
                    [type (hash-table-get meta 'type (lambda () #f))])
               ;; DO WICKED STUFF HERE
               (cond
                 [(equal? abr 'align) (void)]
                 [(equal? abr 'procentry) 
                  (print "~n/* PROC : ~a */~n" 
                         ;; V300 Was list->string, now list->bytes
                         (list->bytes opr))]
                 [(equal? abr 'jentry)
                  (print "goto globalname_~a;~n" 
                         (remove-dot 
                          ;; V300 Was list->string
                          (list->bytes opr)))]
                 [(equal? abr 'setlab)
                  (print "label~a:~n" opr) 
                  (debug 'cprintf (print "printf(\"label : ~a\\n\");~n" opr))
                  ]
                 [(equal? abr 'globalname)
                  (print "globalname_~a:~n" (remove-dot 
                                             ;; V300 Was list->string
                                             (list->bytes opr)))]
                 [(equal? abr 'sectionlab)
                  (print "label~a:~n" opr)
                  (debug 'cprintf (print "printf(\"label : ~a\\n\");~n" opr))
                  ]
                 
                 [(equal? abr 'load-label)
                  (if (hash-table-get data-bytes opr (lambda () #f))
                      (begin
                        (print "/* load-label ~a */~n" opr)
                        (print "oreg = (WORD) label~a;~n" opr))
                      (print "oreg = (WORD) &&label~a;~n" opr))
                  (print "ins_ldc();~n")
                  ]
                 [(equal? abr 'load-label-difference)
                  (print "/* ~a : ~a */~n" abr opr)
                  (print "oreg = (WORD) &&label~a;~n" (car opr))
                  (print "ins_ldc();~n")]
                 [(equal? abr 'data-bytes)
                  (void)]
                 [else
                  (print "/* ~a ~a  ~a*/~n" abr opr type)
                  
                  (cond
                    [(equal? type 'primary)
                     ;; WARNING 
                     ;; There is no good reason for the negative numbers to be 
                     ;; of type integer?, while all other numbers are string?
                     (cond
                       ;; Positive numbers
                       [(string? opr)
                        (print "oreg = 0x~a;~n" opr)]
                       [else 
                        (print "oreg = ~a;~n" opr)])]
                    
                    [(equal? type 'special)
                     (if (member abr '(j cj call))
                         (print "iptr = 0;~n"))
                     
                     (if (member abr '(j cj call))
                         (print "oreg = (WORD) &&label~a;~n" opr)
                         (error 'native "No idea."))]) 
                  ;; end cond primary/special
                  
                  (let ([magic-label (gensym 'magic_in_out_label)])
                    
                    #|
startp	(iptr = 0;)
ret	(; goto iptr)
endp	(; goto iptr)
cj	(iptr = 0; ifgoto iptr)
j	(iptr = 0; goto iptr)
lend	(iptr = 0, areg = -areg; ifgoto iptr)
stoperr	(iptr = magic_label; ifgoto iptr, magic_label:)
in	(iptr = magic_label; ifgoto iptr, magic_label:)
out	(iptr = magic_label; ifgoto iptr, magic_label:)
outbyte	(iptr = magic_label; ifgoto iptr, magic_label:)
outword	(iptr = magic_label; ifgoto iptr, magic_label:)
call	(iptr = magic_label; iptr -= magic_label, goto iptr, magic_label:)
|#	     
                    
                    ;; BEFORE THE INSTRUCTION
                    (if (member abr '(endp startp ret lend))
                        (printf "iptr = 0;~n"))
                    
                    (if (member abr '(out in outbyte outword call))
                        (print "iptr = (BPOOTER) &&~a;~n" magic-label))
                    
                    (debug 'cprintf (print "printf(\"ins_~a\\n\");~n" abr))
                    
                    (if (equal? abr 'lend)
                        (print "areg = -areg;~n"))
                    
                    ;; THE INSTRUCTION
                    ;;
                    ;;(if (member abr '(in out))
                    ;;(print "ins_~a();~n" abr) 
                    (print "~a~n"
                           (hash-table-get database:code abr 
                                           (lambda () (error 'poo "poo"))))
                    ;;)
                    
                    ;; AFTER THE INSTRUCTION
                    (if (member abr '(endp out outword outbyte in j cj call ret lend))
                        (begin
                          (if (equal? abr 'call)
                              (print "iptr = (BPOOTER) (iptr - ((BPOOTER) &&~a));~n" magic-label))
                          (print "if (iptr != 0) { goto *iptr; }~n")))
                    
                    (if (member abr '(out in outbyte outword call))
                        (print "~a:~n" magic-label)))
                  
                  ]
                 ))))
         
         ;; Until we gracefully handle the last ret/align
         (reverse (cddr (reverse (hash->list bytes)))))
        (print "}~n")
        )
      pass))
  
  
  )

