#|
slinker - code-improvement.ss
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
(module code-improvement mzscheme
  (require "helpers.ss"
           "types.ss"
	   "version.ss"
           "linked-list-on-hash.ss"
           (lib "list.ss")
           (lib "pregexp.ss"))
           
  (provide (all-defined))
  
  ;; Add this files revision to the version information
  (put-revision "$Revision: 3119 $")
     
     ;; PASS: old2new
     ;; Converts the old data structure into the new. Metadata is stored
     ;; wholesale; only the instruction stream is injected as "new".
     (define old2new
       (lambda (h)
         (let ([meta (pass-meta h)]
               [h (pass-bytes h)]
               [lloh (new linked-list%)]
               [keylist '()])
           ;; Store the meta structure
           (send lloh add-metadata 'metadata meta)
           
           ;; Extract the keys from the hash, sort
           (hash-table-for-each
            h (lambda (k v)
                (set! keylist (cons k keylist))))
           (set! keylist (quicksort keylist <))
           
           ;; Store the sorted keys as metadata in the lloh
           (send lloh add-metadata 'keylist keylist)
           
           ;; Extract all the instructions; insert them
           ;; into the new structure
           (let ([next (send lloh head)])
             (hash-table-for-each
              h (lambda (k v)
                  ;; Insert each instruction into the linked list.
                  ;; Insert each "next" instruction after the current one.
                  (set! next (send lloh insert/after next v)))))

           ;; Spit out the new structure full of uber goodness
           lloh)))
     
     ;; PASS: new2old 
     ;; The reverse of "old2new". Restores the metadata, and 
     ;; the linked list.
     (define new2old
       (lambda (lloh)
         (let ([meta (send lloh get-metadata 'metadata)]
               [keylist (send lloh get-metadata 'keylist)]
               [h (make-hash-table)]
               [instlist '()])
           
           ;; Extract the instructions in order;
           ;; reverse the extracted list, since cons
           ;; builds the list from the front... wish
           ;; we had an O(1) "snoc"...
           (send lloh walk (lambda (ptr i)
                             (set! instlist (cons i instlist))))
           (set! instlist (reverse instlist))
           
           ;; Load the old-school hash-table
           ;; with data.
           (for-each (lambda (k i)
                       (hash-table-put! h k i))
                     keylist
                     instlist)
           
           ;; Return an old-school "pass"    
           (make-pass meta h))))
           
        
               
               
  ;; PASS: fix-load-labels
  ;; Modifies the load-label and load-label-difference instructions
  ;; so instead of symbolic or numeric labels provided by KRoC, they now
  ;; point to hash table keys. 
  (define fix-load-labels
    (lambda (h)
      (let ([meta (pass-meta h)]
            [h (pass-bytes h)]
            [new-h (make-hash-table)]
            [jmap (make-hash-table)]
            )
        
        (define get-new-location
          (lambda (v)
            (hash-table-get jmap v (lambda () 'foo)) ))
        
        (hash-table-for-each
         h (lambda (k v)
             (cond
               [(ulabel? v) 
                (hash-table-put! jmap (ulabel-value v) k)])))
        
        (hash-table-for-each
         h (lambda (k v)
             (cond
               [(load-label? v)
                (hash-table-put! 
                 new-h k (make-load-label (Instruction-meta v)
					  (get-new-location (load-label-value v))
					  ))]
               
               [(load-label-difference? v)
                (hash-table-put! 
                 new-h k (make-load-label-difference
			  (Instruction-meta v)
                          (get-new-location (load-label-difference-start v))
                          (get-new-location (load-label-difference-end v))
			  ))]
               [else
                (hash-table-put! new-h k v)])))
        (make-pass meta new-h)
        )))
  
  
  ;; PASS: remove-globalname
  ;; We shouldn't just throw these away. They will come back in
  ;; when linking multiple files.
  (define remove-globalname
    (lambda (h)
      (let ([meta (pass-meta h)]
            [h (pass-bytes h)])
      (hash-table-for-each
       h (lambda (k v)
           (if (globalname? v)
               (hash-table-remove! h k))))
        (make-pass meta h))))
  
  
  ;; PASS: get-and-remove-spaces
  ;; The compiler gives us information valuable to the runtime; in 
  ;; particular, the size of the WORKSPACE, the VECTORSPACE,
  ;; and the MOBILESPACE that we will need to execute this program.
  ;; Here, we grab those values into global variables,
  ;; and remove the instructions from the stream.
  (define get-and-remove-spaces
    (lambda (h)
      (let ([meta (pass-meta h)]
            [h (pass-bytes h)])
        (hash-table-for-each
         h (lambda (k v)
             (cond
               [(setws? v)
                (hash-table-remove! h k)
                (if (> (setws-value v) *WS*)
                    (set-workspace! (setws-value v)))]
               [(setvs? v)
                (hash-table-remove! h k)
                (if (> (setvs-value v) *VS*)
                    (set-vectorspace! (setvs-value v)))]
               [(mobilespace-usage? v)
                (hash-table-remove! h k)
                (if (> (mobilespace-usage-value v) *MS*)
                    (set-mobilespace! (mobilespace-usage-value v)))])))
        (make-pass meta h))))
  
  
  (define get-and-remove-filename
    (lambda (h)
      (let ([meta (pass-meta h)]
            [h (pass-bytes h)])
        (hash-table-for-each
         h (lambda (k v)
             (if (source-filename? v)
                 (begin
                   (hash-table-remove! h k)
                   (let ([fn (car (pregexp-split 
                                   "\\." 
                                   (apply string-append 
                                          (map string
                                               (cdr (source-filename-string v))))))])
                     (void)
                     ;;(set-output-filename! (format "~a.~a" fn *OUTPUT-EXTENSION*))
                     )))))
      (make-pass meta h))))
  
 
  
  (define strip-labels
    (lambda (h)
      (let ([meta (pass-meta h)]
            [h (pass-bytes h)])
        (hash-table-for-each
         h (lambda (k v)
             (if (ulabel? v) 
                 (hash-table-remove! h k))))
        (make-pass meta h))))
  
  
  ;; PASS: expand-notprocess
  ;; Converts the notprocess special into a load constant.
  ;; Fixed this so that it works with NOTPROCESS as being 0
  ;; rather than MININT
  (define expand-notprocess
    (lambda (h)
      (let ([meta (pass-meta h)]
            [h (pass-bytes h)])
      (hash-table-for-each
       h (lambda (k v)
           (if (notprocess? v)
	       (let ([m (make-hash-table)])
		 (hash-table-put! m 'abbreviation 'ldc)
                 (hash-table-put! m 'operand 0)
		 ;;(hash-table-put! m 'type 'primary)
		 
		 
		 (hash-table-put! 
		  h k 
		  (make-inst 
		   m
		   *LDC* 0
		   ))))))
      (make-pass meta h))))
  ;; 
  ;; PASS: expand-reschedule
  (define expand-reschedule
    (lambda (h)
      (let ([meta (pass-meta h)]
            [h (pass-bytes h)])
      (hash-table-for-each
       h (lambda (k v)
           (if (etc0_reschedule? v)
	       (let ([m (make-hash-table)])
		 (hash-table-put! m 'abbreviation 'reschedule)
		 (hash-table-put! m 'type 'secondary)
		 
		 (hash-table-put! 
		  h k 
		  (make-inst 
		   m
		   *OP* #x28
		   ))))))
      (make-pass meta h))))


  ;; FIXME: Do we use this one???
  (define expand-notprocess-annotated-DOWEUSETHISONE???
    (lambda (h)
      (let ([meta (pass-meta h)]
            [h (pass-bytes h)])
      (hash-table-for-each
       h (lambda (k v)
           (cond
	    [(notprocess? v)
	     (let ([anno (Instruction-meta v)])
	       (hash-table-put! anno  'source 'notprocess)
	       (hash-table-put! h k (make-inst anno *LDC* *MININT* )))]

	    [(notprocess? v)
	     (let ([anno (make-hash-table)])
	       (hash-table-put! anno 'source 'notprocess)
	       (hash-table-put! h k (make-inst anno *LDC* *MININT* )))]
	    [else (void)])))
      (make-pass meta h))))
  
  
  
  ;; PASS: remove-redundant-jumps
  ;; Removes jumps that jump one space forward.
  (define remove-redundant-jumps1
    (lambda (h)
      (let ([meta (pass-meta h)]
            [h (pass-bytes h)])
        (let ([next-key (next-key h)])
          (hash-table-for-each
           h (lambda (k v)
               (if (ujump? v) 
                   (let ([target (ujump-value v)])
                     (if (= (next-key k) (next-key target))
                         (hash-table-remove! h k))))))
          (make-pass meta h)))))
  
  
  ;; Don't know if this really does what it is supposed to,
  ;; but it does it much, much faster.
  (define remove-redundant-jumps2
    (lambda (h)
      (let ([meta (pass-meta h)]
            [h (pass-bytes h)])
        (let* ([falseproc (lambda () #f)]
               [lookup 
                (lambda (old)
                  (let ([this (hash-table-get h old falseproc)]
                        [next old])
                    (if this
                        old
                        (begin
                          (let loop ([old next])
                            (unless (hash-table-get h old falseproc)
                              ;;(printf "next from ~a is ~a~n" old (add1 old))
                              (set! next old)
                              (loop (add1 old))))
                          (add1 next)
                          ))))])
        
          (hash-table-for-each
           h (lambda (k v)
               (if (ujump? v) 
                   (let ([target (ujump-value v)])
                     ;;(debug (printf "Checking ~a~n" target))
                     (if (= (lookup k) (lookup target))
                         (hash-table-remove! h k))))))
          (make-pass meta h)))))
  
  ;; PASS ...
  ;;WARNING 20040517
  ;; We don't know how to handle these... it's 7:35, and we're tired of this.
  ;; And we're hungry.
  ;; WARNING 20040529 
  ;; The next time we looked at this, it was 6:42... and we're still confused and hungry.
  ;; WARNING 20040906
  ;; The next time I looked at this I am at CPA2004, and worried that some of the
  ;; errors we are getting on the brick are alignment errors, possibly with a bit 
  ;; of endianism mixed in for good measure. We need to not ignore alignment
  ;; anymore I say! But this talk is overrunning, and I am hungry. Oh, and 
  ;; probably slightly confused.
     #|
  (define remove-alignment
    (lambda (h)
      (hash-table-for-each
       h (lambda (k v)
           (if (align? v)
               (hash-table-remove! h k))))
      h))
  |#
  
  
  
  ;; PASS: expand-loopend
  ;; Expands loopend into a few load constants. Basically.
  (define expand-loopend
    (lambda (h)
      (let ([meta (pass-meta h)]
            [h (pass-bytes h)]
            [new-h (make-hash-table)])
	;; Everywhere we add (rather than replace) keys to a hashtable using 
	;; hash-table-for-each, we must insert into a new hash table to
	;; avoid hash-table-for-each breaking (see plt docs)
        (hash-table-for-each
         h (lambda (k v)
             (if (or (loopend? v) (loopend3? v) (loopend-backwards? v))
	       (let ([abbrv #f]
		     [fn #f]
		     [ctrl-block-addr #f]
		     [start #f]
		     [end #f])
		   (cond
		     [(loopend? v) (set! abbrv 'lend) (set! fn #x21) 
		       (set! start loopend-start) (set! end loopend-end)
		       (set! ctrl-block-addr loopend-ctrl-block-addr)]
		     [(loopend3? v) (set! abbrv 'lend3) (set! fn #x26)
		       (set! start loopend3-start) (set! end loopend3-end)
		       (set! ctrl-block-addr loopend3-ctrl-block-addr)]
		     [(loopend-backwards? v) (set! abbrv 'lendbw) (set! fn #x27)
		       (set! start loopend-backwards-start) (set! end loopend-backwards-end)
		       (set! ctrl-block-addr loopend-backwards-ctrl-block-addr)])
		   (debug 'expand-loopend (printf "~a~n" meta))
		   (debug 'expand-loopend (printf "loopend @ ~a~n" k))
                   ;;Remove the loopend instruction.
		   ;; NO LONGER NEEDED AS WE INSERT INTO NEW HASH TABLE
		   ;; AND THIS IS THEREFORE IMPLICITLY REMOVED BY NOT
		   ;; INSERTING IT
                   ;;(hash-table-remove! h k)
                   
                   ;; LOAD BREG with control block address
		   (let ([m (make-hash-table)])
		     (hash-table-put! m 'abbreviation 'ldlp)
		     (hash-table-put! m 'operand (ctrl-block-addr v))
		     (hash-table-put! m 'type 'primary)
		     (set! new-h 
			   (insert-instruction 
			    new-h (+ k 1)
			    (make-inst 
			     m
			     *LDLP* (ctrl-block-addr v) 
			     ))))
		     
                 ;; Load the areg with loop offset
		   (let ([m (make-hash-table)])
		     (hash-table-put! m 'abbreviation 'load-label-difference)
		     (hash-table-put! m 'operand (list (start v) (end v)))
		     (hash-table-put! m 'type 'special)
		     (set! new-h 
			   (insert-instruction
			    new-h (+ k 2)
			    (make-load-label-difference
			     m
			     (start v) (end v) ))))
               
                 ;;The actual LEND instruction
		   (let ([m (make-hash-table)])
		     (hash-table-put! m 'abbreviation abbrv)
		     (hash-table-put! m 'type 'secondary)
		     (set! new-h 
			   (insert-instruction
			    new-h (+ k 3)
			    (make-inst m *OP* fn ))))
                 
                 ;;And a label.
		   (let ([m (make-hash-table)])
		     (hash-table-put! m 'abbreviation 'setlab)
		     (hash-table-put! m 'operand (end v))
		     (hash-table-put! m 'type 'special)
		     (set! new-h 
			   (insert-instruction
			    new-h (+ k 4)
			    (make-ulabel m (end v) ))))
            
                 )
		 (hash-table-put! new-h k v))))
      (make-pass meta new-h))))



    
  ;; PASS: expand-starttable
  ;; Expands startable into the sequence of instructions detailed in the
  ;; compiler writers guide on page 26.
  ;; FIXME: If we prefix optimally this is NOT going to work, and a more clever
  ;; routine has to be devised for this! This bit of code currently relies on
  ;; the fact that all jumps are of equal length!!!
  (define expand-starttable
    (lambda (h)
      (let* ([meta (pass-meta h)]
            [h (pass-bytes h)]
            [new-h (make-hash-table)]
	    [m-label (gensym 'M)]
	    [jt-label (gensym 'jump_table)])
	;; Everywhere we add (rather than replace) keys to a hashtable using 
	;; hash-table-for-each, we must insert into a new hash table to
	;; avoid hash-table-for-each breaking (see plt docs)
        (hash-table-for-each
         h (lambda (k v)
             (if (starttable? v)
                 (begin
		   (debug 'expand-starttable (printf "~a~n" meta))
		   (debug 'expand-starttable (printf "starttable @ ~a~n" k))
                   ;;Remove the starttable instruction.
		   ;; NO LONGER NEEDED AS WE INSERT INTO NEW HASH TABLE
		   ;; AND THIS IS THEREFORE IMPLICITLY REMOVED BY NOT
		   ;; INSERTING IT
                   ;;(hash-table-remove! h k)
                  
		   ;;    X; ldc c; diff; ldc jump_size; prod; ldc (jump_table -M); ldpi
		   ;; M: bsub; gcall;
		   ;; jump_table:
		   ;;    j case_0; j case_1; ... j case_k
		   ;;
		   ;; (Where the initial X, which is the argument to the case,
		   ;; is already on the stack)
		   ;; (I believe that we dont do the ldc c and diff instruction
		   ;; which comes just after the X)


                   ;; Load the wordsize (jump_size)
		   (let ([m (make-hash-table)])
		     (hash-table-put! m 'abbreviation 'ldc)
		     (hash-table-put! m 'operand (* 2 *WORDSIZE*))
		     (hash-table-put! m 'type 'primary)
		     (set! new-h 
			   (insert-instruction 
			    new-h (+ k 1)
			    (make-inst 
			     m
			     *LDC* (* 2 *WORDSIZE*)
			     ))))
		   
		   ;; Multiply (with index: X)
		   (let ([m (make-hash-table)])
		     (hash-table-put! m 'abbreviation 'prod)
		     (hash-table-put! m 'type 'secondary)
		     (set! new-h 
			   (insert-instruction 
			    new-h (+ k 2)
			    (make-inst 
			     m
			     *OP* #x8
			     ))))

                   ;; Load the jump_table offset
		   (let ([m (make-hash-table)])
		     (hash-table-put! m 'abbreviation 'load-label-difference)
		     (hash-table-put! m 'operand (list m-label jt-label))
		     (hash-table-put! m 'type 'special)
		     (set! new-h 
			   (insert-instruction
			    new-h (+ k 3)
			    (make-load-label-difference
			     m
			     m-label jt-label ))))
               
                   ;; 
		   (let ([m (make-hash-table)])
		     (hash-table-put! m 'abbreviation 'ldpi)
		     (hash-table-put! m 'type 'secondary)
		     (set! new-h 
			   (insert-instruction 
			    new-h (+ k 4)
			    (make-inst 
			     m
			     *OP* #x1B
			     ))))



                   ;; Add the M label
		   (let ([m (make-hash-table)])
		     (hash-table-put! m 'abbreviation 'setlab)
		     (hash-table-put! m 'operand m-label)
		     (hash-table-put! m 'type 'special)
		     (set! new-h 
			   (insert-instruction
			    new-h (+ k 5)
			    (make-ulabel m m-label))))
            

		   ;; Byte subscript
		   (let ([m (make-hash-table)])
		     (hash-table-put! m 'abbreviation 'bsub)
		     (hash-table-put! m 'type 'secondary)
		     (set! new-h 
			   (insert-instruction 
			    new-h (+ k 6)
			    (make-inst 
			     m
			     *OP* #x2
			     )))) 

		   ;; general call 
		   (let ([m (make-hash-table)])
		     (hash-table-put! m 'abbreviation 'gcall)
		     (hash-table-put! m 'type 'secondary)
		     (set! new-h 
			   (insert-instruction 
			    new-h (+ k 7)
			    (make-inst 
			     m
			     *OP* #x6
			     )))) 



                   ;; Add the jump_table label
		   (let ([m (make-hash-table)])
		     (hash-table-put! m 'abbreviation 'setlab)
		     (hash-table-put! m 'operand jt-label)
		     (hash-table-put! m 'type 'special)
		     (set! new-h 
			   (insert-instruction
			    new-h (+ k 8)
			    (make-ulabel m jt-label))))

                 )
		 (hash-table-put! new-h k v))))
      (make-pass meta new-h))))


  (define expand-ffi-stubnames
    (lambda (h)
      (let ([meta (pass-meta h)]
            [h (pass-bytes h)]
            [new-h (make-hash-table)]
            [get-ffi-index 
             (lambda (name)
	       (let ([specialmatch (pregexp-match 
				    "^C\\.tvmspecial\\.([0-9]+).*" 
				    (bytes->string/locale name))])
		 (cond
                   ;; 20061019 MCJ
                   ;; We're fixing this under CLJ's guidance.
                   [specialmatch 
                    ;; We don't want a '0' here. That would be bad.
                    ;; We add one, and let the interpreter say 
                    ;; "Hey! This is negative! So, I must need to handle it
                    ;; as a static special!" Of course... this is whacked...
                    (- (add1 (string->number (cadr specialmatch))))]
                   [(equal? ffi-type 'dynamic) (add-ffi-name name)]
                   ;; 20061019 MCJ
                   ;; CLJ claims this is old bollux, and can be removed.
                   ;;[else (get-static-ffi-index name)]
                   )))])
	;; Everywhere we add (rather than replace) keys to a hashtable using 
	;; hash-table-for-each, we must insert into a new hash table to
	;; avoid hash-table-for-each breaking (see plt docs)
        (hash-table-for-each
         h (lambda (k v)
             (if (ffi-stubname? v)
                 (let ([name (list->bytes (ffi-stubname-value v))])
                   (let ([ffi-index (get-ffi-index name)])
                     ;; We used to do this, but its bad, cos we have a
                     ;; renumbered instruction stream with no space to insert
                     ;; new instructions, instead we'll make a binary.
                     ;;(hash-table-put! new-h k (make-binary
                     ;;                      (Instruction-meta v)
                     ;;                      (pad (prefix *LDC* ffi-index))))
                     ;;(hash-table-put! new-h (+ k 1) (make-inst
                     ;;			 (Instruction-meta v) 
                     ;;                            *OP* *FFICALL* ))
                     ;;
                     (hash-table-put! new-h k (make-binary
                                               (Instruction-meta v)
                                               (append
                                                (pad (prefix *LDC* ffi-index) 6)
                                                (prefix *OP* *FFICALL*))))
                     (debug 'ffi (printf "FFI STUB: ~a ~a~n" name ffi-index))
                     ))                      
		 (hash-table-put! new-h k v))))
        
        (make-pass meta new-h))))

  (define expand-semaphore
    (lambda (h)
      (let ([meta (pass-meta h)]
            [h (pass-bytes h)]
            [new-h (make-hash-table)])
        (hash-table-for-each
         h (lambda (k v)
             (cond 
	       [(seminit? v)
		 (let* ([c k]
			[ins-loc (lambda () (set! c (add1 c)) c)]
			[m (make-hash-table)])
		   ;; FIXME: Temporary until I figure out what this code
		   ;; has to be!!!!
		   (insert-instruction 
		     new-h (ins-loc)
		     (make-binary m (prefix *OP* *SEMINIT*))))]
	       [(semclaim? v)
		 (let* ([c k]
			[ins-loc (lambda () (set! c (add1 c)) c)]
			[m (make-hash-table)])
		   ;; FIXME: Temporary until I figure out what this code
		   ;; has to be!!!!
		   (insert-instruction 
		     new-h (ins-loc)
		     (make-binary m (prefix *OP* *SEMCLAIM*))))]
	       [(semrelease? v)
		 (let* ([c k]
			[ins-loc (lambda () (set! c (add1 c)) c)]
			[m (make-hash-table)])
		   ;; FIXME: Temporary until I figure out what this code
		   ;; has to be!!!!
		   (insert-instruction 
		     new-h (ins-loc)
		     (make-binary m (prefix *OP* *SEMRELEASE*))))]

	       [else (insert-instruction new-h k v)])))
        (make-pass meta new-h))))
     
     ;;Barrier instructions 
     (define expand-barrier
       (lambda (h)
         (let ([meta (pass-meta h)]
               [h (pass-bytes h)]
               [new-h (make-hash-table)])
           (hash-table-for-each 
            h (lambda (k v) ;;k is instruction location
                (cond 
                  [(barinit? v) 
                   (let ([m (make-hash-table)])
                     (insert-instruction 
                      new-h k
                      (make-binary m (prefix *OP* *BARINIT*))))]
                  [(barsync? v) 
                   (let ([m (make-hash-table)])
                     (insert-instruction 
                      new-h k
                      (make-binary m (prefix *OP* *BARSYNC*))))]
                  [(barresign? v) 
                   (let ([m (make-hash-table)])
                     (insert-instruction 
                      new-h k
                      (make-binary m (prefix *OP* *BARRESIGN*))))]
                  [(barenroll? v)
                   (let ([m (make-hash-table)])
                     (insert-instruction 
                      new-h k
                      (make-binary m (prefix *OP* *BARENROLL*))))]
                  [else (insert-instruction new-h k v)]
                  )))
           (make-pass meta new-h))))

  (define expand-floatingpoint-specials
       (lambda (h)
         (let ([meta (pass-meta h)]
               [h (pass-bytes h)]
               [new-h (make-hash-table)])
           (hash-table-for-each 
            h (lambda (k v) ;;k is instruction location
                (cond 
                  [(i64toreal? v) 
                   (let ([m (make-hash-table)])
                     (insert-instruction 
                      new-h k
                      (make-binary m (prefix *OP* *I64TOREAL*))))]
                  [else (insert-instruction new-h k v)]
                  )))
           (make-pass meta new-h))))


     
  (define expand-mobileinit
    (lambda (h)
      (let ([meta (pass-meta h)]
            [h (pass-bytes h)]
            [new-h (make-hash-table)])
        (hash-table-for-each
         h (lambda (k v)
             (if (mobileinit? v)
	       (let* ([out-label (gensym 'mobileinit-out)]
		      [pairs (mobileinit-pairs v)]
		      [msp-offset (mobileinit-msp-offset v)]
		      [c k]
		      [ins-loc (lambda () (set! c (add1 c)) c)])
                 ;; 070531 MCJ
                 ;; Addresses #120
                 ;; Spurious printing.
		 (debug 'mobileinit (printf "mobileinit ~a ~a @ ~a~n" msp-offset (length pairs) k))
		 ;; Load the mobilespace pointer out of workspace
		 (let ([m (make-hash-table)])
		   (hash-table-put! m 'abbreviation 'ldl)
		   (hash-table-put! m 'operand msp-offset)
		   (hash-table-put! m 'type 'primary)
		   (insert-instruction
		    new-h (ins-loc)
		    (make-binary m (prefix *LDL* msp-offset))))
		  (debug 'mobileinit (printf "@~a ~a~n" c (map hex (binary-value (hash-table-get new-h c)))))

		 ;; Load the mobilespace pointer out of workspace
		 (let ([m (make-hash-table)])
		   (hash-table-put! m 'abbreviation 'ldnl)
		   (hash-table-put! m 'operand 0)
		   (hash-table-put! m 'type 'primary)
		   (insert-instruction
		    new-h (ins-loc)
		    (make-binary m (prefix *LDNL* 0))))
		 (debug 'mobileinit  (printf "@~a ~a~n" c (map hex (binary-value (hash-table-get new-h c)))))
		 ;; Min int
		 (let ([m (make-hash-table)])
		   (hash-table-put! m 'abbreviation 'mint)
		   (hash-table-put! m 'type 'secondary)
		   (insert-instruction
		    new-h (ins-loc)
		    (make-binary m (prefix *OP* *MINT*))))
		 (debug 'mobileinit (printf "@~a ~a~n" c (map hex (binary-value (hash-table-get new-h c)))))

		 ;; DIFF the loaded value and MINT to figure out if things have
		 ;; been initialised (DIFF to avoid overflow)
		 (let ([m (make-hash-table)])
		   (hash-table-put! m 'abbreviation 'diff)
		   (hash-table-put! m 'type 'secondary)
		   (insert-instruction
		    new-h (ins-loc)
		    (make-binary m (prefix *OP* *DIFF*))))
		 (debug 'mobileinit (printf "@~a ~a~n" c (map hex (binary-value (hash-table-get new-h c)))))

		;; Do the if not zero bit
		;; FIXME: Is this essentially the same as boolinvert
		;; and if so, should we get rid of boolinvert and use
		;; this instruction instead?????
		 (let ([m (make-hash-table)])
		   (hash-table-put! m 'abbreviation 'eqc)
		   (hash-table-put! m 'operand 0)
		   (hash-table-put! m 'type 'primary)
		   (insert-instruction
		    new-h (ins-loc)
		    (make-binary m (prefix *EQC* 0))))
		 (debug 'mobileinit (printf "@~a ~a~n" c (map hex (binary-value (hash-table-get new-h c)))))

		;; FIXME: Do I have to do stuff for this comparison to work
		;; right? given the result above? 
		 ;; Branch on NOT ZERO (skipping init bit)
		 (let ([m (make-hash-table)])
		   (hash-table-put! m 'abbreviation 'cj)
		   (hash-table-put! m 'operand out-label)
		   (hash-table-put! m 'type 'primary)
		   (insert-instruction
		    new-h (ins-loc)
		    (make-ujump m out-label *CJ*)))
		 (debug 'mobileinit (printf "@~a ~a~n" c (map hex (binary-value (hash-table-get new-h c)))))
		 (debug 'mobileinit (printf "@~a ~a~n" c (hash-table-get  new-h c)))

		 (let loop ([p pairs])
		   (if (not (null? p))
		     (let* ([pair (car p)]
		            [rest (cdr p)]
			    [slot-off (car pair)]
			    [data-off (cdr pair)])
		       (debug 'mobileinit (printf "~a - ~a~n" slot-off data-off))
		       (cond
			 ;; FIXME FIXME FIXME: This comapison does not work, as
			 ;; -1 is decoded as 4294967295... by decode-num
			 ;;[(< slot-off 0) 
			 [(< (->signed slot-off) 0)
                          ;; MOBILEMINT
                          ;; 070531 MCJ
                          ;; Addressses #120
                          (debug 'mobileinit (printf "mobilemint ~a~n" data-off))
			   ;; MINT
			   (let ([m (make-hash-table)])
			     (hash-table-put! m 'abbreviation 'mint)
			     (hash-table-put! m 'type 'secondary)
			     (insert-instruction
			      new-h (ins-loc)
			      (make-binary m (prefix *OP* *MINT*))))
			   ;; Load the mobilespace pointer out of workspace
			   (let ([m (make-hash-table)])
			     (hash-table-put! m 'abbreviation 'ldl)
			     (hash-table-put! m 'operand msp-offset)
			     (hash-table-put! m 'type 'primary)
			     (insert-instruction
			      new-h (ins-loc)
			      (make-binary m (prefix *LDL* msp-offset))))
			   ;; STNL
			   (let ([m (make-hash-table)])
			     (hash-table-put! m 'abbreviation 'stnl)
			     (hash-table-put! m 'operand data-off)
			     (hash-table-put! m 'type 'primary)
			     (insert-instruction
			      new-h (ins-loc)
			      (make-binary m (prefix *STNL* data-off))))
			   ;;(error "MOBILEMINT not tested!")
                           ;; 070531 MCJ
                           ;; Addresses #120
			   (debug 'mobileinit (printf "WARNING: MOBILEINIT/MOBILEMINT not tested a lot!~n"))
			   ]
			 [(> data-off 0)
                          ;; MOBILEPAIR
                           ;; 070531 MCJ
                          ;; Addresses #120
                          (debug 'mobileinit (printf "mobilempair ~a ~a~n" slot-off data-off))
			   ;; Load the mobilespace pointer out of workspace
			   (let ([m (make-hash-table)])
			     (hash-table-put! m 'abbreviation 'ldl)
			     (hash-table-put! m 'operand msp-offset)
			     (hash-table-put! m 'type 'primary)
			     (insert-instruction
			      new-h (ins-loc)
			      (make-binary m (prefix *LDL* msp-offset))))
                           ;; 070531 MCJ
                           ;; Addresses #120
                           (debug 'mobileinit 
                                  (printf "@~a ~a~n" c (map hex (binary-value (hash-table-get new-h c)))))
			   ;; Get the address of data offset
			   (let ([m (make-hash-table)])
			     (hash-table-put! m 'abbreviation 'ldnlp)
			     (hash-table-put! m 'operand data-off)
			     (hash-table-put! m 'type 'primary)
			     (insert-instruction
			      new-h (ins-loc)
			      (make-binary m (prefix *LDNLP* data-off))))
			   ;; 070531 MCJ
                           ;; Addresses #120
                           (debug 'mobileinit (printf "@~a ~a~n" c (map hex (binary-value (hash-table-get new-h c)))))
			   ;; Load the mobilespace pointer out of workspace
			   (let ([m (make-hash-table)])
			     (hash-table-put! m 'abbreviation 'ldl)
			     (hash-table-put! m 'operand msp-offset)
			     (hash-table-put! m 'type 'primary)
			     (insert-instruction
			      new-h (ins-loc)
			      (make-binary m (prefix *LDL* msp-offset))))
                           ;; 070531 MCJ
                           ;; Addresses #120
			   (debug 'mobileinit 
                                  (printf "@~a ~a~n" c (map hex (binary-value (hash-table-get new-h c)))))
			   ;; Store it into the slot
			   (let ([m (make-hash-table)])
			     (hash-table-put! m 'abbreviation 'stnl)
			     (hash-table-put! m 'operand slot-off)
			     (hash-table-put! m 'type 'primary)
			     (insert-instruction
			      new-h (ins-loc)
			      (make-binary m (prefix *STNL* slot-off))))
                           ;; 070531 MCJ
                           ;; Addresses #120
			   (debug 'mobileinit (printf "@~a ~a~n" c (map hex (binary-value (hash-table-get new-h c)))))
			   ]
			 [(< data-off 0)
                          ;; MOBILENULL
                          ;; 070531 MCJ
                          ;; Addresses #120
                          (debug 'mobileinit 
                                 (printf "mobilenull ~a~n" slot-off))
                          (error "MOBILENULL not tested!")]
			 [else 
                          ;; MOBILEARRAY
                          ;; 070531 MCJ
                          ;; Addresses #120
                          (debug 'mobileinit 
                                 (printf "mobilearray ~a ~a~n" slot-off data-off))

                          ;; MINT
                          (let ([m (make-hash-table)])
			     (hash-table-put! m 'abbreviation 'mint)
			     (hash-table-put! m 'type 'secondary)
			     (insert-instruction
			      new-h (ins-loc)
			      (make-binary m (prefix *OP* *MINT*))))
			   ;; Load the mobilespace pointer out of workspace
			   (let ([m (make-hash-table)])
			     (hash-table-put! m 'abbreviation 'ldl)
			     (hash-table-put! m 'operand msp-offset)
			     (hash-table-put! m 'type 'primary)
			     (insert-instruction
			      new-h (ins-loc)
			      (make-binary m (prefix *LDL* msp-offset))))
                           ;; 070531 MCJ
                           ;; Addresses #120
			   (debug 'mobileinit 
                                  (printf "@~a ~a~n" c (map hex (binary-value (hash-table-get new-h c)))))
			   ;; STNL
			   (let ([m (make-hash-table)])
			     (hash-table-put! m 'abbreviation 'stnl)
			     (hash-table-put! m 'operand slot-off)
			     (hash-table-put! m 'type 'primary)
			     (insert-instruction
			      new-h (ins-loc)
			      (make-binary m (prefix *STNL* slot-off))))
			   ;; LDC 0
			   (let ([m (make-hash-table)])
			     (hash-table-put! m 'abbreviation 'ldc)
                             (hash-table-put! m 'operand 0)
			     ;;(hash-table-put! m 'type 'secondary)
			     (insert-instruction
			      new-h (ins-loc)
			      (make-binary m (prefix *LDC* 0))))
			   ;; Load the mobilespace pointer out of workspace
			   (let ([m (make-hash-table)])
			     (hash-table-put! m 'abbreviation 'ldl)
			     (hash-table-put! m 'operand msp-offset)
			     (hash-table-put! m 'type 'primary)
			     (insert-instruction
			      new-h (ins-loc)
			      (make-binary m (prefix *LDL* msp-offset))))
			   ;; STNL
			   (let ([m (make-hash-table)])
			     (hash-table-put! m 'abbreviation 'stnl)
			     (hash-table-put! m 'operand (add1 slot-off))
			     (hash-table-put! m 'type 'primary)
			     (insert-instruction
			      new-h (ins-loc)
			      (make-binary m (prefix *STNL* (add1 slot-off)))))
                           ;; 070531 MCJ
                           ;; Addresses #120
			   (debug 'mobileinit 
                                  (printf "WARNING: MOBILEINIT/MOBILEARRAY not tested a lot!~n"))
			   ;(error "MOBILEARRAY not tested!")
			   ]
			   )

		       (loop rest))))


		 ;; Set the label for the end of the init
		 (let ([m (make-hash-table)])
		   (hash-table-put! m 'abbreviation 'setlab)
		   (hash-table-put! m 'operand out-label)
		   (hash-table-put! m 'type 'special)
		   (insert-instruction
		    new-h (ins-loc)
		    (make-ulabel m out-label))))

	       ;; else
		 (insert-instruction new-h k v))))
        (make-pass meta new-h))))

  
  )
