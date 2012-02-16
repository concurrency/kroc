#|
slinker - code-simplification.ss
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
(module code-simplification mzscheme
  (require "helpers.ss"
	   "version.ss"
           (prefix srfi13: (lib "13.ss" "srfi"))
           (prefix srfi1: (lib "1.ss" "srfi"))
           )
  (require (lib "pregexp.ss"))
  (provide (all-defined))

  ;; Add this files revision to the version information
  (put-revision "$Revision: 3119 $")

  #|  PASS 5 : remove-unknown-things  |#
  #| ################################ |#    
  ;; Really, we shouldn't have this pass. At least,
  ;; we're pretty sure we shouldn't.
  ;; WARNING WARNING WARNING
  ;; FIXME FIXME FIXME
  (define remove-unknown-things
    (lambda (h)
      (let ([meta (pass-meta h)]
            [h (pass-bytes h)])
      (hash-table-for-each
       h (lambda (k v)
           (if (or (loadwsmap? v) (unloadwsmap? v)  
                   (realresult? v) (fppop? v) 
                   (contrsplit? v) (funcresults? v) (funcreturn? v) (contrjoin? v)
		   (checknotnull? v)) ;; FIXME: Implement this rather than remove?
               (begin
                 (debug 'simplification 
			(printf 
			 "BIG UGLY WARNING!!! Removing 'unknown' thing: ~a~n" v))
                 (hash-table-remove! h k)))))
      (make-pass meta h))))


  #| EXPLORING LINE NUMBERS |#
  (define (show-all-line-numbers h)
    (let ([meta (pass-meta h)]
          [bytes (pass-bytes h)])
      (debug 'lines
             (for-each
              (lambda (p)
                (let ([k (car p)]
                      [v (cdr p)])
                  (printf "~a:\t~a\t~a~n"
                          k (if (binary? v)
                                (map hex (binary-value v))
                                v)
                          (let ([im (Instruction-meta v)])
                            (hash-table-get im 'line (lambda () "None")))
                          )))
              (hash->list bytes)))
      h))
  
  #|  PASS 6 : propagate-line-numbers  |#
  #| ################################# |#    
  #|
  ;; 20051220 MCJ
  ;; This pass is not currently used, and probably shouldn't be.
  ;; I don't think it does what we think it might do. 
  (define propagate-line-numbers
    (lambda (h)
      (let ([meta (pass-meta h)]
            [bytes (pass-bytes h)]
	    [line 1]
	    [comment #f]
	    )
	
	(define annotate
	  (lambda (i)
	    (let ([h (Instruction-meta i)])
	      (if (not (hash-table? h))
		  (error 'prop-line-numbers "~a~n" i))
	      (hash-table-put! h 'line line)
	      (set-Instruction-meta! i h)
	      i)))
	
	(let ([ls (hash->list bytes)])
	  (mapl
	   (lambda (pair)
	     (let ([k (car pair)]
		   [i (cdr pair)])
	       (hash-table-put! bytes k (annotate i))))
	   ls))
	(make-pass meta bytes))))
  |#

  ;; This propagates debugging info from the debugging information embedded in the
  ;; bytecode stream. This works because we map in order, from first to last
  ;; instruction. The previous methods of doing this, used hash-table-for-each,
  ;; which performs a mapping which is as good as random.
  (define propagate-debugging-info
    (lambda (h)
      (let ([meta (pass-meta h)]
            [bytes (pass-bytes h)]
	    [current-line 'empty]
	    [current-procname 'empty]
	    [current-filename 'empty])

	;; Map over the instruction stream in order(!)
	(for-each
	  (lambda (p)
	   (let* ([k (car p)]
	 	  [v (cdr p)]
		  [meta (Instruction-meta v)])
	     (cond
	       [(linenum? v)
		 ;; If we see a line number, update the linenumber we are
		 ;; currently on
		 ;; We will not remove the line number information, as we will do
		 ;; that in a seperate pass
		 ;; Actually doing hash-table-remove! bytes k breaks things a LOT!
		 (set! current-line (linenum-value v))]
	       [(procentry? v)
	         (set! current-procname (procentry-string v))]
	       [(source-filename? v)
	         (set! current-filename (source-filename-string v))]
	       [else
		 ;; If we see an instruction, update its debugging info
		 (if (not (equal? current-line 'empty)) (hash-table-put! meta 'line current-line))
		 (if (not (equal? current-procname 'empty)) (hash-table-put! meta 'proc current-procname))
		 (if (not (equal? current-filename 'empty)) (hash-table-put! meta 'file current-filename))
		 ])
	     ))
	  (hash->list bytes))

	(make-pass meta bytes))))

  ;; Remove debugging instructions pass:
  ;; Remove the instructions (but debugging information which has been
  ;; propagated into meta data will not be touched) which contain the debugging
  ;; data.
  (define remove-debugging-instructions
  (lambda (h)
    (let ([meta (pass-meta h)]
	  [bytes (pass-bytes h)])
    (hash-table-for-each
     bytes (lambda (k v)
	     (if (or (linenum? v)
		     (debugline? v)
		     (source-filename? v)
		     ;; Procentry is used for debugging, but is also used for
		     ;; other things, so we wont remove it at this point.
		     ;;(procentry? v)
		     )
		 (hash-table-remove! bytes k))))
      (make-pass meta bytes))))

  ;; This pass prunes debugging info. It is mainly for libraries, where there is
  ;; no need to record the debuggin information for every instruction in the
  ;; saved library file. If we prune data such that we keep only the instances
  ;; where data changes, we can save a lot of space in the library file.
  (define prune-debugging-info
    (lambda (h)
      (let ([meta (pass-meta h)]
            [bytes (pass-bytes h)]
	    [current-line 'empty]
	    [current-proc 'empty]
	    [current-file 'empty])

	;; Map over the instruction stream in order(!)
	(for-each
	  (lambda (p)
	   (let* ([k (car p)]
	 	  [v (cdr p)]
		  [inst-meta (Instruction-meta v)]
		  [line (hash-table-get inst-meta 'line (lambda () 'none))]
		  [proc (hash-table-get inst-meta 'proc (lambda () 'none))]
		  [file (hash-table-get inst-meta 'file (lambda () 'none))])

	     ;; Deal with line numbers
	     (if (or (equal? line 'none) (equal? line current-line))
	       ;; If there was no debugging info of this type at this
	       ;; instruction, or the info has not changed, remove the debugging
	       ;; info (we can successfully remove a nonexistant key).
	       (hash-table-remove! inst-meta 'line)
	       ;; Otherwise leave it in, and update the current value
	       (set! current-line line))

	     ;; Deal with proc names
	     (if (or (equal? proc 'none) (equal? proc current-proc))
	       ;; If there was no debugging info of this type at this
	       ;; instruction, or the info has not changed, remove the debugging
	       ;; info (we can successfully remove a nonexistant key).
	       (hash-table-remove! inst-meta 'proc)
	       ;; Otherwise leave it in, and update the current value
	       (set! current-proc proc))

	     ;; Deal with file names
	     (if (or (equal? file 'none) (equal? file current-file))
	       ;; If there was no debugging info of this type at this
	       ;; instruction, or the info has not changed, remove the debugging
	       ;; info (we can successfully remove a nonexistant key).
	       (hash-table-remove! inst-meta 'file)
	       ;; Otherwise leave it in, and update the current value
	       (set! current-file file))
	     ))
	  (hash->list bytes))
	(make-pass meta bytes))))


 

  #|  PASS 7 : tag-instructions  |#
  #| ########################### |#
  (define tag-instructions
    (lambda (h)
      (let ([meta (pass-meta h)]
            [bytes (pass-bytes h)])

	(mapl
	 (lambda (p)
	   (let ([k (car p)]
		 [v (cdr p)])
	     (if (inst? v)
		 (let ([fn (inst-fn v)]
		       [op (inst-op v)]
		       ;; This could fail if an <inst> comes through
		       ;; without being annotated -- depends on ordering
		       [meta (Instruction-meta v)]
		       [primary? (lambda (op) (< op 15))]
		       [secondary? (lambda (x) #t)])

		   (cond
		    [(primary? fn)
		     (hash-table-put! meta 'abbreviation (list-ref primaries fn))
		     (if (> op #x80000000)
			 (hash-table-put! meta 'operand (* -1 (add1 (bitwise-mattnot op))))
			 (hash-table-put! meta 'operand (hex op)))
		     (hash-table-put! meta 'type 'primary)
		     
		     (debug 'simplification
			    (printf "~a  ~a~n" 
				    (hash-table-get meta 'abbreviation)
				    (hash-table-get meta 'operand)))
		     ]

		    [(secondary? op)
		     (let ([list-index (quotient op 16)]
			   [abbrv-index (remainder op 16)])

		       (hash-table-put! meta 'type 'secondary)
		       (hash-table-put! meta 'operand #f)

		       (if (< list-index (length secondaries))
			   (hash-table-put! meta
					    'abbreviation
					    (list-ref (list-ref secondaries list-index)
						      abbrv-index))
			   (error 'tag-instructions "Secondary not in tables [FN ~a, OP ~a]~n"
				  (hex fn) (hex op)))
		       (debug 'simplification
			      (printf "~a  ~a~n" 
				      (hash-table-get meta 'abbreviation)
				      (hash-table-get meta 'operand)))
		       )])
		   
		   )) ;;end if
	     )) ;; end lambda
	   (hash->list bytes))
	
	(make-pass meta bytes))))


  
  
  
  (define pad-string
    (lambda (str n)
      (string-append 
       str 
       (make-string (- n (string-length str)) #\space ))))
	     
      
    
  

  
  ;;STEP 5
  ;; remove-useless-text
  ;; Removes comments.
  ;; Also removes the filename, because we 
  ;; don't need it either.
  ;; Basically, we'll remove useless text.
  ;; And line numbers.
  (define remove-useless-text
    (lambda (h)
      (let ([meta (pass-meta h)]
            [bytes (pass-bytes h)])
      (hash-table-for-each
       bytes (lambda (k v)
               (if (or (compiler-comment? v)
                       (codemap? v)
                       ;;(align? v)
                       ;;(linenum? v)
                       ;;(debugline? v)
                       (tsdepth? v)
                       ;;(source-filename? v)
                       ;;(procentry? v)
		       ;;(etc0_reschedule? v)
		       )
                   (hash-table-remove! bytes k))))
        (make-pass meta bytes))))
  
  (define remove-slightly-less-useless-text
    (lambda (h)
      (let ([meta (pass-meta h)]
            [bytes (pass-bytes h)])
      (hash-table-for-each
       bytes (lambda (k v)
               (if (or (compiler-comment? v)
                       (debugline? v)
                       (tsdepth? v)
                       (source-filename? v)
                       )
                   (hash-table-remove! bytes k))))
        (make-pass meta bytes))))
 
  ;; The information gathered in this pass is used when optimaly prefixing. This
  ;; is so we can prefix on a per proc basis, rather than across the whole
  ;; program in one go.
   (define find-procentries
    (lambda (h)
      (let ([meta (pass-meta h)]
            [h (pass-bytes h)])
        (hash-table-for-each
         h (lambda (k v)
             ;; If we find a procentry, insert it's 
             ;; location and value into the procentries hash
             (if (procentry? v)
                 (let ([name (procentry-string v)])
		   ;;(hash-table-remove! h k)
		   (hash-table-put! 
		    procentries
		    k (list->symbol name))))))
      ;; Just return the same pass data
      (make-pass meta h)
      )))
  
  (define (get-tlp-types str)
    (let* ([user-tlp (list-ref (pregexp-match ".*\\((.*?)\\).*" str) 1)]
           [user-tlp (pregexp-replace* " OF" user-tlp "")]
           [compress-spaces (pregexp-replace* "\\s+" user-tlp " ")]
           [split-up (pregexp-split "," compress-spaces)]
           #;[foo (printf "split-up: ~s~n" split-up)]
           [space-split (map (lambda (str)
                               (pregexp-split " " str))
                             split-up)]
           #;[foo (printf "space-split: ~s~n" space-split)]
           [last-dropped (map (lambda (ls)
                                (reverse (cdr (reverse ls))))
                              space-split)]
           #;[foo (printf "last-dropped: ~s~n" last-dropped)]
           [spaces-back (map (lambda (ls)
                               (list-intersperse " " ls))
                             last-dropped)]
           [combined (map (lambda (ls)
                            (srfi13:string-trim-both (apply string-append ls)))
                           spaces-back)])
      
      ;;(printf "user-tlp-types: ~s~n" combined)
      (srfi1:filter (lambda (s)
                      (not (equal? s "")))
                    combined)
      
      ))
             
  
  (define (check-types given expected)
    (let ([given-tlp-types    (get-tlp-types given)]
          [expected-tlp-types (get-tlp-types expected)])
      (let ([given-len (length given-tlp-types)]
            [expected-len (length expected-tlp-types)])
        (cond
          [(not (equal? given-len expected-len))
           (let ([msg ""])
             (if (> given-len expected-len)
                 (set! msg "Too much")
                 (set! msg "Not enough"))
             (raise-user-error (format 
                                "~a parameters in top level.~n\tExpected ~a parameters, ~s~n\tGiven: ~a, ~s~n" 
                                msg
                                (length expected-tlp-types)
                                expected-tlp-types
                                (length given-tlp-types)
                                given-tlp-types
                                )))]
          [else
           (andmap (lambda (t1 t2)
                     ;;(printf "Comparing: ~s ~s~n" t1 t2)
                     (equal? t1 t2))
                   given-tlp-types expected-tlp-types)]))))
          
        
  ;; TLP: "PROC main(CHAN OF BYTE kyb?,CHAN OF BYTE scr!,CHAN OF BYTE err!)\nSEQ\nscr!\nerr!\n:"
  ;; TLP-TYPES: "CHAN INT IN, CHAN BYTE OUT"
  (define check-tlp-already-done? #f)
  (define (check-tlp a-pass)
    ;; By default, we're not checking.
    (unless (or check-tlp-already-done? (equal? TLP-TYPES "NONE"))
      (let* ([bytes (pass-bytes a-pass)]
             [meta  (pass-meta a-pass)] 
             [TLP   (hash-table-get meta 'TLP)])
        #;(printf "TLP: ~s~n" TLP)
        #;(printf "TLP-TYPES: ~s~n" TLP-TYPES)
        
        (unless (check-types TLP TLP-TYPES)
          (raise-user-error (format "Channels at top level have incorrect types.~n")))
        
        ;;(check-direction TLP TLP-TYPES)
        
        
        (set! check-tlp-already-done? #t)
        ))
    a-pass)
      
        
      
          
         
      
  
  
  ;;PASS 6
  ;; fix-jumps
  ;; jumps and conditional jumps go to setlabs. So, for each setlab, we 
  ;; look for j or cj instructions that refer to it, and set
  ;; the j and cj to the index of the setlab. This way, we can eventually
  ;; work out the offsets directly.
  ;;
  ;; It is likely that more instructions will disappear in the future. 
  ;; We will need to "fix" the jumps again, but the reason we can't 
  ;; reuse this pass is because it must look into the setlab structure
  ;; to find what label value it contains, and look for js and cjs that
  ;; point to that label.
  ;;
  ;; When this pass is finished, js and cjs point to table indices, as 
  ;; opposed to label locations. From now on, whenever we remove code, we only
  ;; need to make the jumps point to the next valid location.
  
  (define fix-jumps
    (lambda (h)
      (let ([meta (pass-meta h)]
            [h (pass-bytes h)])
      (let ([new-h (make-hash-table)])
        ;; fix-j-and-cj just takes the new location and rewrites
        ;; the jump and cj instructions, inserting the new (corrected)
        ;; locations into the table.
        (define fix-j-and-cj
          (lambda (jump-loc new-loc)
            
            (hash-table-for-each 
	     h
	     (lambda (k v)
	       (if (and (ujump? v)
			(equal? (ujump-value v) jump-loc))
		   (hash-table-put! 
		    new-h k 
		    (make-ujump 
		     (Instruction-meta v)
		     new-loc
		     (ujump-fn v)))))
	     )))
        
        (define ->symbol
          (lambda (obj)
            (cond
              [(string? obj) (string->symbol obj)]
              [else obj])))
        
        ;;find-labs finds the labels in the table, and 
        ;; kick-starts the rest of the process. Relies on
        ;; both fix-j-and-cj and next-key
        (define find-labs
          (lambda (h)
            (hash-table-for-each
             h (lambda (k v)
                 (cond
                   [(ulabel? v)
                    (fix-j-and-cj (ulabel-value v) k)
                    (hash-table-put! new-h k v)
                    ]
                   
                   [(not (ujump? v))
                    (hash-table-put! new-h k v)])
                 ))))
        
        (find-labs h)
        (make-pass meta new-h)
        ))))
  
  (define fix-jumps2
    (lambda (h)
      (let ([meta (pass-meta h)]
	    [bytes (pass-bytes h)])
	(let ([jumps (make-hash-table)]
	      [labels (make-hash-table)])
	  
	  ;; Find all the jumps and labels
	  (hash-table-for-each
	   bytes (lambda (k v)
		   (cond
		    [(ujump? v)
		     (debug 'simplification (printf "UJ: ~a~n" (ujump-value v)))
		     (let ([tgt (ujump-value v)])
		       (hash-table-put! 
			jumps 
			k (if (list? tgt)
			      (string->symbol 
			       (list->string tgt))
			      tgt)))]
		    [(ulabel? v)
		     (debug 'simplification (printf "UL: ~a~n" (ulabel-value v)))
		     (let ([lab (ulabel-value v)])
		       (hash-table-put! 
			labels 
			(if (list? lab)
			    (string->symbol 
			     (list->string lab))
			    lab)
			    k))])))
	  
	  (hash-table-for-each
	   jumps
	   (lambda (k v)
	     (let ([there? (hash-table-get labels v (lambda () #f))])
	       (if there? 
		   (hash-table-put! 
		    bytes k 
		    (make-ujump 
		     (let ([m (Instruction-meta (hash-table-get bytes k))])
		       (hash-table-put! m 'operand there?)
		       m)
		     there? 
		     (ujump-fn (hash-table-get bytes k))
		     ))
                   ;; FIXME: we really ought to do this to catch bad jumps
                   ;; BUT at the moment, jentry2jump puts in a hash table location
                   ;; for its jump value, at this stage all other jumps have labels
                   ;; (label reference numbers) in them, and it is impossible to 
                   ;; destringueisn between a label reference which should be resolved
                   ;; and the jetrys jump hash table location which should be left alone
                   ;;(error 'fix-jumps2 (format "Jump (@~a) to unknown label ~a" k v))
                   
                   )
	       )))
	  
	  (make-pass meta bytes)
	  ))))
		  
  (define slinker-exit
    (lambda (pass)
      (exit)))
	



  ;;PASS ...
  ;; fix-jentry
  (define fix-jentry
    (lambda (h)
      (let ([meta (pass-meta h)]
            [h (pass-bytes h)])
      (let ([new-h (make-hash-table)]
            [jmap (make-hash-table)]
            )
        
        ;; Stamp in the location of the procentries
        (hash-table-for-each
         h (lambda (k v)
             (if (procentry? v)
                 (let ([sym (string->symbol
                             (list->string 
                              (procentry-string v)))])
                   (hash-table-put! jmap sym k)))))
        
        ;;Fix the values of the jentries
        (hash-table-for-each
         h (lambda (k v)
             (if (jentry? v)
                 (hash-table-put! 
                  new-h k (make-jentry 
			   (Instruction-meta v)
                           (hash-table-get 
                            jmap (string->symbol
                                  (list->string 
                                   (jentry-string v))))
			   ))
                 (hash-table-put! new-h k v))))
        (make-pass meta new-h)
        ))))
  
  (define strip-jentry
    (lambda (pass)
      (let ([meta (pass-meta pass)]
	    [bytes (pass-bytes pass)])
	(hash-table-for-each
	 bytes (lambda (k v)
		 (if (jentry? v)
		     (hash-table-remove! bytes k))))
	(make-pass meta bytes))))


  ;; jentries are not only used to jump to the 'main' PROC in an occam program.
  ;; It can also be used for PROC 'alias'es like:
  ;;   PROC a()
  ;;     b()
  ;;   :
  ;; where the PROC does nothing but call another proc. Instead of dropping a
  ;; CALL, the compiler drops a JENTRY. If this happens in a library, (when we
  ;; used to strip all jentries (by using the pass: strip-jentry)) these 'alias'
  ;; PROCs would be broken.
  #|
   | THIS DID NOT work, cos each file that is compiled into the library gets a
   | starting hash number where the last file left off. Ie all files do not
   | start hash location 0, they start where the last file left off :(
   | I think the easiest way to deal with this, is to not remove the jentries at
   | all, but to leave them in the files (and resolve them).
   (define strip-first-jentry
    (lambda (pass)
      (let ([meta (pass-meta pass)]
	    [bytes (pass-bytes pass)])
	(let loop ([key 0])
	  (let ([inst (hash-table-get bytes key (lambda () 'nothing))])
	    (debug 'jentry (printf "strip-first-jentry: key: ~a~n" key))
	    (cond
	      ;; If we find a jentry, its the first one, delete it exit
	      [(jentry? inst) (hash-table-remove! bytes key)]
	      ;; If we find a 'nothing, there is no inst at this position,
	      ;; so we add one to the key, and look again
	      [(equal? inst 'nothing) (loop (add1 key))])))
	      ;; Otherwise, if we find anything else, we quit, as then the first
	      ;; thing we found was not a jentry, and we only want to remove
	      ;; jentries if they are the very first instruciton we find
	(make-pass meta bytes))))
   |#
	    
  ;;PASS
  ;; jentry->jump
  ;; Turn the jentry into a jump.
  (define jentry2jump
    (lambda (h)
      (let ([meta (pass-meta h)]
            [bytes (pass-bytes h)])
        (hash-table-for-each
         bytes (lambda (k v)
	     (if (jentry? v)
		 (begin
		   (debug 'simplification
			  (printf "j2j: ~a -> ~a~n" 
				  (jentry-string v)
                                  (hash-table-get globalnames (string->symbol 
							       (list->string 
								(jentry-string v))))))
		   ;;(sleep 2)
		   
		   (hash-table-put! 
		    bytes k 
		    (make-j 
		     (let ([m (Instruction-meta v)])

		       ;; Setting up the metadata
                       ;; 20060430 MCJ
                       ;; Wondering if I can just turn this into a plain-old jump.
		       ;; (hash-table-put! m 'abbreviation 'j2j)
                       (hash-table-put! m 'abbreviation 'j)
		       (hash-table-put! 
                        m 'operand 
                        ;; 060430 Was this...
                        ;;(string->symbol (list->string (jentry-string v)))
                        ;; 060518 V300                       
                        ;; The list->string operation isn't going to work anymore
                        ;; as we are now working with bytes, not chars. Thus, the jentry-string
                        ;; will just be a list of numbers. bytes->symbol is defined
                        ;; in "helpers.ss"
                        (hash-table-get globalnames (bytes->symbol (jentry-string v)))
                        ;;(string->symbol (list->string (jentry-string v)))
                        )
		       m)
		     (bytes->symbol (jentry-string v))
                     )
		    )))))
        (make-pass meta bytes)
      )))
  
  
  
  ;;PASS ...
  ;; Handles j, cj, and call.
  
  (define unify-jumps
    (lambda (h)
      (let ([meta (pass-meta h)]
            [bytes (pass-bytes h)])
	
	(hash-table-for-each
	 bytes 
	 (lambda (k v)
	   (cond
	    [(j? v) (hash-table-put! 
		     bytes k (make-ujump 
			      (Instruction-meta v)
			      (j-value v) *J*
			      ))]

	    [(cj? v) (hash-table-put! 
		      bytes k (make-ujump 
			       (Instruction-meta v)
			       (cj-value v) *CJ*
			       ))]
	    [(call? v) (hash-table-put! 
			bytes k (make-ujump 
				 (Instruction-meta v)
				 (call-value v) *CALL*
				 ))]

	    
	    )))
	(make-pass meta bytes))))
  
  (define unify-labels
    (lambda (h)
      (let ([meta (pass-meta h)]
            [bytes (pass-bytes h)])

	(hash-table-for-each
	 bytes (lambda (k v)
		 (cond 
		  [(setlab? v) (hash-table-put! 
				bytes k 
				(make-ulabel (Instruction-meta v)
						       (setlab-value v)
						       ))]
		  [(sectionlab? v) (hash-table-put! 
				    bytes k 
				    (make-ulabel
				     (Instruction-meta v) 
				     (sectionlab-value v)
				     ))]

		  [(globalname? v) (hash-table-put! 
				    bytes k 
				    (make-ulabel
				     (Instruction-meta v) 
				     (globalname-string v)
				     ))]

		  [(procentry? v) (hash-table-put! 
				    bytes k 
				    (make-ulabel
				     (Instruction-meta v) 
				     (procentry-string v)
				     ))]

		  )))
	
	(make-pass meta bytes))))
  
  ;;PASS
  ;; shiftimm->shifts
  ;; Turn the jentry into a jump.
  (define shiftimm->shifts
    (lambda (h)
      (let ([meta (pass-meta h)]
            [bytes (pass-bytes h)]
	    [new-bytes (make-hash-table)]
	    [*SAVECREG* #xAD]
	    [*RESTORECREG* #xAE])
        
	(define put! 
	  (lambda (loc bin) 
	    (if (hash-table-get new-bytes loc (lambda () #f))
		(error 'shifts "Trying to overwrite location ~a" loc)
		(hash-table-put! new-bytes loc bin))))
        
	(define make-anno
          (case-lambda
            [(msg)
             (let ([h (make-hash-table)])
               (hash-table-put! h 'abbreviation msg)
               h)]
            [(h msg)
             (hash-table-put! h 'abbreviation msg)
             h]
            [(h msg op)
             (hash-table-put! h 'abbreviation msg)
             (hash-table-put! h 'operand op)
             h]
            ))
        
        (define add-type
          (lambda (h sym)
            (hash-table-put! h 'type sym)
            h))
        
        
        (debug 'simplification (printf "Running shiftimm->shifts~n"))
        (hash-table-for-each
         bytes (lambda (k v)
                 (cond
                   [(sllimm? v)
                    (begin
                      (debug 'simplification
                             (printf "LARGEISH UNPLEASANT WARNING! sllimm -> ldc, shl @ ~a~n"
				     k))
                      
                      (hash-table-put! new-bytes k (make-binary
                                                (add-type (make-anno 'savecreg) 'secondary)
                                                (prefix *OP* *SAVECREG*)))
                      (put! (+ k 1) (make-binary 
                                     (add-type (make-anno (make-hash-table) 'ldc (sllimm-value v)) 'primary)
                                     (prefix *LDC* (sllimm-value v))))
                      (put! (+ k 2) (make-binary 
                                     (add-type (make-anno 'shl) 'secondary)
                                     (prefix *OP* *SHL*)))
                      (put! (+ k 3) (make-binary 
                                     (add-type (make-anno 'restorecreg) 'secondary)
                                     (prefix *OP* *RESTORECREG*)))
                      #|
                      (hash-table-put!
                       bytes k
                       (make-binary 
			(Instruction-meta v)
                        (append (prefix *OP* *SAVECREG*)
				(prefix *LDC* (sllimm-value v))
                                (prefix *OP* *SHL*)
				(prefix *OP* *RESTORECREG*))
                        ))
                      |#
                      )]
                   [(slrimm? v)
                    (begin
                      (debug 'simplification
                             (printf "LARGEISH UNPLEASANT WARNING! srlimm -> ldc, shr @ ~a~n"
				     k))

                      (hash-table-put! new-bytes k (make-binary
                                                (add-type (make-anno 'savecreg) 'secondary)
                                                (prefix *OP* *SAVECREG*)))
                      (put! (+ k 1) (make-binary 
                                     (add-type (make-anno (make-hash-table) 'ldc (slrimm-value v)) 'primary)
                                     (prefix *LDC* (slrimm-value v))))
                      (put! (+ k 2) (make-binary 
                                     (add-type (make-anno 'shr) 'secondary)
                                     (prefix *OP* *SHR*)))
                      (put! (+ k 3) (make-binary 
                                     (add-type (make-anno 'restorecreg) 'secondary)
                                     (prefix *OP* *RESTORECREG*)))
                      
                      #|
                      (hash-table-put!
                       bytes k
                       (make-binary
			(Instruction-meta v)
                        (append (prefix *OP* *SAVECREG*)
                                (prefix *LDC* (slrimm-value v))
                                (prefix *OP* *SHR*)
				(prefix *OP* *RESTORECREG*))))
                      |#
                        )]
		    [else
		     (hash-table-put! new-bytes k v)]
                   )))
        (make-pass meta new-bytes))))

  (define check-for-imm
    (lambda (h)
      (let ([meta (pass-meta h)]
            [bytes (pass-bytes h)])
        (hash-table-for-each
         bytes (lambda (k v)
                 (if (or (sllimm? v)
                         (slrimm? v))
                     (error 'check-for-imm "Still am IMM? ~a, ~a~n" k v))))
        
        (hash-table-for-each
         bytes (lambda (k v)
                 (let ([m (Instruction-meta v)])
                   (let ([sym (hash-table-get m 'abbrevation (lambda () 'foo))])
                   (if (or (equal? sym 'sllimm)
                           (equal? sym 'slrimm))
                       (error 'check-for-imm "In the abbrev? ~a, ~a~n" k v))))))
        
        (make-pass meta bytes))))
               
  ;; 
  (define show-symbolic-jump-info
    (lambda (h)
      (let ([meta (pass-meta h)]
            [bytes (pass-bytes h)])
	(debug 
	 'symbolic-jump-info 
	  (hash-table-for-each
	   bytes (lambda (k v)
		   (cond
		    [(ujump? v) 
		     (printf "J :: ~a~n"  (ujump-value v))]
		    [(ulabel? v)
		     (printf "L :: ~a~n"  (ulabel-value v))])))

	  (printf "Globalnames:~n")
	  (hash-table-for-each
	   globalnames
	   (lambda (k v)
	     (printf "~a : ~a~n" k v))))

	
	h)))
  
     ;; Do not use
  (define (sleep-one h) (sleep 1) h)

  
  )
