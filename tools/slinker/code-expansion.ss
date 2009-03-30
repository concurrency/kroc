#|
slinker - code-expansion.ss
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
(module code-expansion mzscheme
  (require "helpers.ss"
           "types.ss"
	   "version.ss"
           (prefix srfi: (lib "13.ss" "srfi"))
           (lib "pregexp.ss"))
  (provide (all-defined))

 
  ;; Add this files revision to the version information
  (put-revision "$Revision: 3119 $")

  (define (char->bytes ch)
  (let ([int (char->integer ch)])
    (if (and (< 0 int)
             (> 256 int))
        int
        (error 'char->bytes
          (string-append
            "Attempting to convert '~a' to '~a' "
            "out of range [0,255]") ch int)
    ))) 

  #| PASS 0 : read-tvm-bytes3   |#
  #| ########################## |#  
  (define read-tvm-bytes3
    (lambda (*INPUT-FILES*)
      (let ([code* '()])
        (for-each
         (lambda (file)
           (let* ([file-start-time (current-milliseconds)]
		  [filesize (file-size file)]
		  [ip (open-input-file file 'binary)]
                  [meta (make-hash-table)]
                  [extension (reverse (pregexp-split "\\." (if (path? file) (path->string file) file)))]
                  ;;[code '()]
		  [code (make-vector filesize)]
		  [i 0]
		  )

             ;;Load the metadata for this file
             (hash-table-put! meta 'filename file)
             (hash-table-put! meta 'extension (car extension))
             (hash-table-put! meta 'filename-noext (apply string-append 
                                                          (reverse (cdr extension))))

	     ;;Now, we don't want to pass precompiled libs off as code.
	     ;; If it is a precomp library, then set it aside. Otherwise,
	     ;; it's code.
             (if (equal? (hash-table-get meta 'extension) "precomp")
                 (use-precompiled-library file)
                 ;; v300 CHANGE
                 ;; Read bytes instead of chars. The char type is now
                 ;; unicode-sized.
		 (let ([read-char read-byte])
		   ;; Read the file
		   (debug 'read-tvm-bytes (printf "rtb3: ~s [~s]~n" file extension))
		   (let loop ([char (read-char ip)])
		     (unless (eof-object? char)
		       ;;(printf "~s~n" char)
		       ;;(set! code (cons char code))
		       (vector-set! code i char)
		       (set! i (add1 i))
		       (loop (read-char ip))))
		   ;;(set! code (reverse code))
		   ;; This takes a while to do for the big libraries, like 1/2 secs
		   (set! code (vector->list code))
		   
		   
		   (set! code* (cons (make-pass meta code) code*))
		   (debug 'read-tvm-bytes
		   (printf "~a took ~a ms to load~n" file (- (current-milliseconds) file-start-time))))
             
             )))
         *INPUT-FILES*)
        code*)))


  (define dump-pass*-to-screen
    (lambda (apass)
      (let ([passcode (pass-bytes apass)]
            [count 0])
        (for-each 
         (lambda (b)
           (set! count (modulo (add1 count) 10))
           (printf "~a" b)
           (if (zero? count) (printf "~n")))
         (list-intersperse " " passcode )))
      apass))
  
    
  (define bomb (lambda args (exit)))
  
  #|  PASS 1 : tcoff2bin  |#
  #| #################### |#
  (define tcoff2bin
    (let ([cur-meta 'void])
      ;; Hide the negate param, could probably do case lambda
      (define get-num
	(lambda (data pos)
	  (real-get-num data pos #f)))
      ;; Real get num takes a value deciding if the value output should be
      ;; negated, as  real get num might call itself
      (define real-get-num
        (lambda (data pos neg)
	  (define negate
	    (lambda (number)
	      ;; Are there any repercussions of not using bitwise-mattnot here?
	      ;; dont think so... We are just interested in the number, not that
	      ;; it is clamped within WORDSIZE which is what bitwise-mattnot
	      ;; does...
	      (if neg (bitwise-not number) number)))
          (let ([num (vector-ref data pos)])
            (cond
              [(< num 251)
               (values (negate num) (add1 pos))]
              [(= num 251)
               (set! num (vector-ref data (add1 pos)))
               (set! pos (+ pos 2))
               (values (negate num) pos)]
              [(= num 252)
               (set! num 
                     (bitwise-ior 
                      (<< (vector-ref data (+ pos 2)) 8)
                      (vector-ref data (add1 pos))))
               (set! pos (+ pos 3))
               (values (negate num) pos)]
              ;;n = (*(ptr + 3) << 24) | (*(ptr + 3) << 16) |
              ;;                      (*(ptr + 2) << 8) | *(ptr + 1);
              [(= num 253) 
               (set! num
                     (bitwise-ior
                      (<< (vector-ref data (+ pos 4)) 24)
                      (<< (vector-ref data (+ pos 3)) 16)
                      (<< (vector-ref data (+ pos 2)) 8)
                      (vector-ref data (add1 pos))))
               (set! pos (+ pos 5))
	       (values (negate num) pos)]
	      [(= num 255)
		;;Check for negative numberness
		(real-get-num data (add1 pos) #t)]
              [else 
               ;;We're not dealing with 8-byte numbers yet...
               (error (format "Bad number (~a) in tcoff2bin pass!" num))]))))
      
      (define main
        (lambda (pos data)
          (let ([new (void)]
		[new-counter 0]
		[data (list->vector data)])
            (while (< pos (vector-length data))
                   (let ([tag (vector-ref data pos)]
			 [dbg-start-time (current-milliseconds)]
			 [dbg-start-pos pos])
		     (debug 'tcoff2bin (printf "Section [~a~a]... " (if (< tag 10) "0" "") tag))
		     (debug 'tcoff2bin (flush-output (current-output-port)))
                     (cond
                       [(> tag *TCOFF_MAX_TAG*)
                        (error 
                         (format "Bad TCOFF tag (~a) @ ~a~n" tag pos))]
                       [(= tag 6)
                        (set! pos (add1 pos))
                        (let*-values ([(len pos1) (get-num data pos)]
                                      [(len pos2) (get-num data pos1)])
                          (set! pos pos2)
			  (set! new (make-vector len))
                          (let* ([startpos pos]
				 [endpos (+ startpos len)])
                            (while (< pos endpos)
				   (vector-set! new new-counter (vector-ref data pos))
				   (set! new-counter (add1 new-counter))
                                   (set! pos (add1 pos)))))]
		       ;; Comment sections look like this 
		       ;; (from the document: Transputer Common Object File Format)
		       ;;
		       ;;    comment => cm_header : header (COMMENT_TAG), 
		       ;;            cm_copy : BOOLEAN, 
		       ;;            cm_print : BOOLEAN, 
		       ;;            cm_text : string ; 
		       ;;
		       ;; This is designed for use with other tool's where
		       ;; the linker is required to ignore cm_text. Eg. the 
		       ;; compilers could supply filenames and version numbers
		       ;; in comments in the linker file. 
		       ;; If cm_copy is TRUE, then the comment directive is copied into
		       ;; the linker's output in an implementation 
		       ;; defined manner. If it is FALSE, then it is discarded.
		       ;; This provides a mechanism for compilers to pass 
		       ;; information through to other tools. For instance, the
		       ;; date and time of compilation and the language the 
		       ;; module was written in could be stored in the linker
		       ;; output for each component module. 
		       ;; If cm_print is FALSE then a lister tool knows that the cm_string is not
		       ;; ascii. 
		       ;; 
		       ;; We use comments to be able to pass information from
		       ;; the source code to the linker. This information can
		       ;; not be linked to specific parts of code, only to a
		       ;; file.
		       ;;
		       ;; We use it currently for specifying shared libraries
		       ;; that we need to load at tvm runtime. These will be
		       ;; passed to a .ffi file.
		       ;;
		       ;; For these comments to be picked up as something that
		       ;; the slinker needs to deal with, it needs to be
		       ;; prefixed with a 'spragma'. Ie for dynamic libraries,
		       ;; you might see the following:
		       ;;   #PRAGMA COMMENT "(spragma (dynlib occSDL))"
		       [(= tag 20)
		       	;; Skip over the tag
		        (set! pos (add1 pos))
			;; Get the various values before the string we want to read
                        (let*-values ([(len pos1) (get-num data pos)]
				      [(cm_copy pos2) (get-num data pos1)]
				      [(cm_print pos3) (get-num data pos2)]
                                      [(len pos4) (get-num data pos3)])
 
			 ;; Ensure we have skipped the 'header'
                         (set! pos pos4)
			 (let* ([comment (make-vector len)]
				[comment-counter 0]
				[startpos pos]
			        [endpos (+ startpos len)])
                           
			  ;; Read the characters in the comment
			  (while (< pos endpos)
				 (vector-set! comment comment-counter (vector-ref data pos))
				 (set! comment-counter (add1 comment-counter))
				 (set! pos (add1 pos)))
			  ;; Turn the vector into a string, do a (read) from the
			  ;; string, as a good spragma is made of of s-exps, and
			  ;; will therefore turn into a nice list
			  (let ([spragma 
				 (read (open-input-bytes 
					 (list->bytes (vector->list comment))
                                          ))])
			  ;; Check if we have a list, and it starts with a
			  ;; spragma, if so we (probably) have a good spragma,
			  ;; and we'll add it to our list of spragmas.
			  (if (and (list? spragma) (equal? (car spragma) 'spragma))
			    (add-spragma (hash-table-get cur-meta 'filename-noext) spragma)))
			  (debug 'tcoff2bin
			    (printf "A TCOFF comment~n  cm_copy: ~a; cm_print ~a; len ~a;~n  ~a~n" 
				    cm_copy cm_print len (list->bytes (vector->list comment))
                                                          ))))]
			;; Deal with the descriptor tag which contains headers
			;; for all the PROCs in the file, we are interested in
			;; matching the TLP against the kinda procs we accept.
                       [(= tag 26)
                        ;; Skip over the tag
		        (set! pos (add1 pos))
			;; Get the various values before the string we want to read
                        (let*-values ([(len pos1) (get-num data pos)]
				      [(de_symbol pos2) (get-num data pos1)]
				      [(de_language pos3) (get-num data pos2)]
                                      [(strleng pos4) (get-num data pos3)]
				      ;; Everything after this point seems to be
				      ;; a bit messy and arbitrary, I have not
				      ;; managed to fully understand whats
				      ;; actually encoded in the "string" which
				      ;; follows... Although, the first three
				      ;; things are ws, vs and ms. Then the PROC
				      ;; header, and various other things
				      ;; follow... donno what they all are.
				      ;; (see be/objwrt.c:154)
				      [(ws pos5) (get-num data pos4)]
				      [(vs pos6) (get-num data pos5)]
				      [(ms pos7) (get-num data pos6)]
				      ;; Information about whats actually in the
				      ;; string which follows these three
				      ;; numbers is in:
				      ;; fe/objlib.c:733 (create_descriptor_string)
				      ;; and various companion functions...
				      ;; though this is definitely an exercise
				      ;; in understanding code, as there does
				      ;; not seem to be any definition of what
				      ;; actually goes on.
				      )
        		 ;; Ensure we have skipped the 'header'
                         (set! pos pos7)
			 (let* ([slen (- strleng (- pos7 pos4))] ;; Skip over the ws, vs, ms crap
				[str (make-vector slen)]
				[str-counter 0]
				[startpos pos]
			        [endpos (+ startpos slen)]
				)
                           
			  ;; Read the characters in the comment
			  (while (< pos endpos)
				 (vector-set! str str-counter (vector-ref data pos))
				 (set! str-counter (add1 str-counter))
				 (set! pos (add1 pos)))
                  
                          (debug 'tcoff2bin 
                                 ;;{printf "~a ~a ~a ~a ~a ~a ~a ~a~n" 
                                 ;;        len pos1
                                 ;;        de_symbol pos2
                                 ;;        de_language pos3 
                                 ;;        strleng pos4}}
				 (printf "DESCRIPTOR len: ~a sym: ~a lang: ~a pstrlen: ~a ws: ~a vs: ~a ms: ~a str: ~a~n"
					 (hex len) (hex de_symbol) (hex de_language) (hex strleng) (hex ws) (hex vs) (hex ms) 
					 (if (<= (vector-length str) 0) "" (list->bytes (vector->list  str))
                                                                            )))
                             ;; If the string contains PROC, we'll asume that its
                             ;; the Top Level Process (TLP) descriptor. There
                             ;; might be a better way of detecting this than just
                             ;; looking for PROC, but I think just looking for
                             ;; PROC is ok for now. If it is a TLP descriptor,
                             ;; save it in the metadata so we can check it later.
                             ;; We probably cant check it now, as we dont know
                             ;; what file we are looking at, and thus the TLP
                             ;; descriptor we are looking at now, might not be
                             ;; what will be the final TLP, but just the TLP for a
                             ;; single file.
                             (let ([str (bytes->string/locale (list->bytes (vector->list  str)))])
                               (if (srfi:string-contains str "PROC") 
                                   (hash-table-put! cur-meta 'TLP str)))
                          )

                          (set! pos (+ strleng pos4))
                          )]
 
                       [else
                        (set! pos (add1 pos))
                        (let-values ([(len pos1) (get-num data pos)])
                          (set! pos (+ pos1 len)))])
		     (debug 'tcoff2bin 
			    (printf "(time: ~a ms) (len: ~a bytes)~n" 
				    (- (current-milliseconds) dbg-start-time)
				    (- pos dbg-start-pos)))))
            
	    (vector->list new)
	    )))
	    
      
      (lambda (code)
	(set! cur-meta (pass-meta code))
	;; When reading spragmas it is important that we preserve case
	(read-case-sensitive #t)
        ;; v300 CHANGE
        ;; (main 0 (map char->integer (pass-bytes code))) => (main 0 (pass-bytes code))
        (let ([result (main 0 (pass-bytes code))])
          ;;(printf "RESULT~n~n~a~n~n" result)
          (make-pass (pass-meta code)
                     ;; v300 CHANGE
                     ;;(map integer->char result)
                     result
                     )))
      ))

  
  #|  PASS 2 : decode  |#
  #| ################# |#  
  (define decode
    (let ([hashn 0])  

      (define build-instruction-metadata
	(lambda (fn op)
	  (let ([meta (make-hash-table)]
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
	      (let* ([list-index (quotient op 16)]
		    [abbrv-index (remainder op 16)]
		    [elist-index (- list-index 32)])
		
		(hash-table-put! meta 'type 'secondary)
		(hash-table-put! meta 'operand "")
		
		(cond
		  [(< list-index (length secondaries))
		   (hash-table-put! meta
				     'abbreviation
				     (list-ref (list-ref secondaries list-index)
					       abbrv-index))
		  ]

		  [(and (>= elist-index 0) (< elist-index (length extended-secondaries)))
		   (hash-table-put! meta
				     'abbreviation
				     (list-ref (list-ref extended-secondaries elist-index)
					       abbrv-index))
		  ]

		  [else (error 'tag-instructions "Secondary not in tables [FN ~a, OP ~a]~n"
			   (hex fn) (hex op))])

		(debug 'simplification
		       (printf "~a  ~a~n" 
			       (hash-table-get meta 'abbreviation)
			       (hash-table-get meta 'operand)))
		)]
	     [else (error 'decode "Can't tag instruction: ~a ~a ~n" fn op)])
	    meta
	    )))
      
      (lambda (code)
        (let ([h (make-hash-table)]
              [meta (pass-meta code)]
              [code (list->vector (pass-bytes code))])
          (let loop ([pos 0])
            ;;(printf "%: ~a~n" (/ pos (vector-length code)))
            ;;(print-hash h)
            
            (unless (>= pos (vector-length code))
              
              (set! hashn (+ *HASH-NUMBERING* hashn))
              
              (let-values ([(fn op startpos pos) 
                            (decode-ins code pos)])
                
                ;;(printf "~a\t~a~n" (hex fn) (hex op))
                
                (if (special? fn op) 
                    (cond
                      [(special:0->15? fn op)
                       ;;(printf "SPECIAL 0-15!~n")
                       (let-values ([(num endpos)
                                     (decode-num code pos)])
                         ;;(printf "Copy-Data: ~s~n" (copy-data code pos endpos))
                         (hash-table-put! 
                          h hashn 
                          (make-spec (void) op (copy-data code 
							  pos
							  endpos)))
                         (loop endpos)
                         )]
                      
                      [(special:string? fn op)
                       ;;(printf "SPECIAL STRING!~n")
                       (let-values ([(strleng endpos)
                                     (decode-num code pos)])
                         (hash-table-put!
                          h hashn 
                          (make-spec (void) op (copy-data code endpos
                                                   (+ strleng endpos))))
                         (loop (+ endpos strleng))
                         )]

                      [(special:jump? fn op)
                       ;;(printf "SPECIAL JUMP!~n")
                       (let-values ([(jfn jop jstartpos jendpos)
                                     (decode-ins code pos)])
                         (cond
                           [(= jfn *LDC*) 
                            (let-values ([(ifn iop istartpos iendpos)
                                          (decode-ins code jendpos)])
                              (hash-table-put!
                               h hashn 
                               (make-spec (void) op (copy-data code jstartpos iendpos)))
                              (loop iendpos))]
                           [else
                            (hash-table-put! 
                             h hashn
                             (make-spec (void) op (copy-data code jstartpos jendpos)))
                            (loop jendpos)]))]
                      
                      [(special:mobilespace? fn op)
                       ;;EXACTLY THE SAME AS 0-15
                       (let-values ([(num endpos)
                                     (decode-num code pos)])
                         ;;(printf "Copy-Data: ~s~n" (copy-data code pos endpos))
                         (hash-table-put! 
                          h hashn 
                          (make-spec (void) op (copy-data code pos endpos)))
                         (loop endpos)
                         )]
                      
                      [(or 
			 (special:lend? fn op)
			 (special:lend3? fn op)
			 (special:lendbw? fn op))
                       (let-values ([(jfn jop jstartpos jendpos)
                                     (decode-ins code pos)])
                         (let-values ([(lab-end epos)
                                       (decode-num code jendpos)])
                           (let-values ([(lab-start spos)
                                         (decode-num code epos)])
                             
                             (hash-table-put! 
                              h hashn 
                              (make-spec (void) op (copy-data code pos spos)))
                             (loop spos)
                             )))]
                      
                      [(special:loadwsmap? fn op)
                       (let-values ([(jfn jop jstartpos jendpos)
                                     (decode-ins code pos)])
                         (let-values ([(lab-end epos)
                                       (decode-num code jendpos)])
                             
                             (hash-table-put! 
                              h hashn 
                              (make-spec (void) op (copy-data code pos epos)))
                             (loop epos)
                             ))]
                      
                      [(special:unloadwsmap? fn op)
                       (let-values ([(jfn jop jstartpos jendpos)
                                     (decode-ins code pos)])
                         (let-values ([(lab-end epos)
                                       (decode-num code jendpos)])
                             
                             (hash-table-put! 
                              h hashn 
                              (make-spec (void) op (copy-data code pos epos)))
                             (loop epos)
                             ))]                    

                      [(special:mobileinit? fn op)
		       (let ([pairs '()]
			     [epos pos])
			 (let-values ([(msp-offset msp-offsetep)
				       (decode-num code pos)])
			   (let-values ([(count countep)
					 (decode-num code msp-offsetep)])
			     ;; Update the epos in case there is no
			     ;; initialisation going on
			     (set! epos countep)
			   (printf "mobileinit msp: ~a count: ~a~n"
				   msp-offset count)
			     (let mobileinit-loop ([c count]
					[e countep])
			       (if (> c  0)
				 (let-values ([(slotoff slotoffep)
					       (decode-num code e)])
				   (let-values ([(dataoff dataoffep)
						 (decode-num code slotoffep)])
				     (set! pairs 
				       (append pairs 
					       (list (cons slotoff dataoff))))

				     (printf "mobileinit slot: ~a data: ~a~n"
					     slotoff dataoff)
				     (set! epos dataoffep)
				     (mobileinit-loop (sub1 c) dataoffep)))))
			    (printf "pairs: ~a~n" pairs) 
			    (printf "data:  ~a~n" (map hex (copy-data code pos epos)))
			    ;; No way in hell I am doing this decoding again!
			    ;; So we are going to NOT make a spec, but make
			    ;; the instruction itself
                            ;; (hash-table-put! 
                            ;;  h hashn 
                            ;;  (make-mobileinit (void) msp-offset count pairs))
			    ;; Ok, so that breaks stuff, which sucks, really
			    ;; need to sort that out! Its stupid decoding it
			    ;; twice. I think I might pump the decoded data
			    ;; into the special instruction, rather than the
			    ;; raw data.
                             (hash-table-put! 
                              h hashn 
                              (make-spec (void) op 
					 (list msp-offset count pairs)))
			     (loop epos)
			     )))]

                      [else
                       (debug 'expansion (printf "------------------~n"))
                       (error 'decode (format "Unknown Special: fn: ~a, op: ~a~n"
                                      (hex fn)
                                      (hex op)))])
                    ;;It isn't a special.
                    ;; We've already decoded the instruction,
                    ;; so now we need to stuff it in.
                    (begin
                      (hash-table-put!
                       h hashn
                       (make-inst (build-instruction-metadata fn op) fn op))
                      (loop pos))
                    ) ;;end of if
                )))
          (make-pass meta h)
          ))))

  (define (dump-hash apass)
    (let ([h (pass-bytes apass)]
          [count 0])
      (for-each
       (lambda (i)
         (set! count (modulo (add1 count) 3))
         (printf "~a" i)
         (if (zero? count) (printf "~n"))
         )
       (hash->list h))
      apass))
  
  #|  PASS 3 : specialize-trivially  |#
  #| ############################### |#    
  (define specialize-trivially
    (lambda (h)
      (let ([meta (pass-meta h)]
            [h (pass-bytes h)]
	    [current-line 1])
	(define add-abbreviation
	  (lambda (s)
	    (let ([h (make-hash-table)])
	      (hash-table-put! h 'abbreviation s)
	      (hash-table-put! h 'line current-line)
	      h)))
	(define add-abbreviations
	  (lambda (p*)
	    (let ([h (make-hash-table)])
	      (for-each
	       (lambda (p)
		 (hash-table-put! h (car p) (cadr p)))
	       p*)
	      (hash-table-put! h 'line current-line)
	      h)))
	(define add-line
	  (lambda (v)
	    (let ([h (Instruction-meta v)])
	      (hash-table-put! h 'line current-line)
	      (set-Instruction-meta! v h)
	      v)))
        (define just-num
          (lambda (data)
            (let-values ([(num pos) (decode-num data 0)]) 
              num)))
        (define sv-helper
          (lambda (special)
            (let ([op (bitwise-and (spec-op special) 255)]
                  [data (spec-data special)])
              (case op
                ;; SIMPLE INSTRUCTIONS
                ;; We only want to specialize simple instructions here.
                ;; We'll pass complex instructions (that require additional
                ;; decoding) through.
                [(#x01) (make-tsdepth (add-abbreviation 'tsdepth) (just-num data))]
                [(#x02) (make-funcresults (add-abbreviation 'funcresults) (just-num data))]
                [(#x03) (make-funcreturn (add-abbreviation 'funcreturn) (just-num data))]
                [(#x04) (make-endws (add-abbreviation 'endws) (just-num data))]
                [(#x05) (make-realresult (add-abbreviation 'realresult) (just-num data))]
                [(#x06) (make-setlab (add-abbreviations
				      `((abbreviation setlab)
					(operand ,(just-num data))))
				      (just-num data))]
                [(#x07) (make-sectionlab 
			 (add-abbreviations `((abbreviation sectionlab)
					      (operand ,(just-num data))))
			 (just-num data))]
                [(#x08) (make-align (add-abbreviation 'align) (just-num data))]
                [(#x09) 
		 (set! current-line (just-num data))
		 ;;(printf "current-line: ~a~n" current-line)
		 (make-linenum (add-abbreviations 
				`((abbreviation linenum) 
				  (operand ,(just-num data))
				  (line ,(just-num data))))
			       (just-num data))]
                [(#x0A) (make-debugline (add-abbreviation 'debugline) (just-num data))]
                [(#x0B) (make-setws (add-abbreviation 'setws) (just-num data))]
                [(#x0C) (make-setvs (add-abbreviation 'setvs) (just-num data))]
                [(#x0D) (make-sllimm (add-abbreviation 'sllimm) (just-num data))]
                [(#x0E) (make-slrimm (add-abbreviation 'slrimm) (just-num data))]
                [(#x0F) (make-loopheadtop (add-abbreviation 'loopheadtop) (just-num data))]
                [(#x11) 
                 ;; 060518 V300
                 ;; Looks like I need real strings for doing pregexp operations.
		 (let ([str (bytes->string/locale (list->bytes data))])
		   (cond
		    [(pregexp-match "^C\\..*" str)
		     (make-ffi-stubname (add-abbreviations
					 `((abbreviation stubname)
					   (operand ,data)))
					data)]
		    [(pregexp-match "^B\\..*" str)
		     (printf "WARNING: Converting B. to C. in FFI import of ~a.~n" str)
		     (make-ffi-stubname (add-abbreviations
					 `((abbreviation stubname)
					   (operand ,(cons (char->bytes #\C) (cdr data)))))
					(cons (char->bytes #\C) (cdr data)))]
		    [(pregexp-match "^BX\\..*" str)
		     (printf "WARNING: Converting BX. to C. in FFI import of ~a.~n" str)
		     (make-ffi-stubname (add-abbreviations
					 `((abbreviation stubname)
					   (operand ,(cons (char->bytes #\C) (cdr data)))))
					(cons (char->bytes #\C) (cdr data)))]
		    [else
		     (make-stubname (add-abbreviations
				     `((abbreviation stubname)
				       (operand ,data)))
				    data)]))
		 ]
                [(#x12) (make-globalname (add-abbreviations
					  `((abbreviation globalname)
					    (operand ,data))) 
					 data)]
                [(#x13) 
                 (debug 'expansion (printf "jentry data: ~a~n" data))
                 (make-jentry (add-abbreviations
				      `((abbreviation jentry)
					(operand ,data)))
				     data)]
                [(#x14) (make-procentry (add-abbreviations
					 `((abbreviation procentry)
					   (operand ,data)))
					data)]
                [(#x15) (make-source-filename (add-abbreviation 'source-filename) data)]
                [(#x16) (make-compiler-comment (add-abbreviation 'compiler-comment) data)]
                ;;This is used in the occam-pi.occ file for occam8 and some barrier stuff.
                [(#x17) 
                 ;; Resolves #164
                 (debug 'expansion (printf "ETCS7 CODEMAP~n"))
                 (make-codemap (add-abbreviation 'codemap) data)] 
                
                ;; 20070227 MCJ BYTESWAP
                [(#x18)
                 (let ([data (with-swap
                              (let ()
                                ;; 070531 MCJ
                                ;; Left the statements here, but we don't really
                                ;; want these, actually. Too noisy.
                                ;; (debug 'byteswap (printf "LE: ~a~n" data))
                                data)
                              (let ([be-data (swap-bytes data)])
                                ;; As above.
                                ;;(debug 'byteswap (printf "BE: ~a~n" be-data))
                                be-data))])
                   (make-data-bytes (add-abbreviations 
                                     `((abbreviation data-bytes)
                                       `(operand ,data)))
                                    data))]
                [(#x19) (make-message-bytes (add-abbreviation 'message-bytes) data)]
                [(#x1C)
                 ;; Just ignore this for now.
                 (debug 'expansion (printf "ETCS12 GLOBNAMEEND~n"))
                 #f]
                [(#x24) (make-mobilespace-usage (add-abbreviation 'mobilespace-usage) (just-num data))]
                ;; COMPLEX INSTRUCTIONS
                ;; These need to be decoded in the next pass.
                [(#x00 #x20 #x21 #x22 #x23 #x25 #x27 #x28) special]
                ;; ERROR!
                ;; If we get here, we didn't know what to do with the 
                ;; instruction. We should look at what we've got, and 
                ;; make a decision: is it SIMPLE, or COMPLEX?
                [else 
                 (error (format 
                         (string-append
                          "specialize-trivially:~n"
                          "Unknown special~n"
                          "Op:   ~a~n"
                          "Data: ~a~n")
                         (hex (spec-op special))
                         (spec-data special)))]))))
        
	;; clj3 20060112
	;; It looks like this code propagates line numbers. (Amonts other
	;; things) It will however not work!
	;; This code expects hash-table-for-each to return keys in order, this
	;; is not the case, so this breaks.
        (let ([new (make-hash-table)])
          (hash-table-for-each h
	   (lambda (k v)
	       (if (inst? v)
		   ;;(hash-table-put! new k (add-line v))
		   (hash-table-put! new k v)
		   (let ([decoded (sv-helper v)])
		     (if (not (equal? decoded #f))
		       (hash-table-put! new k decoded)))
		   ))
	   )
          (make-pass meta new)
          ))))
  
  
  

  #|  PASS 4 : specialize-nontrivially  |#
  #| ################################## |#    
  (define specialize-nontrivially
    (lambda (h)
      (let ([meta (pass-meta h)]
            [h (pass-bytes h)]
	    [current-line 1])

	(define add-abbreviation
	  (lambda (s)
	    (let ([h (make-hash-table)])
	      (hash-table-put! h 'abbreviation s)
	      (hash-table-put! h 'line current-line)
	      h)))
	(define add-abbreviations
	  (lambda (p*)
	    (let ([h (make-hash-table)])
	      (for-each
	       (lambda (p)
		 (hash-table-put! h (car p) (cadr p)))
	       p*)
	      (hash-table-put! h 'line current-line)
	      h)))        

        (define just-num
          (lambda (data)
            (let-values ([(num pos) (decode-num data 0)]) 
              num)))
	(define add-line
	  (lambda (v)
	    (let ([h (Instruction-meta v)])
	      (hash-table-put! h 'line current-line)
	      (set-Instruction-meta! v h)
	      v)))        
        
        (define sv-helper
          (lambda (i)
            (cond
              ;; If it is an instruction we haven't specialized yet,
              ;; we should handle it here.
              [(spec? i) 
               (let ([op (bitwise-and (spec-op i) 255)]
                     [data (spec-data i)]
                     [err (lambda(meta) (error (format "Unimplemented etc0 op:~a data:~a meta:~a~n" (spec-op i) (spec-data i) meta)))])
                 (case op
		    [(#x00) 
                    (let ([num (just-num data)]
                          [makers 
                           (list make-boolinvert
                                 make-starttable
                                 make-widenshort
                                 make-loopheadbot 
                                 make-contrsplit
                                 make-contrjoin
                                 make-i64toreal
                                 make-notprocess
                                 make-fppop
                                 make-checknotnull
                                 make-semclaim
                                 make-semrelease
                                 make-seminit
                                 make-etc0_reschedule
                                 err
                                 err
                                 err
                                 err
                                 err
                                 err
                                 err
                                 err
                                 make-barinit
                                 make-barsync
                                 make-barresign
                                 make-barenroll)]
                                 
			  [anno 
			   '( boolinvert
			      starttable
			      widenshort
			      loopheadbot 
			      contrsplit
			      contrjoin
			      i64toreal
			      notprocess
			      fppop
			      checknotnull
			      semclaim
			      semrelease
			      seminit
			      etc0_reschedule
            indirect_areg
            indirect_breg
            indirect_creg
            rmwsmap
            mppclone
            mppserialise
            mppdeserialise
            loadcodemap
            barinit
            barsync
            barresign
            barenroll)]
			  )
                      ;;WARNING 20040517 
                      ;; We got a fppop where we expected a 
                      ;; notprocess; lists are indexed from zero,
                      ;; and possibly (although this is guesswork)
                      ;; we need to decrement 'num' to get the 
                      ;; right maker out of the above list.
                      (apply (list-ref makers (sub1 num))
			     (list (add-abbreviation (list-ref anno (sub1 num))))))]
                   
                   [(#x20)
                    (let-values ([(fn op startpos pos) 
                                  (decode-ins data 0)])
                      (cond 
                        [(= fn *LDC*) 
                         (let-values ([(ifn iop istartpos ipos)
                                       (decode-ins data pos)])
                           ;;WARNING 20040517
                           ;; We have not stress-tested any of the
                           ;; linker; therefore, it is possible this
                           ;; test is failing, and everything is 
                           ;; being encoded as a load-lab-diff. 
                           ;; To be discovered...
                           ;;(printf "IFN: ~a~n" (hex iop))
                           ;;WARNING 20040517 
                           ;; This could have to do with word-size.
                           ;; We might get a different number of F's
                           ;; if we're in (say) 16-bit land.
                           ;; Great.
                           (if (= iop *MAXWORD*)
                               (make-load-label 
				(add-abbreviations
				 `((abbreviation load-label)
				   (operand ,op)))
				op)
                               (make-load-label-difference  
				(add-abbreviations 
				 `((abbreviation load-label-difference)
				   (operand ,(list op iop))))
				op iop)))]
                        [(= fn *J*)    (make-j  (add-abbreviations 
						 `((abbreviation j)
						   (operand ,op)
						   (type special)
						   )) op)]
                        [(= fn *CALL*) (make-call (add-abbreviations
						   `((abbreviation call)
						     (operand ,op)
						     (type special)
						     )) op)]
                        [(= fn *CJ*)   (make-cj (add-abbreviations 
						 `((abbreviation cj)
						   (operand ,op)
						   (type special)
						   )) op)]
                        [else (error (format "#x20~nfn: ~a op: ~a~n" fn op))]
                        ))]
                  
		    ;; lend, lend3 lendbw
                   [(#x21 #x22 #x23)
                    (let-values ([(ifn iop istartpos ipos)
                                  (decode-ins data 0)])
                      (let*-values ([(n1 p1) (decode-num data ipos)]
                                    [(n2 p2) (decode-num data p1)])
			(let ([make-loopthing 
			       (case op 
				 [(#x21) make-loopend]
				 [(#x22) make-loopend3]
				 [(#x23) make-loopend-backwards])])
                        (make-loopthing (add-abbreviations
				       `((abbreviation loopend)
					 (operand (,n1 ,n2))))
				      iop n1 n2))))]
		   [(#x25)
		    (let ([msp (car data)]
			  [count (cadr data)]
			  [pairs (caddr data)])
		      (make-mobileinit (add-abbreviations
					 `((abbreviation mobileinit)
					   (operand (,msp ,count ,pairs))))
				       msp count pairs))]
                   [(#x27)
                    (let-values ([(ifn iop istartpos ipos)
                                  (decode-ins data 0)])
                      (let*-values ([(n1 p1) (decode-num data ipos)])
                        (make-loadwsmap (add-abbreviation 'loadwsmap) iop n1)))]          
                   [(#x28)
                    (let-values ([(ifn iop istartpos ipos)
                                  (decode-ins data 0)])
                      (let*-values ([(n1 p1) (decode-num data ipos)])
                        (make-unloadwsmap (add-abbreviation 'unloadwsmap) iop n1)))]
                   [else (error 
                          (format 
                           (string-append
                            "specialize-nontrivially:~n"
                            "No op ~a with data ~a~n")
                           (hex op) data))]
                   ))]

              
              ;; Otherwise, it's been handled, and should be returned untouched.
              [else i])))
       
	;; clj3 20060112
	;; It looks like this code propagates line numbers. (Amonts other
	;; things) It will however not work!
	;; This code expects hash-table-for-each to return keys in order, this
	;; is not the case, so this breaks.
        (let ([new (make-hash-table)])
          (hash-table-for-each h
	   (lambda (k v)
	       (cond
		;;[(linenum? v)
                ;; (debug 'lines (printf "Found line #~a~n" (linenum-value v)))
		;; (set! current-line (linenum-value v))]
		[(inst? v)
		 (hash-table-put! new k v)]
		[else
		 ;;(hash-table-put! new k (add-line (sv-helper v)))])
		 (hash-table-put! new k (sv-helper v))])
	       ))
          (make-pass meta new)
          ))))
  
  
  (define (load-static-ffi-table pass)
    (cond
      [(equal? ffi-type 'static)
       ;; Already checked to see it exists
       (let ([ip (open-input-file static-ffi-table-name)])
         (let loop ([line (read-line ip)])
           (unless (eof-object? line)
             (let ([sym (string->symbol line)])
               (add-ffi-name sym))
             (loop (read-line ip))))
         (close-input-port ip))
       ;; RETURN THE PASS!
       pass
       ]
      [else pass]))
  
  


  ;; JUNK
#|  
  (define read-tvm-bytes2
    (lambda (file)
      (let ([code '()])
        (debug 'expansion (printf "Operating on ~s~n" file))
        (let ([ip (open-input-file file 'binary)])
          (let loop ([char (read-char ip)])
            (unless (eof-object? char)
              ;;(printf "~s~n" char)
              (set! code (cons char code))
              (loop (read-char ip))))
          (set! code (reverse code)) )
        code)))
  
  

  (define read-tvm-bytes
    (lambda (*INPUT-FILES*)
      (let ([code* '()])
        (for-each
         (lambda (file)
           ;;(printf "Operating on ~s~n" file)
           (let ([ip (open-input-file file 'binary)]
                 [code '()])
             (let loop ([char (read-char ip)])
               (unless (eof-object? char)
                 ;;(printf "~s~n" char)
                 (set! code (cons char code))
                 (loop (read-char ip))))
             (set! code (reverse code))
             (set! code* (cons code code*))))
         *INPUT-FILES*)
        code*)))
|#
    
    )
  
