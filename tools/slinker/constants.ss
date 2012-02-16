#|
slinker - constants.ss
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
(module constants mzscheme
  (require
   "version.ss"
   "types.ss"
   (lib "defmacro.ss")
   (lib "plt-match.ss")
   (lib "pregexp.ss")
   (prefix srfi: (lib "1.ss" "srfi"))
   (prefix srfi: (lib "13.ss" "srfi")))
  (provide (all-defined))
  
  ;; Add this files revision to the version information
  (put-revision "$Revision: 3119 $")

  ;;CONSTANTS
  ;; Primaries
  (define *J* 0)
  (define *LDLP* #x1)
  (define *PFIX* #x2)
  (define *LDNL* #x3)
  (define *LDC* #x4)
  (define *LDNLP* #x5)
  (define *NFIX* #x6)
  (define *LDL* #x7)
  (define *CJ* #xA)
  (define *CALL* #x9)
  (define *EQC* #xC)
  (define *STNL* #xE)
  (define *OP* #xF)
  ;; Secs
  (define *DIFF* #x04)
  (define *SUB* #x0C)
  (define *LDPI* #x1B)
  (define *SHR* #x40)
  (define *SHL* #x41)
  (define *MINT* #x42)
  (define *BOOLINVERT* #x23)
  (define *WIDENSHORT* #x24)
  (define *SEMINIT* #x7A)
  (define *SEMCLAIM* #x7B)
  (define *SEMRELEASE* #x7C)
  
  ;; FFI Instructions
  (define *FFICALL*  #x25)
  (define *FFIBCALL* #x26)
  
  ;; Barrier Instructions
  (define *BARINIT* #xB0)
  (define *BARSYNC* #xB1)
  (define *BARRESIGN* #xB2)
  (define *BARENROLL* #xB3)
 
  ;; Floating point specials-that-become-instructions
  (define *I64TOREAL* #xD0)
  
  ;; If we want dumping of debug information for libraries,
  ;; we need to dump it with the library builds.
  ;;(define *debug-modes* '(library-debug))
  (define *debug-modes* '())
  (define add-debug-mode!
    (lambda (v)
      (set! *debug-modes* (cons (string->symbol v) *debug-modes*))))
  
     (define-macro debug
                   (lambda (level . bodies)
                     `(if (member ,level *debug-modes*)
                          (parameterize ([current-output-port (current-error-port)])
                            (begin ,@bodies)))))

  ;; I have some data structures I want to be global to 
  ;; all of the passes. This seems like an ugly way to 
  ;; do it...
  (define *precompiled-libraries* '())
  (define add-precompiled-library!
    (lambda (bytes)
      (set! *precompiled-libraries* (cons bytes *precompiled-libraries*))))
  (define set-precompiled-libraries!
    (lambda (arg)
      (set! *precompiled-libraries* arg)))

  ;; Get the full path of a precompiled library given the library name.
  (define find-precompiled-library
    (lambda (name)
      (find-in-lib-path (format "~a.precomp" name))))

  ;; Add a precompiled library to the list if we haven't already done so.
  (define use-precompiled-library
    (lambda (input)
      (let ([path (if (path? input) input (string->path input))])
        ;; Check if the library has already been scheduled for
        ;; addition, ie it was supplied as a runtime library twice,
        ;; or it was included as a normal library on the command
        ;; line
        (if (not (srfi:any (lambda (library) 
                             ;; Get something that we can try to
                             ;; compare for equality even when the
                             ;; path to the file has been specified
                             ;; differently. However, I'm not quite
                             ;; sure I understand mzschemes
                             ;; mechanisms here, and perhaps I
                             ;; should just check on filename?
                             (let ([full-path (path->complete-path path)]
                                   [against-path (path->complete-path (hash-table-get (pass-meta library) 'filename))])
                               (equal? full-path against-path)))
                           *precompiled-libraries*))
            (let ([meta (make-hash-table)]
                  [extension (reverse (pregexp-split "\\." (path->string path)))])
              ;; Make a pass structure with correct metadata to
              ;; schedule the library for addition. I think its only
              ;; actually filename thats important here, but where
              ;; other library loading happens this is what is done,
              ;; so we'll do it too here.
              (debug 'libraries (printf "Using precompiled library ~a~n" path))
              (hash-table-put! meta 'filename (path->string path))
              (hash-table-put! meta 'extension (car extension))
              (hash-table-put! meta 'filename-noext (apply string-append (reverse (cdr extension))))
              (add-precompiled-library! (make-pass meta (make-hash-table))))))))

  ;; This should be set some other way.
  (define *precompiled-library-version* 3)

  ;; Not sure if this is the right place (the file is called constants.ss after
  ;; all) but there are a lot of other non-constants here, so I'll just go on...

  (define get-distance-cache (make-vector 0))
  (define set-get-distance-cache!
    (lambda (arg)
      (set! get-distance-cache arg)))

  (define globalnames (make-hash-table))
  (define set-globalnames! 
    (lambda (arg)
      (set! globalnames arg)))

  (define procentries (make-hash-table))
  (define set-procentries! 
    (lambda (arg)
      (set! procentries arg)))

  (define TLP-TYPES "NONE")
  (define (set-tlp-types! str)
    (if (equal? TLP-TYPES "NONE")
        (set! TLP-TYPES str)))
  
  (define shd #f)
  
  (define ffi-type 'dynamic)
  (define (set-ffi-type! sym)
    (if (member sym '(dynamic static))
        (set! ffi-type sym)
        (raise-user-error 'ffi-type "Invalid FFI type; must be either 'static' or 'dynamic'")))
  
  (define static-ffi-table-name "UNDEFINED")
  (define (set-static-ffi-table-file! file)
    (if (file-exists? file)
        (set! static-ffi-table-name file)
        (raise-user-error 'set-static-ffi-table-name
               "File ~a does not exist." file)))
  
  (define ffi-names (make-hash-table 'equal))
  ;; Add a new function to the list of ffi-names
  (define add-ffi-name
    (lambda (arg)
      (debug 'ffi (printf "ffi name is a string?: ~a~n" (string? arg)))
      (debug 'ffi (printf "ffi name is a symbol?: ~a~n" (symbol? arg)))
      (debug 'ffi (printf "ffi name is a symbol?: ~a~n" (bytes? arg)))
      (debug 'ffi (printf "Adding ffi name: ~a" arg))
      (let ([index (hash-table-get ffi-names arg (lambda () #f))]
            [new-index (hash-table-count ffi-names)])
        (if (equal? index #f)
            ;; If there was no ffi name called arg, insert one
            (begin
              (hash-table-put! ffi-names arg new-index)
	      (debug 'ffi (printf ", inserted at ~a~n" new-index))
              new-index)
            ;; Otherwise return the index of the key arg
            ;; 060331 was (index), but MCJ thinks that would crash.
	    (begin
	      (debug 'ffi (printf ", already at ~a~n" new-index))
	      index))
        )))
  ;; If we are doing static FFI, then we don't want to get the
  ;; numbering of the functions w/ add-ffi-name
  (define (get-static-ffi-index name)
    (if (string? name)
        (set! name (string->symbol name)))
    (let ([retrieved (hash-table-get ffi-names name 
                                     ;; If we can't find the FFI name, you're 
                                     ;; toast. Fail. Fail now. 
                                     (lambda () 
                                       (printf "My static FFI table looks like:~n")
                                       (hash-table-for-each
                                        ffi-names
                                        (lambda (k v)
                                          (printf "~s\t~s~n" k v)))
                                       
                                       (raise-user-error 
                                        'static-ffi 
                                        "No static FFI function loaded with the name '~a' found."
                                        name)))])
      ;;(printf "Retrieved: ~s~n" retrieved)
      retrieved))
  
     

  ;; *****************************************
  ;;              SPRAGMA SUPPORT
  ;; *****************************************

  ;; Dynlib spragma stuff
  (define spragma-dynlibs (make-hash-table 'equal))
  ;;FIXME THIS IS GROSS
  (define get-all-dynlibs
    (lambda ()
      (let [(vals '()) (vals2 '()) (vals3 '())]
	(hash-table-for-each spragma-dynlibs 
    ;;This gets all the values out of the hash table.
	  (lambda (key val)
	    (set! vals (cons val vals))))
    ;;This flattens the maximum (we hope) 1 deep list
    (for-each (lambda (val)
      (set! vals2 (append val vals2))) vals)
    ;;This makes all elements in the list unique, avoiding duplicate libraries being loaded
    ;;at runtime. 
    (for-each (lambda (val)
      (if (not (member val vals3)) 
          (set! vals3 (cons val vals3)))) vals2)
	vals3)))



  ;; Adds a spragma to the appropriate spragma-table
  (define add-spragma
    (lambda (filename spragma-list)
      (match spragma-list
        [`(spragma (uselib ,lib* ...))
          (for-each
             (lambda (lib)
               (use-precompiled-library (find-precompiled-library lib)))
             lib*)
        ]
	;; DYNLIB parsing
	[`(spragma (dynlib ,dlib* ...))
    (debug 'ffi (printf "dlib* is ~a~n" dlib*))
	(hash-table-put! 
	   spragma-dynlibs filename 
	   (append dlib* 
	     (hash-table-get spragma-dynlibs filename
		(lambda () '()))))
	])))

    
  ;; *****************************************
  ;;          END OF SPRAGMA SUPPORT
  ;; *****************************************


  (define *PRECOMPILE* #f)
  (define set-precompile-flag!
    (lambda (arg)
      (set! *PRECOMPILE* arg)))
  
  (define *INPUT-FILES* '())
  (define set-input-files!
    (lambda (arg)
      (set! *INPUT-FILES* arg)))
  
  (define *OUTPUT-EXTENSION* "tbc")
  (define *OUTPUT-FILENAME* "out.tbc")
  (define set-output-filename!
    (lambda (arg)
      (set! *OUTPUT-FILENAME* arg)))
  (define get-output-filename
    (lambda ()
      *OUTPUT-FILENAME*))
  (define set-output-extension!
    (lambda (arg)
      (set! *OUTPUT-EXTENSION* arg)))

  (define *LIB-PATH* (list (current-directory)))
  (define add-lib-path!
    (lambda (arg)
      (set! *LIB-PATH* (append *LIB-PATH* (list arg)))))
  (define find-in-lib-path
    (lambda (filename)
      (let loop ([dirs *LIB-PATH*])
        (if (null? dirs)
           (raise-user-error 'libraries "Could not find ~a in library path ~a." filename *LIB-PATH*))
        (let ([path (build-path (car dirs) filename)])
           (if (file-exists? path)
             (begin
               (debug 'libraries "Found ~a in path at ~a" filename path)
               path)
             (loop (cdr dirs)))))))
 
  (define *USE-COMPACT-LIBRARIES* #t)
  (define set-use-compact-libraries!
    (lambda (arg)
      (set! *USE-COMPACT-LIBRARIES* arg)))
  (define get-use-compact-libraries 
    (lambda ()
      *USE-COMPACT-LIBRARIES*))

  ;; WARNING 20040517
  ;; This is word-size dependant
  (define *MININT* (void))
  (define *MAXWORD* (void))
  (define *WORDSIZE* 4)
  (define set-wordsize!
    (lambda (arg)
      (set! *WORDSIZE* arg)))
  
  (define *TCOFF_MAX_TAG* 31)
  
  (define *WS* 4)
  (define set-workspace!
    (lambda (arg)
      (set! *WS* arg)))
  
  (define *VS* 0)
  (define set-vectorspace!
    (lambda (arg)
      (set! *VS* arg)))
  
  (define *MS* 0)
  (define set-mobilespace!
    (lambda (arg)
      (set! *MS* arg)))
  
  #|
     (define *filename* "out.tbc")
     (define set-filename!
       (lambda (arg)
         (set! *filename* arg)))
     |#
  
  (print-hash-table #t)
  
  ;; Options, not technically constants, but things
  ;; like *WORDSIZE* which is not a constant either
  ;; (anymore) are in here as well, so this will 
  ;; do nicely!
  (define *OUTPUT-FORMAT* 'output-bytecode)
  (define set-output-format!
    (lambda (arg)
      (set! *OUTPUT-FORMAT* arg)))
  
  ;;(define *OPTIMISATION* 'none)
  ;;(define set-optimization!
  ;;  (lambda (arg)
  ;;    (set! *OPTIMISATION* arg)))

  (define *OPT-OPTIMAL-PFX* #f)
  (define set-opt-optimal-pfx!
    (lambda (arg)
      (set! *OPT-OPTIMAL-PFX* arg)))
  (define get-opt-optimal-pfx
    (lambda () *OPT-OPTIMAL-PFX*))


  (define *OPT-RM-J0* #f)
  (define set-opt-rm-j0!
    (lambda (arg)
      (set! *OPT-RM-J0* arg)))
  (define get-opt-rm-j0
    (lambda () *OPT-RM-J0*))

  (define *OPT-DEAD-CODE* #f)
  (define set-opt-rm-dead-code!
    (lambda (arg)
      (set! *OPT-DEAD-CODE* arg)))
  (define get-opt-rm-dead-code
    (lambda () *OPT-DEAD-CODE*))

  (define *OPT-DEAD-DATA* #f)
  (define set-opt-rm-dead-data!
    (lambda (arg)
      (set! *OPT-DEAD-DATA* arg)))
  (define get-opt-rm-dead-data
    (lambda () *OPT-DEAD-DATA*))


  (define *BYTESWAP* #f)
  (define (do-byteswap!)(set! *BYTESWAP* #t))
  (define-syntax (with-swap stx)
    (syntax-case stx ()
      [(_ little big)
       #`(if (not *BYTESWAP*)
             little 
             big)
       ]))
  
  (define (swap-bytes lon)
    (cond
      [(= *WORDSIZE* 2)
       (let ([v (list->vector lon)])
         (let loop ([n 0])
           (when (< (add1 n) (length lon))
             (let ([tmp #f])
               (set! tmp (vector-ref v n))
               (vector-set! v n (vector-ref v (add1 n)))
               (vector-set! v (add1 n) tmp))
             (loop (+ n *WORDSIZE*))))
         (vector->list v))]
      [(= *WORDSIZE* 4)
       (let ([v (list->vector lon)])
         (let loop ([n 0])
           (when (< (+ n (sub1 *WORDSIZE*)) (length lon))
             (let ([tmp0 #f]
                   [tmp1 #f]
                   [tmp2 #f])
               (set! tmp0 (vector-ref v n))
               (set! tmp1 (vector-ref v (+ n 1)))
               (set! tmp2 (vector-ref v (+ n 2)))
               (vector-set! v (+ n 0) (vector-ref v (+ n 3)))
               (vector-set! v (+ n 1) tmp2)
               (vector-set! v (+ n 2) tmp1)
               (vector-set! v (+ n 3) tmp0)
               (loop (+ n *WORDSIZE*)))))
         (vector->list v))]))
                     
    
  
  ;; FIXME: I think there is a problem with making this hash-numbering
  ;; very large, as at some point, we go through and check all if all keys
  ;; exist. ie we dont do a hash-table-for-each, but rather a
  ;; for-all-possible-hash-keys. If this is the case, then it is a bad idea
  ;; making this stride too large.
  ;;
  ;; So maybe we need to sort out another way of doing this. Possibly inserting
  ;; an instruction hash, into the instruction hash, when we are adding
  ;; instrucitons, and then later (probably during renumber) merge all hashes
  ;; which are hanging off the primary hash, back into the primary hash.
  ;;
  ;; This has become a problem due to mobiles, which may generate an arbitrayly
  ;; large sequence of code, due to static mobile initialisations.


  ;; We store the instructions in a hash while operating on them in the
  ;; linker. We sometimes insert instructions between existing instructions;
  ;; we do this in a fragile way by inserting new keys, and those keys
  ;; are numbered. To cut down the time it takes to search through
  ;; removed instructions, this numbering should be kept as small as
  ;; possible. It was 10, but 4 is adequate for the time being.
  (define *HASH-NUMBERING* 100)
  (define *MAX-PREFIXING-ITERATIONS* 100)
  (define set-max-prefix! 
    (lambda (arg)
      (set! *MAX-PREFIXING-ITERATIONS* arg)))
  
  
  
  (define set-wordsize-constants
    (case-lambda 
      [()
       (case *WORDSIZE*
         [(4) 
          (set! *MININT* #x80000000)
          (set! *MAXWORD* #xFFFFFFFF)
          ]
         [(2) 
          (set! *MININT* #x8000)
          (set! *MAXWORD* #xFFFF)]
         [(1)
          (set! *MININT* #x80)
          (set! *MAXWORD* #xFF)])]
      [(x . any)
       (set-wordsize-constants)
       x
       ]))


  (define primaries '(j ldlp pfix ldnl ldc ldnlp nfix ldl adc call cj ajw eqc stl stnl opr))
  (define secondaries 
    '((rev lb bsub endp diff add gcall in prod gt wsub out sub startp outbyte outword)
      (seterr mreleasep* resetch csub0 extvrfy* stopp ladd stlb sthf norm ldiv ldpi stlf xdble ldpri rem)
      (ret lend ldtimer boolinvert* widenshort* *fficall* lend3* lendb* reschedule* testerr testpranal tin div *2.D* dist disc)
      (diss lmul not xor bcnt lshr lshl lsum lsub runp xword sb gajw savel saveh wcnt)
      (shr shl mint alt altwt altend and enbt enbc enbs move or csngl ccnt1 talt ldiff)
      (sthb taltwt sum mul sttimer stoperr cword clrhalterr sethalterr testhalterr dup move2dinit move2dall move2dnonzero move2dzero *2.F*)
      ;; 6
      (extin* extout* minn* unpacksn moutn* *6.5* *6.6* *6.7* *6.8* *6.9* *6.10* *6.11* postnormsn roundsn *6.14* *6.15*)
      ;; 7
      (*7.0* ldinf fmul *7.3* *7.4* *7.5* *7.6* *7.7* *7.8* pop* seminit* semclaim* semrelease* *7.13* *7.14* *7.15*)
      ;; 8
      (*8.0* *8.1* *8.2* *8.3* *8.4* *8.5* *8.6* *8.7* *8.8* *8.9* *8.10* *8.11* *8.12* *8.13* *8.14* *8.15*)
      ;; 9
      (*9.0* *9.1* *9.2* *9.3* *9.4* *9.5* *9.6* *9.7* *9.8* *9.9* *9.10* *9.11* *9.12* *9.13* *9.14* *9.15*)
      ;; A
      (fpldzerodb fpint getpri* fpdup fprev setpri* fpldnladddb *A.7* fpldnlmuldb *A.9* fpldnladdsn fpentry fpldnlmulsn *savecreg* *restorecreg* *A.F*)  
      ;; B
      (*B.0* *B.1* *B.2* *B.3* *B.4* *B.5* *B.6* *B.7* *B.8* *B.9* *B.10* *B.11* *B.12* *B.13* *B.14* *B.15*)
      ;; C
      (*C.0* *C.1* *C.2* *C.3* *C.4* *C.5* *C.6* *C.7* *C.8* *C.9* *C.10* *C.11* *C.12* *C.13* *C.14* *C.15*)
      ;;D
      (*D.0* *D.1* *D.2* *D.3* *D.4* *D.5* *D.6* *D.7* *D.8* *D.9* *D.10* *D.11* *D.12* *D.13* *D.14* *D.15*)
      (mnew* mfree* malloc* mrelease* min* mout* min64* mout64* xable* xin* xmin* xmin64* xend* *E.13* *E.14* *E.15*)
      (*F.0* *F.1* *F.2* *F.3* *F.4* *F.5* *F.6* *F.7* *F.8* *F.9* *F.10* *F.11* *F.12* null* *F.14* *F.15*)))
  (define extended-secondaries 
    '((*0.0* *0.1* *0.2* *0.3* *0.4* *0.5* *0.6* *0.7* *0.8* *0.9* *0.10* *0.11* *0.12* *0.13* *0.14* *0.15*)
      (*1.0* *1.1* *1.2* *1.3* *1.4* *1.5* *1.6* *1.7* *1.8* *1.9* *1.10* *1.11* *1.12* *1.13* *1.14* *1.15*)
      (*2.0* *2.1* *2.2* *2.3* *2.4* *2.5* *2.6* *2.7* *2.8* *2.9* *2.10* *2.11* *2.12* *2.13* *2.14* proc-alloc*)
      (proc-param* proc-mt-copy* proc-mt-move* proc-start* proc-end* getaff* setaff* getpas* mt-alloc* mt-release* mt-clone* mt-in* mt-out* mt-xchg* mt-lock* mt-unlock*)
      (mt-enroll* mt-resign* mt-sync* mt-xin* mt-xout* mt-xxchg* mt-dclone* *4.7* *4.8* *4.9* *4.10* *4.11* *4.12* *4.13* *4.14* *4.15*)))

  
  )
