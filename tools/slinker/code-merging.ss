#|
slinker - code-merging.ss
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
(module code-merging mzscheme
  (require
   "code-output.ss"
   "helpers.ss"
   "constants.ss"
   "types.ss"
   "version.ss"
   (lib "pretty.ss")
   (lib "list.ss")
   (lib "pregexp.ss")
   (prefix srfi: (lib "1.ss" "srfi"))
   (prefix srfi: (lib "13.ss" "srfi"))
   ;;(lib "errortrace.ss" "errortrace")   
   )
  (provide (all-defined))
  
  ;; Add this files revision to the version information
  (put-revision "$Revision: 3119 $")
  
  (define check-globalnames
    (lambda (pass)
      (debug 'merging
             (let ([meta (pass-meta pass)]
                   [bytes (pass-bytes pass)])
               
               (hash-table-for-each
                meta (lambda (k v) 
                       (printf "K: ~a~n" k)))
               
               (hash-table-for-each
                globalnames (lambda (k v)
                              (printf "GK: ~a ~a~n" k v)))))
      pass))
  
  
  (define guarantee-unique-label-names
    (lambda (h)
      (let ([meta (pass-meta h)]
            [bytes (pass-bytes h)]
            [labelmap (make-hash-table)]
            [->key
             (lambda (o)
               (cond
                 ;; V300
                 ;; No longer using lists of chars; now just bytes.
                 [(list? o) (bytes->symbol o)]
                 [else o]))])
        
        ;; Walk the table. Generate a new, unique label for each old label.
        ;; Insert it into a lookup table (existing label value -> new label)
        ;; and then mutate the existing label to contain the new label.
        (hash-table-for-each
         bytes (lambda (k v)
                 (cond 
                   [(ulabel? v)
                    (let ([new-label (gensym (format "label-L~a-~a" (ulabel-value v) (current-seconds)))])
                      (if (not (or (list? (ulabel-value v))
                                   (symbol? (ulabel-value v))
                                   (string? (ulabel-value v))))
			  ;; This will translate all labels which are not lists,
			  ;; symbols or strings already. Ie mostly labels which
			  ;; are numeric I think. Numeric labels are the local
			  ;; (non-exported) labels (mostly at least).
                          (begin
                            (debug 'unique (printf "UL: ~a -> ~a~n" (->key (ulabel-value v)) new-label))
                            (hash-table-put! labelmap (->key (ulabel-value v)) new-label)
                            (set-ulabel-value! v new-label)
                            (hash-table-put! bytes k v))
			  ;; The labels here are generally the exported labels
			  ;; for PROC names etc, these are not uniqueified.
                          (begin
                            (debug 'unique (printf "UL: Ignoring ~a~n" (->key (ulabel-value v))))
                            (hash-table-put! labelmap (->key (ulabel-value v)) (->key (ulabel-value v))))
                          ))]
                   )))
        
        ;; For each jump, replace it's current target symbol with one frmo
        ;; the lookup table (which will be unique for this compilation).
        ;; Should be globally unique as long as the clock is continuously increasing.
        (hash-table-for-each
         bytes (lambda (k v)
                 (cond 
		   ;; ujumps can jump either to a unique label (ie a translated
		   ;; label which we can find in the labelmap, or to a
		   ;; globalname, so we need to look up in both tables. When
		   ;; looking up in the globalnames table however, we are only
		   ;; interested in a good lookup, not in the value we look up.
		   ;; When looking up in the globalnames table, and the label
		   ;; exists, we return the label, rather than the value we
		   ;; looked up.
                   [(ujump? v)
		    ;; Try the lookup in the local labelmap
                    (let ([new-target (hash-table-get labelmap (->key (ujump-value v))
						      ;; If that fails, try the
						      ;; lookup in the globalnames
						      (lambda ()
							(if (hash-table-get globalnames (->key (ujump-value v))
                                                                            (lambda ()
                                                                              (error 'unique-labels 
                                                                                     "Can't get lookup for ujump label (in local labels or global names) ~a~n" 
                                                                                     (->key (ujump-value v)))))
                                                            ;; If the lookup was successfull, return
                                                            ;; the same labelname to the ujump.
                                                            (ujump-value v))
                                                        ))])
                      (debug 'unique (printf "UJ: ~a -> ~a~n" (->key (ujump-value v)) new-target))
                      (set-ujump-value! v new-target)
                      (hash-table-put! bytes k v))]
                   [(load-label? v)
                    (let ([new-target (hash-table-get labelmap (->key (load-label-value v))
                                                      (lambda ()
                                                        (error 'unique-labels "Can't get lookup for load label ~a~n" 
                                                               (->key (load-label-value v)))
                                                        ))])
                      (debug 'unique (printf "ULL: ~a -> ~a~n" (->key (load-label-value v)) new-target))
                      (set-load-label-value! v new-target)
                      (hash-table-put! bytes k v))]
                   [(load-label-difference? v)
                    (let ([new-start (hash-table-get labelmap (->key (load-label-difference-start v))
                                                     (lambda ()
                                                       (error 'unique-labels "Can't get lookup for load label diff (start) ~a~n" 
                                                              (->key (load-label-difference-start v)))
                                                       ))]
                          [new-end (hash-table-get labelmap (->key (load-label-difference-end v))
                                                   (lambda ()
                                                     (error 'unique-labels "Can't get lookup for load label diff (end) ~a~n" 
                                                            (->key (load-label-difference-end v)))
                                                     ))]
                          )
                      (debug 'unique (printf "ULLD: ~a -> ~a~n" (->key (load-label-difference-start v)) new-start))
                      (debug 'unique (printf "ULLD: ~a -> ~a~n" (->key (load-label-difference-end v)) new-end))
                      (set-load-label-difference-start! v new-start)
                      (set-load-label-difference-end! v new-end)
                      (hash-table-put! bytes k v))]
                   )))
        
        (make-pass meta bytes))))
  
  
  
  (define show-code
    (lambda (pass)
      (let ([bytes (pass-bytes pass)])
        (pretty-print
         (quicksort 
          (hash-table-map
           bytes
           (lambda (k v) (list k v)))
          (lambda (a b)
            (> (car a) (car b))))(current-output-port) ))
      pass))
  
  #|
  ;; For DPE?
  (define *live-procs* '())
  (define (add-proc n)
    (set! *live-procs* (cons n *live-procs*)))
  
  (define (add0 n) (+ 0 n))
  (define (find-procs-referenced-in-proc code loc)
    (let ([codels (hash->list code)])
      (let loop ([ndx loc])
        (unless (>= ndx (length codels))
          (let ([ins (list-ref codels ndx)])
            (if #t ;(ulabel? ins)
                (printf "~a~n" ins)))
          (loop (add1 ndx))))))
    |#      
  
  (define dead-library-elimination
    (lambda (pass)
      (let ([meta (pass-meta pass)]
            [bytes (pass-bytes pass)]
            [stubnames '()]
            [new-precomp-libraries '()]
            [added '()])
        
        (define (add-stubname! s)
          (set! stubnames (cons s stubnames)))
        
        (define (stubname->symbol v)
          (cond
            [(list? (stubname-value v))
             (list->symbol (stubname-value v))]
            [(string? (stubname-value v))
             (string->symbol (stubname-value v))]
            [else
             (error 
              'resolve-stubnames 
              "Cannot mangle stubname; is neither a list of characters nor a string: ~a~n" 
              (stubname-value v))]))
        
        ;; This shows me the names of the stubnames in my file.
        ;; It also gathers them up.
        (hash-table-for-each
         bytes
         (lambda (k v)
           (cond
             [(stubname? v) 
              (debug 'filter-libs (printf "S: ~s ~s~n" k (stubname->symbol v)))
              (add-stubname! (stubname->symbol v))
              ])))
        
        ;; Now, an iterative/recursive process.
        ;; I want to know which libraries I need for the
        ;; current list of stubnames. However, when I find
        ;; a library that I need, I'll have to update the list
        ;; of stubnames that we need. Therefore, I go through
        ;; this process until the list of stubnames remains unchanged.
        (let loop ([count 0]
                   [orig-stubs stubnames])
          (for-each (lambda (lib)
                      (let* ([meta (pass-meta lib)]
                             [lib-name (hash-table-get meta 'filename)]
                             [lib-globals (hash-table-get meta 'globalnames)]
                             [lib-precomp-meta (hash-table-get meta 'precomp-meta)]
                             
                             [include? #f])
                        ;; (printf "~n====~n[~a]~n====~n~n" lib-name)
                        
                        (if (not (member lib-name added))
                            (begin
                              ;; Check the current list of stubnames
                              (for-each (lambda (stub)
                                          (if (hash-table-get lib-globals stub (lambda () #f))
                                              (begin
                                                ;; (printf "Found stub [~a] in ~a~n" stub lib-name)
                                                (set! include? #t)))) stubnames)
                              
                              ;; If we have to include the library, add those stubnames to the list
                              (if include?
                                  (let ([lib-stubs (hash-table-get lib-precomp-meta 'required-stubnames)])
                                    ;; (printf "Adding ~a stubnames from library [~s]~n" (length lib-stubs) lib-name)
                                    (set! added (cons lib-name added))
                                    (for-each (lambda (s)
                                                (debug 'filter-libs (printf "Adding: ~s~n" s))
                                                (add-stubname! s))
                                              lib-stubs)
                                    ))
                                    
                              ;; (printf "[~a] orig~n~a~n~nnew-stubs~n~a~n~n" count orig-stubs stubnames )
                              
                              
                              )))) *precompiled-libraries*)
          
          ;; If the list has grown, then we will loop.
          ;; Otherwise, we're done.
          (if (not (equal? orig-stubs stubnames))
              (loop (add1 count) stubnames))
          
          )

        
        ;; This breaks out the step of deciding which libraries we actually need.
        ;; So, I walk through, and make sure the ones we added are in the new
        ;; library list.
        (for-each 
         (lambda (lib)
           (let* ([libmeta (pass-meta lib)]
                  [libbytes (pass-bytes lib)]
                  [lib-precomp-meta (hash-table-get libmeta 'precomp-meta)])
             
             (debug 'filter-libs (printf "META: ~a~n" lib-precomp-meta))
             
             ;; If we're importing the library, we should keep it in the global
             ;; *precompiled-libraries* list.
             (if (member (hash-table-get libmeta 'filename) added)
                 (set! new-precomp-libraries (cons lib new-precomp-libraries)))))
         *precompiled-libraries*)
                
        
        (debug 'filter-libs
               (printf "Old precomp libraries:~n")
               (for-each (lambda (lib)
                           (printf "\t~a~n" (hash-table-get (pass-meta lib) 'filename)))
                         *precompiled-libraries*)
               (printf "~n~n"))
        
        ;; This line breaks things if it is incomplete.
        (set-precompiled-libraries! new-precomp-libraries)
        
        ;; Just checking.
        (debug 'filter-libs
               (printf "New precomp libraries: ~n")
               (for-each (lambda (lib)
                           (printf "\t~a~n" (hash-table-get (pass-meta lib) 'filename)))
                         *precompiled-libraries*)
               (printf "~n~n"))
        
        )
      pass
      ))
  
  (define insert-precompiled-libraries
    (lambda (pass)
      (let ([meta (pass-meta pass)]
            [bytes (pass-bytes pass)]
            )
        
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;; FOREACH LIBRARY
        (for-each 
         (lambda (lib)
           (let ([libmeta (pass-meta lib)]
                 [libbytes (pass-bytes lib)])
             
             ;; SLOWDOWN
             ;; This call, right here, slows us down. It appends line number info
             ;; for each library into the master debugging file. This is slow.
             ;;(dump-debugging-info (hash-table-get libmeta 'filename (lambda () "Unknown library")) lib)
             
             (hash-table-for-each libbytes
                                  (lambda (k v)
                                    ;; (printf "lib-bytes K: ~a~n" k )
                                    
                                    ;;(output-instruction k-v)
                                    
                                    ;; Check; we don't want to overwrite something
                                    (if (not (hash-table-get bytes k (lambda () #f)))
                                        ;; Was k in both of these for location (key)
                                        (begin
                                          ;;(printf "Inserted at ~a~n" (hash-table-count bytes))
                                          (hash-table-put! bytes k v)
                                          ) ;;end begin
                                        (error 'insert-precompiled-libraries
                                               "Tried to insert ~a over ~a (inserting new key ~a into old ~a)~n"
                                               v (hash-table-get bytes k) k k)))
                                  )
             ))
         *precompiled-libraries*)
        
        (make-pass meta bytes))))
  
  
  (define merge-library-dynlibs
    (lambda (pass)
      (for-each
       (lambda (lib)
         (let ([meta (pass-meta lib)])
           (let ([dynlibs (hash-table-get meta 'spragma-dynlibs)])
             (hash-table-for-each 
              dynlibs
              (lambda (k v) 
                ;; We dont care if it is already defined (?) just insert
                ;; everything
		(hash-table-put! 
		 spragma-dynlibs k
		 (append v 
                         (hash-table-get spragma-dynlibs k
                                         (lambda () '()))))))
             )))
       *precompiled-libraries*)
      pass))
  
  
  (define merge-library-globalnames
    (lambda (pass)
      (let ([new (make-hash-table)])
	(define merge 
	  (lambda (h1 h2)
	    (hash-table-for-each
	     h2
	     (lambda (k v)
	       (if (hash-table-get h1 k (lambda () #f))
		   (raise-user-error 'merge-library-globalnames "Library imports locally defined name: ~a" k)
		   (hash-table-put! h1 k v))))
	    h1))
        
        
        (for-each
         (lambda (lib)
           (let ([meta (pass-meta lib)])
             (let ([globals (hash-table-get meta 'globalnames)])
               (hash-table-for-each 
                globals
                (lambda (k v) 
                  ;;Check to make sure we don't have two libraries
                  ;; exporting the same function
                  (if (not (hash-table-get new k (lambda () #f)))
                      (begin
                        (debug 'merging (printf "Merging proc ~a at loc ~a~n" k v))
                        (hash-table-put! new k v))
                      (raise-user-error (format "Two libraries export ~a!~n" k)))
                  ))
	       )))
         *precompiled-libraries*)
        (set-globalnames! (merge new globalnames)))
      pass))
  
  (define annotate-global-procedures
    (lambda (pass)
      (let ([meta (pass-meta pass)]
	    [bytes (pass-bytes pass)])
	(hash-table-for-each
	 globalnames
	 (lambda (gk gv)
	   (let ([b (hash-table-get bytes gv)])
             (cond 
               [(binary? b)
                (hash-table-put! 
                 bytes gv 
                 (make-binary
                  (let ([bm (Instruction-meta b)])
                    ;; WARNING
                    ;; This used to insert a 'comment, but instead
                    ;; I unserted an 'abbreviation. This shouldn't break
                    ;; anything, but it is a "violation" of the annotation metadata field.
                    (hash-table-put! bm 'comment gk)
                    bm)
                  (binary-value b)))]
               [else (error 'annotate-global-procedures "Shouldn't be here!")]
	       ))))
	(make-pass meta bytes))))
  
  
  
  (define renumber-libraries
    (lambda (pass)
      
      (set-precompiled-libraries!
       (map (lambda (lib)
              (let ([libmeta (pass-meta lib)]
                    [libbytes (pass-bytes lib)])
                (hash-table-for-each
                 libmeta (lambda (k v) 
			   (debug 'merging (printf "key: ~a\tval: ~a~n" k v))))
                
                (debug 'merging
		       (printf "Renumbering ~a starting at ~a~n"
			       (hash-table-get libmeta 'filename)
			       (hash-table-get libmeta 'offset) ))
                
                (let* ([offset (hash-table-get libmeta 'offset)]
                       [res (renumber 
                             (make-pass libmeta libbytes) 
                             offset)])
                  res
                  )))
            *precompiled-libraries*))
      pass))
  
  (define insert-bogus-align
    (lambda (pass)
      (let ([meta (pass-meta pass)]
            [bytes (pass-bytes pass)])
        (hash-table-put! bytes (hash-table-count bytes)
                         (make-align (let ([h (make-hash-table)])
                                       (hash-table-put! h 'abbreviation 'the-end)
                                       h)
                                     '()))
        (make-pass meta bytes))))
  
  (define set-precompiled-library-offsets
    (lambda (pass)
      (let ([meta (pass-meta pass)]
            [bytes (pass-bytes pass)])
	(debug 'splo (printf "About to calculate offset.~n"))
        (let ([current-offset (hash-table-count bytes)])
	  (debug 'splo (printf "Byte array: ~a~n" current-offset))
          ;; The metadata for the precomp libraries already contains
          ;; an 'offset field, which is indexed correctly for all
          ;; the libraries; we just need to add the length of the 
          ;; bytecode to all of these offsets.
          (set-precompiled-libraries!
           (mapl
            (lambda (lib)
              (let ([libmeta (pass-meta lib)]
                    [libbytes (pass-bytes lib)])
                
		;; Bump all the globalname pointers for this library,
		;; and up the current-offset
		(let ([globs (hash-table-get libmeta 'globalnames)])
		  (hash-table-for-each
		   globs (lambda (k v)
			   (debug 'splo 
                                  (printf "~a : ~a -> ~a (+ ~a ~a)~n" k v (+ v current-offset) v current-offset))
			   (hash-table-put! globs k (+ v current-offset))))
		  (hash-table-put! libmeta 'globalnames globs)
		  
		  ;; Bump the whole library to it's new home. 
		  ;; Move all instructions up by the current-offset.
		  (let ([newbytes (make-hash-table)])
		    (hash-table-for-each
		     libbytes 
		     (lambda (k v)
		       (hash-table-put! newbytes (+ k current-offset) v)))
		    (set! libbytes newbytes))
		  
		  (set! current-offset 
			(+ current-offset (hash-table-count libbytes)))
		  ) ;;let globs
                
                (make-pass libmeta libbytes)
                ))
            *precompiled-libraries*))
          ))
      pass))
  
  ;; Now, instead of just a list of binaries, we have a list of lists.
  ;; The first list is the list of binary data, and the second
  ;; list is a assoc list of annotations.
  (define make-binaries-in-precompiled-libraries
    (lambda (pass)
      (for-each 
       (lambda (lib)
	 (let ([libmeta (pass-meta lib)]
	       [bytes (pass-bytes lib)])
	   (hash-table-for-each
	    bytes
	    (lambda (k v)
	      (let ([b (car v)]
		    [ann (cadr v)]
		    [h (make-hash-table)])
		;; Make an annotations hash
		(for-each
		 (lambda (pair)
		   (hash-table-put! h (car pair) (cadr pair)))
		 ann)
		;; Now, we're annotating these things; so, if 
		;; This means we have to check what kind of array of 
		;; data this is.
		(cond
                  [(and (list? b)
			(equal? (car b) 'b))
                   (hash-table-put! 
                    bytes k (make-binary h (cdr b)))]
                  [(and (list? b)
			(equal? (car b) 'stub))
                   (hash-table-put! 
                    bytes k (make-stubname h (cdr b)))]
                  [(and (list? b)
			(equal? (car b) 'ffi))
                   (hash-table-put! 
                    bytes k (make-ffi-stubname h (cdr b)))]
                  [else
                   (raise-user-error 'make-binaries-in-precomps "Don't know what kind of structure is in the precompiled library: ~a ~a~n" k b)]
                  )))
	    )))
       *precompiled-libraries*)
      pass))
  
  (define load-precompiled-libraries
    (lambda (h)
      (let ([ls '()])
        ;; Libraries are stored as a Scheme expression to
        ;; be evaled; currently, it returns two values:
        ;; a hash with the library's globalnames, and
        ;; the bytecode hash (in that order).
        (define load-lib
          (lambda (meta fname)
            (let-values ([(globals dynlibs precomp-meta bytes)
                          ;; WARNING 20041209 Might want some checks here... 
                          (eval (read (open-input-file fname)))])
              (let ([metas (make-hash-table)])
                (hash-table-for-each
                 meta (lambda (k v)
			;;(printf "lpl1: ~a ~a~n" k v)
                        (hash-table-put! metas k v)))
                
                (hash-table-put! metas 'globalnames globals)
                (hash-table-put! metas 'spragma-dynlibs dynlibs)
                (hash-table-put! metas 'precomp-meta precomp-meta)

                ;; Add a library version check here?
                
                (make-pass metas bytes)))
            ))
        
        (define add-offsets
          (lambda (ls index)
            (cond
              [(null? ls) '()]
              [else
               (let ([meta (pass-meta (car ls))]
                     [bytes (pass-bytes (car ls))])
                 ;;Add the current index to the metadata for
                 ;; this library
                 (hash-table-put! meta 'offset index)
                 
		 ;; This was a map over the table which I took the
		 ;; length of; count should be faster/better/whatever
                 (let ([pass-length (hash-table-count bytes)])
                   (hash-table-put! meta 'pass-length pass-length)
                   (cons (make-pass meta bytes)
                         (add-offsets (cdr ls) pass-length))))])))
        
        
	;; If we are allowing compact libraries, allow the reading of bytecode
	;; compiled libraries
	;; FIXME: Make some nice excaption type thing happen here so that we
	;; catch if a compiled library is being read in when this is not
	;; allowed. Tell the user in a meaningfull way.
	(if (get-use-compact-libraries) (read-accept-compiled #t))
        
        ;; First load the files into a temporary list
        ;; Before this point, the "bytes" portion of the pass
        ;; contains nothing, and the filename is in the metadata
        (for-each 
         (lambda (pass)
           (let ([meta (pass-meta pass)]
                 [bytes (pass-bytes pass)])
             (let ([file (hash-table-get meta 'filename)])
               (set! ls (cons (load-lib meta file) ls)))))
         *precompiled-libraries*)
        
        ;; Add metadata to each file in the list so it
        ;; knows how far in it is (the offset to the start 
        ;; of the library. Recursive, returns list
        (set! ls (add-offsets ls 0))
        
        ;;Mutate the precompiled libraries list to contain
        ;; the full files, annotated.
        (set-precompiled-libraries! ls)
        ;;(printf "I think I loaded ~a libraries.~n" (length *precompiled-libraries*))
	;;(sleep 5)
        ;; Return the hash; this is a pass-through pass w.r.t.
        ;; the binary data.
        h)))
  
  
  (define find-globalnames
    (lambda (h)
      (let ([meta (pass-meta h)]
            [h (pass-bytes h)])
        (hash-table-for-each
         h (lambda (k v)
             ;; If we find a globalname, insert it's 
             ;; location and value into the globalnames hash
             (if (globalname? v)
                 (let ([name (globalname-string v)])
                   (if (hash-table-get globalnames name (lambda () #f))
                       (raise-user-error (format "~a defined in multiple input files.~n" name))
                       (hash-table-put! 
                        globalnames 
                        (list->symbol name) k)))))))
      ;; Just return the same pass data
      h
      ))
  
  
  
  (define resolve-stubnames-permissive
    (lambda (h)
      (resolve-stubnames h)))
  
  (define resolve-stubnames-picky
    (lambda (h)
      (resolve-stubnames h #t)))
  
  ;; New behavior; used to die if it couldn't find things;
  ;; now, it only does an insert if it can find a binding
  ;; for the globalname. This should all use local globalnames
  ;; tables instead of the global variable...
  ;;
  ;; Also, it can now either be 'picky' (where we die if we don't find a name)
  ;; or permissive (default behavior) where we just ignore names we cant find.
  (define resolve-stubnames
    (case-lambda
      [(h)
       (resolve-stubnames h #f)]
      [(h picky?)
       (let ([meta (pass-meta h)]
             [h (pass-bytes h)])
         
         (hash-table-for-each
          h (lambda (k v)
              (if (stubname? v)
                  (let ([the-stubname
                         (cond
                           [(list? (stubname-value v))
                            (list->symbol (stubname-value v))]
                           [(string? (stubname-value v))
                            (string->symbol (stubname-value v))]
                           [else
                            (error 
                             'resolve-stubnames 
                             "Stubname is neither a list of characters nor a string: ~a~n" 
                             (stubname-value v))])])
                    
                    (let ([new-target
                           (hash-table-get
                            globalnames 
                            the-stubname
                            (lambda () (if picky?
                                           (begin
                                             (debug 'merging
                                                    (printf "I know the following globalnames: ~n")
                                                    (hash-table-for-each
                                                     h (lambda (k v)
                                                         (if (stubname? v)
                                                             (printf "\t~a~n" 
                                                                     (list->symbol (stubname-value v)))))))
                                             (raise-user-error 'resolve-stubnames
                                                               (format 
                                                                "No globalname name found for ~a~n" 
                                                                (list->symbol (stubname-value v)))))
                                           #f)))])
                      
                      
                      ;; If we found the stubname (and we're not being picky), then
                      ;; a number came back and we want to insert a jump
                      ;; to that location. However, we'll use the symbolic,
                      ;; rather than numeric information now. Why is
                      ;; my indentation so screwed up?
                      (if (number? new-target)
                          (hash-table-put!
                           h k (make-fjump 
                                (let ([m (Instruction-meta v)])
                                  (hash-table-put! m 'abbreviation 'j)
                                  (if picky?
                                      (begin
                                        (hash-table-put! m 'operand (hex new-target))
                                        (debug 'merging (printf "Picky: resolving ~a to ~a~n" the-stubname new-target)))
                                      (begin
                                        ;;(debug 'merging (printf "Permissive: I put ~a into location ~a~n" the-stubname k))
                                        (hash-table-put! m 'operand the-stubname)))
                                  m)
                                (if picky? 
                                    new-target
                                    the-stubname) *J* (* 2 *WORDSIZE*)
                                                  ))))))))
         (make-pass meta h))]))
  )
