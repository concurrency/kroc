#|
skroc - skroc.ss
An occam compiler convenience wrapper for occ21 and the slinker
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
(module skroc mzscheme
  (require (lib "process.ss")
	   (lib "cmdline.ss")
	   (lib "pregexp.ss")
           (prefix list: (lib "list.ss"))
	   (prefix srfi13: (lib "13.ss" "srfi"))
           "defaults.scm"
           )
  
  ;; See notes in ticket #160 for why this does not work
  ;; exactly as expected. This probably wont change until
  ;; changes happen in mzc.
  (error-display-handler
   (lambda (msg e)
     (if (exn:break? e)
         ;; If its a break, display the message, but *don't* display a stacktrace
         (begin (display msg (current-error-port)) (newline (current-error-port)))
         ;; Otherwise just let the normal error display handler do the work
         ((error-display-handler) msg e))))
  
  
  (define (die format . args)
    (begin
      (apply fprintf (cons (current-error-port) (cons (string-append "skroc: " format) args)))
      (exit 1)))

  (define (warn format . args)
    (apply fprintf (cons (current-error-port) (cons (string-append "skroc: warning: " format) args))))
  
  ;; This is 1337 christian scheme!
  (define find-executable
    (lambda (name)
      (let* ([skrocpath (if (getenv "SKROCPATH") (getenv "SKROCPATH") "./")]
             [inskrocpath (ormap ;; map, but returns first non #f value, or #f
                            (lambda (path)
                              (find-executable-path 
                                (build-path path name) #f))
                            (path-list-string->path-list skrocpath 
                                                         `(,(string->path ".")))
                            )])
        (if inskrocpath inskrocpath
            (let ([inpath (find-executable-path name #f)])
              (if inpath inpath
		  (if inpath inpath
                      (die (string-append "Could not find ~a executable in:~n"
                                          "PATH: ~a~n"
                                          "SKROCPATH: ~a~n"
                                          "please add the path containing ~a to the PATH "
                                          "environment variable, or set the~nSKROCPATH "
                                          "environment varaiable to the path containing ~a "
                                          "and any other required~n binaries.~n")
                           name
                           (if (getenv "PATH") (getenv "PATH") "<empty>")
                           (if (getenv "SKROCPATH") (getenv "SKROCPATH") "<empty>")
                           name name))))))))
  
  (define parameters (make-hash-table))
  (define set-parameter!
    (lambda (k v)
      (hash-table-put! parameters k v)))
  (define get-parameter
    (case-lambda
      [(k)
       (hash-table-get parameters k
                       (lambda () 
                         (error 'skroc "Undefined parameter: ~a~n" k)))]
      [(k thunk)
       (hash-table-get parameters k thunk)]))
  (define append-parameter
    (lambda (k v)
      (set-parameter! k (append (get-parameter k (lambda () '()))
                                (list v)))))
  (define get-environment
    (case-lambda
      [(k) (getenv k)]
      [(k otherwise)
       (let ([v (getenv k)])
         (if v v (otherwise)))]))
        

  (define isverbose? 
    (lambda ()
      (get-parameter 'verbose (lambda () #f))))
  
  (define (flatten ls)
    (cond
      [(null? ls) '()]
      [(list? (car ls))
       (append (flatten (car ls)) (flatten (cdr ls)))]
      [else (cons (car ls) (flatten (cdr ls)))]))
  
  (define printcmd
    (lambda (cmd args)
      (if (isverbose?)
          (begin
            (printf "~a " cmd)
            (for-each 
             (lambda (x)
               (printf "\"~a\" " x))
             args)
            (printf "~n")))))
  
  (define exec
    (lambda (cmd args)
      (apply system*/exit-code (cons cmd args))))
  
  (define intelli-split-str
    (lambda (arg)
      (let ([end (string-length arg)]
	    [sregexp (pregexp "(?:\".*?\"|'.*?'|[^[:space:]\"']+)")])
	(let loop ([start 0] [larg '()])
	  (let* ([match  (begin
                           (pregexp-match-positions sregexp arg start end))]
		 [mstart (if (list? match) (caar match) #f)]
		 [mend   (if (list? match) (cdar match) #f)]
		 [substr (if (list? match)
			     (substring arg mstart mend) 
			     (substring arg start end))])
	    (if (and mend (< start end)) (loop mend (append larg (list substr))) larg))))))
  
  (define intelli-split
    (lambda (args)
      (list:filter 
       (lambda (x) (> (string-length x) 0))
       (map 
	(lambda (x) (srfi13:string-trim (pregexp-replace* "[\"']" x "")))
	(flatten (map
		  (lambda (x) (intelli-split-str x))
		  args))))))
  
  ;; Make sure the list of arguments is flat, does not contain any '()'s and
  ;; otherwise has only strings in it...
  (define argify
    (lambda (args)
      (map (lambda (x) (format "~a" x))
	   (list:filter (lambda (x) (not (null? x)))
                        (flatten args)))))
  
  
  (define add-exe
    (lambda (name)
      (case (system-type)
        [(windows) (format "~a.exe" name)]
        [else name])))
  
  ;; Find a tool that we want to run, either looking along the path
  ;; or in an in-tree directory.
  (define find-tool
    (lambda (name in-tree-dir)
      (let ([in-tree (get-parameter 'in-tree (lambda () #f))]
	    [real-name (add-exe name)])
	   (if in-tree (format "~a/~a/~a" in-tree in-tree-dir real-name)
		       (find-executable real-name)))))

  (define skroc-version "0.6.2")
  (define skroc-revision (pregexp-replace* " ?\\$" "$Revision$" ""))
  (define skroc-c-year "2006")

  (define occ21 (lambda () (find-tool "occ21" "tools/occ21")))
  (define slinker (lambda () (find-tool "slinker" "tools/slinker")))
  (define fixme-library-slinker (lambda () (find-tool "library2" "tools/slinker")))
  (define ilibr (lambda () (find-tool "ilibr" "tools/ilibr")))
  (define tranx86 (lambda () (find-tool "tranx86" "tools/tranx86")))
  
  (define isearch-separator 
    (case (system-type)
      [(windows) ";"]
      [else ":"]))
  
  (define show-banner
    (lambda ()
      (printf "skroc version ~a (~a)~n" skroc-version skroc-revision)))
  
  (define show-version
    (lambda ()
      (begin
        (show-banner)
        (printf "Copyright ~a M. C. Jadud, C. L. Jacobsen - " skroc-c-year)
        (printf "www.transterpreter.org~n"))))
  
  (define show-ilibr-version
    (lambda ()
      (printf "Path to ilibr: ~a~n" (ilibr))
      (system* (ilibr))))
  
  (define show-occ21-version
    (lambda ()
      (printf "Path to occ21: ~a~n" (occ21))
      (system* (occ21))))
  
  (define show-tranx86-version
    (lambda ()
      (printf "Path to tranx86: ~a~n" (tranx86))
      (system* (tranx86) "--version")))
  
  (define show-slinker-version
    (lambda ()
      (printf "Path to slinker: ~a~n" (slinker))
      (system* (slinker) "--version" )))
  
  (define show-library2-version
    (lambda ()
      (printf "Path to library2 ~a~n" (fixme-library-slinker))
      (system* (fixme-library-slinker) "--version")))
  
  (define parse-cmd
    (lambda ()
      ;;(printf "CMD: ~a~n" (current-command-line-arguments))
      (command-line
       "skroc"
       (current-command-line-arguments)
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;; Command line flags we can see many times
       (multi
	[("-l" "--lib")
	 library
	 "Include a pre-compiled library."
	 (append-parameter 'slinker-opts (format "-l ~a" library))]

	[("-L" "--library-path")
	 library-path
	 "Adds library-path to the list of library paths"
	 (append-parameter 'library-paths library-path)]

        [("--slinker-opts")
	 opts
	 "Sets options which are passed directly to the slinker (or library2)"
         (append-parameter 'slinker-opts opts)]

        [("--occ21-opts")
	 opts
	 "Sets options which are passed directly to occ21"
	 (append-parameter 'occ21-opts opts)]

	[("-D" "--define")
	 def
	 "Adds a preprocessor define <def[=val]>"
         (append-parameter 'defines def)]
	)
       
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;;Command line flags we only want to see once
       (once-each
        [("--in-tree")
	 build-root
	 "\n\tUse tools from the KRoC source tree (internal use only)"
	 (set-parameter! 'in-tree build-root)]
        [("--with-tvm_config_h")
         filename
         "\n\tuse a particular tvm_config.h file (or set env var SKROC_TVM_CONFIG_H)"
         (set-parameter! 'tvm_config.h filename)]
	;;Going to phase this out, --target is more better/more flexible
	;;djd20 31/5/06
	[("-w" "--word-size")
	 wordsize
	 "\n\tThe wordsize of our target architecture. Depricated - use --target instead"
	 (raise-user-error 'skroc "This is deprecated, do not use")]
	[("-f" "--filename")
	 filename
	 "\n\tSets the output filename for the linker."
	 (set-parameter! 'output-filename filename)]
	["--brief"
	 "Output brief error messages"
	 (set-parameter! 'brief #t)]
	["--lego"
	 "Output a .lx for the LEGO Mindstorms (very deprecated)"
	 (set-parameter! 'target-processor 't2)
	 (set-parameter! 'output 'lx)
         ;; WARNING MCJ 20060331
         ;; We have no idea where this will pick this
         ;; up from. Hopefully, the current working directory?
         ;; THIS MUST BE LOOKED UP MORE INTELLIGENTLY.
         ;; That is, it should be in the libs directory, or similar.
         ;; We'll fix this spoon enough.
         (append-parameter 'slinker-opts "--static-ffi lego.staticffi")
         ]
        
        ["--blackfin"
	 "Output a TBC for the Surveyor SRV-1 running the Analog Devices BF537 processor."
	 (set-parameter! 'target-processor 't4)
	 (set-parameter! 'output 'blackfin)
	 (append-parameter 'occ21-opts "-ZUS")
         ]
        
        ;; 20070601 MCJ
        ;; This could, eventually, become the "--lego" option.
        ;; For now, I'm calling it "srec".
        ["--srec"
         "Output a .srec for the LEGO Mindstorms"
         (set-parameter! 'target-processor 't2)
         (set-parameter! 'output 'srec)]
        
        ["--occam"
         "Output a .occ for including in... occam-pi programs..."
         (set-parameter! 'target-processor 't4)
         (set-parameter! 'output 'occam)]
        
        ["--keep-temp-files"
         "Keep temporary files generated during compilation."
         (set-parameter! 'keep-temp-files #t)]
        
        
	["--bytecode"
	 "Output bytecode for running on the Transterpreter. (DEFAULT)"
	 (set-parameter! 'output 'bytecode)]
        [("-t" "--target")
	 target
         "Set parameter to one of t2, t4, t8 (t4 or t8 are default)" 
         (if (list? (member (string->symbol target) '(t2 t4 t8)))
             (set-parameter! 'target-processor (string->symbol target))
             (raise-user-error 'skroc "-t or --target only take values of t2, t4 or t8"))]
	["--c"
	 "Outputs a C file containing an array of bytecodes."
	 (set-parameter! 'output 'c)]
        ["--efficientc"
	 "Outputs an efficient C file to be compiled native by GCC."
	 (set-parameter! 'output 'efficientc)]
        ["--tce"
         "Outputs a TCE (i.e. does not run the slinker)."
         (set-parameter! 'output 'tce)]
        ["--etc"
         "Outputs a ETC (i.e. does not run the slinker)."
         (set-parameter! 'output 'etc)]
	["--binary"
	 "Compiles a native binary on supported platforms."
	 (set-parameter! 'output 'binary)]
	["--library"
	 "Precompiles the code as a library."
	 (set-parameter! 'output 'library)]
        ["--no-std-libs"
         "Don't try to link with the standard occam* libraries."
         (set-parameter! 'use-std-libs #f)]
	["--decompile"
	 "Takes a TCE file and decompiles it. Ish."
	 (set-parameter! 'output 'decompile)]
	[("-v" "--verbose")
	 "Display extra information when compiling"
	 (set-parameter! 'verbose #t)]
        ["--version"
         "Displayes version of skroc"
         (show-version)
         (exit)]
        ["--occ21-version"
         "Displayes version of occ21"
         (show-occ21-version)
         (exit)]
        ["--ilibr-version"
         "Displayes version of ilibr"
         (show-ilibr-version)
         (exit)]
        ["--tranx86-version"
         "Displayes version of tranx86"
         (show-tranx86-version)
         (exit)]
        ["--slinker-version"
         "Displayes version of slinker"
         (show-slinker-version)
         (exit)]
        ["--library2-version"
         "Displayes version of library2"
         (show-library2-version)
         (exit)])
       (once-any
        ["--lib-only"
         "When compiling with --library only create the .lib file"
         (if (equal? (get-parameter 'output) 'library)
             (set-parameter! 'library-output 'lib-only)
             (raise-user-error 'skroc "--lib-only can only be used in conjunction with --library"))]
        ["--precomp-only"
         "When compiling with --library only create the .precomp file"
         (if (equal? (get-parameter 'output) 'library)
             (set-parameter! 'library-output 'precomp-only)
             (raise-user-error 'skroc "--precomp-only can only be used in conjunction with --library"))])
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;; The remaining arguments
       (args 
	files 
        (set-parameter! 
         'input-files 
         (append
          (get-parameter 'input-files (lambda () '()))
          files)))
       )))

  ;; This procedure checks the output-filename parameter, and if it is not
  ;; already set it tries to set it from the first input file name
  (define fix-output-name
    (lambda () 
      
      ;;(printf "~a ~a~n" (not (get-parameter 'output-filename (lambda () #f)))
      ;;         (empty? (get-parameter 'input-files (lambda () #f))))
      ;; If there is not output-filename set, and we have input files
      (if (and (not (get-parameter 'output-filename (lambda () #f)))
               (not (null? (get-parameter 'input-files (lambda () '())))))
          (let ([output-format (get-parameter 'output)])
            (cond
              [(equal? output-format 'bytecode)
               ;; Take the first input file name and use that (with appropriate extension)
               (set-parameter! 'output-filename (->tbc (car (get-parameter 'input-files))))]
	      [(equal? output-format 'c)
               ;; Take the first input file name and use that (with appropriate extension)
               (set-parameter! 'output-filename (->c (car (get-parameter 'input-files))))]
              [(equal? output-format 'efficientc)
               ;; Take the first input file name and use that (with appropriate extension)
               (set-parameter! 'output-filename (->c (car (get-parameter 'input-files))))]
	      [(equal? output-format 'lx)
               ;; Take the first input file name and use that (with appropriate extension)
               (set-parameter! 'output-filename (->lx (car (get-parameter 'input-files))))]
              
              [(equal? output-format 'srec)
               ;; Take the first input file name and use that (with appropriate extension)
               (set-parameter! 'output-filename (->srec (car (get-parameter 'input-files))))]
              
              [(equal? output-format 'blackfin)
               ;; Take the first input file name and use that (with appropriate extension)
               (set-parameter! 'output-filename (->srv (car (get-parameter 'input-files))))]
              
              [(equal? output-format 'library)
               (if (equal? (get-library-build-type) 'lbb)
                   (set-parameter! 'output-filename (stripext (car (get-parameter 'input-files))))
                   
                   (die "Output filename needed for library compilation with .occ files~n"))]
              )))
      ))
  
  (define make-occ21-flags
    (lambda ()
      ;; First, set the environment so we can search for libraries
      (putenv "ISEARCH" 
	      ;; FIXME:
	      ;; We used to add the current directory here automatically, though
	      ;; I am not sure if that is a good idea??? So I an not at the
	      ;; moment, we can always change this
	      (crunch (list-intersperse 
                        isearch-separator 
                        (append
                          (get-parameter 'library-paths (lambda () '()))
                          (if (getenv "ISEARCH")
                            (list (getenv "ISEARCH"))
                            '())))))
      (if (isverbose?) (printf "ISEARCH = ~a~n" (getenv "ISEARCH")))
      ;; Then, create and return the flags
      (append
       (intelli-split-str
        ;;format "~a ~a -etc -w -y -znd -znec -udo -zncc -init -xin -mobiles -zep -msf"
        (format "~a ~a -etc -w -y -znd -znec -udo -zncc -init -xin -mobiles -zrpe -zcxdiv -zcxrem -zep ~a ~a"
                ;; from kroc-1.4.0-pre0
                ;; removed -npc (which disables placed chans)
                ;; removed -zen (enhanced alt enabeling) [some instructions not
                ;;                                        implemented]
                ;; removed -revalt (reverse alt enabeling sequence)
                (cond
                  [(equal? (get-parameter 'target-processor) 't2) "-t2 -V"]
                  [(equal? (get-parameter 'target-processor) 't4) "-t4"]
                  [(equal? (get-parameter 'target-processor) 't8) "-t8 -zqa"]
                  )
                (apply string-append 
                       (map (lambda (x) 
                              (format "-DEF ~a " x)) 
                            (get-parameter 
                             'defines
                             (lambda () '()))))
                (if (equal? (get-parameter 'target-endian) 'BIG) "-tbe" "-tle")
                (if (get-parameter 'brief (lambda () #f)) "-b" "")))
       (argify (intelli-split (get-parameter 'occ21-opts (lambda () '()))))
       )))
  
  ;; There does not seem to be a function to do this already!
  (define stripext
    (lambda (f)
      (pregexp-replace "\\.[^\\.]*$" f "")))
  
  (define ->tce
    (lambda (f)
      (string-append (stripext f) ".tce")))
  
  (define ->tvmdbg
    (lambda (f)
      (string-append (stripext f) ".tvmdbg")))
  
  (define ->tbc
    (lambda (f)
      (string-append (stripext f) ".tbc")))
  
  (define ->srv
    (lambda (f)
      (string-append (stripext f) ".srv")))
  
  (define ->occ
    (lambda (f)
      (string-append (stripext f) ".occ")))
  
  (define occ?
    (lambda (f)
      (pregexp-match "\\.occ$" f)))

  (define ->h
    (lambda (f)
      (string-append (stripext f) ".h")))
  
  (define ->c
    (lambda (f)
      (string-append (stripext f) ".c")))
  
  (define ->lx
    (lambda (f)
      (string-append (stripext f) ".lx")))
  
  (define ->srec
    (lambda (f)
      (string-append (stripext f) ".srec")))

  
  (define mapl
    (lambda (fn ls)
      (cond
        [(null? ls) '()]
        [else
         (cons 
          (fn (car ls))
          (mapl fn (cdr ls)))])))
  
  (define make-slinker-flags
    (lambda ()
      (list
       "-f" (get-parameter 'output-filename)
       "-w" (cond 
              [(equal? (get-parameter 'target-processor) 't2) 2 ] 
              [(equal? (get-parameter 'target-processor) 't4) 4 ] 
              [(equal? (get-parameter 'target-processor) 't8) 4 ] 
              )
       "-o" (get-parameter 'output)
       (map (lambda (dir) (list "-L" dir)) 
            ;; 20080424 clj3
            ;; making skroc pick up paths from ISEARCH to make it easier to
            ;; do builds of the tvm w/o doing pesky make install steps. This
            ;; should be rolled into a better set of environment variables
            ;; which affet how skroc behaves, so really, this is a FIXME
            ;; We use a 'pristine' version of ISEARCH, as we pullute the
            ;; environment in other places in the program.
            ;; Also, I'm not sure why that second arg is needed in
            ;; path-list-string->path-list, but it is. I wish i'd just return
            ;; an empty list if there was nothing in the 'path-list-string',
            ;; instead of insisting that we supply a valid path... bahh.
            (append
              (get-parameter 'library-paths)
              (if (not (srfi13:string-null? 
                         (get-parameter 'pristine-isearch)))
                (map path->string 
                     (path-list-string->path-list 
                       (get-parameter 'pristine-isearch) 
                       (list (string->path ".")))) 
                '())))
       (intelli-split (get-parameter 'slinker-opts (lambda () '())))
       (if (get-parameter 'use-std-libs)
         ;; Include all the default runtime libraries -- dead library
         ;; elimination will remove them if they're not actually needed.
         (map (lambda (lib) (list "-l" lib))
              (intelli-split-str default-runtime-libraries))
         '()))))
  
  (define crunch
    (lambda (ls)
      (apply string-append ls)))
  
  (define list-intersperse
    (lambda (obj ls)
      (cond
        [(null? ls) '()]
        [(null? (cdr ls)) (list (car ls))]
        [else (cons (car ls) 
                    (cons obj
                          (list-intersperse obj (cdr ls))))]
        ))) 
  
  ;; 'compile' is already taken :(
  (define compile-files
    (lambda (files)
      (let ([occ21-flags (make-occ21-flags)])
	(for-each
	 (lambda (f)
	   (let ([args (argify (list occ21-flags f "-o" (->tce f)))])
	     (printcmd (occ21) args)
	     (unless (zero? (exec (occ21) args))
	       (die "Could not compile ~a.~n" f))
	     (set-parameter! 'temp-files
	                     (cons (->tce f) (get-parameter 'temp-files)))
	     ))
	 files))))
  
  (define go
    (lambda ()
      ;; Check to see that we've got some input files
      (if (null? (get-parameter 'input-files))
	  (die "You didn't include any files to be compiled and slinked.~n"))
      
      ;; Check that the files we were given are of the correct type. 
      ;; All the files are either .occ or .tce; we don't process .tce files.
      (if (not (andmap (lambda (f)
                         (or (pregexp-match "\\.(occ|tce)$" f)))
                       (get-parameter 'input-files)))
          (die "Output type: --~a only consumes occam source files (ending in .occ)~n"
	       (get-parameter 'output)))
      
      ;; Run occ21 on all the source files; don't run it on .tce files.
      (compile-files (list:filter occ? (get-parameter 'input-files)))
      
      ;; Are we outputing TCE? If so stop now
      (if (equal? (get-parameter 'output) 'tce) (exit))
      
      ;; Are we outputting ETC? If so, do that, and stop
      (if (equal? (get-parameter 'output) 'etc)
          (begin
            (for-each (lambda (file)
			(let ([args (argify (list "-r" "-m386" file))])
			  (printcmd (tranx86) args)
                          (unless (zero? (exec (tranx86) args))
                            (die "Couldn't generate an etc file for ~a~n" file))))
                      (mapl ->tce (get-parameter 'input-files)))
            (exit)))                    
      
      ;; Slinker everything
      ;; (eventually want to be smarter--eg, libraries, lego, etc.)
      (let ([slinker-args (argify 
                           (list (make-slinker-flags)
                                 (mapl ->tce (get-parameter 'input-files))))])
	(printcmd (slinker) slinker-args)
        (unless (zero? (exec (slinker) slinker-args))
          (die "Couldn't slink one or more files.~n")))
      
      ;;We're done
      ))
  
  (define get-library-build-type
    (lambda ()
      ;; Check that the files we were given are of the correct type.
      (let ([files (get-parameter 'input-files)])
        (if (and (= (length files) 1)
                 (pregexp-match "\\.lbb$" (car files)))
            ;; There was only one file in the list, and it ends in .lbb
            'lbb
            ;; Check if we have a list of occam sourcefiles
            (if (andmap (lambda (f) (pregexp-match "\\.(occ|tce)$" f)) files)
                'occ
                ;; No? thats bad!
                (die "Output type: --~a consumes occam source file(s) or a single .lbb file~n"
		     (get-parameter 'output)))))))
  
  ;; Checks if a list of files all exists, bombs out if not, otherwise does nothing.
  (define files-exist
    (lambda (files)
      (for-each 
       (lambda (f) (if (not (file-exists? f)) 
                       (die "file: ~a not found~n" f)))
       files)))
  
  ;; Reads in the contents of an lbb file, excluding any comments
  ;; (starting with --)
  (define read-lbb
    (lambda (lbbname)
      (let ([ip (open-input-file lbbname)]
            [files '()])
        (let loop ([line (read-line ip)])
          (unless (eof-object? line)
            (if (not (pregexp-match "^[[:space:]]*--" line))
                (set! files (cons (pregexp-replace "--.*$" line "") files)))
            (loop (read-line ip))))
        (close-input-port ip)
        files)))
  
  ;; Go procedure for compiling libraries using either a list of .occ files
  ;; or a single .lbb file from which the .occ files will be obtained.
  ;;
  ;; Matt, I would like to point out that you are not as good as documenting
  ;; your code as I am :p
  ;;
  ;; FIXME: Currently I dont use the .lbb file (if we were given one) to pass
  ;; to ilibr, is that a bad thing? probably not since it seems quite happy
  ;; being passed individual tce's
  (define go-library
    (lambda ()
      (let ([libout (get-parameter 'library-output)])
        ;; Check some things
        ;; Are all the files on the commandline that we need around?
        (files-exist (get-parameter 'input-files))
        ;; Check the output filename has no extension
        (if (or (pregexp-match "\\.precomp$" (get-parameter 'output-filename))
                (pregexp-match "\\.lib$" (get-parameter 'output-filename)))
            (die "library name (-f <libname>) should have no extension~n"))
        
        ;; If we are building an lbb library, read in the filenames from there.
        ;; And make sure the files we are going to try to compile are called .occ
        ;; We clobber 'input-files, which used to contain the .lbb filename, but
        ;; will now contain the .occ files which were embedded in the .lbb file
        (if (equal? (get-library-build-type) 'lbb)
            (set-parameter! 'input-files 
                            (mapl ->occ (read-lbb (car (get-parameter 'input-files))))))
        
        ;; Compile all the files to tce
        (compile-files (list:filter occ? (get-parameter 'input-files)))
        
        ;; Make .lib file by calling ilibr
        (if (or (equal? libout 'all) (equal? libout 'lib-only))
	    (let ([args (argify (list
                                 "-O" (format "~a.lib" (get-parameter 'output-filename))
                                 (mapl ->tce (get-parameter 'input-files))))])
	      (printcmd (ilibr) args)
              (unless (zero? (exec (ilibr) args))
                (die "Couldn't slink one or more files.~n"))))
        ;; Make .precomp file by calling the slinker
        (if (or (equal? libout 'all) (equal? libout 'precomp-only))
	    (let ([args 
		   (argify (list
                            (map (lambda (dir) (list "-L" dir))
                            ;; 20080424 clj3, see other comment with this date
                            (append
                              (get-parameter 'library-paths)
                              (if (not (srfi13:string-null? 
                                         (get-parameter 'pristine-isearch)))
                                (map path->string 
                                     (path-list-string->path-list
                                       (get-parameter 'pristine-isearch)
                                       (list (string->path "."))))
                                '())))
                            (intelli-split 
                             (get-parameter 'slinker-opts (lambda () '())))
                            "-f" (format "~a.precomp" (get-parameter 'output-filename))
                            (mapl ->tce (get-parameter 'input-files))))])
	      (printcmd (fixme-library-slinker) args)
              (unless (zero? (exec (fixme-library-slinker) args))
                (die "Couldn't slink one or more files.~n"))))
        )))
  
  (define (remove-temp-files)
    (define (cond-rm f) (if (file-exists? f) (delete-file f)))
    (for-each cond-rm (get-parameter 'temp-files)))
  
  (define init
    (lambda ()
	(set-parameter! 'output 'bytecode)
	(set-parameter! 'library-output 'all)
	(set-parameter! 'use-std-libs #t)
	(set-parameter! 'target-endian #f)
	(set-parameter! 'target-processor #f)
	(set-parameter! 'temp-files '())
	(set-parameter! 'keep-temp-files #f)
	(set-parameter! 'done #f)
        (set-parameter! 'pristine-isearch (if (getenv "ISEARCH") 
                                            (getenv "ISEARCH") ""))
        ))

  (define read-config-file
    (lambda ()
      ;; for the config-file, we first check if the user supplied
      ;; --with-tvm_config_h, if so, use that. If not, check for the env
      ;; variable SKROC_TVM_CONFIG_H, if set, use that. Otherwise fall back to 
      ;; $install_dir/include/tvm_config.h
      (let ([config-file (get-parameter 
                           'tvm_config.h 
                           (lambda () 
                             (let ([in-tree (get-parameter 'in-tree (lambda () #f))])
                               (if in-tree
                                 (format "~a/runtime/libtvm/tvm_config.h" in-tree)
                                 (get-environment 
                                   "SKROC_TVM_CONFIG_H"
                                   (lambda () 
                                     (format "~a/include/tvm_config.h" install-dir)))))))]
	    [target-endian 'LITTLE]
	    [target-processor 't4])
	(if (file-exists? config-file)
	  (let ([ip (open-input-file config-file)])
	    (let loop ([line (read-line ip)])
	      (unless (eof-object? line)
	        (let ([emu (pregexp-match "^#define.*TVM_EMULATE_T([0-9]+)[[:space:]]" line)]
		      [endian (pregexp-match "^#define.*TVM_(.*)_ENDIAN[[:space:]]" line)])
		  (if emu
		    (set! target-processor (string->symbol (format "t~a" (list-ref emu 1)))))
		  (if endian
		    (set! target-endian (string->symbol (list-ref endian 1))))
                 (loop (read-line ip))))))
          (warn "could not read defaults from ~a\n" config-file))
	;; Set the corresponding parameters if they've not been set explicitly
	;; by the user.
	(if (not (get-parameter 'target-endian (lambda () #f)))
	  (set-parameter! 'target-endian target-endian))
	(if (not (get-parameter 'target-processor (lambda () #f)))
	  (set-parameter! 'target-processor target-processor))
	)))

  (define (check-output-name-not-input-name)
    (let ([in* (get-parameter 'input-files)]
          [out (get-parameter 'output-filename)])
      (if (member out in*)
          (begin
            (raise-user-error
             (format "`~a' is both an input and output file.~n~nYou probably do not want to output the file `~a', as it is also one of the files you are using as input to skroc.~n~nI will not be party to your shenanigans.~n" out out))
            (exit)))))
  
  ;; Init defaults
  (init)
  ;; Parse the command-line arguments
  (parse-cmd)
  ;; Read the configuration file
  (read-config-file)
  (let ([in-tree (get-parameter 'in-tree (lambda () #f))])
    (cond
      [in-tree
	;; Append in-tree search directories to the search path.
	(map
	  (lambda (dir) (append-parameter 'library-paths (format "~a/~a" in-tree dir)))
	  '("modules/inmoslibs/libsrc/forall" "tvm/posix"))
	]
      [else
	;; Tack the library directory onto the end of the search path, so that
	;; -L options will override it.
	(append-parameter 'library-paths (format "~a/lib/transterpreter" install-dir))
	]))

  ;; If not output filename was supplied, try to autodetect
  (fix-output-name)
  ;; Make sure we aren't about to overwrite ourselves
  ;; But this won't work for libraries.
  (unless (equal? (get-parameter 'output) 'tce)
    (check-output-name-not-input-name))
  
  ;; And do stuff with that information
  (cond
    [(equal? (get-parameter 'output) 'library)
     (go-library)
     (set-parameter! 'done #t)]
    [else
     (go)
     (set-parameter! 'done #t)
     ])
  
  ;; This removes temporary files if we're done, and
  ;; the user did not say to keep them.
  (unless (and (get-parameter 'done)
               (get-parameter 'keep-temp-files))
    (remove-temp-files))
  
  )
