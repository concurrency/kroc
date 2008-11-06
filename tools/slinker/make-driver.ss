#|
slinker - make-driver.ss
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
;; mzscheme -mt-- make-driver.ss -f <driver>
#cs
(module make-driver mzscheme
  (require (lib "plt-match.ss")
           (lib "pregexp.ss")
           (lib "pretty.ss")
           (lib "cmdline.ss")
           (lib "date.ss")
           "helpers.ss")

  ;; Running the module is now accomplished via command line
  ;; flags look like:
  ;; mzscheme -mt-- make-driver.ss -f <driver>
  ;;             ^^ 
  ;;             This tells MzScheme to let other flags
  ;;             be passed to the script, not the runtime.
  ;; <driver> is the name of the driver script (XML file)
  ;; sans the '.xml' extension. Outputs '<driver>.ss', which
  ;; can then be compiled with 'mzc --exe <exename> <driver>.ss'
  
  (define read-driver
    (lambda (file)
      ;; This used to be an XML document that was read in; now
      ;; we'll just write it directly in SXML.
      (read (open-input-file file))))  
  
 
  (define (check-driver-syntax exp)
    (define TOP
      (match-lambda 
          [`(process ,e ,e* ...)
            `(process ,(Structure e) ,@(mapl Structure e*))]
        [else 
         (error (format "Not a top-level form: ~a~n" else))]))
    
    (define BooleanExp
      (match-lambda
        [(? symbol? exp) exp]
        [`(,(? (lambda (s)
                 (member s '(and or))) sym) ,exp ,exp* ...)
          `(,sym ,(BooleanExp exp) ,@(mapl BooleanExp exp*))]
        [`(not ,exp)
          `(not ,(BooleanExp exp))]
        ))
          
    
    (define Structure
      (match-lambda
        [(? symbol? sym) sym]
        [`(,(and struct (or 'seq 'par)) ,(? string? e) ,(? string? e*) ...)
          `(,struct ,(string->symbol e) ,@(mapl string->symbol e*))]
        [`(begin ,e ,e* ...)
          `(begin ,(Structure e) ,@(mapl Structure e*))]
        [`(if ,bexp ,case-e1)
          `(if ,(BooleanExp bexp)
               ,(Structure case-e1))]
        [`(if ,bexp
              ,case-e1
              ,case-e2)
          `(if ,(BooleanExp bexp)
               ,(Structure case-e1)
               ,(Structure case-e2))]
        [`(case ,sym
            ((,sym** ...) ,exp*) ...)
          (let ([sym** (mapl (lambda (sym*)
                               (if (andmap symbol? sym*)
                                   sym*
                                   (error 'check-syntax "Not a symbol in case list: ~s" sym*)))
                             sym**)]
                [exp* (mapl (lambda (exp)
                              (Structure exp)) exp*)])
            `(case ,sym
               ,@(map (lambda (sym* exp)
                         `(,sym* ,exp)) sym** exp*)))]
        
        [`(seq ,exp* ...)
          `(seq ,@(mapl Structure exp*))]
        
        [`(par ,exp* ...)
          `(par ,@(mapl Structure exp*))]
        
        [else
         (error (format "check-syntax: Not a valid Structure: ~a~n" else))]))
    
    (TOP exp))
  
  ;; All passes will be wrapped in a SEQ or a PAR. So, 
  ;; if we want to change the way passes work, 
  ;; we need to do it here.
  (define (expand-seq-and-par exp)
    (define TOP
      (match-lambda 
          [`(process ,e ,e* ...)
            `(process ,(Structure e) ,@(mapl Structure e*))]
        [else 
         (error (format "Not a top-level form: ~a~n" else))]))
    
       (define BooleanExp
      (match-lambda
        [(? symbol? exp) exp]
        [`(,(? (lambda (s)
                 (member s '(and or))) sym) ,exp ,exp* ...)
          `(,sym ,(BooleanExp exp) ,@(mapl BooleanExp exp*))]
        [`(not ,exp)
          `(not ,(BooleanExp exp))]
        ))
          
 
    (define Structure
      (match-lambda
          [(? symbol? sym) sym]
        [`(begin ,e ,e* ...)
          `(begin ,(Structure e) ,@(mapl Structure e*))]
        [`(seq ,e ,e* ...)
          `(for-each
            (lambda (pass)
              (debug 'driver (printf "SEQ: ~a~n" pass))
              ;; This has no business here; instead, each pass should have
              ;; a set of 'guards' that dictate on a pass-by-pass basis
              ;; whether the pass should run.
		 
		 ;;TIMING
		   (let ([start (current-milliseconds)]
			 [end (void)])
		     (set! h (pass h))
		     (set! end (current-milliseconds))
		     (debug 'profiling 
			     (printf "SEQ ~a : ~a ms~n"
				     pass (- end start))))
	      
	      )
            (list ,(Structure e) ,@(mapl Structure e*)))]
        [`(par ,e ,e* ...)
          `(mapl
            (lambda (hash)
              (for-each
               (lambda (pass)
                 (debug 'driver (printf "PAR: ~a~n" pass))
                 ;;(printf "META: ~a~n" (pass-meta hash))

		 ;;TIMING
		   (let ([start (current-milliseconds)]
			 [end (void)])
		     (set! hash (pass hash))
		     (set! end (current-milliseconds))
		     (debug 'profiling 
			     (printf "PAR ~a : ~a ms~n" pass (- end start))))
		 )
               (list ,(Structure e) ,@(mapl Structure e*)))
              hash)
            h)]
                [`(if ,bexp ,case-e1)
          `(if ,(BooleanExp bexp)
               ,(Structure case-e1))]
        [`(if ,bexp
              ,case-e1
              ,case-e2)
          `(if ,(BooleanExp bexp)
               ,(Structure case-e1)
               ,(Structure case-e2))]
        [`(case ,sym
            ((,sym** ...) ,exp*) ...)
          (let ([sym** (mapl (lambda (sym*)
                               (if (andmap symbol? sym*)
                                   sym*
                                   (error 'check-syntax "Not a symbol in case list: ~s" sym*)))
                             sym**)]
                [exp* (mapl (lambda (exp)
                              (Structure exp)) exp*)])
            `(case ,sym
               ,@(map (lambda (sym* exp)
                         `(,sym* ,exp)) sym** exp*)))]
        [else
         (error (format "expand-seq-and-par Not a valid Structure: ~a~n" 
                        else))]))
    
    (TOP exp))
  
  
  
  
  (define (expand-sym exp)
    (define TOP
      (match-lambda 
          [`(process ,e ,e* ...)
            `(process ,(Structure e) ,@(mapl Structure e*))]
        [else 
         (error (format "Not a top-level form: ~a~n" else))]))
    
    (define Structure
      (match-lambda
          [(? symbol? sym) `(set! h (,sym h))]
        [`(begin ,e ,e* ...) `(begin ,(Structure e) ,@(mapl Structure e*))]
        [`(for-each ,args* ...) `(for-each ,@args*)]
        [`(mapl ,args* ...) `(mapl ,@args*)]
        [`(if ,test ,true) `(if ,test ,true)]
        [`(if ,test ,true ,false) `(if ,test ,true ,false)]
        [`(case ,sym
            ((,sym** ...) ,exp*) ...)
          (let ([sym** (mapl (lambda (sym*)
                               (if (andmap symbol? sym*)
                                   sym*
                                   (error 'check-syntax "Not a symbol in case list: ~s" sym*)))
                             sym**)]
                [exp* (mapl (lambda (exp)
                              (Structure exp)) exp*)])
            `(case ,sym
               ,@(map (lambda (sym* exp)
                         `(,sym* ,exp)) sym** exp*)))]
        [else
         (error (format "expand-sym: Not a valid Structure: ~a~n" 
                        else))]))
    
    (TOP exp))
  
  (define (expand-case exp)
    (define TOP
      (match-lambda 
          [`(process ,e ,e* ...)
            `(process ,(Structure e) ,@(mapl Structure e*))]
        [else 
         (error (format "Not a top-level form: ~a~n" else))]))
    
    (define Structure
      (match-lambda
          [`(begin ,e ,e* ...) `(begin ,(Structure e) ,@(mapl Structure e*))]
        [`(for-each ,args* ...) `(for-each ,@args*)]
        [`(mapl ,args* ...) `(mapl ,@args*)]
        [`(if ,test ,true) `(if ,test ,true)]
        [`(if ,test ,true ,false) `(if ,test ,true ,false)]
        [`(case ,sym ,cases ...) `(case ,sym ,@cases)]
        #|
        [`(branch ,(and sym (? symbol?))
                  (,case-sym1 ,case-e1)
                  (,case-sym* ,case-e*) ...)
          (let ([new-id (gensym)]
		[case-symbol (cond 
			       [(equal? case-sym1 'true) #t]  ;; If the case symbol is true, use #t
			       [(equal? case-sym1 'false) #f] ;; if false use #f
			       [else `(quote ,case-sym1)])])    ;; Otherwise pass the case-symbol through
            `(let ([,new-id ,sym])
               (cond
                 [(equal? ,new-id ,case-symbol) ,case-e1]
                 ,@(map (lambda (lh rh)
			  (let ([case-symb (cond
			          [(equal? lh 'true) #t]  ;; If the case symbol is true, use #t
			          [(equal? lh 'false) #f] ;; if false use #f
			          [else `(quote ,lh)])])    ;; Otherwise pass the case-symbol through
                          `[(equal? ,new-id ,case-symb) ,rh]))
                        case-sym* case-e*))))]
        |#
        [else
         (error (format "expand-case: Not a valid Structure: ~a~n" 
                        else))]))
    
    (TOP exp))
  
  (define (fix-map exp)
    (define TOP
      (match-lambda 
          [`(process ,e ,e* ...)
            `(process ,(Structure e) ,@(mapl Structure e*))]
        [else 
         (error (format "Not a top-level form: ~a~n" else))]))
    
    (define Structure
      (match-lambda
          [`(set! ,arg* ...) `(set! ,@arg*)]
        [`(begin ,e ,e* ...) `(begin ,(Structure e) ,@(mapl Structure e*))]
        [`(for-each ,args* ...) `(for-each ,@args*)]
        [`(mapl ,args* ...) `(begin
                               (set! h (mapl ,@args*))
                               (let ([local-h (make-hash-table)]
                                     [merged-meta (make-hash-table)])
                                 (for-each
                                  (lambda (an-h)
                                    
                                    ;;Merge the hash tables
                                    (hash-table-for-each
                                     (pass-bytes an-h) 
                                     (lambda (k v)
                                       (hash-table-put! local-h k v)))
                                    ;;Merge the metadata
                                    ;; This needs to be thought out... 
                                    ;; Generally, we shouldn't need it once
                                    ;; we are merged... unless precompiling?
                                    
                                    ) h)
                                 (set! h (make-pass merged-meta local-h))))]
        
        [`(if ,test ,true) `(if ,test ,true)]
        [`(if ,test ,true ,false) `(if ,test ,true ,false)]
          
        [`(case ,sym ,cases ...) `(case ,sym ,@cases)]
        [`(let ,args* ...) `(let ,@args*)]
        [else
         (error (format "fix-map: Not a valid Structure: ~a~n" 
                        else))]))
    
    (TOP exp))
  
  (define (expand-process exp)
    (define TOP
      (match-lambda 
          [`(process ,e ,e* ...)
            `(let ([h (void)])
	       (let ([start (current-milliseconds)]
		     [end (void)]
		     [pass read-tvm-bytes3])
		 (debug 'driver (printf "SEQ: ~a~n" pass))
		 (set! h (pass input))
		 (set! end (current-milliseconds))
		 (debug 'profiling 
			 (printf "SEQ: ~a: ~a ms~n"
				 pass (- end start)))
                 )
               ,e ,@e*)]
        [else 
         (error (format "Not a top-level form: ~a~n" else))]))    
    (TOP exp))
  
  
  (define make-driver
    (lambda (file)
      `(define driver
         (lambda (input)
           ,(let* ([sxml (read-driver file)]
                  [prog
                   (expand-process
                    (fix-map
                     (expand-case
                      (expand-sym
                       (expand-seq-and-par
                        (check-driver-syntax sxml))))))])
             prog)))
      ))
      
  
  (define make-module
    (lambda (fname)
      (let ([op (open-output-file (format "~a.ss" fname)
                                  'replace)]
	    [mname (let-values ([(base name isdir) (split-path (string->path fname))])
		     (string->symbol (path->string name)))])
        (fprintf op ";; THIS FILE IS AUTOGENERATED. DO NOT EDIT. DO NOT EDIT. ABORT. ABORT.~n")
        (fprintf op ";; Last generated: ~a~n" (date->string (seconds->date (current-seconds)) #t))
        ;; Because PLT360+ is now case-sensitive by default, we no longer need to declare it explicitly.
        (let ([the-module
               (lambda (body)
                 `(module ,mname mzscheme
                    (require (lib "pregexp.ss")
                             (lib "plt-match.ss")
                             "helpers.ss"
			     "constants.ss"
                             "cmdline.ss"
                             "code-expansion.ss"
                             "code-simplification.ss"
                             "code-merging.ss"
                             "code-improvement.ss"
                             "code-output.ss")
                    (provide (all-defined))
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
                    (parse-command-line)
                    (set-wordsize-constants)
                    ,body
                    (driver *INPUT-FILES*)))])
          
          #|
          (fprintf op "~s~n" 
                   (the-module 
                    (make-driver 
                     (format "~a.xml" fname))))
          |#
          (pretty-print (the-module
                         (make-driver
                          (format "~a.sxml" fname)))
                        op)
          )
        (close-output-port op))))
  
  (define cmdline-handler
    (lambda ()
      (command-line
       "slinker-maker"
       (current-command-line-arguments)
       (once-each
        [("-f" "--driver-script")
         driver-filename
         "\n\tThe name of the driver script to be compiled sans it's '.xml' extension.\n"
         (if (pregexp-match "\\.sxml$" driver-filename)
             (begin
               (printf "Please include the driver filename WITHOUT it's SXML extension.~n")
               (exit -1))
             (begin
               (make-module driver-filename)
               (printf "Driver script output to ~a.ss.~n" driver-filename)))
         (exit 0)
         ]))))
  
  ;;(current-command-line-arguments #("-f" "slinker"))
  (cmdline-handler)
  )


