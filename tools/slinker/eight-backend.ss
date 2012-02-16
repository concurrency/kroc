#cs
(module eight-backend mzscheme
  (require
   (lib "pregexp.ss")
   (lib "plt-match.ss")
   "helpers.ss"
   "constants.ss"
   "cmdline.ss"
   "code-expansion.ss"
   "code-simplification.ss"
   "code-merging.ss"
   "code-improvement.ss"
   "load-eight-code.ss"
   "code-output.ss"
   "version.ss")
  (provide (all-defined))

  ;; Add this files revision to the version information
  (put-revision "$Revision: 784 $")

  (parse-command-line)
  (set-wordsize-constants)
  (define
   driver
   (lambda (input)
     (let ((h (void)))
       (let ((start (current-milliseconds))
             (end (void))
             (pass load-eight-code))
         (debug 'driver (printf "SEQ: ~a~n" pass))
         (set! h (pass input))
         (set! end (current-milliseconds))
         (debug 'profiling (printf "SEQ: ~a: ~a ms~n" pass (- end start))))
       (begin
         (begin
           (set! h
             (mapl
              (lambda (hash)
                (for-each
                  (lambda (pass)
                    (debug 'driver (printf "PAR: ~a~n" pass))
                    (let ((start (current-milliseconds)) (end (void)))
                      (set! hash (pass hash))
                      (set! end (current-milliseconds))
                      (debug
                       'profiling
                       (printf "PAR ~a : ~a ms~n" pass (- end start)))))
                  (list
                   remove-unknown-things
                   get-and-remove-filename
                   remove-useless-text
                   find-globalnames
                   shiftimm->shifts
                   jentry2jump
                   expand-loopend
                   unify-jumps
                   unify-labels
                   guarantee-unique-label-names
                   get-and-remove-spaces
                   expand-notprocess))
                hash)
              h))
           (let ((local-h (make-hash-table)) (merged-meta (make-hash-table)))
             (for-each
               (lambda (an-h)
                 (hash-table-for-each
                   (pass-bytes an-h)
                   (lambda (k v) (hash-table-put! local-h k v))))
               h)
             (set! h (make-pass merged-meta local-h))))
         (for-each
           (lambda (pass)
             (debug 'driver (printf "SEQ: ~a~n" pass))
             (let ((start (current-milliseconds)) (end (void)))
               (set! h (pass h))
               (set! end (current-milliseconds))
               (debug
                'profiling
                (printf "SEQ ~a : ~a ms~n" pass (- end start)))))
           (list
            renumber
            inst2binary
            data-bytes2binary
            boolinvert2binary
            widenshort2binary
            insert-alignment
            load-labels2binary
            load-precompiled-libraries
            make-binaries-in-precompiled-libraries
            set-precompiled-library-offsets
            insert-precompiled-libraries
            merge-library-globalnames
            expand-ffi-stubnames
            inst2binary
            resolve-stubnames-picky
            jumps2binary
            instruction-list
            debug-ip
            dump-external-ffi-table))
         (let ((g181 |*OUTPUT-FORMAT*|))
           (cond
            ((equal? g181 'output-c)
             (for-each
               (lambda (pass)
                 (debug 'driver (printf "SEQ: ~a~n" pass))
                 (let ((start (current-milliseconds)) (end (void)))
                   (set! h (pass h))
                   (set! end (current-milliseconds))
                   (debug
                    'profiling
                    (printf "SEQ ~a : ~a ms~n" pass (- end start)))))
               (list binary2c)))
            ((equal? g181 'output-bytecode)
             (for-each
               (lambda (pass)
                 (debug 'driver (printf "SEQ: ~a~n" pass))
                 (let ((start (current-milliseconds)) (end (void)))
                   (set! h (pass h))
                   (set! end (current-milliseconds))
                   (debug
                    'profiling
                    (printf "SEQ ~a : ~a ms~n" pass (- end start)))))
               (list binary2bytecode)))
            ((equal? g181 'output-lx)
             (for-each
               (lambda (pass)
                 (debug 'driver (printf "SEQ: ~a~n" pass))
                 (let ((start (current-milliseconds)) (end (void)))
                   (set! h (pass h))
                   (set! end (current-milliseconds))
                   (debug
                    'profiling
                    (printf "SEQ ~a : ~a ms~n" pass (- end start)))))
               (list binary2lx)))
            ((equal? g181 'output-library)
             (for-each
               (lambda (pass)
                 (debug 'driver (printf "SEQ: ~a~n" pass))
                 (let ((start (current-milliseconds)) (end (void)))
                   (set! h (pass h))
                   (set! end (current-milliseconds))
                   (debug
                    'profiling
                    (printf "SEQ ~a : ~a ms~n" pass (- end start)))))
               (list binary2scheme)))))))))
  (driver |*INPUT-FILES*|))
