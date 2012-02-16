#|
tinyswig - tinyswig.scm
A tiny wrapper generator for occam
Copyright (C) 2006-2008 Matthew C. Jadud

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
(module tinyswig mzscheme
  ;; This is the important bits of tinyswig. It was broken out
  ;; so I could write tests against it.
  (require (file "tinybase.scm"))
  ;; Use regular expression library.
  (require (lib "pregexp.ss"))
  ;; The MzScheme uber-commandline library.
  (require (lib "cmdline.ss"))
  ;; Could replace SRFI-13 with pregexp, with some work.
  (require (prefix srfi13: (lib "13.ss" "srfi"))
           (lib "pregexp.ss"))
  
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


  ;; This has to go somewhere.
  (set-parameter! 'version 139)
  
  ;; I pulled this out of the tinybase, since it is more execution-specific than
  ;; "library"-like code. 
  (define (set-defaults header-filename)
    (conditional-set-parameter! 'no-prefix #f)
    (conditional-set-parameter! 'prefix "ffi")
    (conditional-set-parameter! 'c-filename 
                                (format "external_~a_hooks.c" (get-basename header-filename)))
    (conditional-set-parameter! 'occam-filename 
                                (format "external_~a_pragmas.occ" (get-basename header-filename)))
    (conditional-set-parameter! 'pad 0)
    )
  
  ;; The command line. 
  ;; This is what drives all of tinyswig. Everything else is just data 
  ;; manipulation behind this interface.
  (define (the-line cmd-line-args)
    (command-line
     "tinyswig"
     cmd-line-args
     (once-each
      [("-v" "--version")
       "Get the current version of TinySwig"
       (printf "tinyswig version ~a (r~a)~n" (get-parameter 'version) (list-ref (pregexp-match "[0-9]+" "$Revision: 4497 $") 0))
       (exit) ]

      [("-p" "--prefix")
       the-prefix
       "Specifies a prefix for the generated occam-pi PROC names."
       (set-parameter! 'prefix the-prefix)]
      [("-n" "--no-prefix")
       "Suppresses the generation of prefixes on occam-pi PROC names."
       (set-parameter! 'no-prefix #t)]
      [("-p" "--pad")
       n
       "Pad externals by <n> spaces."
       (set-parameter! 'pad (string->number n))]
      
      [("-c" "--c-filename")
       the-c-filename
       "Output name for the generated C file. Default is 'external_special_hooks.c'."
       (set-parameter! 'c-filename the-c-filename)]
      [("-o" "--occam-filename")
       the-occam-filename
       "Output name for the generated occam-pi PRAGMAs."
       (set-parameter! 'occam-filename the-occam-filename)])
     (args
      (first-file . additional-files)
      ;; Set the defaults; use the first file for filenames
      (set-defaults first-file)
     
      ;; Check what kind of files we are dealing with
      (let ([all-files (cons first-file additional-files)])
        (cond
          [(andmap (lambda (f) (srfi13:string-suffix? ".c" f)) all-files) 
            ;; First, set defaults for all parameters.
            (for-each (lambda (c-file)
                        (gather-names-from-file c-file))
                      all-files)
            (set-parameter! 'c-header-files (map (lambda (x) (pregexp-replace "c$" x "h")) all-files))
            (output-c-file)
            (output-occam-file)
            (output-h-files) 
            ]
          [(andmap (lambda (f) (srfi13:string-suffix? ".h" f)) all-files)
            ;; First, set defaults for all parameters.
            (for-each (lambda (header)
                        (gather-names-from-file header))
                      all-files)
            (set-parameter! 'c-header-files all-files)
            
            (output-c-file)
            (output-occam-file)
            ]
          [else (raise-user-error 
                 'tinyswig
                 (string-append "expects a list consisting entirely of .c files "
                                "or a list consiting entirely of .h files on the command line"))])))))
  
  (the-line (current-command-line-arguments))
  (exit)
      
  )
