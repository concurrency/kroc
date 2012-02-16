#|
slinker - cmdline.ss
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
(module cmdline mzscheme
  
  (require 
   (prefix cmd: (lib "cmdline.ss"))
   (lib "pregexp.ss")
   "helpers.ss"
   "version.ss")
  
  (provide (all-defined))
  
  ;; Add this files revision to the version information
  (put-revision "$Revision: 3137 $")
  
  (define parse-command-line
    (lambda ()
      (cmd:command-line 
       "slinker" (current-command-line-arguments)
       
       ;; Command line flags we can see many times
       (multi
        
        [("-d" "--debug-mode")
         debug-mode
         "\n\tAdds a debug mode. Means we print different stuff."
         (add-debug-mode! debug-mode)]
        
        [("-l" "--lib")
         lib
         "\b\tInclude a pre-compiled library."
         (let ([fname (find-precompiled-library lib)])
           (set-input-files! (cons fname *INPUT-FILES*)))]
        
        [("-L" "--LIB-PATH")
         lib-path
         "\n\tAdds a directory to the list that will be searched for libraries.\n"
         (add-lib-path! (string->path lib-path))]
        )
       
       
       ;;Command line flags we only want to see once
       ;; Alphabetical order, so we don't repeat one...
       (once-each
        
        ;; 20070617 MCJ/CLJ TLP CHECKING
        [("--tlp-types")
         typestring
         "\n\tDefine a TLP typestring (eg. \"CHAN BYTE IN, CHAN BYTE OUT, CHAN BYTE OUT\")\n"
         (set-tlp-types! typestring)]
        
        ;; 20070227 MCJ BYTESWAP
        [("--byteswap")
         "\n\tFlips all the bytes (for a native big-endian Transterpreter)."
         (add-debug-mode! "byteswap")
         (do-byteswap!)]
        
        [("-f" "--filename")
         filename
         "\n\tSets the output filename for the linker."
         (set-output-filename! filename)]
        
        [("-o" "--output")
         output-format
         "\n\tSets the output format (default 'c')\n\t\tc - output a c header file\n\t\tunified - output a single file with everything in it (bytecode, ffi,    debug etc)\n\t\tbytecode-multifiles - output a bytecode file (and other supporting files, ffi, debug etc as separate files)\n\t\tlx - output a .lx file for the LEGO Mindstorms"
         (cond
           [(string=? output-format "c")             (set-output-format! 'output-c)
                                                     (set-output-extension! "c")]
           
           [(string=? output-format "bytecode-multifiles")      
            (set-output-format! 'output-bytecode)
            (set-output-extension! "tbc")]
           
           [(string=? output-format "bytecode")      (set-output-format! 'output-unified-bytecode)
                                                     (set-output-extension! "tbc")]
           
           [(string=? output-format "lx")            (set-output-format! 'output-lx)
                                                     (set-output-extension! "lx")]
           
           [(string=? output-format "linear")        (set-output-format! 'linear-bytes)
                                                     (set-output-extension! "linearb")]
           
           [(string=? output-format "library")       (set-output-format! 'library)
                                                     (set-output-extension! "precomp")]
           
           [(string=? output-format "efficientc")    (set-output-format! 'output-efficientc)
                                                     (set-output-extension! "c")]
           
           [(string=? output-format "srec")          (set-output-format! 'output-srec)
                                                     (set-output-extension! "srec")]
           
           [(string=? output-format "occam")         (set-output-format! 'output-occam)
                                                     (set-output-extension! "occ")]
           
           [(string=? output-format "blackfin")      (set-output-format! 'output-blackfin)
                                                     (set-output-extension! "srv")]
           
           [else 
            ;; WARNING 
            ;; this hsould be handled more gracefully (eg. should fail on unknown type)
            ;;(set-output-format!  (string->symbol output-format))
            ;;(set-output-extension! "nativec")
            (raise-user-error 'slinker "Unknown output format: ~a" output-format)
            ])]
        
        [("-p" "--prefixing-iterations")
         pfix
         "\n\tThe maximum number of prefixing operations\n\tto be carried out in generating binary. 1 is the smallest number of iterations, 200 is a good practical upper limit. \n\tDefaults to 1."
         (let ([pfix (string->number pfix)])
           (if (>= pfix 0)
               (set-max-prefix! pfix)))]
        
        ["--static-ffi"
         file
         "Drops the FFI table with static indicies for resource-contrained platforms."
         (set-ffi-type! 'static)
         (set-static-ffi-table-file! file)
         ]
        
        ["--use-compact-libraries" 
         yesno
         "Use compact precompiled libraries (compiled into MzScheme bytecode). Takes one argument, yes or no. Default is yes."
         (cond
           [(string=? yesno "yes") (set-use-compact-libraries! #t)]
           [(string=? yesno "no") (set-use-compact-libraries! #f)]
           [else (raise-user-error "Compact libraries takes either 'yes' or 'no' as an argument")])]
        
        ["--version"
         "Displays the version of the slinker/library2"
         (display-version "slinker/library2")
         (exit)]
        
        [("-w" "--word-size")
         wordsize
         "\n\tThe wordsize of our target architecture."
         (let ([wordsize (string->number wordsize)])
           (if (or (= 1 wordsize)
                   (modulo wordsize 2))
               (set-wordsize! wordsize)))]
        
        [("-x" "--optimisation")
         optimisation
         "\n\tSets the optimisations/level of optimisation used:\n\
\tLevels:\n\
\t\tnone\t\tNo optimisations enabled\n\
\t\tall\t\tAll optimisations enabled\n\
\tOptimisations:\n\
\t\topt-pfx\t\toptimal prefixing (!)\n\
\t\trm-j0\t\tremove redundant jumps (*)\n\
\t\tdead-code\tdead code ellimination (*)\n\
\t\tdead-data\tdead data ellimination (*)\n\
\tPrefixing an optimisation with ~ disables that optimisation\n\
\tie '-x ~dead-code' will disable dead code ellimination\n\
\t* = This optimisation is not currently implemented and will\n\
\thave no effect\n\
\t! = This optimisation is probably unsafe at the moment!\n\
\tDefault: No optimisations enabled"
         (let ([set-all (lambda (value)
                          (set-opt-optimal-pfx! value)
                          (set-opt-rm-j0! value)
                          (set-opt-rm-dead-code! value)
                          (set-opt-rm-dead-data! value))])
           (cond
             [(string=? optimisation "none") (set-all #f)]
             [(string=? optimisation "all") (set-all #t)]
             [else 
              (let* ([value (not (string=? (substring optimisation 0 1) "~"))]
                     [str (if value optimisation (substring optimisation 1))])
                (cond
                  [(string=? str "opt-pfx") (set-opt-optimal-pfx! value)]
                  [(string=? str "j0") (set-opt-rm-j0! value)]
                  [(string=? str "dead-code") (set-opt-rm-dead-code! value)]
                  [(string=? str "dead-data") (set-opt-rm-dead-data! value)]
                  [else (raise-user-error (format "Optimisation: ~a not supported" optimisation))]))]))]
        
        
        )
       
       (args files 
             
             ;; 20070530 MCJ
             ;; We should bail out and give the user a clue if they don't include any
             ;; files on the command line.
             ;; Addresses #145 
             (when (null? files)
               (printf "~nThis tool expects one or more input files, most likely TCE files.~n~n")
               (printf "Run the command again with just the '-h' flag for more help.~n~n")
               
               (exit))
             
             ;; If everything is OK, go ahead.
             (set-input-files! (append *INPUT-FILES* files)))
       )))
  
  )
