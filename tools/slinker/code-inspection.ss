#|
slinker - code-inspection.ss
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
(module code-inspection mzscheme
  (require "helpers.ss"
	   "version.ss")
  (provide (all-defined))
  
  ;; Add this files revision to the version information
  (put-revision "$Revision: 3119 $")

  (define coverage
    (lambda (h)
      (let ([used (make-hash-table)]
            [tot 0])
        (hash-table-for-each
         h
         (lambda (k v)
           (let ([sym (vector-ref (struct->vector v) 0)])
             (set! tot (add1 tot))
             (hash-table-put! 
              used
              sym (add1 
                   (hash-table-get 
                    used sym (lambda () 0)))))))
        
        (let ([total 0]
              [strpad (lambda (n)
                        (make-string n #\space))])
          (debug 'inspection (printf "~a~a~a~n" "Inst" (strpad (- 36 4)) "n")
		 (printf "~a~n" (make-string 38 #\-)))
          (hash-table-for-each
           used
           (lambda (k v)
             (debug 'inspection
		    (printf " ~a~a~a~n"
			    k (strpad (- 35 (string-length 
					     (symbol->string k)))) v)
		    (printf "~a~a~a~n" 
			    "Total" (strpad (- 36 5)) tot))
          ))
      h))
  )))