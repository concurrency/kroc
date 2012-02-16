#|
slinker - version.ss
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
(module version mzscheme
  
  (require
   (lib "pregexp.ss"))
  (provide (all-defined))
  
  
  ;;(define revision-string #f)
  (define revision-num-max 0)
  (define revision-num-min 0)
  
  (define version-num "0.5")
  (define c-year "2006")
  
  
  (define put-revision
    (lambda (revision-string)
      (let ([rev-num (string->number (pregexp-replace* "[^0-9]" revision-string ""))])
        (if (number? rev-num)
            (begin
              (if (> rev-num revision-num-max)
                  (set! revision-num-max rev-num))
              (if (or (= revision-num-min 0) (< rev-num revision-num-min))
                  (set! revision-num-min rev-num)))))))
  
  (define display-version
    (lambda (name)
      (if #t ;;(= revision-num-max revision-num-min)
          (printf "~a version ~a (Revision: ~a)~n" name version-num revision-num-max)
          ;; I dont like this display method, it gives no more information than
          ;; the above!
          (printf "~a version ~a (Revision: ~a-~a)~n" name version-num revision-num-min revision-num-max))
      (printf "Copyright ~a M. C. Jadud, C. L. Jacobsen - www.transterpreter.org~n" c-year)
      (printf "The slinker comes with ABSOLUTELY NO WARRANTY. This is free software, and you are welcome to redistribute it under the GNU Public License v2.~n")
      ))
  
  
  ;; Add this files revision to the version information
  (put-revision "$Revision: 3137 $")
  )
