#|
tinyswig - tinyswig-tests.scm
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

;; To run these tests, use the following command line:
;;   mzscheme -u tinyswig-tests.scm

(module tinyswig-tests mzscheme
  ;; This brings in the unit test library
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2 6))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2)))
  ;; This is the file I want to test
  (require (file "tinybase.scm"))

  
  (define list-intersperse-tests
    (test-suite
     "list-intersperse Tests"
     (test-equal? "Zero-element 1"
                  (list-intersperse '() ", ")
                  '())
     (test-equal? "One element 1"
                  (list-intersperse '("a") ", ")
                  (list "a"))
     (test-equal? "Two element 1"
                  (list-intersperse '("a" "b") ", ")
                  (list "a" ", " "b"))
    ))
  
  
  ;; This is the test suite. It could be broken up (later).
  (define tinyswig-tests
    (test-suite
     "All tinyswig tests"
     
     (test-case
      "Fail to match"
      (check-false (match-line "This is a fail-test.") #f))
     
     list-intersperse-tests
     
     ;; End of test suite.
     ))
  
  (test/text-ui tinyswig-tests)
  )