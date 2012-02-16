#|
schemescanner - schemescanner.scm
A funny little dependency scanner.
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
(module schemescanner mzscheme
  (require (lib "cmdline.ss")
           (lib "plt-match.ss"))

  ;; Flag used to decide if we should recur into dependent files and find their
  ;; depdencies too (and so on)
  (define recurse #f)
  ;; Flag used to decide if we should output something that make is going to
  ;; like, or just the list of dependent files
  (define make-output #f)
  ;; Used if the user wants a different make target than usual
  (define make-target #f)
  ;; Should we generate an entry (when make-output is #t) for a svn-revision
  ;; file that depends on everything except itself
  (define svn-revision-file #f)

  (define (get-deps sexp)
    (match sexp
      [`(module ,name ,mzscheme ,bodies ...)
        (map get-deps bodies)]
      [`(require ,reqs ...)
        (map get-deps reqs)]
      [`(file ,(? string? str))
        (if (and recurse (file-exists? str))
          (list str (get-deps (read (open-input-file str))))
          str)]
      [(? string? o)
        (if (and recurse (file-exists? o))
          (list o (get-deps (read (open-input-file o))))
          o)]
      [else #f]))
       
    
  (define (flatten ls)
    (cond
      [(null? ls) '()]
      [(list? (car ls))
       (append (flatten (car ls))
               (flatten (cdr ls)))]
      [else
       (cons (car ls) (flatten (cdr ls)))]))
  
  (define (filter pred? ls)
    (cond
      [(null? ls) '()]
      [(pred? (car ls))
       (cons (car ls)
             (filter pred? (cdr ls)))]
      [else
       (filter pred? (cdr ls))]))
 
  (define (find-extension-start filename)
    (let loop ([n (- (string-length filename) 1)])
      (cond
        [(= -1 n) (string-length filename)]
        [(equal? #\. (string-ref filename n)) n]
        [else (loop (- n 1))])))

  (define (the-line the-args)
    (command-line
     "scheme-deps"
     the-args
     (once-each
       [("-r" "--recurse")
        "Recursively find dependencies. Beware the circular dependency dragon!"
        (set! recurse #t)]
       [("-m" "--make")
        "Output something that make will be happy with, for the target: filename, with no extension"
        (set! make-output #t)]
       [("--make-target")
        target
        "Use 'target' as the make target"
        (set! make-target target)]
       ["--svn-revision-file"
        name
        "Make an entry an svn-revision called 'name' file which depends on everything (except itself)"
        "Used on conjunction with --make"
        (set! svn-revision-file name)]
       )
     (args
      (filename)

      ;; If outputting for make, print the input filename (minus extension) and then :
      ;; Also, output the intput filename as part of the dependencies
      (if make-output (printf "~a: ~a " 
                              (if make-target make-target (substring filename 0 (find-extension-start filename))) 
                              filename))
      (if (file-exists? filename)
          (let ({res
                 (get-deps 
                  (read 
                   (open-input-file filename)))
                 })
            ;; Always print the list of files we found (assuming we found any)
            (map (lambda (o)
                   (printf "~a " o)) 
                 (filter string? (flatten res)))
            ;; If outputting for make
            (if make-output 
              (begin
                ;; Print all the dependencies one on a line with a colon:
                ;; This is to appease makes dependency tracker if a file is removed
                (printf "~n")
                (map (lambda (f)
                       (printf "~a:~n" f))
                     (filter string? (flatten res)))
                ;; If we are using an svn-revision-file, then make one last dependency
                ;; entry with the depdencencies of the input file, but using
                ;; svn-revision-file as the dependent (and exclude svn-revision-file from
                ;; the list)
                (if svn-revision-file
                  (begin
                    (printf "~a: .svn " svn-revision-file)
                    (map (lambda (f)
                           (printf "~a " f))
                         (filter (lambda (x) (not (equal? svn-revision-file x))) (filter string? (flatten res))))
                    (printf "~n.svn:~n")
                    ))))))
        )))
  
  (the-line (current-command-line-arguments))
    )
