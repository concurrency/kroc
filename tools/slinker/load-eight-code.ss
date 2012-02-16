(module load-eight-code mzscheme
  (require "helpers.ss"
           "types.ss"
	   "version.ss"
           (lib "plt-match.ss")
           (lib "pregexp.ss"))
 
  (provide load-eight-code)
  
  ;; Add this files revision to the version information
  (put-revision "$Revision: 784 $")

 (define load-eight-code
   (lambda (f*)
     (map
      (lambda (f)
        (make-pass
         (make-hash-table)
         (make-bytes-table f)))
      f*)))
  
  
  (define make-bytes-table
    (lambda (f)
      (let ([ip (open-input-file f)]
            [bytes (make-hash-table)]
            [c 0])
        (for-each
         (lambda (inst)
           (hash-table-put! bytes c (match-byte-code inst))
           (set! c (+ 10 c)))
         (read ip))
        bytes)))
  
  
  (define lab->num
    (let ([h (make-hash-table)]
          [c 0])
      (lambda (lab)
        (if (hash-table-get h lab (lambda () #f))
            (begin
              (debug 'lec (printf "old ~a -> ~a~n" lab 
                                  (hash-table-get h lab)))
              (hash-table-get h lab))
            (begin
              (set! c (add1 c))
              (hash-table-put! h lab c)
              (debug 'lec (printf "new ~a -> ~a~n" lab c))
              c)
            ))))
      
  (define (e) (make-hash-table))
  (define match-byte-code
    (lambda (i)
      (match i
        [`(jump ,tgt)
          (make-j (e) (lab->num tgt)) ]
        [`(cj ,tgt)
          (make-cj (e) (lab->num tgt))]
        [`(label ,tgt)
          (debug 'lec (printf "setl: ~a[~a]~n" tgt (lab->num tgt)))
          (make-setlab (e) (lab->num tgt))]
        [`(stl ,tgt)
          (make-binary (e) (prefix #xD tgt))]
        [`(ldl ,tgt)
          (make-binary (e) (prefix #x7 tgt))]
        [`(ldc ,tgt)
          (make-binary (e) (prefix #x4 tgt))]
        [`(adc ,tgt)
          (make-binary (e) (prefix #x8 tgt))]
        [`(eqc ,tgt)
          (make-binary (e) (prefix #xC tgt))]
        [`(load-label-difference ,start ,end)
          (debug 'lec (printf "lld: ~a[~a] ~a[~a]~n" 
                              start 
                              (lab->num start)
                              end
                              (lab->num end)))
          (make-load-label-difference 
           (e) (lab->num start) (lab->num end)) ]
        [`(load-label-address ,tgt)
          (debug 'lec (printf "ll: ~a[~a]~n" tgt (lab->num tgt)))
          (make-load-label (e) (lab->num tgt))]
        [`(ldlp ,tgt)
          (make-binary (e) (prefix #x1 tgt))]
        [`(ajw ,tgt)
          (make-binary (e) (prefix #xB tgt))]
        [`(,simple)
         (let ([simp (lambda (op)
                       (make-binary (e) (prefix #xF op)))])
           (case  simple
             [(add)    (simp 5)]
             [(sub)    (simp #xC)]
             [(mul)    (simp #x53)]
             [(div)    (simp #x2C)]
             [(gt)     (simp #x9)]
             [(diff)   (simp #x4)]
             [(enbc)   (simp #x48)]
             [(disc)   (simp #x2F)]
             [(alt)    (simp #x43)]
             [(altend) (simp #x45)]
             [(altwt)  (simp #x44)]
             [(in)     (simp #x7)]
             [(out)    (simp #xB)]
             [(mint)   (simp #x42)]
             [(startp) (simp #xD)]
             [(endp)   (simp #x3)]
             [(ret)    (simp #x20)]
	     [(enbc)   (simp #x48)]
	     [(disc)   (simp #x2f)]
	     [(alt)    (simp #x43)]
	     [(altwt)  (simp #x44)]
	     [(altend) (simp #x45)]
	     [(not)    (simp #x32)]
	     [else (error 'load-eight "Unhandled: ~a~n" simple)]
             ))]
        )))
        
        
  
)
