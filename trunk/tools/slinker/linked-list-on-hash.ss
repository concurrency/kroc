(module linked-list-on-hash mzscheme
  (require (lib "class.ss"))
  
  ;; Using the structure
  ;;
  ;; To create a new linked list, use the "new" constructor.
  ;; (define lloh (new linked-list%))
  ;;
  ;; To add metadata to the list, use the "add-metadata" method
  ;; (send lloh add-metadata 'filename <filename>)
  ;; The implementation of metadata is just a hash table; 
  ;; currently, it doesn't seem like we need anything more.
  ;;
  ;; To insert an instruction, you have to decide where it will go.
  ;; Typically, this is by way of insert/after. With an empty list,
  ;; you begin with the head of the list.
  ;;
  ;; (send lloh head)
  ;; returns the start of the list.
  ;;
  ;; (send lloh insert/after (send lloh head) <a byte or instruction>)
  ;; "insert/after" returns a "pointer" to the instruction you just inserted.
  ;; This way, you can set up a loop like
  ;;
  #|
            (let loop ([a-byte (read-byte ip)]
                       [end-of-list (send IL head)])
              (unless (eof-object? a-byte)
                (loop (read-byte ip)
                      (send IL insert/after end-of-list a-byte)))) )  
  |#
  ;; and load a whole bunch of poo into the lloh.
  ;;
  ;; To follow: insert/replace, insert/after/many, etc.
  ;;
  ;; To locate particular things in the stream, use "walk"
  ;; For example:
  ;;
  ;; (let ([c 0]) 
  ;;   (send lloh walk (lambda (ptr i) (set! c (add1 c))))
  ;;   c)
  ;; will visit every instruction in the lloh, and increment
  ;; the variable "c". It's kinda like "for-each". Also, you
  ;; get the ptr to the current element; this is useful for building
  ;; up a list of locations; for example, you might map out
  ;; the start points for all the procs, or all the labels in the 
  ;; instruction stream, and so on. The ptrs are guaranteed to
  ;; be globally unique. This ptr is usable with "insert/after", for
  ;; example.
  
  (provide 
   linked-list%
   element-obj
   (all-from (lib "class.ss"))
   
   )
  
  (define-struct element (uid pooters obj) (make-inspector))
  (define-struct pooters (next prev) (make-inspector))
  (define-struct LL (metadata contents))
  
  (define (next-uid) (gensym 'uid))
  
  (define linked-list%
    (class object%
      ;;(public insert/after walk add-metadata get-metadata check-metadata length head)
      
      ;; Constructors
      (define (new-il)
        (let* ([h (make-hash-table)]
               [root (make-root h)]
               [meta (make-hash-table)])
          (hash-table-put! meta 'root root)
          (make-LL meta h)))  
      
      (define (make-root h)
        (let* ([uid (next-uid)]
               [e (make-element uid (make-pooters (void) 'root) 'root)])
          (hash-table-put! h uid e)
          e))
      
      ;; Global, private field
      (define *il* (new-il))
      
      ;; Metadata
      (define/public add-metadata 
        (lambda (k v)
          (let ([m (LL-metadata *il*)])
            (hash-table-put! m k v)
            (set-LL-metadata! *il* m))))
        
      (define/public get-metadata 
        (lambda (k)
          (hash-table-get (LL-metadata *il*) k)))
      
      (define/public check-metadata 
        (lambda (func sym val)
          (func (get-metadata sym) val)))
      
      ;; Properties
      ;; The empty list always has a root, which
      ;; is not part of the length.
      (define/public length
        (lambda ()
          (let ([c 0])
            (walk (lambda (x) (set! c (add1 c))))
            (sub1 c))))
      
      (define/public head
        (lambda ()
          (get-metadata 'root)))
      
      ;; Interaction
      (define/public walk 
        (case-lambda
          [(lam)
           (define (walk* h elem)
             ;; Apply the lambda to the uid and the object stored
             (lam (element-uid elem) (element-obj elem))
             (let ([next (pooters-next (element-pooters elem))])
               (unless (void? next)
                 (walk* h (hash-table-get h next)))))
           (walk* (LL-contents *il*) (get-next-element (head)))]))
      
      
      (define (get-next-element uid)
        (hash-table-get (LL-contents *il*) 
                        (get-next uid)))
  
      (define/public insert/after 
        (lambda (elem o)
          (let* ([h (LL-contents *il*)]
                 [new-uid (next-uid)]
                 [new-elem (make-element new-uid (make-pooters (get-next elem) (element-uid elem)) o)])
            ;; The element we're going before needs to point back to us
            ;; Unless, of course, we're the end of the line.
            (if (not (void? (get-next elem)))
                (set-prev! (hash-table-get h (get-next elem)) (element-uid new-elem)))
            (set-next! elem (element-uid new-elem))
            (hash-table-put! h new-uid new-elem)
            (set-LL-contents! *il* h)
            new-elem
            )))
      
      
      ;; Accessors
      (define (get-next e)
        (pooters-next (element-pooters e)))
      
      ;; Mutators
      (define (set-prev! e id)
        (let ([ptrs (element-pooters e)])
          (set-pooters-prev! ptrs id)
          (set-element-pooters! e ptrs)))
      
      (define (set-next! e id)
        (let ([ptrs (element-pooters e)])
          (set-pooters-next! ptrs id)
          (set-element-pooters! e ptrs)))
      
      ;; Initialize
      (super-new)
      ))
  
  )