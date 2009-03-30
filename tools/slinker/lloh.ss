(module lloh mzscheme
  (require (lib "class.ss"))
  
  (provide lloh% run-tests (all-from (lib "class.ss")))
  
  (define-struct list-node (prev next id obj))
  (define mz:cons cons)
  
  (define lloh%
    (class object%
      
      (init-field (<h> (make-hash-table))
                  (meta (make-hash-table))
                  (head #f)
                  (tail #f))
      
      ;; Metadata
      (define/public add-metadata 
        (lambda (k v)
          ;;(printf "Inserting ~a at ~a~n" v k)
          (hash-table-put! meta k v)))
      
      
      (define/public get-metadata 
        (lambda (k)
          (hash-table-get meta k (lambda () 'MetadataNotFound))))
      
      (define/public check-metadata 
        (lambda (func sym val)
          (func (get-metadata sym) val)))
      
      (define/public extract-all-metadata (lambda () meta))
      (define/public insert-all-metadata
        (lambda (h)
          (set! meta h)))
      
      ;; CONTRACT
      ;; <insert> : node -> void
      ;; PURPOSE
      ;; Inserts into the magic inner hashtable.
      (define (insert node)
        (let ([id (list-node-id node)])
          (hash-table-put! <h> id node)))
      
      (define (unsafe-remove id)
        (hash-table-remove! <h> id))
      
      (define (get-node node-id)
        (hash-table-get <h> node-id (lambda () #f)))
      
      (define/public (get node-id)
        (let ([n (hash-table-get <h> node-id (lambda () #f))])
          (if n (list-node-obj n) #f)))
      
      (define empty-node #f)
      
      ;; CONTRACT
      ;; cons : obj -> sym
      ;; PURPOSE
      ;; Attaches (destructively) an object to the head of the list.
      ;; Returns the id of the inserted object.
      (define/public (cons obj)
        (let ([head-node (if head (get-node head) head)]
              [id (gensym 'cons)])
          (cond
            [(not head-node)
             ;; Doubly-linked list; point head and tail at
             ;; new node
             (set! head id)
             (set! tail id)
             
             ;; Insert the new node
             (insert (make-list-node empty-node empty-node id obj))]
            
            [else
             ;; Point the head's previous to this node
             ;;(printf "Pointing the prev of [~a ~a] to [~a ~a]~n" (get head) head obj id)
             (set-list-node-prev! head-node id)
             
             ;; Insert the new node
             ;;                         prev  next me obj
             (insert (make-list-node empty-node head id obj))
             
             ;; Update the head pointer.
             (set! head id)
             id])))
      
      ;; CONTRACT
      ;; snoc : obj -> sym
      ;; PURPOSE
      ;; Attaches (destructively) an object to the tail of the list
      ;; Returns the ID of the inserted object.
      (define/public (snoc obj)
        (let ([tail-node (if tail (get-node tail) tail)]
              [id (gensym 'snoc)])
          (cond 
            [(not tail-node)
             ;; Adding to an empty list; same as cons. In fact,
             ;; I'll bounce it there.
             (cons obj)]
            [else
             ;; Point the tail's next to this node.
             (set-list-node-next! tail-node id)
             
             ;; Insert the new node
             ;;                         prev next  me obj
             (insert (make-list-node tail empty-node id obj))
             
             ;; Update the tail pointer
             (set! tail id)
             id
             ])))
      
      ;; CONTRACT
      ;; insert/after : id obj -> new-id
      ;; PURPOSE
      ;; Inserts the given object after the given id.
      (define/public (insert/after id obj)
        (let* ([n (get-node id)]
               [n++ (get-node (list-node-next n))]
               [new-id (gensym 'i/after)]
               [new-node (make-list-node #f #f new-id obj)])
          ;; Set the previous pointer of the new node to
          ;; the node we're going in after
          (set-list-node-prev! new-node (list-node-prev n++))
          
          ;; And set the new node's next pointer to
          ;; the next of the node we're going in after.
          (set-list-node-next! new-node (list-node-next n))
          
          ;; And set the next node's prev pointer to the new node
          (set-list-node-prev! n++ new-id)
          
          ;; And the current node's next pointer to the new node.
          (set-list-node-next! n new-id)
          
          ;; Insert the new node into the hash
          (insert new-node)
          
          ;; Return the id of the node we just inserted
          new-id
          ))
      
      
      ;; CONTRACT
      ;; insert/at : id obj -> new-id
      ;; PURPOSE
      ;; Inserts the given object at the given id.
      (define/public (insert/at id obj)
        (let* ([n (get-node id)]
               [n++ (get-node (list-node-next n))]
               [n-- (get-node (list-node-prev n))]
               [new-id (gensym 'i/at)]
               [new-node (make-list-node #f #f new-id obj)])
          ;; Set the previous pointer of the new node to
          ;; the node we're going in at
          (set-list-node-prev! new-node (list-node-prev n))
          
          ;; And set the new node's next pointer to
          ;; the next of the node we're going in at.
          (set-list-node-next! new-node (list-node-next n))
          
          ;; And set the next node's prev pointer to the new node
          (set-list-node-prev! n++ new-id)
          
          ;; And the previous node's next pointer to the new node.
          (set-list-node-next! n-- new-id)
          
          ;; Remove the old node, to be thorough
          (unsafe-remove id)
          
          ;; Insert the new node into the hash
          (insert new-node)
          
          ;; Return the id of the node we just inserted
          new-id
          ))
      
      ;; CONTRACT
      ;; remove id 
      ;; PURPOSE
      ;; Removes an element from the list.
      (define/public (remove id)
        (let* ([n (get-node id)]
               [n++ (get-node (list-node-next n))]
               [n-- (get-node (list-node-prev n))])

          ;; And set the next node's prev pointer to the new node
          (set-list-node-prev! n++ (list-node-id n--))
          
          ;; And the previous node's next pointer to the new node.
          (set-list-node-next! n-- (list-node-id n++))
          
          ;; Remove the old node, to be thorough
          (unsafe-remove id)
                    
          ))
      
      (define/public (replace id obj)
        (let* ([n (get-node id)])
          (set-list-node-obj! n obj)
          ))
      
      
      ;; CONTRACT
      ;; foreach : (ptr obj) -> void
      ;; foreach : sym (ptr obj) -> void
      ;; foreach : sym id id (ptr obj) -> void
      ;; PURPOSE
      ;; Does an ordered traversal of the list in the given
      ;; direction. Takes a function of two arguments, which
      ;; are the current pointer and the object at that location.
      (define/public foreach
        (case-lambda
          [(dir start end lam)
           (let ([acc (if (equal? dir '->)
                          list-node-next
                          list-node-prev)])
             (define (inner-walk current)
               ;;(printf "inner-walk: ~a~n" current)
               (cond
                 [(not current) (void)]
                 [else
                  (let ([n (get-node current)])
                    (lam current (list-node-obj n))
                    (inner-walk (acc n)))]))
             (inner-walk start))]
          [(dir lam) 
           (let ([start (if (equal? dir '->)
                            head
                            tail)]
                 [end (if (equal? dir '->)
                          tail
                          head)])
             (foreach dir start end lam))]
          [(lam)
           (foreach '-> head tail lam)]))
      
      
      (define/public (dump)
        (let loop ([nodeid head])
          (unless (not nodeid)
            (let ([n (get-node nodeid)])
              (printf "~a <- ~a -> ~a\t~a~n" 
                      (list-node-prev n)
                      (list-node-id n)
                      (list-node-next n)
                      (list-node-obj n))
              (loop (list-node-next n))))))
      
      (define/public (->list)
        (let ([ls '()])
          (let loop ([nodeid tail])
            (unless (not nodeid)
              (let ([n (get-node nodeid)])
                (set! ls (mz:cons (list-node-obj n) ls))
                (loop (list-node-prev n)))))
          ls))
      
      
      (define/public (find/first eh?)
        (let ([loc #f])
          (foreach '-> (lambda (ptr o)
                         ;;(printf "f/f ~a ~a~n" ptr o)
                         (if (eh? o)
                             (set! loc ptr))))
          loc))
      
      (define/public (find/last eh?)
        (let ([loc #f])
          (foreach '<- (lambda (ptr o)
                         (if (eh? o)
                             (set! loc ptr))))
          loc))
      
      (define/public find/all
        (case-lambda
          [(eh?)
           ;; This form returns the list of improper pairs
           (let ([loc (new lloh%)])
             (foreach '-> (lambda (ptr o)
                            (if (eh? o)
                                (send loc snoc (cons ptr o)))))
             loc)]
          [(key eh?)
           ;; This form carries the result in the metadata of the table.
           ;; It stores the result as another lloh%, but as improper pairs;
           ;; in particular, (uid . object)
           (let ([loc (new lloh%)])
             (foreach '-> (lambda (ptr o)
                            ;;(printf "f/a ~a ~a -> ~a~n" ptr o (eh? o))
                            (if (eh? o)
                                (send loc snoc (mz:cons ptr o)))))
             (add-metadata key loc))]))
      
      (define/public (length)
        (let ([l 0])
        (foreach (lambda (ptr o)
                   (set! l (add1 l))))
          l))
      
      (define/public (nuke-data)
        (set! <h> (make-hash-table))
        (set! head #f)
        (set! tail #f))      
              
      
      ;; Superclass initialization.
      (super-new)
      ))
  
  
  
  
  (define (run-tests)
    (begin
      
      (define (test assert test result)
        (printf "~a :: (~a ~a ~a)~n"
                (assert test result) assert test result))
      
      (let ([lloh (new lloh%)])
        
        (send lloh cons 'a)
        (send lloh cons 'b)
        (send lloh snoc 'c)
        (test equal? (send lloh ->list) '(b a c))
        
        (let ([an-a (send lloh find/first (lambda (o) (equal? o 'a)))])
          (send lloh insert/after an-a 'd)
          (test equal? (send lloh ->list) '(b a d c))
          (send lloh insert/at an-a 'e)
          (test equal? (send lloh ->list) '(b e d c)))
        
        
        ;; Finding things
        (send lloh cons 3)
        (send lloh snoc 5)
        (send lloh dump)

        ;; This form stores the value back into the metadata of the hash table
        (send lloh find/all 'num number?)
        (send lloh find/all 'sym symbol?)
        (test equal? (map cdr (send (send lloh get-metadata 'num) ->list)) '(3 5))
        (test equal? (map cdr (send (send lloh get-metadata 'sym) ->list)) '(b e d c)) )
      
      (for-each (lambda (iter)
                  (printf "Insert ~a elements.~n=====~n" iter)
                  (let ([lloh (new lloh%)])
                    (define (iota n)
                      (if (zero? n)
                          '()
                          (cons n (iota (sub1 n)))))
                    (time (for-each (lambda (n)
                                      (if (even? n)
                                          (send lloh snoc n)
                                          (send lloh cons n)))
                                    (iota iter)))
                    ;;(printf "~n~a~n" (send lloh ->list))
                    (printf "~n")))
                '(100 1000 10000 100000)) ;;1000000
                      
      
      ))
  
  )
