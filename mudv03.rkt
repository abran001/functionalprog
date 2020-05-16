#lang racket
;dependencies
(require srfi/1)
(require srfi/13)

;;directions, objects and actions;; MUD 

;---defining objects------------------;
(define objects '((1 "a sword ")
                  (2 "a gold coin")
                  (5 "a cake")
                  (8 "a shield")
                  (9 "a statue")))
                  
;-------defining locations---------------------------------;
(define descriptions '((1 "You are in the lobby.")
                       (2 "You are in the hallway.")
                       (3 "You are in a swimming pool.")
                       (4 "You are in the bathroom.")
                       (5 "You are in the dining room.")
                       (6 "You are in the kitchen.")
                       (7 "You are in the basement.")
                       (8 "You are in the garden.")
                       (9 "You are in the bedroom.")
                       (10 "You are in the treasure room.")))


;---------------------action lists-----------------------------------------------------;
(define look '(((directions) look) ((look) look) ((examine room) look)))
(define quit '(((exit game) quit) ((quit game) quit) ((exit) quit) ((quit) quit)))
(define pick '(((get) pick) ((pickup) pick) ((pick) pick)))
(define put '(((put) drop) ((drop) drop) ((place) drop) ((remove) drop)))
(define inventory '(((inventory) inventory) ((bag) inventory)))
(define actions `(,@look ,@quit ,@pick ,@put ,@inventory))


;-----------------------------directions----------------------------------;
(define decisiontable `((1 ((south) 2) ((west) 4) ((east) 3) ,@actions)
                        (2 ((north) 1) ((south) 5) ,@actions)
                        (3 ((north) 0) ((south) 0) ((west) 2) ((east) 0) ,@actions)
                        (4 ((north) 0) ((south) 0) ((west) 0) ((east) 2) ,@actions)
                        (5 ((north) 2) ((south) 6) ((west) 0) ((east) 0) ,@actions)
                        (6 ((north) 5) ((south) 0) ((west) 8) ((east) 7) ,@actions)
                        (7 ((north) 0) ((south) 0) ((west) 6) ((east) 0) ,@actions)
                        (8 ((north) 0) ((south) 9) ((west) 0) ((east) 0) ,@actions)
                        (9 ((north) 8) ((south) 0) ((west) 0) ((east) 10) ,@actions)
                        (10 ((north) 0) ((south) 0) ((west) 9) ((east) 0) ,@actions)))




;;Database loading

;----object and invetory databases stored in hash tables--------;
(define objectdb (make-hash))
(define inventorydb (make-hash))

;-----------add object to object database----------------------;
(define (add-object db id object)
  (if (hash-has-key? db id)
      (let ((record (hash-ref db id)))
        (hash-set! db id (cons object record)))
      (hash-set! db id (cons object empty))))

(define (add-objects db)
  (for-each
   (lambda (r) 
     (add-object db (first r) (second r))) objects))

(add-objects objectdb)

;---------displaying objects------------------------------;
(define (display-objects db id)
  (when (hash-has-key? db id)
    (let* ((record (hash-ref db id))
           (output (string-join record " and ")))
      (when (not (equal? output ""))
        (if (eq? id 'bag)
            (printf "You are carrying ~a.\n" output)
            (printf "You can see ~a.\n" output))))))

;----------removing objects from the room----------------------------------;
(define (remove-object-from-room db id str)
  (when (hash-has-key? db id)
    (let* ((record (hash-ref db id))
           (result (remove (lambda (x) (string-suffix-ci? str x)) record))
           (item (lset-difference equal? record result)))
      (cond ((null? item) 
             (printf "I don't see that item in the room!\n"))
            (else
             (printf "Added ~a to your bag.\n" (first item))
             (add-object inventorydb 'bag (first item))
             (hash-set! db id result))))))

;------------removing objects from the inventory--------------------------;
(define (remove-object-from-inventory db id str)
  (when (hash-has-key? db 'bag)
    (let* ((record (hash-ref db 'bag))
           (result (remove (lambda (x) (string-suffix-ci? str x)) record))
           (item (lset-difference equal? record result)))
      (cond ((null? item)
             (printf "You are not carrying that item!\n"))
            (else
             (printf "Removed ~a from your bag.\n" (first item))
             (add-object objectdb id (first item))
             (hash-set! db 'bag result))))))

;-----picking item from rooom-----------------;             
(define (pick-item id input)
  (let ((item (string-join (cdr (string-split input)))))
    (remove-object-from-room objectdb id item)))

;----------placing item to inventory-----------------------;
(define (put-item id input)
  (let ((item (string-join (cdr (string-split input)))))
    (remove-object-from-inventory inventorydb id item)))

;-------displaying objects stored in the bag-------;
(define (display-inventory)
  (display-objects inventorydb 'bag))

;--converting list to string------------;
(define (slist->string l)
  (string-join (map symbol->string l)))

;--navigation---------------;
(define (get-directions id)
  (let ((record (assq id decisiontable)))
    (let* ((result (filter (lambda (n) (number? (second n))) (cdr record)))
           (n (length result)))
      (cond ((= 0 n)
             (printf "You appear to have entered a room with no exits.\n"))
            ((= 1 n)
             (printf "You can see an exit to the ~a.\n" (slist->string (caar result))))
            (else
             (let* ((losym (map (lambda (x) (car x)) result))
                    (lostr (map (lambda (x) (slist->string x)) losym)))
               (printf "You can see exits to the ~a.\n" (string-join lostr " and "))))))))

(define (assq-ref assqlist id)
  (cdr (assq id assqlist)))

(define (assv-ref assqlist id)
  (cdr (assv id assqlist)))

(define (get-description id)
  (car (assq-ref descriptions id)))

(define (get-keywords id)
  (let ((keys (assq-ref decisiontable id)))
    (map (lambda (key) (car key)) keys)))


;; outputs a list in the form: (0 0 0 2 0 0)
(define (list-of-lengths keylist tokens)
  (map 
   (lambda (x)
     (let ((set (lset-intersection eq? tokens x)))
       ;; apply some weighting to the result
       (* (/ (length set) (length x)) (length set))))
   keylist))

(define (index-of-largest-number list-of-numbers)
  (let ((n (car (sort list-of-numbers >))))
    (if (zero? n)
      #f
      (list-index (lambda (x) (eq? x n)) list-of-numbers))))


(define (lookup id tokens)
  (let* ((record (assv-ref decisiontable id))
         (keylist (get-keywords id))
         (index (index-of-largest-number (list-of-lengths keylist tokens))))
    (if index 
      (cadr (list-ref record index))
      #f)))

(define (display-description id)
  (printf "~a\n" (get-description id)))

;------------Main game loop--------------------------------------------------------------------------;
(define (startgame initial-id) ;intial room (id)
  (let loop ((id initial-id) (description #t))
    (when description
      (display-description id)
      (display-objects objectdb id))
    (printf "> ")
    (let* ((input (read-line))
           (string-tokens (string-tokenize input))
           (tokens (map string->symbol string-tokens)))
      (let ((response (lookup id tokens)))
        (cond ((number? response)
               (loop response #t))
              ((eq? #f response)
               (printf "huh? I didn't understand that!\n")
               (loop id #f))
              ((eq? response 'look)
               (get-directions id)
               (loop id #t))
              ((eq? response 'pick)
               (pick-item id input)
               (loop id #f))
              ((eq? response 'drop)
               (put-item id input)
               (loop id #f))
              ((eq? response 'inventory)
               (display-inventory)
               (loop id #f))              
              ((eq? response 'quit)
               (printf "So Long, and Thanks for All the Fish...\n")
               (exit)))))))
(startgame 1)
