#lang racket

;this mud game runs if these libraro
(require srfi/1)
(require srfi/13)
(require srfi/48)

;-----------------room descriptions-------;
(define room-type '((0 "Reception") ;room type = 0 == reception
                    (1 "corridor")
                    (2 "ballroom")
                    (3 "kitchen")
                    (4 "car park" )
                    (5 "indoor garden")
                    (6 "backstage")
                    (7 "stage")
                    (8 "dressing room")
                    (9 "bar")
                    (10 "resturant")))

;--------------object despriptions--------;
(define objects '((0 "a silver dagger") ;object = 0 == sliver dagger 
                  (1 "a gold coin") 
                  (2 "a long sword")
                  (3 "a rope")
                  (4 "a statue")
                  (5 "a statue")
                  (6 "a statue")
                  (7 "a shield")
                  (9 "a statue")))

;-statue description to unlock the gate----;
(define key_objects '((0 "a red statue") ;the key-objects used to unlock the gate is randomised with objects and rooms.
                      (1 "a bronze statue")
                      (2 "a sliver statue")
                      (3 "a gold statue")))

;----------------action pairs-------------------;
(define look '(((directions) look) ((look) look) ((examine room) look))) ;explore the game scene command
(define quit '(((exit game) quit) ((quit game) quit) ((exit) quit) ((quit) quit))) ;quit game command
(define pick '(((get) pick ) ((pickup) pick) ((pick) pick))) ;pick up object command
(define directions '(((south) direction) ((north) direction) ((west) direction) ((east) direction))) ;navigating commands
(define put '(((put) drop) ((drop) drop) ((place) drop) ((remove) drop))) ;dropping object
(define inventory '(((inventory) inventory) ((bag) inventory) ((backpack) inventory))) ;checking objects thst's stored in inventory 
(define mazemap '(((map) mazemap) ((show map) mazemap)((see map) mazemap) ((look map) mazemap))) ;checking the map of the maze
(define actions `(,@look ,@quit ,@pick ,@put ,@inventory ,@directions ,@mazemap)) ;definig actions

;---------------------navigating table that allows user to make decision on which path to take---; 
(define decisiontable `((1 ,@actions)
                        (2 ((south) 1) ,@actions )
                        (3 ,@actions)))

;----maze function starts-----------------;
;~~~user config~~~;
(define X 8) ;limit maze size to 8 squares along the x axis.
(define Y 6) ;limits maze size to 6 squares along the y axis.


;maze algorithm with X and Y as M and N .

(define (paths start) ; the path of the maze
  (match-define (maze N M tbl) m)
  (map (lambda (x)
         (let ((first (map = start x))
               (second (map < start x)))
           (cond [(car first)
                  (if (cadr second) 'south 'north)]
                 [else
                  (if (car second) 'east 'west)])))
       (connections tbl start)))
                                
;the structure representing a maze of size N times M which is 8*6
(struct maze (N NM tbl))
;; a dictionary is an instance of a datatype that 
;; e. g. hash table, list, structures
(define (connections tbl c) (dict-ref tbl c '()))

;;dict-set! maps keys to v in dict, overwriting any existing mapping for key
(define (connect! tbl c n)
  (dict-set! tbl c (cons n (connections tbl c)))
  (dict-set! tbl n (cons c (connections tbl n))))

(define (connected? tbl a b) (member a (connections tbl b)))

;; returns a maze of a given size
;; build-maze :: Index Index -> Maze
(define (build-maze M N)
  (define tbl (make-hash))
  (define (visited? tbl c) (dict-has-key? tbl c))
  (define (neigbours c)
    (filter
     (match-lambda [(list i j) (and (<= 0 i (- N 1)) (<= 0 j (- M 1)))])
     (for/list ([d '((0 1) (0 -1) (-1 0) (1 0))]) (map + c d))))
  ;;generate the maze
  (let move-to-cell ([c (list (random N) (random M))])
    (for ([n (shuffle (neigbours c))] #:unless (visited? tbl n))
      (connect! tbl c n)
      (move-to-cell n)))
  ;;return the result
  (maze N M tbl))

;;show a maze
(define (show-maze m pos)
  (match-define (maze X Y tbl) m)
  (for ([i X]) (display "+---"))
  (displayln "+")
  (for ([j Y])
    (display "|")
    (for ([i (- X 0)])
      (if (equal? (list i j) pos)
          (display " *")
          (display "  "))
      (if (connected? tbl (list i j) (list (+ 1 i ) j))
          (display "  ")
          (display " |")))
   
    (newline)
    (for ([i X])
      (if (connected? tbl (list i j) (list i (+ j 1)))
          (display "+   ")
          (display "+---")))
    (displayln "+")))

;deal with room change
;the maze is build from top left (0 0)
(define (move-room room input)
               (cond [(eq? input 'south)
                      (move-x room +)]
                     [(eq? input 'north)
                      (move-x room -)]
                     [(eq? input 'west)
                      (move-y room -)]
                     [(eq? input 'east)
                      (move-y room +)]))

(define (move-x room fun)
  (cons (car room) (map (lambda(x) (fun x 1)) (cdr room))))

(define (move-y room fun)
  (cons (fun (car room) 1) (cdr room)))

(define (add-object db id object)
  (if (hash-has-key? db id)
      (let ((record (hash-ref db id)))
        (hash-set! db id (cons object record)))
      (hash-set! db id (cons object empty))))

(define (add-objects db)
  (for-each
   (lambda (r)
     (add-object db (first r) (second r))) objects))

;display objects that are in the room or in the bag
(define (display-objects db id)
  (cond ((hash-has-key? db id)
         (let* ((record (hash-ref db id))
                (output (string-join record " and ")))
           (cond ((not(equal? output ""))
                       (if (eq? id 'bag)
                           (printf "You are carrying ~a. \n" output)
                           (printf "You can see ~a. \n" output))))))
        (else
         (if (eq? id 'bag)
             (printf "Your bag is empty! \n")
             (printf "The room is empty! \n")))))



;;check if a equals b and return 'bag or the id
;this function is necessary to avoid repetition in the
;remove-object function
(define (evaluate a b id)
  (cond ((eq? a b)
       'bag)
        (else
         id)))


(define (remove-object db id from input)
  (let*((str (string-join (cdr (string-split input)))) 
        (newid (evaluate from 'bag id))) 
    (when (hash-has-key? db newid)
      (let* ((record (hash-ref db newid))
                 (result (remove (lambda (x) (string-suffix-ci? str x)) record))
                 (item (lset-difference equal? record result)))
        (cond ((null? item)
               (printf "I don't see that item in the ~a! \n" from))
              (else
               (cond((eq? from 'room)
                     (printf "Added ~a to your bag.\n" (first item))
                     (add-object inventorydb 'bag (first item))
                     (hash-set! db id result))
                    (else
                     (printf "Removed ~a from your bag . \n" (first item))
                     (add-object objectdb id (first item))
                     (hash-set! db 'bag result)))))))))



(define (handle-item from id input)
  (if(eq? from 'bag)
    (remove-object inventorydb id 'bag input)
    (remove-object objectdb id 'room input)))


(define (display-inventory)
  (display-objects inventorydb 'bag))


;;END OF OBJECTS FUNCTIONS

;; DEFINITIONS OF DATABASES AND MAP

(define objectdb (make-hash))  ;;define object hash
(define inventorydb (make-hash)) ;;define bag hash
(define rooms (make-hash)) ;; define hash for carry the rooms names
(define m (build-maze X Y)) ;;build the maze
(define gatekey "")
;; END OF DEFINITIONS



; THIS FUNCTION WILL DEFINE THE START POINT
(define (startpoint)
  (let*((start_x (random X))
        (start_y (random Y)))
  (list start_x start_y)))

;; refactored functions assq-ref and assv-ref into only one ass-ref
;; we pass what we want as parameter (assq or assv)
(define (ass-ref assqlist id x)
  (cdr (x id assqlist)))

;(random-allocator)
;randomly allocates something to a position in the maze
;a rate can be applied to allocate only to some cells (rooms)
;for instance: if the rate is 50, a room will have 50%
;chance of have a random item.
(define (random-allocator db types rate)
  (for ((j X))
    (for ((i Y))
      (cond ((<= (random 100) rate)
             (cond((equal? db rooms) ; add the name to the room
                   (hash-set! db (list j i) (car( ass-ref types (random (- (length types) 1)) assq))))
                  (else ;add to objectdb
                   (add-object db (list j i) (car (ass-ref types (random (- (length types) 1)) assq))))))))))


;will place one unit of each type of key randomly on the maze
(define (random-key-location db types)
  (for ((i (length types)))
    (add-object db (list (random X) (random Y)) (car (ass-ref types i assq)))))





;;get the keywords on association table
(define (get-keywords id)
  (let ((keys (ass-ref decisiontable id assq)))
    (map (lambda (key) (car key)) keys)))


;; outputs a list in the form: (0 0 0 2 0 0) based on some weightening
(define (list-of-lengths keylist tokens)
  (map 
   (lambda (x)
     (let ((set (lset-intersection eq? tokens x)))
       ;; apply some weighting to the result
       (* (/ (length set) (length x)) (length set))))
   keylist))


;return the index of the highest number on the list provided by the function
;(list-of-lenghts)
(define (index-of-largest-number list-of-numbers)
  (let ((n (car (sort list-of-numbers >))))
    (if (zero? n)
        #f
        (list-index (lambda (x) (eq? x n)) list-of-numbers))))


             

;;Receive a function as parameters so it can be reused
;;this function can both get the actions and words attached to it
;;depending on the function passed
(define (call-actions id tokens func)
  (let* ((record (ass-ref decisiontable 1 assv)) ;;get the references
         (keylist (get-keywords 1)) ;;get the keywords
         ;;description in the functions
         (index (index-of-largest-number (list-of-lengths keylist tokens)))) 
    (if index 
        (func (list-ref record index)) ;;return result if match, return false if dont
        #f)))


;;THIS FUNCTION WILL EVALUATE IF THE USER HAVE THE KEY NECESSARY TO OPEN THE GATE
(define (door-handle gatekey)
  (printf "You can see the exit gate, but it is locked. \n")
  (cond ((hash-has-key? inventorydb 'bag)
         (let* ((record (hash-ref inventorydb 'bag)) ;;get items list in bag
                (result (remove (lambda (x) (string-suffix-ci? gatekey x)) record)) ;;result = record - bag
                (item (lset-difference equal? record result))) ;; compare them
           (cond ((null? item) ;;if there is no difference, the key was removed, return true
               #t))))
        (else
         #f)))



;;START OF ALLOCATION OF ITENS AND ROOM NAMES
(random-allocator rooms room-type 100)       ;;allocate names to the rooms
(random-allocator objectdb objects 50)       ;;allocate items to the rooms
(random-key-location objectdb key_objects)   ;;allocate keys to the rooms
;;END OF ALLOCATION

;; ADVANCED COMMAND LINE PROCESSOR WITH MAZE
(define (startgame-maze)
  (let* ((gatekey (car (ass-ref key_objects (random(length key_objects)) assq)))
         (gate_x (random X))
         (gate_y (random Y))
         (start (startpoint)))
   ;;the following prints will help with testing, telling the developer where the gate is located and what key is the right one
    (printf "~a \n" gate_x)
    (printf "~a \n" gate_y)
    (printf "~a \n" gatekey)
    (printf "~a \n " start)
    (let loop ((rid start))    
      (printf "You are in the ~a \n>" (hash-ref rooms rid))
      (let* ((input (read-line))
             (string-tokens (string-tokenize input))
             (tokens (map string->symbol string-tokens))
             (response (call-actions rid tokens cadr))) ;;get action

      
        (cond ((eq? response 'direction)
               (let* ((direction (call-actions rid tokens caar)) ;get direction typed
                      (newlocation (move-room rid direction)))  ;get future location after move
                 (cond((member direction (paths rid)) ;check if direction is in path
                       (cond ((equal? newlocation (list gate_x gate_y)) ;end of game condition
                              (cond ((not (door-handle gatekey))
                                     (printf "It seems that you don't have the key to open the gate. \n")
                                     (loop newlocation))
                                    (else
                                     (printf "You used the key to open the gate. You are free! \n")
                                     (exit))))
                         (else
                          (loop newlocation))));;not in the gate
   
                      (else ;;direction not in path
                       (printf "You can not go that way!\n")
                       (loop rid)))))
            
              ((eq? #f response) ;unknow
               (format #t "I am sorry, but I didn't understand that!\n")
               (loop rid))
            
              ((eq? response 'look)
              (show-maze m rid)
               (display-objects objectdb rid)
               (loop rid))
              ((eq? response 'mazemap)
               (show-maze m rid)
              ;(display-objects objectdb rid)
               (loop rid))
            
              ((eq? response 'pick)
             ;remove item from room and put into inventory
               (handle-item 'room rid input)
               (loop rid))
            
              ((eq? response 'inventory)
               (display-inventory) ;show inventorydb
               (loop rid))
            
              ((eq? response 'quit)
               (format #t "So Long, and Thanks for All the Fish...\n")
               (exit))
            
              ((eq? response 'drop)
               ;remove item from inventory and drop on the current room
               (handle-item 'bag rid input)
               (loop rid)))))))

;(startgame-new start)
(startgame-maze)