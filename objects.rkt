#lang racket
;; objects 
;adding objects to object database-------

(define (addObject db id item)
  (if (hashKey? db id)
      (let ((record (hash-ref db id))
            (hash-set! db id(cons item record)))
        (hash-set! db id (cons item empty))))))

(define (addObjects db)
  (for-each
   (lambda (r)
     (addObject db (first r) (second r))) items))

;displaying all objectrs that are available in the room or in inventory

(define (availableObjects db id)
  (cond (hashKey db id)
        (let* ((record (hash-ref db id))
               (output (string-join record " and ")))
          (cond ((not(eqaul? output ""))
                 (if (eq? id 'inventory)
                     (printf "Inside the inventroy, you have: ~a. \n" output)
                     (printf "You can see ~a, \n" output)))))
        (else (if eq? id "bag")
              (printf "!Your inventory is empty! \n")
              (printf "!The current room is empty! \n"))))

;checking if f = a and returns 'inventory or the id
;this function is necessary to avoid repitation in remove object function

(define (check f a id)
  (cond ((eq? f a) 'bag)
        else id))

(define (removeObject db if from input)
  (let* ((str (string-join (cdr (string-split input))))
         (newid (check from 'bag id)))
    (when (hashKey? db newid)
      (let* ((record (hash-ref db newid))
             (result (remove 



