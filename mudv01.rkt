#lang racket

;; The first version of MUD game


(define descriptions '( (1 "You are in the entance. This is where the game starts. You can see an exit to the south")
                        (2 "You are in the hallway there are exits to the north, east, south and west")
                        (3 "You are in the living room, there is an ")
                        (4 "You are in the bathroom, there is an exit in east")
                        (5 "You are in the Dining room, there is an exit to ")
                        (6 "You are in the kitchen, there is  ")
                        (7 "You are in the basement ")
                        (8 "You are in garden" )
                        (9 "You are in bedroom" )
                        (10 "You are in the secret room")))
                        

(define directions '( (1 (north 0) (south 2) (east 0) (west 0))
                      (2 (north 1) (south 5) (east 3) (west 4))
                      (3 (north 0) (south 0) (east 0) (west 2))
                      (4 (north 0) (south 0) (east 2) (west 0))
                      (5 (north 2) (south 6) (east 0) (west 0))
                      (6 (north 5) (south 0) (east 7) (west 8))
                      (7 (north 0) (south 0) (east 0) (west 6))
                      (8 (north 0) (south 9) (east 0) (west 0))
                      (9 (north 8) (south 0) (east 10) (west 0))
                      (10 (north 0) (south 0) (east 0) (west 10)) ))
                      

(define (assq-ref assqlist id)
  (cdr (assq id assqlist)))

(define (get-room-description rid)
  (car (assq-ref descriptions rid)))

(define (lookup room-id direction)
  (car (assq-ref (assq-ref directions room-id) direction)))

(define (startgame room-id)
  (let loop ((rid room-id))
    (printf "~a\n" (get-room-description rid))
    (printf "> ")
    (let ((input (read)))
      (if (eq? input 'quit) (exit) 'continue)
      (if (member input '(north south east west))
          (let ((direction (lookup rid input)))
            (if (zero? direction)
                (loop rid)
                (loop direction)))
          (begin
            (printf "huh? I didn't understand: ~a\n" input)
            (loop rid))))))

(startgame 1)
