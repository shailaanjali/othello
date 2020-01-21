#lang typed/racket
(require typed/test-engine/racket-tests)

;;;;;;;;;;;;;;



;;Shaila Sundram
;; PROJECT


;;;;;;;;;;;;;;


(define-type Player (U 'black 'white))

(define-struct Pos
  ([row : Integer]  ;; an integer on the interval [0,7]
   [col : Integer]) ;; an integer on the interval [0,7]
  #:transparent)

(define-struct Board
  ([squares : (Listof (U Player 'none))]) ;; a list of length 64
  #:transparent)

(define-struct Game
  ([board : Board]
   [next  : Player])
  #:transparent)


;.......................


(: sample-pos : Pos)
(define sample-pos (make-Pos 1 3))

(: sample-pos2 : Pos)
(define sample-pos2 (make-Pos 0 0))

;; creating sample positions for quicker
;; check-expects later


;.........................


(: blank-board : Board)
(define blank-board (make-Board (make-list 64 'none))) 

(check-expect (append (take (Board-squares blank-board) 2) (list 'none) 
      (list-tail (Board-squares blank-board) 62)) 
              '(none none none none none))

;; testing out take, append and list-tail to use
;; in subsequent function


;..........................



(: update-board (Board Player Pos -> Board))
;; given player & location, adds piece to board

(define (update-board bd col po)
 (make-Board 
  (append
   (take (Board-squares bd)  (+ (* 8 (Pos-row po)) (Pos-col po)))
   (list col)
   (list-tail (Board-squares bd) ( + (+ (* 8 (Pos-row po))(Pos-col po)) 1)))))

;
;(check-expect 
; (board-ref (update-board blank-board 'black sample-pos) sample-pos)'black)
;(check-expect 
; (board-ref (update-board blank-board 'white sample-pos2) sample-pos2)'white)

;; update-board outputs Board (too long for check-expect)
;; so, I used board-ref to test whether a particular
;; position has been changed accordingly


;..........................



(: setup (Board -> Board))
;; set up board for game

(define (setup x)
  (update-board
  (update-board
  (update-board
  (update-board x 'black (make-Pos 3 4))
    'black (make-Pos 4 3))
    'white (make-Pos 3 3))
    'white (make-Pos 4 4)))

;(setup blank-board)

(check-expect (board-ref (setup blank-board) (make-Pos 3 4)) 'black)
(check-expect (board-ref (setup blank-board) (make-Pos 3 3)) 'white)
(check-expect (board-ref (setup blank-board) (make-Pos 2 2)) 'none)

;; again: outputs Board (too long for check-expect)
;; so, I used board-ref to test whether a particular
;; position has been changed accordingly


;...........................


(: start-board : Board)
(define start-board (setup blank-board)) 

(: new-game : Game)
(define new-game (make-Game start-board 'black))


;..............................




(: board-ref : Board Pos -> (U Player 'none))
;;Return the piece at the given square, or 'none

(define (board-ref bd po)
  (list-ref (Board-squares bd) (+ (* 8 (Pos-row po)) (Pos-col po))))


(check-expect (board-ref start-board (make-Pos 4 3)) 'black)
(check-expect (board-ref start-board (make-Pos 3 4)) 'black)
(check-expect (board-ref start-board (make-Pos 3 3)) 'white)
(check-expect (board-ref start-board (make-Pos 4 4)) 'white)



;..................................




(: onboard? : Pos -> Boolean)
;; returns #t if position is on the board
;; #f if it is not

(define (onboard? po)
  (cond
    [(and (and (and (> 8 (Pos-row po)) (<= 0 (Pos-row po)))
     (> 8 (Pos-col po))) (<= 0 (Pos-col po)))
    #t]
    [else #f]))

(check-expect (onboard? (make-Pos 0 -1)) #f) 
(check-expect (onboard? (make-Pos 0 0)) #t) 
(check-expect (onboard? (make-Pos 7 7)) #t)




;......................................


(: around : Pos -> (Listof Pos))
;; given a position returns a list of
;; all surrounding positions on the board

(define (around po)
  
(local [(define x (Pos-row po))]
 (local [(define y (Pos-col po))]
   (local [(define ps
             (list (make-Pos (- x 1) (- y 1))
                   (make-Pos (- x 1) y)
                   (make-Pos (- x 1) (+ y 1))
                   (make-Pos x (- y 1))
                   (make-Pos x (+ y 1))
                   (make-Pos (+ x 1) (- y 1))
                   (make-Pos (+ x 1) y)
                   (make-Pos (+ x 1) (+ y 1))))]     
     (filter onboard? ps)))))


(check-expect (around (make-Pos 1 1)) 
              (list (Pos 0 0) (Pos 0 1) (Pos 0 2)
                    (Pos 1 0) (Pos 1 2)
                    (Pos 2 0) (Pos 2 1) (Pos 2 2)))   

;; fliter was used so that all coordinates
;; have values between 0 and 7 (ie are actually
;; on the gameboard



;.............................................


(: switch : Player -> Player)
;; given a player, returns the opponent

(define (switch x)
  (cond 
    [(equal? x 'black)'white]
    [else 'black]))
  
;; useful for flipping pieces (flipper) & checking
;; for opponent's pieces (flips)

;............................................


(define-struct Slope
  ([cx : Integer]
   [cy : Integer])
  #:transparent)

;; slope consists of change in
;; x and change in y




(: get-xy : Pos Pos -> Slope)
;; makes slope between two positions

(define (get-xy a b)
  (make-Slope 
   (- (Pos-row b) (Pos-row a))
   (- (Pos-col b) (Pos-col a))))




(: slopes : Pos Pos -> (Listof Pos))
;; takes in two positions, returns line leading 
;; out from positions until end of board

(define (slopes p1 p2)
  (local [(define p3
   (make-Pos
      (+ (Slope-cx (get-xy p1 p2)) (Pos-row p2)) 
      (+ (Slope-cy (get-xy p1 p2)) (Pos-col p2))))]
    (cond      
      [(or (or (< (Pos-row p3) 0) (> (Pos-row p3) 7))
         (or (< (Pos-col p3) 0) (> (Pos-col p3) 7)))
     (list p2)]
      [else (cons p2 (slopes p2 p3))]))) 
                         
(check-expect (slopes (make-Pos 4 7) (make-Pos 5 7)) 
              (list (Pos 5 7) (Pos 6 7) (Pos 7 7))) 
  


(check-expect (slopes (make-Pos 4 4) (make-Pos 5 4))
              (list (Pos 5 4) (Pos 6 4) (Pos 7 4)))

;............................................





(define-type Line (Listof Pos))

(: potential : Board Player Pos -> (Listof Pos))
; Given a board, a player and a move position, 
; return the list of positions directly around move position
; containing pieces to flip if that move is enacted

(define (potential bd col po)
  (local [(define pss (around po))]
    (local [(define opp (switch col))]
      (filter (lambda ([x : Pos])(equal? opp (board-ref bd x))) pss))))

                    
(check-expect (potential start-board 'black (make-Pos 2 1)) '())
(check-expect (potential start-board 'black (make-Pos 2 3))(list (Pos 3 3)))




(: getlines : Pos (Listof Pos) -> (Listof Line))
;;finds lines of pieces leading out from chosen location

(define (getlines po ps)
  (cond
    [(empty? ps) '()]
    [else (cons (slopes po (first ps))(getlines po (rest ps)))]))
    


(: until : Line Player Board -> Line)
;; truncates lines where finds a matching piece or 
;; a 'none

(define (until l col bd)  
  (cond
    [(empty? l) '()]
    [(equal? 'none (board-ref bd (first l))) '()]
    [(equal? col (board-ref bd (first l))) (list (first l))]
    [else (cons (first l)(until (rest l) col bd))]))
           

          


(: flank : Board Player (Listof Line) -> (Listof Line))
;; throws out lines that arent flanked

(define (flank bd col ls)
  (filter (lambda ([x : Line])(equal? col (board-ref bd (last x)))) ls))





(: to-lop : (Listof Line) -> Line)
; move positions from multiple lines back into
; a list of positions

(define (to-lop ls)
  (cond
      [(empty? ls) '()]
      [else (append (first ls)(to-lop (rest ls)))]))
       




(: flips : Board Player Pos -> (Listof Pos))
; Given a board, a player and a move position, 
; return the list of positions on board
; containing pieces to flip if that move is enacted

(define (flips bd col po)
  (local [(define opp (switch col))]
  (filter (lambda ([x : Pos])(equal? opp (board-ref bd x)))
    (to-lop
         (flank bd col
           (map (lambda ([x : Line])(until x col bd)) 
                (getlines po (potential bd col po))))))))
    



(: test-board : Board)
(define test-board
  (update-board
  (update-board  
  (update-board
  (update-board
  (update-board  
  (update-board
  (update-board
  (update-board
  (update-board 
   blank-board 
    'black (make-Pos 2 5))
    'black (make-Pos 0 7))
    'white (make-Pos 3 6))
    'white (make-Pos 3 7))
    'white (make-Pos 2 7))
    'white (make-Pos 1 7))
    'white (make-Pos 5 7))
    'white (make-Pos 6 7))
    'white (make-Pos 7 7)))


(: tb2 : Board)
(define tb2
  (update-board  
  (update-board
  (update-board
  (update-board
  (update-board 
   blank-board 
    'white (make-Pos 1 7))
    'black (make-Pos 0 6))
    'black (make-Pos 1 4))
    'black (make-Pos 1 5))
    'black (make-Pos 1 6)))



(: tb3 : Board)
(define tb3
  (update-board
  (update-board
  (update-board
  (update-board 
   blank-board 
    'white (make-Pos 0 2))
    'black (make-Pos 0 3))
    'white (make-Pos 0 4))
    'black (make-Pos 0 6)))


(check-expect 
 (flips test-board 'black (make-Pos 4 7))
(list (Pos 3 6) (Pos 3 7) (Pos 2 7) (Pos 1 7)))



;.......................................................



(: outflanks? : Board Player Pos -> Boolean)
; Given a board, a player and a position, the 
; function outflanks? returns true if that player 
; can outflank the opponent by placing a piece at that position

(define (outflanks? bd col po)
  (cond
    [(empty? (flips bd col po)) #f]
    [(not (equal? 'none (board-ref bd po))) #f]
    [else #t]))
       
(check-expect (outflanks? start-board 'white (make-Pos 3 3)) #f)
(check-expect (outflanks? test-board 'white (make-Pos 2 2)) #f)
(check-expect (outflanks? test-board 'black (make-Pos 4 7)) #t)

;; two conditions used: one to check whether
;; there are pieces to flip, and another ensuring
;; that the given position is actually empty


;......................................
;
;
(: flipper : Game (Listof Pos) Pos -> Game)
;;; given a Game and list of positions, flips all
;;; pieces at given locations, and switches state of
;;; game to next player

(define (flipper gm xs po)
  (cond
    [(empty? xs) (make-Game (update-board (Game-board gm) (Game-next gm) po) 
                            (switch (Game-next gm)))]
    [else (flipper (make-Game (update-board (Game-board gm) (Game-next gm) 
                                            (first xs)) (Game-next gm))
                   (rest xs) po)]))



(: test-board2 : Board)
(define test-board2
(Game-board
 (flipper (make-Game test-board 'black) 
          (list (Pos 0 0) (Pos 0 1) (Pos 0 2)) (make-Pos 4 7))))

(check-expect (board-ref test-board2 (Pos 0 0)) 'black) 
(check-expect (board-ref test-board2 (Pos 0 1)) 'black)
(check-expect (board-ref test-board2 (Pos 0 2)) 'black)

;; board too long to run a check expect,
;; so I passed a list of positions through flipper
;; and used board-ref to check whether those individual pieces
;; had all been flipped accordingly



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(: apply-move : Game Player Pos -> Game)
;; Given a game, a player and a position, apply 
;; the move to the game board and return the 
;; subsequent game state. If the proposed move is 
;; illegal, raise an error.


(define (apply-move gm col po)
  
(local [(define lis (flips (Game-board gm) col po))]
  (cond
    [(not (equal? (Game-next gm) col)) (error "wrong player")]
    [(equal? (outflanks? (Game-board gm) col po) #f)(error "illegal move")] 
    [(not (equal? (board-ref (Game-board gm) po) 'none))
     (error "illegal move")] 
    [else (flipper gm lis po)])))
    

(: pos-list : (Listof Pos))
;; returns list of positions

(define pos-list
  (map 
   (lambda ([x : Integer])
     (make-Pos
         (floor (/ x 8))
         (remainder x 8)))
       (build-list 64 (lambda ([n : Integer]) n))))


(: game-over? : Game -> Boolean)
;; returns true when neither player can move

(define (game-over? gm)
  (cond
    [(not (empty? (filter (lambda ([x : Pos])
        (outflanks? (Game-board gm) (Game-next gm) x)) pos-list))) #f]
    [(not (empty? (filter (lambda ([x : Pos])
        (outflanks? (Game-board gm) (switch (Game-next gm)) x)) pos-list))) #f]
    [else #t]))

(check-expect (game-over? (make-Game test-board 'black)) #f)
(check-expect (game-over? (make-Game test-board 'white)) #f)
(check-expect (game-over? (make-Game blank-board 'black)) #t)


(: outcome : Game -> (U Player 'tie))
;; Counts the pieces on the board and determines the outcome 
;; (possibly a tie)

(define (outcome gm)
  (local [(define allb 
            (filter (lambda ([x : Pos])
                      (equal? (board-ref (Game-board gm) x) 'black)) 
                    pos-list))]
    (local [(define allw 
              (filter (lambda ([x : Pos])
                        (equal? (board-ref (Game-board gm) x) 'white)) 
                      pos-list))]
      (cond
        [(<(length allb)(length allw))'white]
        [(>(length allb)(length allw))'black]
        [else 'tie]))))


(check-expect (outcome (make-Game start-board 'black)) 'tie)
(check-expect (outcome (make-Game test-board 'black)) 'white)
;(check-expect (outcome game1) 'black)


(require/typed 2htdp/image
   [#:opaque Image image?]
   [rectangle (-> Number Number String String Image)]
   [empty-image Image]
   [image-width (-> Image Number)]
   [image-height (-> Image Number)]
   [text (-> String Number String Image)] 
   [beside (-> Image * Image)]
   [beside/align (-> String Image * Image)]
   [above (-> Image * Image)]
   [above/align (-> String Image * Image)]
   [overlay/align (-> String String Image * Image)]
   [overlay/xy (-> Image Number Number Image Image)]
   [overlay (-> Image * Image)]
   [crop (-> Number Number Number Number Image Image)]
   [flip-vertical (-> Image Image)]
   [flip-horizontal (-> Image Image)]
   [freeze (-> Image Image)]
   [rotate (-> Number Image Image)]
   [circle (-> Integer String String Image)]
   [square (-> Integer String String Image)]
   [triangle (-> Integer String String Image)]
   [star(-> Integer String String Image)]
   [radial-star (-> Integer Integer Integer String String Image)])


(: place-piece : Integer (U Player 'none) Pos Image -> Image)
;; given a player and desired width of whole
;; board image, creates spot on board with
;; given player piece 

(define (place-piece int col po x)
  (cond
    [(equal? col 'black)
     (overlay/xy (circle (round (/ (/ int 9) 4)) "solid" "black") 
                 (* -1 (+ (round (* (/ 4 3) (/ int 9))) 
                          (* (Pos-col po)(round (/ int 9)))))
                 (* -1 (+ (round (* (/ 4 3) (/ int 9))) 
                          (* (Pos-row po)(round (/ int 9)))))
                 x)]
    [(equal? col 'white)
     (overlay/xy (circle (round (/ (/ int 7) 4)) "outline" "black") 
                 (* -1 (+ (round (* (/ 5 4) (/ int 9))) 
                          (* (Pos-col po)(round (/ int 9)))))
                 (* -1 (+ (round (* (/ 5 4) (/ int 9))) 
                          (* (Pos-row po)(round (/ int 9)))))
                 x)]
    [else x])) 

 

(: lp : ((Listof (U Player 'none)) (Listof Pos) Integer Image -> Image))
 ;; overlays boards containing new pieces until board is fully updated

(define (lp t ps int x) 
  (cond
    [(= (length t) 0) x]
    [else (overlay (place-piece int (first t) (first ps) x) 
                   (lp (rest t) (rest ps) int x))]))
            

(: board-image : Board Integer -> Image)
;; Given a board and the desired width of the whole board image, 
;; produce an image of the board. That is, given a board and the number 
;; 180, board-image must produce an image that is 180 pixels wide, and 
;; correspondingly tall (and not necessarily square).

(define (board-image bd int)
 (local [(define size (round (/ int 11)))]
   (local [(define inc (* -1 (round (* (/ 1 3) (/ int 9)))))]
     (local [(define inc2 (* (/ 1 3) (/ int 9)))]
       (local [(define inc3 (/ int 9))]
         
 (overlay/xy  (text "0" size "blue") inc
                 (* -1 (round (+ inc2 inc3)))              
 (overlay/xy  (text "1" size "blue") inc
                 (* -1 (round (+ inc2 (* 2 inc3))))                
 (overlay/xy  (text "2" size "blue") inc
                 (* -1 (round (+ inc2 (* 3 inc3))))
 (overlay/xy  (text "3" size "blue") inc
                 (* -1 (round (+ inc2 (* 4 inc3))))               
 (overlay/xy  (text "4" size "blue") inc
                 (* -1 (round (+ inc2 (* 5 inc3))))               
 (overlay/xy  (text "5" size "blue") inc
                 (* -1 (round (+ inc2 (* 6 inc3))))                
 (overlay/xy  (text "6" size "blue") inc
                 (* -1 (round (+ inc2 (* 7 inc3))))              
 (overlay/xy  (text "7" size "blue") inc
                 (* -1 (round (+ inc2 (* 8 inc3))))                
 (overlay/xy  (text "0" size "blue") 
                 (* -1 (round (+ inc2 inc3))) inc              
 (overlay/xy  (text "1" size "blue") 
                 (* -1 (round (+ inc2 (* 2 inc3)))) inc                
 (overlay/xy  (text "2" size "blue")
                 (* -1 (round (+ inc2 (* 3 inc3)))) inc
 (overlay/xy  (text "3" size "blue")
                 (* -1 (round (+ inc2 (* 4 inc3)))) inc              
 (overlay/xy  (text "4" size "blue") 
                 (* -1 (round (+ inc2 (* 5 inc3)))) inc               
 (overlay/xy  (text "5" size "blue")
                 (* -1 (round (+ inc2 (* 6 inc3)))) inc               
 (overlay/xy  (text "6" size "blue") 
                 (* -1 (round (+ inc2 (* 7 inc3)))) inc             
 (overlay/xy  (text "7" size "blue")
                 (* -1 (round (+ inc2 (* 8 inc3)))) inc
                 (local [(define s1 (square (round (/ int 9)) 
                                            "outline" "black"))]
  (define r1 (beside s1 s1 s1 s1 s1 s1 s1 s1 s1))
  (above  r1 r1 r1 r1 r1 r1 r1 r1 r1 )))))))))))))))))))))))



(: counter-b : (Listof (U Player 'none)) -> Integer)
;; given game, return black score

(define (counter-b ps)
  (cond
    [(= 0 (length ps)) 0]
    [(equal? (first ps) 'black)(+ 1 (counter-b (rest ps)))]
    [else (counter-b (rest ps))]))


(: counter-w : (Listof (U Player 'none)) -> Integer)
;; given game, return white score

(define (counter-w ps)
  (cond
    [(= 0 (length ps)) 0]
    [(equal? (first ps) 'white)(+ 1 (counter-w (rest ps)))]
    [else (counter-w (rest ps))]))



(: to-string : Player -> String)
;; given player, return string

(define (to-string col)
  (cond
    [(equal? col 'black) "black"]
    [(equal? col 'white) "white"]))



(: background (-> Integer Image Image))
;; adds background to game
(define (background padding i)
  (overlay i (rectangle (+ padding (image-width i)) 
                        (+ padding (image-height i)) 
                        "solid" 
                        "LavenderBlush")))



(: frame (-> Integer Image Image))
;; given padding in pixels and an image, draw a thin
;; black rectangle around the image
(define (frame padding i)
  (overlay i (rectangle (+ padding (image-width i)) 
                        (+ padding (image-height i)) 
                        "outline" 
                        "black")))


(: game-image : Game Integer -> Image)
;; Given a game and the desired width of the whole game image, 
;; produce an image of the game, including the board, an indication 
;; of which player is to move next, and each player's current score.
(define (game-image gm int)
  (background (round (/ int 5))
         (above (above (frame (round (/ int 10))  
                       (beside (text "Next Player:  " (round (/ int 8)) "blue")
                        (circle (round (/ int 14)) "solid" (to-string (Game-next gm)))))
                (rectangle int (round (/ int 20)) "solid" "Lavenderblush"))
                
                (above 
                (beside (frame (round (/ (/ int 7) 2)) 
                               (text (string-append "BLACK: " 
                                            (number->string 
                                             (counter-b (Board-squares 
                                                         (Game-board gm)))) 
                                            " ") 
                             (round (/ int 10)) "black"))
                       (frame (round (/ (/ int 7) 2))
                              (text (string-append "WHITE: " 
                                            (number->string (counter-w 
                                                             (Board-squares 
                                                              (Game-board gm)))) 
                                            " ") 
                             (round (/ int 10)) "black")))
                (rectangle int (round (/ int 15)) "solid" "Lavenderblush"))
                (overlay
                (lp (Board-squares (Game-board gm))   
                    pos-list int (board-image (Game-board gm) int)) 
                         (rectangle int int "solid" "white")))))
  

    
;(game-image (make-Game test-board 'black) 140)                      
;(game-image (apply-move 
;(make-Game test-board 'black) 'black (make-Pos 4 7)) 140)  


;///////////////// PROJECT 2 ////////////////////////


(define-type Strategy (Game -> Pos))


(: first-move : Strategy)
;; first available legal move, starting from the upper 
;; left corner, and moving across each row left-to-right, a row at a time

(define (first-move g)
  (local    
    {(: lp : (Listof Pos) -> Pos)
     ;; loops
     (define (lp ps)
       (cond
         [(= 0 (length ps))(Pos 0 0)]
         [(not (outflanks? (Game-board g) (Game-next g) (first ps)))
          (lp (rest ps))]
         [else (first ps)]))}
    (lp pos-list)))



(: p2board : Board)
(define p2board 
  (update-board
  (update-board
   (update-board 
    (update-board test-board 'white (Pos 0 6))
    'white (Pos 7 1)) 
   'white (Pos 7 2))
  'black (Pos 7 3)))
  
(define gtest (make-Game start-board 'black))
(define gtest2 (make-Game (update-board start-board 'white (Pos 2 3)) 'black))
(define gtest3 (make-Game (update-board test-board 'white (Pos 0 6)) 'black))
(define gtest4 (make-Game p2board 'black))

;(game-image gtest4 180)
;(game-image gtest 180)

(check-expect (first-move gtest) (Pos 2 3))  
(check-expect (first-move gtest2) (Pos 1 2))

;(game-image (make-Game tb2 'black) 180)
 



(: parse-pos : String -> (U Pos String))
;; either parse the string into a position on the board, or, if that 
;; is not possible, return the unparseable string as is

(define (parse-pos str)
  (cond
    [(not (equal? 2 (string-length str))) str]
    [else 
     (local [(define chars 
              (map (lambda ([x : Integer]) (- x 48)) 
                   (map char->integer (string->list str))))]
       (cond 
         [(equal? 2 (length (filter (lambda ([x : Integer]) 
                                      (and (<= 0 x) (<= x 7))) chars)))
          (make-Pos (first chars)(last chars))]
         [else str]))]))


(check-expect (parse-pos "01") (Pos 0 1)) 
(check-expect (parse-pos "what") "what")
(check-expect (parse-pos "77") (Pos 7 7))
(check-expect (parse-pos "08") "08")



(: human : Strategy)
;; Evaluate (read-line), the result of which will either be a string
;; or an EOF object. If it is a string, parse the string and return the 
;; position it refers to (more below). If it is EOF, raise an error

(define (human g)
  (local [(define inp (read-line))]
    (cond
      [(eof-object? inp) (error "EOF")]
      [(string? (parse-pos inp))(human g)]
      [else (local
              {(: posit? : Any -> Pos)
               (define (posit? p)
                 (if (Pos? p) p (error "wrong input")))}
              (posit? (parse-pos inp)))]))) 



(define-struct (a b) Pair
  ([first  : a]
   [second : b])
  #:transparent)



(: countdown : Game Strategy Strategy Integer -> (Pair Player Pos))
;; checks legality of move, returns move if legal
;; returns error if illegal

(define (countdown g sb sw count)
   (local [(define str (if (equal? 'black (Game-next g)) sb sw))]
     (local [(define res (str g))]
       (cond
         [(= 1 count)(error "3 strikes, you're out!!")]
         [(not (equal? 'none (board-ref (Game-board g) res)))
          (begin (display (string-append "space occupied, " 
                                         (number->string (sub1 count)) 
                                         " tries left"))
                 (countdown g sb sw (sub1 count)))]
         [(not (outflanks? (Game-board g)(Game-next g) res))
          (begin (display (string-append "illegal move, "
                                         (number->string (sub1 count)) 
                                         " tries left"))
                 (countdown g sb sw (sub1 count)))]
         [else (make-Pair (Game-next g) res)]))))   



(: play-loop : Game Strategy Strategy -> 
   (Pair (Listof (Pair Player Pos)) Game))
;; loops through game using specified strategy for each player
;; return value of the function is a list of moves (pair of the player 
;; and the position), & final state of the game 

(define (play-loop gm sb sw)
    (local
      {(: lp : Game (Listof (Pair Player Pos)) -> 
          (Pair (Listof (Pair Player Pos)) Game))   
       
       (define (lp g history)
         (begin
           (display (game-image g 180))
           (cond
             [(game-over? g) (Pair (reverse history) g)]
             [(empty? (filter (lambda ([x : Pos]) 
                                (outflanks? (Game-board g)(Game-next g) x)) 
                              pos-list))
              (play-loop (make-Game (Game-board g)(switch (Game-next g))) 
                         sb sw)]
             [else (local [(define move (countdown g sb sw 3))]
                     (lp (apply-move g (Pair-first move) (Pair-second move)) 
                         (cons move history)))])))}
      (lp gm '()))) 

;(play-loop gtest human human)




(define corners (list (Pos 0 0) (Pos 0 7) (Pos 7 0) (Pos 7 7)))
(define edges (filter (lambda ([x : Pos])
                        (or (equal? 0 (Pos-row x))
                            (equal? 7 (Pos-row x))
                            (equal? 0 (Pos-col x))
                            (equal? 7 (Pos-col x)))) pos-list))
(define non-corner-edges (remove* corners edges))






(: open-corners : Game -> (Listof Pos))
;; given a game, returns legal corners to which you can move
(define (open-corners g)
  (filter (lambda ([x : Pos]) 
            (outflanks? (Game-board g) (Game-next g) x))
          corners))





(: open-edge : Game -> (Listof Pos))
;; given a game, returns legal edges to which you can move

(define (open-edge g)
  (filter (lambda ([y : Pos])
            (outflanks? (Game-board g) (Game-next g) y))
          edges))


(check-expect (open-corners gtest) '())
(check-expect (open-corners gtest4) (list (Pos 7 0)))
(check-expect (open-edge gtest4) (list (Pos 0 5) (Pos 4 7) (Pos 7 0)))






(: most-flips : Game (Listof Pos) -> Pos)
;; returns the position from a list that will flip the most pieces

(define (most-flips g xs)
  (local [(define lis 
            (map (lambda ([x : Pos]) 
                   (make-Pair x (length (flips (Game-board g)
                                               (Game-next g) x)))) xs))]
    (local 
      {(: lp : (Listof (Pair Pos Real)) (Pair Pos Real) -> Pos)
       ;;loops to find greatest 
       (define (lp pr n)
         (cond
           [(empty? pr) (Pair-first n)]
           [(> (Pair-second (first pr)) (Pair-second n)) 
            (lp (rest pr) (first pr))]
           [else (lp (rest pr) n)]))}
    (lp lis (first lis)))))


(check-expect (most-flips gtest2 
                          (list (Pos 1 2) (Pos 1 3) (Pos 5 4))) (Pos 1 3))
(check-expect (most-flips gtest2 
                          (list (Pos 1 2) (Pos 5 4))) (Pos 1 2))
           





(: immediate-tactics : Strategy)
;; if a corner move is available, choose it,
;; then, if a non-corner edge move is available, choose it,
;; then, choose any available interior move.

(define (immediate-tactics g)
  (cond
    [(not (empty? (open-corners g)))(most-flips g (open-corners g))]
    [(not (empty? (open-edge g)))(most-flips g (open-edge g))]
    [else (most-flips g (filter (lambda ([x : Pos]) 
                                (outflanks? (Game-board g)(Game-next g) x)) 
                              pos-list))]))


(check-expect (immediate-tactics gtest)(Pos 2 3)) 
(check-expect (immediate-tactics gtest4)(Pos 7 0))
(check-expect (immediate-tactics gtest3)(Pos 4 7))




(define-type Heuristic (Game -> Integer))

(: piece-counting : Heuristic)
;; number of black pieces minus 
;; number of white pieces 
  
(define (piece-counting g)
    (- (counter-b (Board-squares (Game-board g)))
       (counter-w (Board-squares (Game-board g)))))
  
(check-expect (piece-counting gtest4) -7)
(check-expect (piece-counting gtest) 0)
(check-expect (piece-counting gtest2) -1)






(: prefer-edges : Integer -> Heuristic)
;; consumes an integer, and then returns a function that 
;; behaves like piece-counting above, but counts every edge 
;; piece as being worth that many interior pieces
  
(define (prefer-edges int)
  (lambda ([g : Game])
    (- (+ (counter-b (Board-squares (Game-board g)))
          (* (sub1 int)(counter-b 
                        (map (lambda ([x : Pos]) 
                               (board-ref (Game-board g) x)) edges))))
       (+ (counter-w (Board-squares (Game-board g)))
          (* (sub1 int)(counter-w 
                        (map (lambda ([x : Pos]) 
                               (board-ref (Game-board g) x)) edges)))))))


(check-expect ((prefer-edges 2) gtest4) -14)
(check-expect ((prefer-edges 2) gtest3) -12)




(: prefer-edges-and-corners : Integer Integer -> Heuristic)
;; consume a weight for counting non-corner edge pieces, and a weight for  
;; corners, and count similarly to prefer-edges above.

(define (prefer-edges-and-corners int1 int2)
  (lambda ([g : Game])
    (- (+ (counter-b (Board-squares (Game-board g)))
          (* (sub1 int1)(counter-b 
                         (map (lambda ([x : Pos]) 
                                (board-ref (Game-board g) x)) 
                              non-corner-edges)))
          (* (sub1 int2)(counter-b 
                         (map (lambda ([x : Pos]) 
                                (board-ref (Game-board g) x)) 
                              corners))))
       (+ (counter-w (Board-squares (Game-board g)))
          (* (sub1 int1)(counter-w 
                        (map (lambda ([x : Pos]) 
                               (board-ref (Game-board g) x)) 
                             non-corner-edges)))
          (* (sub1 int2)(counter-w 
                         (map (lambda ([x : Pos]) 
                                (board-ref (Game-board g) x)) 
                              corners)))))))
  

(define gtest5 (make-Game (update-board (Game-board gtest3) 
                                        'black (Pos 7 0)) 'black))

(check-expect ((prefer-edges-and-corners 2 1) gtest3) -12)
(check-expect ((prefer-edges-and-corners 2 4) gtest5) -8)
(check-expect ((prefer-edges-and-corners 2 2) gtest2) -1)






(: minimax-eval : Heuristic Integer Game -> Integer)
;; consume the ply (the number of moves to look ahead) and a game, and 
;; assign a score using the given heuristic function

(define (minimax-eval hf ply g)
  (local 
    [(define poses (filter (lambda ([x : Pos])
                             (outflanks? (Game-board g) 
                                         (Game-next g) x)) pos-list))
     (define lis 
       (map (lambda ([p : Pos])
              (apply-move g (Game-next g) p)) poses))]
  (cond
    [(= 0 ply)(hf g)]
    [else
            (match (Game-next g)
              ['white
               (apply min (map (lambda ([y : Game]) 
                                 (minimax-eval hf (sub1 ply) y)) lis))]
              ['black
               (apply max (map (lambda ([y : Game])
                                 (minimax-eval hf (sub1 ply) y)) lis))])])))



         
(check-expect (minimax-eval (prefer-edges 1) 1 
                            gtest4) 2)                    
(check-expect (minimax-eval (prefer-edges 1) 2 
                            (make-Game 
                             (Game-board gtest2) 'white)) -1)
(check-expect (minimax-eval (prefer-edges 1) 2 
                            (make-Game tb2 'white)) -1)
(check-expect (minimax-eval (prefer-edges 2) 3 
                            new-game) 3)
(check-expect (minimax-eval (prefer-edges 1) 3 
                            new-game) 3)
(check-expect (minimax-eval (prefer-edges 1) 1 
                            (make-Game 
                             (update-board tb2 'black (Pos 2 7)) 'white)) -3)  
(check-expect (minimax-eval piece-counting 3 new-game) 3)



  
(: find-min : (Listof (Pair Pos Integer)) (Pair Pos Integer) -> Pos)
;;loops to find min integer 

(define (find-min pr n)
  (cond
    [(empty? pr) (Pair-first n)]
    [(< (Pair-second (first pr)) (Pair-second n)) (find-min (rest pr) 
                                                            (first pr))]
    [else (find-min (rest pr) n)]))

(check-expect 
 (find-min (list (Pair (Pos 0 0) 2) (Pair (Pos 0 1) 5) (Pair (Pos 0 2) 1)) 
           (Pair (Pos 0 0) 2)) (Pos 0 2))
(check-expect 
 (find-min (list (Pair (Pos 0 1) 0) (Pair (Pos 0 2) 1)) 
           (Pair (Pos 0 0) 2)) (Pos 0 1))




(: find-max : (Listof (Pair Pos Integer)) (Pair Pos Integer) -> Pos)
;;loops to find min integer 

(define (find-max pr n)
  (cond
    [(empty? pr) (Pair-first n)]
    [(> (Pair-second (first pr)) (Pair-second n)) (find-max (rest pr) 
                                                            (first pr))]
    [else (find-max (rest pr) n)]))

(check-expect 
 (find-max (list (Pair (Pos 0 0) 2) (Pair (Pos 0 1) 5) (Pair (Pos 0 2) 1)) 
           (Pair (Pos 0 0) 2)) (Pos 0 1))
(check-expect 
 (find-max (list (Pair (Pos 0 0) 0) (Pair (Pos 0 2) 1)) 
           (Pair (Pos 0 0) 0)) (Pos 0 2))




(: minimax : Heuristic Integer -> Strategy)
;; using minimax-eval build a Strategy given a ply and a game
;; Sonja helped with organization

(define (minimax hf ply)
  (lambda ([g : Game])
    (local
      [(define lis 
         (map (lambda ([p : Pos])
                (make-Pair p (apply-move g (Game-next g) p))) 
              (filter (lambda ([x : Pos]) 
                        (outflanks? (Game-board g)
                                    (Game-next g) x)) pos-list)))]
    (local [(define results 
              (map (lambda ([pr : (Pair Pos Game)])
                     (make-Pair 
                      (Pair-first pr)
                      (minimax-eval hf 
                                    (sub1 ply) 
                                    (Pair-second pr)))) lis))]
      (cond
        [(equal? 'black (Game-next g))(find-max results (first results))]
        [else (find-min results (first results))])))))

  
(check-expect ((minimax piece-counting 1)(make-Game tb3 'black))
              (Pos 0 1))
(check-expect ((minimax (prefer-edges 1) 1) gtest4) 
              (Pos 4 7))
(check-expect ((minimax (prefer-edges-and-corners 1 10) 1) gtest4) 
              (Pos 7 0))
(check-expect ((minimax (prefer-edges 1) 2) (make-Game tb2 'white))
              (Pos 1 3))




    

(: pick-upto : (All (a) Integer (Listof a) -> (Listof a)))
;; choose that many items, at random, from a list

(define (pick-upto n xs)
    (cond
      [(= n 0) '()]
      [(= 1(length xs)) xs]
      [else 
       (local [(define choice (list-ref xs (random (sub1 (length xs)))))]
         (cons choice (pick-upto (sub1 n) (remove choice xs))))]))
                                        
                      
;(pick-upto 3 (list 2 3 6 8 9))
;(pick-upto 3 (list 2 3 6))
;(pick-upto 3 (list 2 3))
(check-expect (pick-upto 3 (list 1)) '(1))




(: montymax : Heuristic Integer Integer -> Strategy)
;; algorithm is essentially the same as minimax, except at every 
;; level of the search tree, a random selection of branches is 
;; chosen to explore further

(define (montymax hf ply maxv)
  (lambda ([g : Game])
    (local
      [(define lis 
         (map (lambda ([p : Pos])
                (make-Pair p (apply-move g (Game-next g) p))) 
              (filter (lambda ([x : Pos]) 
                        (outflanks? (Game-board g)
                                    (Game-next g) x)) pos-list)))]
    (local [(define results 
              (map (lambda ([pr : (Pair Pos Game)])
                     (make-Pair 
                      (Pair-first pr)
                      (minimax-eval hf 
                                    (sub1 ply) 
                                    (Pair-second pr)))) (pick-upto maxv lis)))]
      (cond
        [(equal? 'black (Game-next g))(find-max results (first results))]
        [else (find-min results (first results))])))))


(game-image gtest2 180)
((montymax (prefer-edges 1) 1 3) (make-Game (update-board 
                                  (update-board (Game-board gtest2) 
                                                'white (Pos 5 4))
                                  'white (Pos 6 4)) 'black))  




(test)
  


