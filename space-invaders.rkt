;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun 
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))
(define TANK-Y (- HEIGHT TANK-HEIGHT/2))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left
(define T-LEFT  (make-tank 0  -1))      ;tank at left extreme, trying to go to left
(define T-RIGHT (make-tank WIDTH 1))    ;tank at the right extreme, trying to go to right

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right
(define I4 (make-invader (- WIDTH 10) (/ HEIGHT 2) 10)) ;; hitted right side
(define I5 (make-invader 10 (/ HEIGHT 2) -10))    ;; hitted left side


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))
(define G4 (make-game (list I1 I2 I3) (list M1 M2 M3) T1))
(define G-LEFT  (make-game empty empty T-LEFT))
(define G-RIGHT (make-game empty empty T-RIGHT))


;; =================
;; Functions:

;; WS -> WS
;; start the world with ...
;; 
(define (main g)
  (big-bang g                    ; Game
            (on-tick   tock)     ; Game -> Game
            (to-draw   ...)      ; Game -> Image
            (stop-when lose)     ; Game -> Boolean
            (on-key    handle-key)))   ; Game KeyEvent -> Game


;; Game -> Game
;; produces the next state of the game

; (define (tock g) g) ;Stub

(check-expect (tock G0) (make-game empty empty T0))
(check-expect (tock G1) (make-game empty empty T1))
(check-expect (tock G2) (make-game
                         (list(make-invader 162 101.5 12))
                         (list(make-missile (+ 150 MISSILE-SPEED) (+ 300 MISSILE-SPEED)))
                         T1))

(define (tock g)
  (make-game
   (move-loi (create-invader g))
   (move-lom (game-missiles g))
   (game-tank g)))

;; CREATE-INVADER
;; append a new invader to the list of invaders

(define (create-invader g)
  (append (game-invaders g)
          (list (make-invader (random WIDTH) 0 10))))

;; ListOfInvaders -> ListOfInvaders
;; made the list of invaders move by SPEED etc

; (define (move-loi loi) loi) ;Stub

(check-expect (move-loi (list I1 I2 I3))
              (list
                   (make-invader 162 (+ 100 INVADER-Y-SPEED)    12)
                   (make-invader 140 (+ HEIGHT INVADER-Y-SPEED) -10)
                   (make-invader 160 (+ (+ HEIGHT 10) INVADER-Y-SPEED) 10)))
(check-expect (move-loi (list I4 I5))
              (list
               (make-invader WIDTH (+ (/ HEIGHT 2) INVADER-Y-SPEED) -10)
               (make-invader 0 (+ (/ HEIGHT 2) INVADER-Y-SPEED) 10))) 

(define (move-loi loi)
  (cond [(empty? loi) empty]            
        [else (cons (move-invader (first loi))
                   (move-loi (rest loi)))]))

;; MOVE-INVADER

;; Invader -> Invader
;; A function that moves a single invader

; (define (move-invader invader) invader) ;Stub

(define (move-invader invader)
  (make-invader (+ (invader-x invader) (invader-dx invader))
                (+ (invader-y invader) INVADER-Y-SPEED) 
                (out-of-bonds? (invader-x invader) (invader-dx invader))))

;; OUT-OF-BONDS?

;; Number Number -> Number
;; A function that given a x and dx, returns the value multiply by -1 if it is reaching out of bounds

; (define (out-of-bonds? x dx) dx) ;Stub

(define (out-of-bonds? x dx)
  (cond [(>= (+ x dx) WIDTH) (* -1 dx)]
        [(>= 0 (+ x dx))      (* -1 dx)]
        [else dx]))


;; MOVE LIST OF MISSILES
;; ListOfMissiles -> ListOfMissiles
;; made the list of missiles move by SPEED etc

; (define (move-lom lom) lom) ;Stub

(check-expect (move-lom empty) empty)
(check-expect (move-lom (list M1 M2 M3))
              (list (make-missile (+ 150 MISSILE-SPEED) (+ 300 MISSILE-SPEED))
                    (make-missile (+ (invader-x I1) MISSILE-SPEED) (+ (+ (invader-y I1) 10) MISSILE-SPEED))
                    (make-missile (+ (invader-x I1) MISSILE-SPEED) (+ (+ (invader-y I1)  5) MISSILE-SPEED))))


(define (move-lom lom)
  (cond [(empty? lom) empty]            
        [else (cons (move-missile (first lom))                
                   (move-lom (rest lom)))]))

;; MOVE SINGLE MISSILE
;; Missile -> Missile
;; makes the missile move x and y coordinates by MISSILE-SPEED


; (define (move-missile missile) missile) ;Stub

(check-expect (move-missile M1) (make-missile (+ 150 MISSILE-SPEED) (+ 300 MISSILE-SPEED)))

(define (move-missile missile)
  (make-missile (+ (missile-x missile) MISSILE-SPEED) (+ (missile-y missile) MISSILE-SPEED)))

;; STOPS THE GAME WHEN YOU LOSE
;; Game -> Boolean
;; returns true if you lose the fucking game, loser

; (define (lose g) false) ;Stub

(check-expect (lose G0) false) ;empty game
(check-expect (lose G1) false) ;only a tank, no way to lose
(check-expect (lose G2) false) ;alien has not landed yet!!!
(check-expect (lose G3)  true) ;the alien has landed!
(check-expect (lose G4)  true) ;has already conquered all the land!

(define (lose s) (any-landed? (game-invaders s)))

;; HAS ANY ALIEN IN HERE?
;; ListOfInvaders -> Boolean
;; returns true if any of the invaders in the list have landed

; (define (any-landed? loi) false) ; Stub

(check-expect (any-landed? (list I1))    false) ; A list with a alien that has not landed
(check-expect (any-landed? (list I1 I2))  true) ; A list where the second alien has landed :)
(check-expect (any-landed? (list I4 I5)) false) ; Neither aliens have landed

(define (any-landed? loi)
  (cond [(empty? loi) false]
        [else
         (or (landed? (first loi))
             (any-landed? (rest loi)))]))

;; DID YOU ARRIVED AT YOUR DESTINATION?
;; Invader -> Boolean
;; returns true if the Y coordinate is >= HEIGHT

; (define (landed? i) true) ; Stub

(check-expect (landed? I1) false)  ; MORE TESTS MAH FRIEND
(check-expect (landed? I2)  true)  ; I REALLY LOVE TDD
(check-expect (landed? I4) false)

(define (landed? i)
  (>= (invader-y i) HEIGHT))

;; LET'S MOVE THINGS A LITTLE
;; Game KeyEvent -> Game
;; moves the tank and shot missiles, gurrrlll!

; (define (handle-key g ke) g) ; Stub

(check-expect (handle-key G0 "left")
              (make-game empty empty (make-tank (- (/ WIDTH 2) TANK-SPEED) -1))) 
(check-expect (handle-key G0 "right")
              (make-game empty empty (make-tank (+ (/ WIDTH 2) TANK-SPEED)  1)))
(check-expect (handle-key G0 " ")
              (make-game empty
                         (list (make-missile (tank-x (game-tank G0)) TANK-HEIGHT/2)) T0))
(check-expect (handle-key G2 " ")
              (make-game (list I1)
                         (list M1 (make-missile (tank-x (game-tank G2)) TANK-HEIGHT/2)) T1))
(check-expect (handle-key G-LEFT "left")
              (make-game empty empty (make-tank 0 -1)))
(check-expect (handle-key G-RIGHT "right")
              (make-game empty empty (make-tank WIDTH 1)))
            
(define (handle-key g ke)
  (cond [(key=? ke  "left") (move-tank g "left" )]
        [(key=? ke "right") (move-tank g "right")]
        [(key=? ke " ")     (shoot g)]
        [else g]))


;; MOOOOOVE TANK!!
;; Game String -> Game
;; return the new game with the tank moved to the direction, or with a direction change

; (define (move-tank g dir) g) ;Stub

(check-expect (move-tank G0 "left")
              (make-game empty
                         empty
                         (make-tank (- (/ WIDTH 2) TANK-SPEED) -1)))
(check-expect (move-tank G0 "right")
              (make-game empty empty (make-tank (+ (/ WIDTH 2) TANK-SPEED)  1)))
(check-expect (move-tank G-LEFT "left")
              (make-game empty empty (make-tank 0 -1)))
(check-expect (move-tank G-RIGHT "right")
              (make-game empty empty (make-tank WIDTH 1)))

(define (move-tank g dir)
  (make-game
   (game-invaders g)
   (game-missiles g)
   (cond
     [(string=? dir "left")
      (cond
        [(< (- (tank-x (game-tank g)) TANK-SPEED) 0) (make-tank 0 -1)]
        [else
         (make-tank
        (- (tank-x (game-tank g)) TANK-SPEED) -1)])]
     [(string=? dir "right")
      (cond
        [(> (+ (tank-x (game-tank g)) TANK-SPEED) WIDTH) (make-tank WIDTH 1)]
        [else
         (make-tank
        (+ (tank-x (game-tank g)) TANK-SPEED) 1)])]
     [else (game-tank g)])))

;; MOves


;; SHOOT MISSILES
;; Game -> Game
;; return the game with the missile appended to the list of missiles

; (define (shoot g) g) ;Stub

(check-expect (shoot G0)
              (make-game empty
                         (list (make-missile (tank-x (game-tank G0)) TANK-HEIGHT/2)) T0))
(check-expect (shoot G2)
              (make-game (list I1)
                         (list M1 (make-missile (tank-x (game-tank G2)) TANK-HEIGHT/2)) T1))

(define (shoot g)
  (make-game
   (game-invaders g)
   (append (game-missiles g)
           (list (make-missile (tank-x (game-tank g)) TANK-HEIGHT/2)))
   (game-tank g)))

;; RENDEEEEEEEEEEEEEER!!
;; Game -> Image
;; returns the game rendered!

(define (render g) empty-image) ; Stub

(check-expect (render G0)
              (place-image TANK (/ WIDTH 2) TANK-Y BACKGROUND))
(check-expect (render G2)
                (place-images
                 (list INVADER MISSILE)
                 (list (make-posn  150 100) (make-posn 150 300))
                 (place-image TANK 50 TANK-Y BACKGROUND)
                 ))
(check-expect (render G3)
              (place-images
                 (list INVADER INVADER MISSILE MISSILE)
                 (list
                  (make-posn  150 100)
                  (make-posn  150 HEIGHT)
                  (make-posn 150 300)
                  (make-posn (invader-x I1) (+ (invader-y I1) 10)))
                 (place-image TANK 50 TANK-Y BACKGROUND)
                 ))
             
#;
(define (render g)
  (place-images
   (append (make-listof-inv-img (game-invaders g))
           (make-listof-mis-img (game-missiles g)))
   (append (make-list-coords-i (game-invaders g))
           (make-list-coords-m (game-missiles g)))
   (place-image TANK (tank-x (game-tank g)) TANK-Y BACKGROUND)))


;; MAKE-LIST-IMAGES GREAT AGAIN!!
;; list (Invaders) -> list (Images)

; (define (make-listof-inv-img loi) empty) ; Stub

(check-expect (make-listof-inv-img (list I1)) (list INVADER))
(check-expect (make-listof-inv-img (list I1 I2)) (list INVADER INVADER))
(check-expect (make-listof-inv-img (list I1 I2 I3)) (list INVADER INVADER INVADER))


(define (make-listof-inv-img loi)
  (cond
    [(empty? loi) empty]
    [else (cons INVADER
                (make-listof-inv-img (rest loi)))]))

;; MAKE-LIST-IMAGES GREAT AGAIN!!
;; list (Missiles) -> list (Images)

; (define (make-listof-mis-img loi) empty) ; Stub

(check-expect (make-listof-mis-img (list M1)) (list MISSILE))
(check-expect (make-listof-mis-img (list M1 M2)) (list MISSILE MISSILE))
(check-expect (make-listof-mis-img (list M1 M2 M3)) (list MISSILE MISSILE MISSILE))

(define (make-listof-mis-img lom)
  (cond
    [(empty? lom) empty]
    [else (cons MISSILE
                (make-listof-mis-img (rest lom)))]))


;; MAKE-LIST-IMAGES GREAT AGAIN!!
;; list (Invaders) -> list (Make-Posn)

; (define (make-list-coords-i loi) empty) ; Stub

(check-expect (make-list-coords-i (list I1)) (list (make-posn  150 100)))
(check-expect (make-list-coords-i (list I1 I2)) 
              (list
               (make-posn  150 100)
               (make-posn  150 HEIGHT)))
(check-expect (make-list-coords-i (list I1 I2 I3))
              (list
               (make-posn 150 100)
               (make-posn 150 HEIGHT)
               (make-posn 150 (+ HEIGHT 10))))


(define (make-list-coords-i loi)
  (cond
    [(empty? loi) empty]
    [else (cons (make-posn (invader-x (first loi))
                           (invader-y (first loi)))
                (make-list-coords-i (rest loi)))]))


;; MAKE-LIST-IMAGES GREAT AGAIN!!
;; list (Missiles) -> list (Make-Posn)

; (define (make-list-coords-m lom) empty) ; Stub

(check-expect (make-list-coords-m (list M1)) (list (make-posn  150 300)))
(check-expect (make-list-coords-m (list M1 M2)) 
              (list
               (make-posn  150 300)
               (make-posn  (invader-x I1) (+ (invader-y I1) 10))))
(check-expect (make-list-coords-m (list M1 M2 M3))
              (list
               (make-posn 150 300)
               (make-posn (invader-x I1) (+ (invader-y I1) 10))
               (make-posn (invader-x I1) (+ (invader-y I1) 5))))


(define (make-list-coords-m lom)
  (cond
    [(empty? lom) empty]
    [else (cons (make-posn (missile-x (first lom))
                           (missile-y (first lom)))
                (make-list-coords-m (rest lom)))]))



