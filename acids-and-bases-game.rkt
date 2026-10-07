#lang racket/gui

;; --------------------------------------------------------------
;; Required functions/teachpacks for playing sounds, running tests etc.
;;( For the tests, see the TESTS Section. )
(require (only-in racket/gui/base (play-sound play-sound)))
(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/batch-io)

;; the .mp3 files:
(define sound_path1 "T.mp3")
(define sound_path2 "F.mp3")

;; -------------------------------------------------------------
;; STRUCTURES:
;; -------------------------------------------------------------
(define-struct button (im pos text))
(define-struct menu (start leaderboard))
(define-struct gameScene (gm))
(define-struct gameOver (btnRestart))
(define-struct leaderBoard (btnReturn))
(define-struct overallgame (menu gameScene gameOver leaderBoard goal score currentScene leaderboardfile))

(define-struct fish (fishimage pH name posi velo))
(define-struct bucket (bucketimage pH posi))
(define-struct game (AFishList score acidBucket baseBucket time tick-counter))
(define-struct pos (x y))
(define-struct vel (vx vy))

;; -------------------------------------------------------------
;; HELPFUL FUNCTIONS:
;; -------------------------------------------------------------
;; Purpose: Write the game score to the leaderboard file.
;; Contract: writeScore: g(overallgame) -> void
;; Function:
(define (writeScore g)
  (cond
    ((not (file-exists? (overallgame-leaderboardfile g)))
     (write-file (overallgame-leaderboardfile g)
                 (string-append
                  "Leaderboard\n"
                  "Your Score: "
                  (number->string (game-score (gameScene-gm (overallgame-gameScene g))))
                  "\n\n")))
    (else
     (write-file (overallgame-leaderboardfile g)
                 (string-append
                  (read-file (overallgame-leaderboardfile g))
                  "Your Score: "
                  (number->string (game-score (gameScene-gm (overallgame-gameScene g))))
                  "\n\n")))))

;; Purpose: Check whether the cursor object (or button) is clicked.
;; Contract: isClicked: o(any) x(number) y(number) getPosFunc(function) getImFunc(function) -> boolean
;; Function:
(define (isClicked o x y getPosFunc getImFunc)
  (<= (calcDist (getPosFunc o) x y)
      (/ (image-width (getImFunc o)) 2)))

;; Purpose: Calculate the distance between a position and (x, y)
;; Contract: calcDist: p(pos) x(number) y(number) -> number
;; Function:
(define (calcDist p x y)
  (sqrt (+ (sqr (- (pos-x p) x))
           (sqr (- (pos-y p) y)))))

;; Purpose: Decide whether a fish is inside the image
;; Contract: isInFish: a-fish(fish) x(number) y(number) -> boolean
;; Function:
(define (isInFish a-fish x y)
  (<= (calcDist (fish-posi a-fish) x y)
      (/ (image-width (fish-fishimage a-fish)) 2)))

;; Purpose: Move a position by velocity
;; Contract: move: loc(pos) v(vel) -> pos
;; Function:
(define (move loc v)
  (make-pos (+ (pos-x loc) (vel-vx v))
            (+ (pos-y loc) (vel-vy v))))

;; Purpose: Bounce fish from the edges
;; Contract: bounceFish: a-fish(fish) width(number) height(number) -> fish
;; Function:
(define (bounceFish a-fish width height)
  (let* ((pos (fish-posi a-fish))
         (vel (fish-velo a-fish))
         (new-vx (if (or (< (pos-x pos) 0) (> (pos-x pos) width))
                     (- (vel-vx vel))
                     (vel-vx vel)))
         (new-vy (if (or (< (pos-y pos) 0) (> (pos-y pos) height))
                     (- (vel-vy vel))
                     (vel-vy vel))))
    (make-fish (fish-fishimage a-fish)
               (fish-pH a-fish)
               (fish-name a-fish)
               (make-pos (max 0 (min width (pos-x pos)))
                         (max 0 (min height (pos-y pos))))
               (make-vel new-vx new-vy))))

;; Purpose: Check whether a fish collided with a bucket
;; Contract: isCollidedWithBucket: a-fish(fish) a-bucket(bucket) offset(number) -> boolean
;; Function:
(define (isCollidedWithBucket a-fish a-bucket offset)
  (<= (calcDist (fish-posi a-fish)
                (pos-x (bucket-posi a-bucket))
                (pos-y (bucket-posi a-bucket)))
      offset))

;; Purpose: Define the fish type
;; Contract: fish-type: a-fish(fish) -> string
;; Function:
(define (fish-type a-fish)
  (if (< (fish-pH a-fish) 7) "Acid" "Base"))

;; -------------------------------------------------------------
;; BACKGROUND FUNCTIONS (1500 x 1000):
;; -------------------------------------------------------------

; Backgrounds of the scenes:
(define menu-background (bitmap "menu background.png"))
(define leaderboard-background (bitmap "leaderboards background.png"))
(define gameover-background (bitmap "gameover background.png"))

; Bubbles for the game scene's background:
(define bubble1  (circle 10 "solid" "white"))
(define bubble2  (circle 8  "solid" "white"))
(define bubble3  (circle 12 "solid" "white"))
(define bubble4  (circle 10 "solid" "white"))
(define bubble5  (circle 8  "solid" "white"))
(define bubble6  (circle 12 "solid" "white"))
(define bubble7  (circle 10 "solid" "white"))
(define bubble8  (circle 8  "solid" "white"))
(define bubble9  (circle 12 "solid" "white"))
(define bubble10 (circle 10 "solid" "white"))
(define bubble11 (circle 8  "solid" "white"))
(define bubble12 (circle 12 "solid" "white"))
(define bubble13 (circle 10 "solid" "white"))
(define bubble14 (circle 8  "solid" "white"))
(define bubble15 (circle 12 "solid" "white"))
(define bubble16 (circle 10 "solid" "white"))
(define bubble17 (circle 8  "solid" "white"))
(define bubble18 (circle 12 "solid" "white"))

; Background for the game scene in complete form:
(define (create-bubble-background)
  (let* ((water (rectangle 1500 1500 "solid" "lightblue"))
         (background-with-bubble1  (place-image bubble1  150 950 water))
         (background-with-bubble2  (place-image bubble2  500 300 background-with-bubble1))
         (background-with-bubble3  (place-image bubble3  800 450 background-with-bubble2))
         (background-with-bubble4  (place-image bubble4  700 1000 background-with-bubble3))
         (background-with-bubble5  (place-image bubble5  1200 150  background-with-bubble4))
         (background-with-bubble6  (place-image bubble6  100  200 background-with-bubble5))
         (background-with-bubble7  (place-image bubble7  350 800 background-with-bubble6))
         (background-with-bubble8  (place-image bubble8  650 700 background-with-bubble7))
         (background-with-bubble9  (place-image bubble9  850 200 background-with-bubble8))
         (background-with-bubble10 (place-image bubble10 1250 300 background-with-bubble9))
         (background-with-bubble11 (place-image bubble11 300  600 background-with-bubble10))
         (background-with-bubble12 (place-image bubble12 600  300 background-with-bubble11))
         (background-with-bubble13 (place-image bubble13 900  800 background-with-bubble12))
         (background-with-bubble14 (place-image bubble14 1100 550 background-with-bubble13))
         (background-with-bubble15 (place-image bubble15 1000 950 background-with-bubble14))
         (background-with-bubble16 (place-image bubble16 450  400 background-with-bubble15))
         (background-with-bubble17 (place-image bubble17 750  100 background-with-bubble16))
         (final-background          (place-image bubble18 1350 650 background-with-bubble17)))
    final-background))

;; -------------------------------------------------------------
;; UPDATING FUNCTIONS:
;; -------------------------------------------------------------

; Used for creating a new game after each try.
(define (create-new-game)
  (make-game
    ListOfFishes      
    0                 
    ACID-BUCKET       
    BASE-BUCKET       
    60                
    0))              

; Purpose: updating the game with tick. ( This updateOnTick is responsible for updating the game in the gameScene. )
; Contract: updateOnTick: g(game) -> game
;; Function:
(define (updateOnTick g)
  (let* ((acidBucket (game-acidBucket g))
         (baseBucket (game-baseBucket g))
         (fish-list (game-AFishList g))
         (current-time (game-time g))
         (counter (game-tick-counter g))
         (new-counter (+ counter 1))
         (new-fish-list
          (map (lambda (a-fish)
                 (cond
                   ((or (isCollidedWithBucket a-fish acidBucket 15)
                        (isCollidedWithBucket a-fish baseBucket 15))
                    (bounceFish (make-fish (fish-fishimage a-fish)
                                           (fish-pH a-fish)
                                           (fish-name a-fish)
                                           (move (fish-posi a-fish) (fish-velo a-fish))
                                           (fish-velo a-fish))
                              1500 1500))
                   (else
                    (bounceFish (make-fish (fish-fishimage a-fish)
                                           (fish-pH a-fish)
                                           (fish-name a-fish)
                                           (move (fish-posi a-fish) (fish-velo a-fish))
                                           (fish-velo a-fish))
                              1500 1500))))
               fish-list))
         (new-score (game-score g)) 
         (one-second? (= new-counter 28))
         (updated-time (if one-second?
                           (max 0 (- current-time 1))
                           current-time))
         (reset-counter (if one-second? 0 new-counter)))
    (make-game new-fish-list
               new-score
               acidBucket
               baseBucket
               updated-time
               reset-counter)))

; Purpose: updating the game with tick. ( This updateOverallGameOnTick is responsible for updating the total game with all the scenes and additions.)
                                      ; ( The overallgame is the full form of the game while the normal game is only the game in the gameScene. )
; Contract: updateOverallGameOnTick: og(overallgame) -> game
;; Function:
(define (updateOverallGameOnTick og)
  (cond
    ((string=? (overallgame-currentScene og) "gameScene")
     (let* ((old-gscene (overallgame-gameScene og))
            (old-game   (gameScene-gm old-gscene))
            (updated-game (updateOnTick old-game))
            (new-current-scene
             (if (zero? (game-time updated-game))
                 "gameOver"
                 "gameScene"))
            (new-gscene (make-gameScene updated-game))
            (new-overallgame
             (make-overallgame
              (overallgame-menu og)
              new-gscene
              (overallgame-gameOver og)
              (overallgame-leaderBoard og)
              (overallgame-goal og)
              (overallgame-score og)
              new-current-scene
              (overallgame-leaderboardfile og))))
       (if (string=? new-current-scene "gameOver")
           (begin
             (writeScore new-overallgame)
             new-overallgame)
           new-overallgame)))
    (else og)))

;; -------------------------------------------------------------
;; MOUSE INTERACTION FUNCTIONS:
;; -------------------------------------------------------------
; Purpose: update the game with respect to the mouse interaction. ( This updateGameOnMouse is responsible for the mouse interaction of the game in the gameScene.)
; Contract: updateOnMouse: g(game) x(number) y(number) event(mouse event) -> obj
; Function:

(define (updateGameOnMouse g x y event)
  (let ((acidBucket (game-acidBucket g))
        (baseBucket (game-baseBucket g))
        (old-score (game-score g)))
    (define new-score old-score

    )

    (define (correct-bucket? a-fish)
      (or (and (isCollidedWithBucket a-fish acidBucket 25)
               (string=? (fish-type a-fish) "Acid"))
          (and (isCollidedWithBucket a-fish baseBucket 25)
               (string=? (fish-type a-fish) "Base"))))

    (define (incorrect-bucket? a-fish)
      (or (and (isCollidedWithBucket a-fish acidBucket 25)
               (not (string=? (fish-type a-fish) "Acid")))
          (and (isCollidedWithBucket a-fish baseBucket 25)
               (not (string=? (fish-type a-fish) "Base")))))

    (define new-fish-list
      (map
       (lambda (a-fish)
         (cond
           ((and (mouse=? event "drag")
                 (isInFish a-fish x y))
            (make-fish (fish-fishimage a-fish)
                       (fish-pH a-fish)
                       (fish-name a-fish)
                       (make-pos x y)
                       (fish-velo a-fish)))
           ((and (mouse=? event "button-up")
                 (isInFish a-fish x y))
            (cond
              ((correct-bucket? a-fish)
               (displayln "Dropped into the correct bucket. +50 points.")
               (set! new-score (+ new-score 50))
               (play-sound sound_path1 #true)
               (make-fish (fish-fishimage a-fish)
                          (fish-pH a-fish)
                          (fish-name a-fish)
                          (make-pos (random 1500) (random 1500))
                          (fish-velo a-fish)))
              ((incorrect-bucket? a-fish)
               (displayln "Dropped into the wrong bucket. 0 points.")
               (play-sound sound_path2 #true)
               a-fish)
              (else a-fish)))
           (else a-fish)))
       (game-AFishList g)))

    (make-game new-fish-list
               new-score
               acidBucket
               baseBucket
               (game-time g)
               (game-tick-counter g))))

; Purpose: updating the game with mouse interaction. ( This updateOverallGameOnMouse is responsible for the mouse interaction of the total game with all the scenes and additions.)
                                                   ; ( The overallgame is the full form of the game while the normal game is only the game in the gameScene. )
; Contract: updateOnMouse: og(overallgame) x(number) y(number) event(mouse event) -> obj
;; Function:
(define (updateOverallGameOnMouse og x y event)
  (cond
    ((string=? (overallgame-currentScene og) "menu")
     (cond
       ((and (mouse=? event "button-down")
             (isClicked (menu-start (overallgame-menu og)) x y
                        button-pos button-im))
        (make-overallgame
         (overallgame-menu og)
         (make-gameScene (create-new-game)) 
         (overallgame-gameOver og)
         (overallgame-leaderBoard og)
         0 
         0 
         "gameScene"
         (overallgame-leaderboardfile og)))
       ((and (mouse=? event "button-down")
             (isClicked (menu-leaderboard (overallgame-menu og)) x y
                        button-pos button-im))
        (make-overallgame
         (overallgame-menu og)
         (overallgame-gameScene og)
         (overallgame-gameOver og)
         (overallgame-leaderBoard og)
         (overallgame-goal og)
         (overallgame-score og)
         "leaderBoard"
         (overallgame-leaderboardfile og)))
       (else og)))
    ((string=? (overallgame-currentScene og) "leaderBoard")
     (cond
       ((and (mouse=? event "button-down")
             (isClicked (leaderBoard-btnReturn (overallgame-leaderBoard og)) x y
                        button-pos button-im))
        (make-overallgame
         (overallgame-menu og)
         (overallgame-gameScene og)
         (overallgame-gameOver og)
         (overallgame-leaderBoard og)
         (overallgame-goal og)
         (overallgame-score og)
         "menu"
         (overallgame-leaderboardfile og)))
       (else og)))
    ((string=? (overallgame-currentScene og) "gameOver")
     (cond
       ((and (mouse=? event "button-down")
             (isClicked (gameOver-btnRestart (overallgame-gameOver og)) x y
                        button-pos button-im))
        (make-overallgame
         (overallgame-menu og)
         (make-gameScene (create-new-game)) 
         (overallgame-gameOver og)
         (overallgame-leaderBoard og)
         0 
         0 
         "menu"
         (overallgame-leaderboardfile og)))
       (else og)))
    ((string=? (overallgame-currentScene og) "gameScene")
     (let* ((old-gscene (overallgame-gameScene og))
            (old-game   (gameScene-gm old-gscene))
            (new-game   (updateGameOnMouse old-game x y event))
            (new-gscene (make-gameScene new-game)))
       (make-overallgame
        (overallgame-menu og)
        new-gscene
        (overallgame-gameOver og)
        (overallgame-leaderBoard og)
        (overallgame-goal og)
        (overallgame-score og)
        (overallgame-currentScene og)
        (overallgame-leaderboardfile og))))
    (else og)))
;; -------------------------------------------------------------
;; DRAWING FUNCTIONS (1500x1000)
;; -------------------------------------------------------------
; Purpose: draw button
; Contract: drawButton: b(button) -> image
; Function:
(define (drawButton b)
  (overlay (text (button-text b) 25 "white") (button-im b)))

;; Purpose: Draw the menu scene
;; Contract: drawMenu: g(game) -> image
;; Function:
(define (drawMenu m)
  (place-image
   (drawButton (menu-start m))
   750 500
   (place-image
    (drawButton (menu-leaderboard m))
    750 700
    menu-background)))

;; Purpose: Drawing the game scene
;; Contract: drawGameScene: g(game) -> image
;; Function:
(define (drawGameScene theGame)
  (let* ((background (create-bubble-background))
         (acidB (game-acidBucket theGame))
         (baseB (game-baseBucket theGame))
         (curr-time (game-time theGame))
         (curr-score (game-score theGame))

         (scene-with-acid
          (place-image (bucket-bucketimage acidB)
                       (pos-x (bucket-posi acidB))
                       (pos-y (bucket-posi acidB))
                       background))

         (scene-with-buckets
          (place-image (bucket-bucketimage baseB)
                       (pos-x (bucket-posi baseB))
                       (pos-y (bucket-posi baseB))
                       scene-with-acid))

         (scene-with-text
          (place-image
           (text (string-append "Score: " (number->string curr-score)) 25 "orange")
           1425 25
           (place-image
            (text (string-append "Time: " (number->string curr-time)) 25 "blue")
            75 25
            scene-with-buckets)))

         (final-scene
          (foldr (lambda (a-fish scn)
                   (place-image (fish-fishimage a-fish)
                                (pos-x (fish-posi a-fish))
                                (pos-y (fish-posi a-fish))
                                scn))
                 scene-with-text
                 (game-AFishList theGame))))
    final-scene))

;; Purpose: Draw the Leaderboard scene
;; Contract: drawLeaderboard: scene(leaderBoard) filename(string) -> image
;; Function:
(define (drawLeaderboard scene filename)
  (cond
    ((not (file-exists? filename))
     (place-image
      (text "-Leaderboard-" 50 "orange") 750 150
      (place-image (text "No record is found..." 12 "black") 750 750
                   (place-image
                    (drawButton (leaderBoard-btnReturn scene))
                    750 750
                    leaderboard-background))))
    (else
     (place-image
      (text "-Leaderboard-" 50 "orange") 750 150
      (place-image (text (read-file filename) 12 "black") 750 500
                   (place-image
                    (drawButton (leaderBoard-btnReturn scene))
                    750 750
                    leaderboard-background))))))

;; Purpose: Draw the Game Over scene
;; Contract: drawGameOver: scene(gameOver) score(number) -> image
;; Function:
(define (drawGameOver scene score)
  (place-image
   (drawButton (gameOver-btnRestart scene))
   750 600 
   (place-image
    (text (string-append "You completed the game with "
                         (number->string  (game-score (gameScene-gm score)))
                         " points...")
          50
          "darkblue")
    750 400 
    gameover-background)))

;; Purpose: Draw the active game state
;; Contract: drawGameActive: g(overallgame) -> image
;; Function:
(define (drawGameActive og)
  (cond
    ((string=? (overallgame-currentScene og) "menu")
     (drawMenu (overallgame-menu og)))
    ((string=? (overallgame-currentScene og) "gameScene")
     (drawGameScene (gameScene-gm (overallgame-gameScene og))))
    ((string=? (overallgame-currentScene og) "gameOver")
     ;; Pass (overallgame-score og) to show final points
     (drawGameOver (overallgame-gameOver og)
                   (overallgame-gameScene og)))
    ((string=? (overallgame-currentScene og) "leaderBoard")
     (drawLeaderboard (overallgame-leaderBoard og)
                      (overallgame-leaderboardfile og)))))

;; -------------------------------------------------------------
;; THE GAME FUNCTIONS:
;; -------------------------------------------------------------

;The game objects:

(define ACID-BUCKET
  (make-bucket (bitmap "Acid Bucket.png")
               6
               (make-pos 500 200)))

(define BASE-BUCKET
  (make-bucket (bitmap "Base Bucket.png")
               8
               (make-pos 1000 200)))

(define ListOfFishes
  (list
    (make-fish (bitmap "Stomach Acid.png")
               2
               "Stomach Acid"
               (make-pos 50 100)
               (make-vel 1.5 1.5))
    (make-fish (bitmap "Battery Liquid.png")
               1
               "Battery Liquid"
               (make-pos 200 300)
               (make-vel -3 3))
    (make-fish (bitmap "Soda.png")
               4
               "Soda"
               (make-pos 400 150)
               (make-vel 1 -2))
    (make-fish (bitmap "Lemon.png")
               3
               "Lemon"
               (make-pos 500 200)
               (make-vel 10 -30))
    (make-fish (bitmap "Milk.png")
               6
               "Milk"
               (make-pos 600 400)
               (make-vel -5 -6))
    (make-fish (bitmap "Coke.png")
               2.5
               "Coke"
               (make-pos 100 600)
               (make-vel 4 4))
    (make-fish (bitmap "Bleach.png")
               12
               "Bleach"
               (make-pos 600 100)
               (make-vel -2 -3))
    (make-fish (bitmap "Drain Cleaner.png")
               14
               "Drain Cleaner"
               (make-pos 100 600)
               (make-vel 1 1))
    (make-fish (bitmap "Oven Cleaner.png")
               13
               "Oven Cleaner"
               (make-pos 750 350)
               (make-vel -1 3))
    (make-fish (bitmap "Detergent.png")
               10
               "Detergent"
               (make-pos 300 300)
               (make-vel 7 3))
    (make-fish (bitmap "Hand Soap.png")
               9
               "Hand Soap"
               (make-pos 250 550)
               (make-vel -3 1))
    (make-fish (bitmap "Baking Soda.png")
               8
               "Baking Soda"
               (make-pos 650 150)
               (make-vel 1 -1))))
; The GAME:

(define theGAME
  (make-game
   ListOfFishes
   0
   ACID-BUCKET
   BASE-BUCKET
   60 ;; start time: 60 seconds
   0)) ;; tick-counter

(define OVERALLGAME
  (make-overallgame
    (make-menu
      (make-button (rectangle 200 50 "solid" "blue")
                   (make-pos 750 500) 
                   "START")
      (make-button (rectangle 200 50 "solid" "blue")
                   (make-pos 750 700) 
                   "LEADERBOARD"))
    (make-gameScene theGAME)
    (make-gameOver
     (make-button (rectangle 150 50 "solid" "blue")
                  (make-pos 750 600) 
                  "RETURN"))
    (make-leaderBoard
     (make-button (rectangle 250 50 "solid" "blue")
                  (make-pos 750 750) 
                  "RETURN MENU"))
    0 
    0 
    "menu"
    "leaderboard.txt"))

; big-bang:

(big-bang
 OVERALLGAME
 (to-draw   drawGameActive)
 (on-tick   updateOverallGameOnTick)
 (on-mouse  updateOverallGameOnMouse))

;; -------------------------------------------------------------
;; TESTS:
;; -------------------------------------------------------------


; calcDist:
(check-expect (calcDist (make-pos 0 0) 3 4) 5)
(check-expect (calcDist (make-pos -3 -4) 0 0) 5)

; isClicked
(define test-button (make-button (rectangle 100 50 "solid" "blue") (make-pos 200 200) "TEST"))
(check-expect (isClicked test-button 200 200 button-pos button-im) #true)
(check-expect (isClicked test-button 300 300 button-pos button-im) #false)

; isInFish
(define test-fish (make-fish (circle 10 "solid" "red") 7 "TestFish" (make-pos 100 100) (make-vel 2 2)))
(check-expect (isInFish test-fish 105 105) #true)
(check-expect (isInFish test-fish 120 120) #false)

(define test-bucket (make-bucket (rectangle 50 50 "solid" "blue") 7 (make-pos 50 50)))
(check-expect (isCollidedWithBucket test-fish test-bucket 5) #false)

(check-expect (fish-type (make-fish (circle 10 "solid" "red") 3 "AcidFish" (make-pos 0 0) (make-vel 1 1))) "Acid")
(check-expect (fish-type (make-fish (circle 10 "solid" "blue") 8 "BaseFish" (make-pos 0 0) (make-vel 1 1))) "Base")

(check-expect (image-width (create-bubble-background)) 1500)
(check-expect (image-height (create-bubble-background)) 1500)

(define test-game (make-game ListOfFishes 100 ACID-BUCKET BASE-BUCKET 30 0))
(check-expect (image-width (drawGameScene test-game)) 1500)
(check-expect (image-height (drawGameScene test-game)) 1500)

;; Define the test-menu
(define test-menu
  (make-menu
    (make-button (rectangle 200 50 "solid" "blue") (make-pos 750 500) "START")
    (make-button (rectangle 200 50 "solid" "blue") (make-pos 750 700) "LEADERBOARD")))

;; Define the expected-menu-image
(define expected-menu-image
  (place-image
   (drawButton (make-button (rectangle 200 50 "solid" "blue") (make-pos 750 500) "START"))
   750 500
   (place-image
    (drawButton (make-button (rectangle 200 50 "solid" "blue") (make-pos 750 700) "LEADERBOARD"))
    750 700
    (empty-scene 1500 1500))))


;; Define test-gameOver
(define test-gameOver
  (make-gameOver
    (make-button (rectangle 150 50 "solid" "blue")
                 (make-pos 750 600)
                 "RESTART")))

;; Define expected-gameOver-image for a score of 150
(define expected-gameOver-image
  (place-image
   (drawButton (make-button (rectangle 150 50 "solid" "blue")
                            (make-pos 750 600)
                            "RESTART"))
   750 600 ;; Centered restart button
   (place-image
    (text "You completed the game with 150 points..." 12 "orange")
    750 400 ;; Centered text
    (empty-scene 1500 1500))))


(test)

