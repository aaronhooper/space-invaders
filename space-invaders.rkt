;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/image)
(require 2htdp/universe)


;; ===========
;; CONSTANTS


(define OUTER-LASER (ellipse (* 2 4) (* 2 12) "solid" "red"))
(define INNER-LASER (ellipse (* 2 3) (* 2 9) "solid" "orange"))
(define LASER-CORE (ellipse (* 2 2) (* 2 6) "solid" "white"))
(define LASER (overlay LASER-CORE INNER-LASER OUTER-LASER))

(define TANK-BASE (rectangle 40 22 "solid" "dark olive green"))
(define TANK-CANNON (rectangle 6 40 "solid" "dark olive green"))
(define TANK-LID (overlay (ellipse 50 20 "outline" "teal")
                          (ellipse 50 20 "solid" "dark slate gray")))
(define TANK (overlay/align "center" "bottom"
                            TANK-LID
                            TANK-CANNON
                            TANK-BASE))

(define INVADER-CHASSIS (ellipse 40 25 "solid" "ghostwhite"))
(define INVADER-COCKPIT (ellipse 25 35 "outline" "ghostwhite"))
(define INVADER (overlay/align "center" "bottom"
                               INVADER-CHASSIS
                               INVADER-COCKPIT))

(define WIDTH 480)
(define HEIGHT 640)
(define NIGHT-SKY (rectangle WIDTH HEIGHT "solid" "midnight blue"))

(define TANK-SPEED 5)
(define LASER-SPEED 10)
(define INVADER-SPEED 3)
(define INVADER-SPAWN-RATE 60)


;; ==================
;; DATA DEFINITIONS


;; Direction is one of:
;;  - "left"
;;  - "right"

#;
(define (fn-for-direction d)
  (cond [(string=? d "left") (...)]
        [(string=? d "right") (...)]))


(define-struct tank (x direction))
;; Tank is (make-tank Number Direction)
;; interp. a tank with an x coordinate and a direction of travel

(define T1 (make-tank (/ WIDTH 2) "left"))
(define T2 (make-tank (- WIDTH 1) "right"))

#;
(define (fn-for-tank t)
  (... (tank-x t)
       (fn-for-direction (tank-direction t))))


(define-struct laser (x y))
;; Laser is (make-laser Number Number)
;; interp. a laser beam with x and y coordinates

(define L1 (make-laser 20 35))
(define L2 (make-laser 4 135))

#;
(define (fn-for-laser l)
  (... (laser-x l) (laser-y l)))


;; ListOfLaser is one of:
;;  - empty
;;  - (cons Laser ListOfLaser)
;; interp. a list of laser beams

(define LOL1 empty)
(define LOL2 (cons (make-laser 20 35) empty))
(define LOL3 (cons (make-laser 20 35)
                   (cons (make-laser 32 36) empty)))

#;
(define (fn-for-lol lol)
  (cond [(empty? lol) (...)]
        [else (... (fn-for-laser (first lol))
                   (fn-for-lol (rest lol)))]))


(define-struct invader (x y direction))
;; Invader is (make-invader Number Number Direction)
;; interp. an invader spaceship with x and y coordinates and a direction
;;         of travel

(define I1 (make-invader 20 23 "left"))
(define I2 (make-invader 52 156 "right"))

#;
(define (fn-for-invader i)
  (... (invader-x i)
       (invader-y i)
       (fn-for-direction (invader-direction i))))


;; ListOfInvader is one of:
;;  - empty
;;  - (cons Invader ListOfInvader)
;; interp. a list of invader spaceships

(define LOI1 empty)
(define LOI2 (cons (make-invader 20 23 "left") empty))
(define LOI3 (cons (make-invader 20 23 "left")
                   (cons (make-invader 52 156 "right") empty)))

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else (... (fn-for-invader (first loi))
                   (fn-for-loi (rest loi)))]))


;; TickCounter is Integer[0,1680)
;; interp. the number of ticks that have passed in the game world up to one
;; minute, or 1680 ticks at 28 ticks/s

(define TC1 0)
(define TC2 132)
(define TC3 1473)
(define TC4 1679)

#;
(define (fn-for-tc tc)
  (... tc))


(define-struct gamestate (tank lasers invaders tick-count over?))
;; GameState is (make-gamestate Tank
;;                              ListOfLaser
;;                              ListOfInvader
;;                              TickCounter
;;                              Boolean)
;; interp. the game world state that tracks a tank, many laser beams, many
;;         invader spaceships, a world tick counter and a running status

(define GS0 (make-gamestate (make-tank 0 "right")
                            empty empty
                            0 false))
(define GS1 (make-gamestate (make-tank 36 "right")
                            empty empty
                            12
                            false))
(define GS2 (make-gamestate (make-tank 34 "left")
                            (list (make-laser 20 35)
                                  (make-laser 4 135))
                            (list (make-invader 20 23 "left")
                                  (make-invader 52 156 "right"))
                            18
                            false))

#;
(define (fn-for-gamestate gs)
  (... (fn-for-tank (gamestate-tank gs))
       (fn-for-lol (gamestate-lasers gs))
       (fn-for-loi (gamestate-invaders gs))
       (fn-for-tc (gamestate-tick-count gs))
       (gamestate-over? gs)))


;; ===========
;; FUNCTIONS


;; GameState -> GameState
;; The main program routine.
(define (main gs)
  (big-bang gs
    (name "Wuhan Invaders (Space Pathogen)")
    (on-tick handle-tick)
    (to-draw handle-draw)
    (on-key handle-key)
    (stop-when handle-stop
               game-over-scene)))


;; GameState -> GameState
;; Returns the next state of the game world.
;(define (handle-tick gs) gs)  ; stub

(check-expect (handle-tick (make-gamestate (make-tank 34 "left")
                                           (list (make-laser 20 35)
                                                 (make-laser 4 135))
                                           (list (make-invader 20 23 "left")
                                                 (make-invader 52 156 "right"))
                                           2
                                           false))
              (spawn-invader INVADER-SPAWN-RATE
                             (gamestate-invader-victory-check
                              (increment-counter
                               (remove-collisions
                                (tick-gamestate-invaders
                                 (tick-tank
                                  (tick-gamestate-lasers
                                   (make-gamestate
                                    (make-tank 34 "left")
                                    (list (make-laser 20 35)
                                          (make-laser 4 135))
                                    (list (make-invader 20 23 "left")
                                          (make-invader 52 156 "right"))
                                    2
                                    false)))))))))

(define (handle-tick gs)
  (spawn-invader INVADER-SPAWN-RATE
                 (gamestate-invader-victory-check
                  (increment-counter
                   (remove-collisions
                    (tick-gamestate-invaders
                     (tick-tank
                      (tick-gamestate-lasers gs))))))))


;; GameState -> GameState
;; Checks that an invader has not reached the end of the screen.
;; If it has, sets the over? field to true and subsequently ends the game.
;(define (gamestate-invader-victory-check gs) gs)  ; stub

(check-expect (gamestate-invader-victory-check (make-gamestate
                                                (make-tank 0 "right")
                                                empty empty
                                                0 false))
              (make-gamestate (make-tank 0 "right")
                              empty empty
                              0 (invader-victory-check empty)))

(define (gamestate-invader-victory-check gs)
  (make-gamestate (gamestate-tank gs)
                  (gamestate-lasers gs)
                  (gamestate-invaders gs)
                  (gamestate-tick-count gs)
                  (invader-victory-check (gamestate-invaders gs))))


;; ListOfInvader -> Boolean
;; Returns true if any one of the invaders has reached the end of the screen.
;(define (invader-victory-check loi) false)  ; stub

(check-expect (invader-victory-check empty) false)
(check-expect (invader-victory-check (list (make-invader 0 20 "right")))
              false)
(check-expect (invader-victory-check (list
                                      (make-invader 0 20 "right")
                                      (make-invader 0 (+ HEIGHT 1) "left")))
              true)

(define (invader-victory-check loi)
  (cond [(empty? loi) false]
        [(> (invader-y (first loi)) HEIGHT) true]
        [else (invader-victory-check (rest loi))]))


;; GameState -> GameState
;; Returns the next game state of the invaders.
;(define (tick-gamestate-invaders gs) gs)  ; stub

(check-expect (tick-gamestate-invaders
               (make-gamestate (make-tank 34 "left")
                               (list (make-laser 20 35)
                                     (make-laser 4 135))
                               (list (make-invader 20 23 "left")
                                     (make-invader 52 156 "right"))
                               18
                               false))
              (make-gamestate (make-tank 34 "left")
                              (list (make-laser 20 35)
                                    (make-laser 4 135))
                              (tick-invaders
                               (list (make-invader 20 23 "left")
                                     (make-invader 52 156 "right")))
                              18
                              false))

(define (tick-gamestate-invaders gs)
  (make-gamestate (gamestate-tank gs)
                  (gamestate-lasers gs)
                  (tick-invaders (gamestate-invaders gs))
                  (gamestate-tick-count gs)
                  (gamestate-over? gs)))


;; ListOfInvader -> ListOfInvader
;; Returns the next state of the given invaders.
;(define (tick-invaders loi) loi)  ; stub

(check-expect (tick-invaders empty) empty)
(check-expect (tick-invaders (list (make-invader 20 30 "right")
                                   (make-invader 40 50 "left")))
              (list (tick-invader (make-invader 20 30 "right"))
                    (tick-invader (make-invader 40 50 "left"))))

(define (tick-invaders loi)
  (cond [(empty? loi) empty]
        [else (cons (tick-invader (first loi))
                    (tick-invaders (rest loi)))]))


;; Invader -> Invader
;; Returns the next state of the given invader.
;; When the invader hits the left or right bound, it changes direction.
;; Any movement of the invader out of the scene left and right bounds will
;; move the invader 1 pixel in from the edge to avoid the invader "sticking"
;; to the wall.
;(define (tick-invader invader) i)  ; stub

(check-expect (tick-invader (make-invader 20 23 "left"))
              (make-invader (- 20 INVADER-SPEED)
                            (+ 23 INVADER-SPEED)
                            "left"))
(check-expect (tick-invader (make-invader 52 156 "right"))
              (make-invader (+ 52 INVADER-SPEED)
                            (+ 156 INVADER-SPEED)
                            "right"))
(check-expect (tick-invader (make-invader -3 23 "left"))
              (make-invader (+ 0 1) (+ 23 INVADER-SPEED) "right"))
(check-expect (tick-invader (make-invader (+ (- WIDTH (image-width INVADER)) 3)
                                          23 "right"))
              (make-invader (- (- WIDTH (image-width INVADER)) 1)
                            (+ 23 INVADER-SPEED) "left"))

(define (tick-invader i)
  (cond [(<= (invader-x i) 0)
         (make-invader 1 (+ (invader-y i)
                            INVADER-SPEED)
                       "right")]
        [(>= (invader-x i) (- WIDTH (image-width INVADER)))
         (make-invader (- (- WIDTH (image-width INVADER)) 1)
                       (+ (invader-y i)
                          INVADER-SPEED)
                       "left")]
        [(string=? (invader-direction i) "left")
         (make-invader (- (invader-x i) INVADER-SPEED)
                       (+ (invader-y i) INVADER-SPEED)
                       "left")]
        [(string=? (invader-direction i) "right")
         (make-invader (+ (invader-x i) INVADER-SPEED)
                       (+ (invader-y i) INVADER-SPEED)
                       "right")]))


;; GameState -> GameState
;; Returns the next game state of the tank.
;; An out of bounds tank is set 1 pixel in from the border to prevent
;; sticking.
;(define (tick-tank gs) gs)  ; stub

(check-expect (tick-tank (make-gamestate (make-tank 34 "left")
                                         empty empty
                                         18
                                         false))
              (make-gamestate (make-tank (- 34 TANK-SPEED) "left")
                              empty empty
                              18
                              false))
(check-expect (tick-tank (make-gamestate (make-tank 34 "right")
                                         empty empty
                                         18
                                         false))
              (make-gamestate (make-tank (+ 34 TANK-SPEED) "right")
                              empty empty
                              18
                              false))
(check-expect (tick-tank (make-gamestate (make-tank -3 "left")
                                         empty empty
                                         18
                                         false))
              (make-gamestate (make-tank (+ 0 1) "right")
                              empty empty
                              18
                              false))
(check-expect (tick-tank (make-gamestate (make-tank (+ (- WIDTH
                                                          (image-width TANK))
                                                       3)
                                                    "right")
                                         empty empty
                                         18
                                         false))
              (make-gamestate (make-tank (- (- WIDTH
                                               (image-width TANK))
                                            1)
                                         "left")
                              empty empty
                              18
                              false))

(define (tick-tank gs)
  (cond [(<= (tank-x (gamestate-tank gs)) 0)
         (make-gamestate (make-tank 1 "right")
                         (gamestate-lasers gs)
                         (gamestate-invaders gs)
                         (gamestate-tick-count gs)
                         (gamestate-over? gs))]
        [(>= (tank-x (gamestate-tank gs)) (- WIDTH (image-width TANK)))
         (make-gamestate (make-tank (- (- WIDTH (image-width TANK)) 1) "left")
                         (gamestate-lasers gs)
                         (gamestate-invaders gs)
                         (gamestate-tick-count gs)
                         (gamestate-over? gs))]
        [(string=? (tank-direction (gamestate-tank gs)) "left")
         (make-gamestate (make-tank (- (tank-x (gamestate-tank gs))
                                       TANK-SPEED)
                                    "left")
                         (gamestate-lasers gs)
                         (gamestate-invaders gs)
                         (gamestate-tick-count gs)
                         (gamestate-over? gs))]
        [(string=? (tank-direction (gamestate-tank gs)) "right")
         (make-gamestate (make-tank (+ (tank-x (gamestate-tank gs))
                                       TANK-SPEED)
                                    "right")
                         (gamestate-lasers gs)
                         (gamestate-invaders gs)
                         (gamestate-tick-count gs)
                         (gamestate-over? gs))]))


;; GameState -> GameState
;; Returns the next game state of the lasers.
;(define (tick-gamestate-lasers gs) gs)  ; stub

(check-expect (tick-gamestate-lasers (make-gamestate (make-tank 34 "left")
                                                     (list (make-laser 20 35)
                                                           (make-laser 4 135))
                                                     empty
                                                     18
                                                     false))
              (make-gamestate (make-tank 34 "left")
                              (list (make-laser 20 (- 35 LASER-SPEED))
                                    (make-laser 4 (- 135 LASER-SPEED)))
                              empty
                              18
                              false))

(define (tick-gamestate-lasers gs)
  (make-gamestate (gamestate-tank gs)
                  (tick-lasers (gamestate-lasers gs))
                  (gamestate-invaders gs)
                  (gamestate-tick-count gs)
                  (gamestate-over? gs)))


;; ListOfLaser -> ListOfLaser
;; Returns the next state of the lasers. Lasers no longer on the scene are
;; removed from the list.
;(define (tick-lasers lol) lol)  ; stub

(check-expect (tick-lasers empty) empty)
(check-expect (tick-lasers (cons (make-laser 20 30) empty))
              (cons (make-laser 20 (- 30 LASER-SPEED)) empty))
(check-expect (tick-lasers (cons (make-laser 20 -1) empty))
              empty)

(define (tick-lasers lol)
  (remove-out-lasers (advance-lasers lol)))


;; ListOfLaser -> ListOfLaser
;; Removes any lasers off the scene from the list.
;(define (remove-out-lasers lol) lol)  ; stub

(check-expect (remove-out-lasers empty) empty)
(check-expect (remove-out-lasers (list (make-laser 20 20)))
              (list (make-laser 20 20)))
(check-expect (remove-out-lasers (list (make-laser 20 -3)
                                       (make-laser 20 20)))
              (list (make-laser 20 20)))
(check-expect (remove-out-lasers (list (make-laser 40 40)
                                       (make-laser 40 -10)))
              (list (make-laser 40 40)))

(define (remove-out-lasers lol)
  (cond [(empty? lol) empty]
        [else (if (< (laser-y (first lol)) 0)
                  (remove-out-lasers (rest lol))
                  (cons (first lol)
                        (remove-out-lasers (rest lol))))]))


;; ListOfLaser -> ListOfLaser
;; Advances the laser positions in the -y direction by their speeds.
;(define (advance-lasers lol) lol)  ; stub

(check-expect (advance-lasers empty) empty)
(check-expect (advance-lasers (list (make-laser 20 20)))
              (list (make-laser 20 (- 20 LASER-SPEED))))
(check-expect (advance-lasers (list (make-laser 20 20)
                                    (make-laser 40 12)))
              (list (make-laser 20 (- 20 LASER-SPEED))
                    (make-laser 40 (- 12 LASER-SPEED))))

(define (advance-lasers lol)
  (cond [(empty? lol) empty]
        [else (cons (make-laser (laser-x (first lol))
                                (- (laser-y (first lol))
                                   LASER-SPEED))
                    (advance-lasers (rest lol)))]))


;; GameState -> GameState
;; Removes any invaders and lasers that have crossed paths with each other.
;(define (remove-collisions gs) gs)  ; stub

(check-expect (remove-collisions
               (make-gamestate (make-tank 34 "left")
                               (list (make-laser 20 35)
                                     (make-laser 4 135))
                               (list (make-invader 20 35 "left")
                                     (make-invader 52 156 "right"))
                               18
                               false))
              (make-gamestate
               (make-tank 34 "left")
               (remove-hit-lasers (list (make-laser 20 35)
                                        (make-laser 4 135))
                                  (list (make-invader 20 35 "left")
                                        (make-invader 52 156 "right")))
               (remove-shot-invaders (list (make-invader 20 35 "left")
                                           (make-invader 52 156 "right"))
                                     (list (make-laser 20 35)
                                           (make-laser 4 135)))
               18
               false))

(define (remove-collisions gs)
  (make-gamestate (gamestate-tank gs)
                  (remove-hit-lasers (gamestate-lasers gs)
                                     (gamestate-invaders gs))
                  (remove-shot-invaders (gamestate-invaders gs)
                                        (gamestate-lasers gs))
                  (gamestate-tick-count gs)
                  (gamestate-over? gs)))


;; ListOfInvader ListOfLaser -> ListOfInvader
;; Returns the invaders that do not intersect with a laser.
;(define (remove-shot-invaders loi lol) loi)  ; stub

(check-expect (remove-shot-invaders empty
                                    (list (make-laser 20 23)
                                          (make-laser 385 340)))
              empty)
(check-expect (remove-shot-invaders (list (make-invader 20 23 "left")
                                          (make-invader 52 156 "right"))
                                    (list (make-laser 20 23)
                                          (make-laser 385 340)))
              (list (make-invader 52 156 "right")))
(check-expect (remove-shot-invaders (list (make-invader 120 150 "left")
                                          (make-invader 52 156 "right"))
                                    (list (make-laser 20 23)
                                          (make-laser 385 340)))
              (list (make-invader 120 150 "left")
                    (make-invader 52 156 "right")))

(define (remove-shot-invaders loi lol)
  (cond [(empty? loi) empty]
        [else (if (invader-is-shot? (first loi) lol)
                  (remove-shot-invaders (rest loi) lol)
                  (cons (first loi)
                        (remove-shot-invaders (rest loi) lol)))]))


;; Invader ListOfLaser -> Boolean
;; Returns true if the given invader has intersected with any of the lasers.
;(define (invader-is-shot? i lol) false)  ; stub

(check-expect (invader-is-shot? (make-invader 20 30 "left")
                                empty)
              false)
(check-expect (invader-is-shot? (make-invader 20 23 "left")
                                (list (make-laser 20 23)
                                      (make-laser 385 340)))
              true)
(check-expect (invader-is-shot? (make-invader 20 23 "left")
                                (list (make-laser 123 234)
                                      (make-laser 385 340)))
              false)

(define (invader-is-shot? i lol)
  (cond [(empty? lol) false]
        [(intersects? (first lol) i) true]
        [else (invader-is-shot? i (rest lol))]))


;; Laser Invader -> Boolean
;; Returns true if the given laser and invader intersect with each other.
;(define (intersects? l i) false)  ; stub

(define L20 (make-laser 20 30))
(define I30 (make-invader 30 40 "left"))
(check-expect (intersects? L20
                           (make-invader 20 30 "left"))
              true)
(check-expect (intersects? L20 I30)
              (and (intersects-x? L20 I30)
                   (intersects-y? L20 I30)))

(define (intersects? l i)
  (and (intersects-x? l i)
       (intersects-y? l i)))


;; Laser Invader -> Boolean
;; Returns true if the given laser and invader intersect on the x axis.
;(define (intersects-x? l i) false)  ; stub

(define L50 (make-laser 50 50))
(define I50 (make-invader 50 65 "left"))
(check-expect (intersects-x? L50 (make-invader 50 50 "left"))
              true)
(check-expect (intersects-x? L50 I50)
              (and (> (laser-x L50)
                      (- (invader-x I50)
                         (/ (image-width INVADER) 2)))
                   (< (laser-x L50)
                      (+ (invader-x I50)
                         (/ (image-width INVADER) 2)))))

(define (intersects-x? l i)
  (and (> (laser-x l)
          (- (invader-x i)
             (/ (image-width INVADER) 2)))
       (< (laser-x l)
          (+ (invader-x i)
             (/ (image-width INVADER) 2)))))
  

;; Laser Invader -> Boolean
;; Returns true if the given laser and invader intersect on the y axis.
;(define (intersects-y? l i) false)  ; stub

(check-expect (intersects-y? L50
                             (make-invader 50 50 "left"))
              true)
(check-expect (intersects-y? L50 I50)
              (and (> (laser-y L50)
                      (- (invader-y I50)
                         (/ (image-height INVADER) 2)))
                   (< (laser-y L50)
                      (+ (invader-y I50)
                         (/ (image-height INVADER) 2)))))

(define (intersects-y? l i)
  (and (> (laser-y l)
          (- (invader-y i)
             (/ (image-height INVADER) 2)))
       (< (laser-y l)
          (+ (invader-y i)
             (/ (image-height INVADER) 2)))))


;; ListOfLaser ListOfInvader -> ListOfLaser
;; Returns the lasers that do not intersect with an invader.
;(define (remove-hit-lasers lol loi) lol)  ; stub

(check-expect (remove-hit-lasers empty
                                 (list (make-invader 20 23 "left")
                                       (make-invader 385 340 "right")))
              empty)
(check-expect (remove-hit-lasers (list (make-laser 20 23)
                                       (make-laser 52 156))
                                 (list (make-invader 20 23 "left")
                                       (make-invader 385 340 "right")))
              (list (make-laser 52 156)))
(check-expect (remove-hit-lasers (list (make-laser 120 150)
                                       (make-laser 52 156))
                                 (list (make-invader 20 23 "left")
                                       (make-invader 385 340 "right")))
              (list (make-laser 120 150)
                    (make-laser 52 156)))

(define (remove-hit-lasers lol loi)
  (cond [(empty? lol) empty]
        [else (if (laser-has-hit? (first lol) loi)
                  (remove-hit-lasers (rest lol) loi)
                  (cons (first lol)
                        (remove-hit-lasers (rest lol) loi)))]))


;; Laser ListOfInvader -> Boolean
;; Returns true if the laser has hit any of the invaders.
;(define (laser-has-hit? l loi) false)  ; stub

(check-expect (laser-has-hit? (make-laser 20 30)
                              empty)
              false)
(check-expect (laser-has-hit? (make-laser 20 23)
                              (list (make-invader 20 23 "left")
                                    (make-invader 385 340 "right")))
              true)
(check-expect (laser-has-hit? (make-laser 20 23)
                              (list (make-invader 123 234 "left")
                                    (make-invader 385 340 "right")))
              false)

(define (laser-has-hit? l loi)
  (cond [(empty? loi) false]
        [(intersects? l (first loi)) true]
        [else (laser-has-hit? l (rest loi))]))


;; GameState -> GameState
;; Increments the tick counter in the game world.
;(define (increment-counter gs) gs)  ; stub

(check-expect (increment-counter
               (make-gamestate (make-tank 34 "left")
                               empty empty
                               18
                               false))
              (make-gamestate (make-tank 34 "left")
                              empty empty
                              19
                              false))
(check-expect (increment-counter
               (make-gamestate (make-tank 34 "left")
                               empty empty
                               1679
                               false))
              (make-gamestate (make-tank 34 "left")
                              empty empty
                              0
                              false))

(define (increment-counter gs)
  (if (>= (gamestate-tick-count gs) 1679)
      (make-gamestate (gamestate-tank gs)
                      (gamestate-lasers gs)
                      (gamestate-invaders gs)
                      0
                      (gamestate-over? gs))
      (make-gamestate (gamestate-tank gs)
                      (gamestate-lasers gs)
                      (gamestate-invaders gs)
                      (add1 (gamestate-tick-count gs))
                      (gamestate-over? gs))))


;; Integer GameState -> GameState
;; Spawns an invader in the game world at every specified number of ticks of
;; the game world.
;(define (spawn-invader tick gs) gs)  ; stub

(check-random (spawn-invader 28 (make-gamestate
                                 (make-tank 34 "left")
                                 (list (make-laser 20 35)
                                       (make-laser 4 135))
                                 (list (make-invader 20 23 "left")
                                       (make-invader 52 156 "right"))
                                 28
                                 false))
              (make-gamestate (make-tank 34 "left")
                              (list (make-laser 20 35)
                                    (make-laser 4 135))
                              (list (make-invader 20 23 "left")
                                    (make-invader 52 156 "right")
                                    (make-invader (random WIDTH) 0
                                                  (random-direction "")))
                              28
                              false))
(check-random (spawn-invader 52 (make-gamestate
                                 (make-tank 34 "left")
                                 (list (make-laser 20 35)
                                       (make-laser 4 135))
                                 (list (make-invader 20 23 "left")
                                       (make-invader 52 156 "right"))
                                 28
                                 false))
              (make-gamestate (make-tank 34 "left")
                              (list (make-laser 20 35)
                                    (make-laser 4 135))
                              (list (make-invader 20 23 "left")
                                    (make-invader 52 156 "right"))
                              28
                              false))
(check-random (spawn-invader 26 (make-gamestate
                                 (make-tank 34 "left")
                                 (list (make-laser 20 35)
                                       (make-laser 4 135))
                                 (list (make-invader 20 23 "left")
                                       (make-invader 52 156 "right"))
                                 52
                                 false))
              (make-gamestate (make-tank 34 "left")
                              (list (make-laser 20 35)
                                    (make-laser 4 135))
                              (list (make-invader 20 23 "left")
                                    (make-invader 52 156 "right")
                                    (make-invader (random WIDTH) 0
                                                  (random-direction "")))
                              52
                              false))

(define (spawn-invader tick gs)
  (if (= (modulo (gamestate-tick-count gs) tick) 0)
      (make-gamestate (gamestate-tank gs)
                      (gamestate-lasers gs)
                      (add-invader (gamestate-invaders gs))
                      (gamestate-tick-count gs)
                      (gamestate-over? gs))
      gs))


;; String -> Direction
;; Returns a random direction.
;(define (random-direction s) "left")  ; stub

(check-random (random-direction "")
              (cond [(= (random 2) 0) "left"]
                    [else "right"]))

(define (random-direction s)
  (cond [(= (random 2) 0) "left"]
        [else "right"]))


;; ListOfInvader -> ListOfInvader
;; Adds an invader with a random x coordinate and direction to the list.
;(define (add-invader loi) loi)  ; stub

(check-random (add-invader empty) (list (make-invader (random WIDTH) 0
                                                      (random-direction ""))))
(check-random (add-invader (list (make-invader 20 30 "left")))
              (list (make-invader 20 30 "left")
                    (make-invader (random WIDTH) 0 (random-direction ""))))

(define (add-invader loi)
  (cond [(empty? loi)
         (cons (make-invader (random WIDTH) 0
                             (random-direction ""))
               empty)]
        [else (cons (first loi)
                    (add-invader (rest loi)))]))


;; GameState -> Image
;; Returns an image of the game world.
;(define (handle-draw gs) empty-image)  ; stub

(check-expect (handle-draw (make-gamestate (make-tank 34 "left")
                                           (list (make-laser 20 35)
                                                 (make-laser 4 135))
                                           (list (make-invader 20 23 "left")
                                                 (make-invader 52 156 "right"))
                                           2
                                           false))
              (underlay/align/offset "left" "bottom"
                                     (underlay/xy
                                      (underlay/xy
                                       (underlay/xy
                                        (underlay/xy NIGHT-SKY 20 23 INVADER)
                                        52 156 INVADER)
                                       20 35 LASER)
                                      4 135 LASER)
                                     34 0 TANK))

(define (handle-draw gs)
  (draw-tank (gamestate-tank gs)
             (draw-lasers (gamestate-lasers gs)
                          (draw-invaders (gamestate-invaders gs)
                                         NIGHT-SKY))))


;; Tank -> Image
;; Draws the tank at its position onto the scene.
;(define (draw-tank tank scene) empty-image)  ; stub

(check-expect (draw-tank (make-tank 20 "left") NIGHT-SKY)
              (underlay/align/offset "left" "bottom"
                                     NIGHT-SKY
                                     20 0 TANK))
(check-expect (draw-tank (make-tank 163 "right") NIGHT-SKY)
              (underlay/align/offset "left" "bottom"
                                     NIGHT-SKY
                                     163 0 TANK))

(define (draw-tank tank scene)
  (underlay/align/offset "left" "bottom"
                         scene
                         (tank-x tank) 0 TANK))


;; ListOfLaser -> Image
;; Draws the lasers at their positions onto the scene.
;(define (draw-lasers lol scene) empty-image)  ; stub

(check-expect (draw-lasers empty NIGHT-SKY) NIGHT-SKY)
(check-expect (draw-lasers (list (make-laser 20 150))
                           NIGHT-SKY)
              (underlay/xy NIGHT-SKY 20 150 LASER))
(check-expect (draw-lasers (list (make-laser 20 150)
                                 (make-laser 35 250))
                           NIGHT-SKY)
              (underlay/xy (underlay/xy NIGHT-SKY
                                        20 150 LASER)
                           35 250 LASER))

(define (draw-lasers lol scene)
  (cond [(empty? lol) scene]
        [else (underlay/xy (draw-lasers (rest lol) scene)
                           (laser-x (first lol))
                           (laser-y (first lol))
                           LASER)]))


;; ListOfInvader -> Image
;; Draws the invaders at their positions onto the scene.
;(define (draw-invaders loi scene) empty-image)  ; stub

(check-expect (draw-invaders empty NIGHT-SKY) NIGHT-SKY)
(check-expect (draw-invaders (list (make-invader 25 35 "left"))
                             NIGHT-SKY)
              (underlay/xy NIGHT-SKY 25 35 INVADER))
(check-expect (draw-invaders (list (make-invader 25 35 "left")
                                   (make-invader 55 75 "right"))
                             NIGHT-SKY)
              (underlay/xy (underlay/xy NIGHT-SKY
                                        25 35 INVADER)
                           55 75 INVADER))

(define (draw-invaders loi scene)
  (cond [(empty? loi) scene]
        [else (underlay/xy (draw-invaders (rest loi) scene)
                           (invader-x (first loi))
                           (invader-y (first loi))
                           INVADER)]))


;; GameState KeyEvent -> GameState
;; Handles all of the key events that interact with the game world.
;; The left and right arrow keys trigger movement of the tank towards
;; the respective direction.
;; The up arrow key or the space key causes the tank to fire a laser.
;; All other keys do not modify the game state.
;(define (handle-key gs kevent) gs)  ; stub

(check-expect (handle-key (make-gamestate (make-tank 34 "left")
                                          empty empty
                                          3
                                          false)
                          "right")
              (make-gamestate (make-tank 34 "right")
                              empty empty
                              3
                              false))
(check-expect (handle-key (make-gamestate (make-tank 34 "right")
                                          empty empty
                                          4
                                          false)
                          "left")
              (make-gamestate (make-tank 34 "left")
                              empty empty
                              4
                              false))
(check-expect (handle-key (make-gamestate (make-tank 34 "right")
                                          empty empty
                                          3
                                          false)
                          "right")
              (make-gamestate (make-tank 34 "right")
                              empty empty
                              3
                              false))
(check-expect (handle-key (make-gamestate (make-tank 34 "left")
                                          empty empty
                                          4
                                          false)
                          "up")
              (make-gamestate (make-tank 34 "left")
                              (list (make-laser 34 (- (image-height NIGHT-SKY)
                                                      (image-height TANK))))
                              empty
                              4
                              false))
(check-expect (handle-key (make-gamestate (make-tank 34 "left")
                                          empty empty
                                          3
                                          false)
                          " ")
              (make-gamestate (make-tank 34 "left")
                              (list (make-laser 34 (- (image-height NIGHT-SKY)
                                                      (image-height TANK))))
                              empty
                              3
                              false))
(check-expect (handle-key (make-gamestate (make-tank 34 "left")
                                          empty empty
                                          3
                                          false)
                          "down")
              (make-gamestate (make-tank 34 "left")
                              empty empty
                              3
                              false))


(define (handle-key gs kevent)
  (cond [(key=? kevent "left")
         (make-gamestate (make-tank (tank-x (gamestate-tank gs))
                                    "left")
                         (gamestate-lasers gs)
                         (gamestate-invaders gs)
                         (gamestate-tick-count gs)
                         (gamestate-over? gs))]
        [(key=? kevent "right")
         (make-gamestate (make-tank (tank-x (gamestate-tank gs))
                                    "right")
                         (gamestate-lasers gs)
                         (gamestate-invaders gs)
                         (gamestate-tick-count gs)
                         (gamestate-over? gs))]
        [(or (key=? kevent "up")
             (key=? kevent " "))
         (make-gamestate (gamestate-tank gs)
                         (add-laser (gamestate-lasers gs)
                                    (tank-x (gamestate-tank gs)))
                         (gamestate-invaders gs)
                         (gamestate-tick-count gs)
                         (gamestate-over? gs))]
        [else gs]))


;; ListOfLaser Number -> ListOfLaser
;; Adds a laser with the given x coordinate to the list.
;(define (add-laser lol x) empty)  ; stub

(check-expect (add-laser empty 25)
              (cons (make-laser 25 (- (image-height NIGHT-SKY)
                                      (image-height TANK)))
                    empty))
(check-expect (add-laser (cons (make-laser 38 50) empty) 70)
              (cons (make-laser 38 50)
                    (cons (make-laser 70 (- (image-height NIGHT-SKY)
                                            (image-height TANK)))
                          empty)))

(define (add-laser lol x)
  (cond [(empty? lol)
         (cons (make-laser x (- (image-height NIGHT-SKY)
                                (image-height TANK)))
               empty)]
        [else (cons (first lol) (add-laser (rest lol) x))]))


;; GameState Image -> Boolean
;; Stops the world once the over? field is set to true.
;(define (handle-stop gs) false)  ; stub

(check-expect (handle-stop (make-gamestate (make-tank 34 "left")
                                           empty empty
                                           18
                                           false))
              false)
(check-expect (handle-stop (make-gamestate (make-tank 34 "left")
                                           empty empty
                                           18
                                           true))
              true)

(define (handle-stop gs)
  (gamestate-over? gs))


;; GameState -> Image
;; Returns the "GAME OVER" screen that will be shown when the game ends.
;(define (game-over-scene gs) empty-image)  ; stub

(check-expect (game-over-scene (make-gamestate (make-tank 34 "left")
                                               empty empty
                                               18
                                               true))
              (overlay (text "GAME\nOVER" 72 "darkorange")
                       (handle-draw (make-gamestate (make-tank 34 "left")
                                                    empty empty
                                                    18
                                                    true))))

(define (game-over-scene gs)
  (overlay (text "GAME\nOVER" 72 "darkorange")
           (handle-draw gs)))


;; ======
;; MAIN

(main GS0)
