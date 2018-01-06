;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname space_invader) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; DATA

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
(define INVADER-WIDTH/2 (/ (image-width INVADER) 2))
(define INVADER-WIDTH (image-width INVADER))
(define INVADER-HEIGHT/2 (/ (image-height INVADER) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))

(define-struct game (invaders missiles tank))
(define-struct tank (x dir))
(define-struct invader (x y dx))
(define-struct missile (x y))

;; FUNCTIONS

(define (next-invader invader)
  (cond [(> (+ (invader-x invader) (invader-dx invader)) (- WIDTH INVADER-WIDTH/2))
         (make-invader
          (- (- WIDTH INVADER-WIDTH/2) (- (+ (invader-x invader) (invader-dx invader)) (- WIDTH INVADER-WIDTH/2)))
          (+ (invader-y invader) (abs (invader-dx invader)))
          (* -1 (invader-dx invader)))]
        [(< (+ (invader-x invader) (invader-dx invader)) INVADER-WIDTH/2)
         (make-invader
          (abs (+ (invader-x invader) (invader-dx invader)))
          (+ (invader-y invader) (abs (invader-dx invader)))
          (* -1 (invader-dx invader)))]
        [else (make-invader
               (+ (invader-x invader) (invader-dx invader))
               (+ (invader-y invader) (abs (invader-dx invader)))
               (invader-dx invader))]))

(define (next-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))

(define (fn-for-tank t)
  (cond [(equal? (tank-dir t) -1)
         (if (<= (tank-x t) 0)
             (make-tank 0 -1)
             (make-tank (- (tank-x t) TANK-SPEED) (tank-dir t)))]
        [(equal? (tank-dir t) 1)
         (if (>= (tank-x t) WIDTH)
         (make-tank WIDTH 1)
         (make-tank (+ (tank-x t) TANK-SPEED) (tank-dir t)))]
        [else (make-tank (tank-x t) (tank-dir t))]))

(define (fn-for-invaders loi)
  (cond [(empty? loi) empty]
        [else (cons (next-invader (first loi))
              (fn-for-invaders (rest loi)))]))

(define (fn-for-missiles lom)
    (cond [(empty? lom) empty]
        [else (cons (next-missile (first lom))
              (fn-for-missiles (rest lom)))]))

(define (get-missile-images lom)
  (cond [(empty? lom) empty]
        [else (cons MISSILE (get-missile-images (rest lom)))]))

(define (get-missile-posns lom)
  (cond [(empty? lom) empty]
        [else (cons (make-posn (missile-x (first lom))
                               (missile-y (first lom)))
                    (get-missile-posns (rest lom)))]))

(define (get-invader-images loi)
  (cond [(empty? loi) empty]
        [else (cons INVADER (get-invader-images (rest loi)))]))

(define (get-invader-posns loi)
  (cond [(empty? loi) empty]
        [else (cons (make-posn (invader-x (first loi))
                               (invader-y (first loi)))
                    (get-invader-posns (rest loi)))]))

(define (fn-for-game s)
  (make-game (fn-for-invaders (generate-invader (check-for-hits (game-missiles s) (game-invaders s))))
             (fn-for-missiles (game-missiles s))
             (fn-for-tank (game-tank s))))

(define (generate-invader loi)
  (if (equal? (random 30) 0)
      (cons (make-invader (random 300) 0 1.5) loi)
      loi))

(define (move-tank w k)
  (cond [(key=? k "left")
         (make-game (game-invaders w)
                    (game-missiles w)
                    (fn-for-tank (make-tank (tank-x (game-tank w)) -1)))]
        [(key=? k "right")
         (make-game (game-invaders w)
                    (game-missiles w)
                    (fn-for-tank (make-tank (tank-x (game-tank w)) 1)))]
        [(key=? k " ")
         (make-game (game-invaders w)
                    (cons (make-missile (tank-x (game-tank w)) (- HEIGHT TANK-HEIGHT/2)) (game-missiles w))
                    (game-tank w))]
        [else
         (make-game (game-invaders w)
                    (game-missiles w)
                    (fn-for-tank (make-tank (tank-x (game-tank w)) 0)))]))
        
(define (check-for-hits lom loi)
  (cond [(empty? loi) empty]
        [(empty? lom) loi]
        [else
         (if (check-for-hit lom (first loi))
             (check-for-hits lom (rest loi))
             (cons (first loi) (check-for-hits lom (rest loi))))]))
             
(define (check-for-hit lom i)
  (cond [(empty? lom) false]
        [(and (>= (missile-y (first lom)) (- (invader-y i) INVADER-HEIGHT/2))
                   (<= (missile-y (first lom)) (+ (invader-y i) INVADER-HEIGHT/2))
                   (>= (missile-x (first lom)) (- (invader-x i) INVADER-WIDTH/2))
                   (<= (missile-x (first lom)) (+ (invader-x i) INVADER-WIDTH/2)))
         true]
        [else (check-for-hit (rest lom) i)]))
         

(define (tock g)
  (fn-for-game g))

(define (render g)
  (place-images
   (append
    (get-missile-images (game-missiles g))
    (get-invader-images (game-invaders g))
    (list TANK))
   (append
    (get-missile-posns (game-missiles g))
    (get-invader-posns (game-invaders g))
    (list (make-posn (tank-x (game-tank g)) (- HEIGHT TANK-HEIGHT/2))))
   BACKGROUND))

(define (main ws)
  (big-bang ws                    
            (on-tick   tock)       
            (to-draw   render)
            (on-key    move-tank)))
