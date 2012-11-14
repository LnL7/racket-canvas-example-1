#lang r5rs 
(#%require "Canvas.rkt")
(#%require "utils.rkt")
(#%require "screen-objects.rkt")
(#%require (only racket/base error)) ;for error

; Game Loop 
; ---------

; Creates a game with the following parameters
; - game-objects: objects in the game
; - game-obstacles: obstacles in the game
; - ui: the ui the game will be drawn on

(define (make-game-loop avatar game-obstacles ui)
     
    ;One iteration of the game loop
    (define (game-advancer)
      
      ;For each obstacle, 
      (for-each (lambda (obstacle)
                  ; Think what needs to be done in each game loop for an obstacle
                  (send-message obstacle 'draw ui))
                game-obstacles)
      
      ;Think what needs to be done for the avatar
      (send-message avatar 'draw ui))
      
    (define (start)
      (start-game-loop game-advancer))
    
    (define (dispatch message)
      (case message
        ((start) start)
        
        (else (error 'game-loop "unknown message ~a" message))))
    dispatch)

;Start a game with one object and two obstacles
(send-message (make-game-loop 
               (make-avatar 20 250 blue)
               (list (make-obstacle 100 100 400 0 red) (make-obstacle 100 300 550 0 red)) 
               (make-canvas-ui))
              'start)
