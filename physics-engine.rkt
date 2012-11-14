#lang r5rs 
(#%require "Canvas.rkt")
(#%require "utils.rkt")
(#%require (only racket/base error)) ;for error
(#%provide (all-defined))

; Physics Engine 
; --------------

(define (make-physics-engine gravity w h)
  (let ((previous-time (current-time))
        (dt 0))
    
    ; Calculate new position, based on given position & speed
    (define (move-coordinates position speed)
      (let ((px (coordinates-x position))
            (py (coordinates-y position))
            (vx (speed-x speed))
            (vy (speed-y speed)))
        (make-coordinates
         (+ px (* vx dt))
         (+ py (* vy dt)))))
    
    ;Update the current time frame
    (define (update-time!)
      (let ((time (current-time)))
        (set! dt (/ (- time previous-time) 10))
        (set! previous-time time)))
    
    ;Change the speed, based on gravity
    (define (update-speed speed)
      (make-speed
       (speed-x speed)
       (- (speed-y speed) (* gravity dt))))
    
    ;Move avatar
    (define (move-avatar avatar)
      (display "AVATAR")
      )
    
    ;Move obstacle
    (define (move-obstacle obstacle)
      (let* ((pos (send-message obstacle 'position))
             (vel (send-message obstacle 'speed))
             (new-x (+ (coordinates-x pos) (speed-x vel)))
             (nex-y (+ (coordinates-y pos) (speed-y vel)))
             )
        (send-message obstacle 'set-position! (make-coordinates new-x new-y))
      ))
    
    
    (define (dispatch message)
      (case message
        ((move-avatar) move-avatar)
        ((move-obstacle) move-obstacle)
        ((update-time!) update-time!)
        
        (else (error 'physics-engine "unknown message ~a" message))))
    
    dispatch))
