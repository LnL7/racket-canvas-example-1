#lang r5rs
(#%require "Canvas.rkt")
(#%require "utils.rkt")
(#%require (only racket/base error)) ;for error
(#%provide (all-defined))


; Avatar
; ----
; Instantiates a (ball) avatar with the given radius and x-coordinate
(define (make-avatar radius x color)
  (let ((curr-pos (make-coordinates x 0))
        (curr-vel (make-speed 0 0))
        (jump-vel-y 10))

    (define (get-position) curr-pos)
    (define (get-speed)    curr-vel)
    (define (get-radius)   radius)
    (define (get-color)    color)

    (define (set-position! pos)
      (set! curr-pos pos))
    (define (set-speed! vel)
      (set! curr-vel vel))

    (define (up!)
      (let ((vel (make-speed (speed-x curr-vel) jump-vel-y)))
        (set! curr-vel vel)))


    ;Draws the object on the given game UI
    (define (draw ui)
      ;does not draw directly, but asks the UI to draw the object instead
      ;this way, the game can be configured with a different UI
      (send-message ui 'draw-avatar dispatch))
    
    ;Processes the events (= user input, sensor input) recorded by event-recorder
    (define (process-events event-recorder)
      (let ((event (send-message event-recorder 'last-recorded-event)))
        ;TODO: this might be slow as there are many events not related to an object   
        (case event
          ((up) (up!)) ;Key-up event was recorded; give object a vertical (up) speed
          
          (else 'do-nothing)))) ;Not an event a object reacts to
    
    (define (dispatch message)
      (case message
        ((position) get-position)
        ((set-position!) set-position!)
        ((speed) get-speed)
        ((set-speed!) set-speed!)
        ((up!) up!)
        ((radius) get-radius)
        ((color) get-color)
        ((draw) draw)
        ((process-events) process-events)
        
        (else (error 'object "unknown message ~a" message))))
    dispatch))

;-----------------------------------------------------------------------------------------------------
; Instantiates a obstacle with the x- and y-coordinate
(define (make-obstacle width height x y color)
  (let ((curr-pos (make-coordinates x y))
        (curr-vel (make-speed -1 0)))

    (define (get-position) curr-pos)
    (define (get-speed)    curr-vel)
    (define (get-width)    width)
    (define (get-height)   height)
    (define (get-color)    color)

    (define (set-position! pos)
      (set! curr-pos pos))

    ;Draws the obstacle on the given game UI
    (define (draw ui)
      ;does not draw directly, but asks the UI to draw the obstacle instead
      ;this way, the game can be configured with a different UI
      (send-message ui 'draw-obstacle dispatch)
     )
    
    (define (dispatch message)
      (case message
        ((position) get-position)
        ((set-position!) set-position!)
        ((speed) get-speed)
        ((width) get-width)
        ((height) get-height)
        ((color) get-color)
        ((draw) draw)
        
        (else (error 'obstacle "unknown message ~a" message))))
    dispatch))

; Canvas UI
; ---------

; META: Example of an OO-based ADT

; UI that draws on a window using the Canvas.rkt library
(define (make-canvas-ui)
  (let ((window-w 800)
        (window-h 600)
        (window-c white))
    
    ;Clears the window by simply painting it entirely in a certain color
    (define (clear)
      (fill-rectangle! 0 0 window-w window-h window-c))
    
    ;Draws the given avatar
    (define (draw-avatar avatar)
      (let ((pos    (send-message avatar 'position))
            (radius (send-message avatar 'radius))
            (color  (send-message avatar 'color)))
      (fill-ellipse!
        (coordinates-x pos)
        (coordinates-y pos)
        radius
        radius
        color)))
    
    ;Draws the given obstacle
    (define (draw-obstacle obstacle)
      (let ((pos (send-message obstacle 'position))
            (width (send-message obstacle 'width))
            (height (send-message obstacle 'height))
            (color (send-message obstacle 'color)))
        (fill-rectangle!
          (coordinates-x pos)
          (coordinates-y pos)
          width
          height
          color)))
    
    (define (dispatch message)
      (case message
        ((clear) clear)
        ((draw-avatar) draw-avatar)
        ((draw-obstacle) draw-obstacle)
        ((width) (lambda () window-w))
        ((height) (lambda () window-h))
        
        (else (error 'canvas-ui "unknown message ~a" message))))
    dispatch))
