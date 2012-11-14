#lang r5rs
(#%require "Canvas.rkt")
(#%require "utils.rkt")
(#%require (only racket/base error)) ;for error
(#%provide (all-defined))


; Avatar
; ----
; Instantiates a (ball) avatar with the given radius and x-coordinate
(define (make-avatar radius x color)
  (let (;current position
        ;current speed
        ) ;vertical speed in case of user input
    
     
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
  (let (;current position
        ;current speed, initialize with the obstacle's initial (constant) speed (it moves to the left!!)
        )
    
       
    ;Draws the obstacle on the given game UI
    (define (draw ui)
      ;does not draw directly, but asks the UI to draw the obstacle instead
      ;this way, the game can be configured with a different UI
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
       ;TODO: write the code for drawing on avatar on the screen
      ;see Canvas.rkt
      )
    
    ;Draws the given obstacle
    (define (draw-obstacle obstacle)
      ;TODO: write the code for drawing on obstacle on the screen
      ;see Canvas.rkt
      )
    
    (define (dispatch message)
      (case message
        ((clear) clear)
        ((draw-avatar) draw-avatar)
        ((draw-obstacle) draw-obstacle)
        ((width) (lambda () window-w))
        ((height) (lambda () window-h))
        
        (else (error 'canvas-ui "unknown message ~a" message))))
    dispatch))