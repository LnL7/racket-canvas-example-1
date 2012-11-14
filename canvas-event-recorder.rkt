#lang r5rs
(#%require "Canvas.rkt")
(#%require "utils.rkt")
(#%require (only racket/base error)) ;for error
(#%provide (all-defined))


; Canvas Event Recorder
; ---------------------

; Using Canvas.rkt, converts the last keyboard input to a game event
(define (make-canvas-event-recorder)
  (let ((event 'no-event)) 
    
    ;Initializes the recorder by linking it to Canvas.rkt
    (define (initialize)
      (clear)
      (on-key! 'up (lambda () (set! event 'up)))
      (on-key! 'down (lambda () (set! event 'down)))
      (on-key! 'right (lambda () (set! event 'right)))
      (on-key! 'left (lambda () (set! event 'left))))
    
    ;Erases the last recorded event by resetting it to a dummy value
    (define (clear)
      (set! event 'no-event))
    
    ;Returns the last recorded event
    ;TODO: recording a single keystroke won't suffice 
    
    (define (last-recorded-event)
      event)
    
    (initialize)
    
    (define (dispatch message)
      (case message
        ((clear) clear)
        ((last-recorded-event) last-recorded-event)
        (else (error 'canvas-event-recorder "unknown message ~a" message))))
    dispatch))