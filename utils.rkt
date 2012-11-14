#lang r5rs
(#%require "Canvas.rkt")
(#%require (only racket/base error)) ;for error
(#%provide (all-defined))


; Auxiliary procedures
; --------------------

;Sends a message (with optional parameters) to an oo-based implementation of an ADT
(define (send-message object message . parameters)
  (let ((procedure (object message)))
    ;Assumes the object's dispatcher always returns a procedure 
    (apply procedure parameters)))


; ADT tagged data
; ---------------

; META: Example of a procedure-based ADT

; Constructors
(define (make-tagged tag data)
  (cons tag data))

; Predicates
(define (tagged? tagged)
  (pair? tagged))

(define (tagged-as? tagged tag)
  (and (tagged? tagged)
       (eq? (tagged-tag tagged tag))))

; Selectors
(define (tagged-tag x)
  (car x))

(define (tagged-data x)
  (cdr x))

; Mutators
(define (tagged-data! x newvalue)
  (set-cdr! x newvalue))


; Coordinates
; -----------

; META: Example of a procedure-based ADT

; Constructors
(define (make-coordinates x y)
  (make-tagged 'coordinates (cons x y)))

; Predicates
(define (coordinates? coos)
  (tagged-as? coos 'coordinates))

; Selectors
(define (coordinates-x coos)
  (car (tagged-data coos)))

(define (coordinates-y coos)
  (cdr (tagged-data coos)))

; Mutators
(define (coordinates-x! coos x)
  (set-car! (tagged-data coos) x))

(define (coordinates-y! coos y)
  (set-cdr! (tagged-data coos) y))

;META: the following functions are very similar, this could be improved
(define (coordinates-inc-y! coos increase)
  (coordinates-y! coos (+ (coordinates-y coos) increase)))

(define (coordinates-dec-y! coos decrease)
  (coordinates-y! coos (- (coordinates-y coos) decrease)))

(define (coordinates-inc-x! coos increase)
  (coordinates-x! coos (+ (coordinates-x coos) increase)))


; Speed
; -----------
; META: This ADT is very similar to the previous one and could be generalised.
; Speed and coordinates would then be instances of this general ADT.

; Constructors
(define (make-speed x y)
  (make-tagged 'speed (cons x y)))

; Predicates
(define (speed? speed)
  (tagged-as? speed 'speed))

; Selectors
(define (speed-x speed)
  (car (tagged-data speed)))

(define (speed-y speed)
  (cdr (tagged-data speed)))

; Mutators
(define (speed-x! speed x)
  (set-car! (tagged-data speed) x))

(define (op-x speed arg op)
  (make-speed (op (speed-x speed) arg)
              (speed-y speed)))

(define (op-y speed arg op)
  (make-speed  (speed-x speed)
               (op (speed-y speed) arg)))

(define (inc-y speed inc)
  (op-y speed inc +))

(define (dec-y speed inc)
  (op-y speed inc -))

(define (inc-x speed inc)
  (op-x speed inc +))

(define (dec-x speed inc)
  (op-x speed inc -))

(define (speed-y! speed y)
  (set-cdr! (tagged-data speed) y))