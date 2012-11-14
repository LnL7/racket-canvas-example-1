#lang racket
;*----------------------------------*
;*        >>>  Canvas   <<<         *
;*  >>>  Jaarproject 2011-2012 <<<  *
;*                                  *
;*    SOFT Software Languages Lab   *
;*               2011               *
;*----------------------------------*

; load graphical library
(require graphics/graphics
         racket/draw)
; initialize the library
(open-graphics)

(provide fill-rectangle! 
         fill-ellipse! 
         draw-line! 
         on-key!
         draw-text!
         put-pixel!
         start-game-loop
         current-time
         red 
         green 
         blue
         black
         white)
;---------------------------------------------------------------------
; Canvas
;---------------------------------------------------------------------
(define ScaleWindowFactor 1)
(define Pixels-x 800)
(define Pixels-y 600)
(define red #xf00)
(define green #x0f0)
(define blue #x00f)
(define black #x000)
(define white #xfff)

(define (Windowscale x)
  (* x ScaleWindowFactor))

(define MainWindow 
  (open-viewport "Jaarproject (2012-2013)" (Windowscale Pixels-x) (Windowscale Pixels-y)))

(define (my-make-posn x y)
  (make-posn (Windowscale x) (Windowscale y)))

(define (12bitColor2rgb color)
  (make-rgb (/ (bitwise-and color #xf00) #xf00)
            (/ (bitwise-and color #x0f0) #x0f0)
            (/ (bitwise-and color #x00f) #x00f)))

(define (fill-rectangle! x y width height color)
  (define posn (my-make-posn x (- Pixels-y y height)))
  ((draw-solid-rectangle MainWindow)
   posn
   (Windowscale width) 
   (Windowscale height) 
   (12bitColor2rgb color)))

(define (draw-text! x y str color)
  (define posn (my-make-posn x (- Pixels-y y)))
  ((draw-string MainWindow) posn str (12bitColor2rgb color)))

(define (put-pixel! x y color)
  (define posn (my-make-posn x (- Pixels-y y)))
  ((draw-pixel MainWindow) posn (12bitColor2rgb color)))

(define (fill-ellipse! x y width height color) 
  (define posn (my-make-posn (-  x (/ width 2) ) (- Pixels-y (/ height 2) y)))
  ((draw-solid-ellipse MainWindow) posn (Windowscale width) (Windowscale  height)  (12bitColor2rgb color)))

(define (draw-line! x1 y1 x2 y2 color)
  (define posn1 (my-make-posn   x1  (- Pixels-y y1)))
  (define posn2 (my-make-posn   x2  (- Pixels-y y2)))
  ((draw-line MainWindow) posn1 posn2 (12bitColor2rgb color)))

(define commands '())

(define (make-command l1 l2)
  (cons l1 l2))

(define (on-key! name function)
  (set! commands (cons (cons name function) commands)))

(define (execute command select)
  (let ((pair (assoc command commands)))
    (when pair
      ((cdr pair)))))

(define (action a) a)
(define (do-action a) (a))

((set-on-key-event MainWindow) 
 (lambda (keypress y) 
   (cond ((not keypress) null)
         ((eq? (key-value keypress) 'release) 'dont-care)
         (else 
          (execute (key-value keypress) action)))))
;---------------------------------------------------------------------
; Timer
;---------------------------------------------------------------------
(define CPU_FREQ 60000000)

(define (make-timer)
  (define time 0) 
  (define wait-time (/ 1 CPU_FREQ))
  
  (define timer-thread
    (thread (lambda ()
              (let loop ()
                (sleep wait-time)
                (set! time (+ 1 time))
                (loop)))))
  
  (define (dispatch msg)
    (cond ((eq? msg 'time) time)
          ((eq? msg 'start)
           (thread-resume timer-thread))
          ((eq? msg 'restart)
           (set! time 0)
           (thread-resume timer-thread))
          ((eq? msg 'reset)
           (thread-suspend timer-thread)
           (set! time 0))
          ((eq? msg 'stop) (thread-suspend timer-thread))
          ((eq? msg 'set-period!)
           (lambda (period)
             (set! wait-time (* (+ period 1) (/ 1 CPU_FREQ)))))
          ))
  (thread-suspend timer-thread)
  (set! time 0)
  dispatch)

(define timer0 (make-timer))
((timer0 'set-period!) 100000)
(define (stop-timer timer) (timer 'stop))
(define (start-timer timer) (timer 'start))
(define (restart-timer timer) (timer 'restart))
(define (reset-timer timer) (timer 'reset))
(define (read-timer timer)  (timer 'time))
(define (write-timer-period timer period) ((timer 'set-period!) period))

(start-timer timer0)

(define (current-time)
  (timer0 'time))

(define (start-game-loop loop)
  ((set-on-tick-event MainWindow) 1 (lambda x (loop))))
