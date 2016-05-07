#lang racket

(require sgl/gl
         mred)

(provide gl-go
         draw-rectangle!
         draw-line!)


(define BACKGROUND-RGBA '(0.95 0.95 0.95 1.0))

;; COMPUTING FPS:
;; how many frames have been drawn since the last reset?
(define frames 0)
;; when did the last reset occur?
(define timemark (current-inexact-milliseconds))

;; increment the `frames` counter. If more than 1 second
;; has passed since the last reset, print out the frame rate
;; and reset.
(define (fpscheck)
  (set! frames (+ 1 frames))
  (define ms-now (current-inexact-milliseconds))
  (define ms-since-mark (- ms-now timemark))
  (cond [(< 1000 ms-since-mark)
         (printf "FPS: ~v\n" (/ frames (/ ms-since-mark 1000)))
         (set! frames 0)
         (set! timemark ms-now)]
        [else 'do-nothing]))

(define okay-to-draw? (make-parameter #f))

;; a canvas% for use with OpenGL
(define glcanvas%
    (class canvas%
      (init-field [(draw-fun gl-draw)])
      (inherit refresh with-gl-context swap-gl-buffers)

      (define init? #f)
      (define/override (on-paint)
        (fpscheck)
        (with-gl-context 
         (lambda ()
           (unless init?
             (gl-init)
             (set! init? #t))
           (gl-pre-draw)
           (parameterize ([okay-to-draw? #t])
             (draw-fun))
           
           (gl-post-draw)
           (swap-gl-buffers)))
        (queue-callback (lambda () (refresh)) #f))
      
      (define/override (on-size w h)
        (with-gl-context 
         (lambda ()
           (gl-resize w h)))
        (refresh))
      
      #;(define/override (on-char key)
        (gl-handlekey key)
        (refresh))
      (super-new (style '(gl no-autoclear)))))

;; prepare to draw:
(define (gl-pre-draw)
  (glClear GL_COLOR_BUFFER_BIT)
  (glMatrixMode GL_PROJECTION)
  (glOrtho -1.0 1.0 -1.0 1.0 0.1 10)
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity)
  ;;(glTranslated 1 0 0)
  (glDisable GL_LIGHTING))

;; flush the buffer after drawing
(define (gl-post-draw)
  (glFlush))

;; ONLY CALL DURING DRAW
(define (draw-rectangle! r g b x0 x1 y0 y1 z)
  (cond [(okay-to-draw?)
         (glColor4f r g b 1.0)
         (glBegin GL_POLYGON)
         (glVertex3f x0 y0 z)
         (glVertex3f x1 y0 z)
         (glVertex3f x1 y1 z)
         (glVertex3f x0 y1 z)
         (glEnd)]
        [else
         (error 'draw-rectangle!
                "can't call draw except in draw callback.")]))

;; draw a line in the line layer
(define (draw-line! lop z)
  (cond [(okay-to-draw?)
         (glLineWidth 3.0)
         (glColor4f 0.0 0.0 0.0 0.05)
         (glBegin GL_LINE_STRIP)
         (for ([p (in-list lop)])
           (match-define (vector x y) p)
           (glVertex3f x y z))
         (glEnd)]
        [else
         (error 'draw-line!
                "can't call draw except in draw callback.")]))





(define (gl-resize width height)
  (glViewport 0 0 width height))

(define (gl-init)
  (apply glClearColor BACKGROUND-RGBA)
  (glEnable GL_BLEND)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
)

;; open the window, use the given gl-draw to draw.
(define (gl-go gl-draw)
  (define my-frame (new frame%
                        [label "this is a frame%"]
                        [width 1300]
                        [height 750]))
  (define my-canvas (new glcanvas%
                         [parent my-frame]
                         [gl-draw gl-draw]))
  
  (send my-frame show #t))

(module+ test
  (gl-go
   (λ ()
     (draw-rectangle! 0.0 0.0 1.0 -0.25 0.75 0.25 0.75))))