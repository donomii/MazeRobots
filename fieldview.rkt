#lang scheme

(require mzlib/defmacro)
(require scheme/class ) ;scheme/gui/base)
[require "simple_figure.rkt"]
(require mred
         ;mzlib/class
         mzlib/math
         sgl
         sgl/gl-vectors)
(require sgl/gl)
[require srfi/1]
(require scheme/foreign)
(define pic-width 468)
(define pic-height 495)
[define display-gl #f]
(define topwin (new (class frame%
                      (augment* [on-close (lambda () (exit))])
                      (augment* [can-close? (lambda () #t)])
                      
                      (define/override (on-focus event)
                        [displayln "focus"])
                      (define/override (on-subwindow-char win event)
                        ;[displayln "Caught key event"]
                        [let [[key [send event get-key-code]]]
                          [when [equal? key #\a]
                            [send display-gl move-left]]
                          [when [equal? key #\d]
                            [send display-gl move-right]]
                          [when [equal? key #\w]
                            [send display-gl move-up]]
                          [when [equal? key #\s]
                            [send display-gl move-down]]])
                      (define/override (on-subwindow-event win event)
                        ;[writeln "Caught mouse event"]
                        [when [equal? [send event get-event-type] 'left-down]
                          [printf "Caught mouse click at ~a,~a~n"[send event get-x][send event get-y]]
                          [displayln [get-gl-pixel [send event get-x][send event get-y]]]
                          ])
                      (super-new))
                    [label "Game"]
                    [style '(metal)]))
(define win (new horizontal-pane% (parent topwin)))
;(define f win)
(define pic (make-object bitmap%  10 10 ))
;(send pic load-file "C:/Users/user/Documents/My Dropbox/3danneal/base0.png" )
;(define bdc (new bitmap-dc% [bitmap pic]))

;(newline)
;(write (send pic get-width))
;(define showpic (lambda (c a-dc) (send a-dc draw-bitmap pic 0 0)))

;(define c
;  (new canvas% (parent win)
;       (min-width pic-width) (min-height pic-height)
;       [paint-callback showpic]))

(define rvec (make-cvector _ubyte (* pic-width (add1 pic-height))))
(define bvec (make-cvector _ubyte  (* pic-width (add1 pic-height))))
(define gvec (make-cvector _ubyte  (* pic-width (add1 pic-height))))

;(define get-png-pixel (lambda (x y) (let ([ col (new color%)]) 
;                                      (send bdc get-pixel x y col)
;                                      col)))
;(define compare-pixel (lambda (a-bitmap-dc x y glred glblue glgreen)
;                        (let ([col (get-png-pixel x y)])
;                          
;                          (+ (abs (- (cvector-ref glred (+ x (* y pic-width))) (send col red)))
;                             (abs (- (cvector-ref glblue (+ x (* y pic-width))) (send col blue)))
;                             (abs (- (cvector-ref glgreen (+ x (* y pic-width))) (send col green)))
;                               
;                             ;           (map (lambda (ind) (write (cvector-ref cvec ind))) (build-list 300 values))  
;                               
;                             ))))
(define count 0)
(define get-gl-data (lambda () (glReadPixels 0 0  pic-width pic-height GL_RED GL_UNSIGNED_BYTE rvec)
                      (glReadPixels 0 0  pic-width pic-height GL_BLUE GL_UNSIGNED_BYTE rvec)
                      (glReadPixels 0 0  pic-width pic-height GL_GREEN GL_UNSIGNED_BYTE rvec)))

[random-seed 1]
[define [vec-angle v1 v2]
  (match-let ([(list x1 y1 z1) v1]
              [(list x2 y2 z2) v2])
    [letrec [[dotprod [+ [* x1 x2] [* y1 y2] [* z1 z2]]]
             [mag1 [sqrt [+ [* x1 x1] [* y1 y1] [* z1 z1]]]]
             [mag2 [sqrt [+ [* x2 x2] [* y2 y2] [* z2 z2]]]]
             [cosa [/ dotprod [+ 0.000001 [* mag1 mag2]]]]
             [a [acos cosa]]
             ]
      ;[displayln [/ [* a 180] 3.14159]]
      ;[/ [* a 180] 3.14159]
      a
      ])]
[define mans [apply append [map [lambda [x]
                                  [map [lambda [y]
                                         `[,x 0 ,y]
                                         ] [iota 5 -2 1]]
                                  ] [iota 5 -2 1]]]]
[define colours  [map [lambda [r]
                        `[,[random] ,[random] ,[random]  ,[random]]
                        ] [iota 25]]]
[random-seed 2]

[define target-list [apply append [map [lambda [x]
                                         [map [lambda [y]
                                                `[,[- [random 30] 15] 0 ,[- [random 30] 15]]
                                                ] [iota 2 -20 8]]
                                         ] [iota 2 -20 8]]]
  ]

[define targets [apply append [map [lambda [x]
                                     [map [lambda [y]
                                            `[,[- [random 10] 5] 0 ,[- [random 10] 5]]
                                            ] [iota 5 -20 8]]
                                     ] [iota 5 -20 8]]]]

[define [update-targets]
  [set! targets [apply append [map [lambda [x]
                                     [map [lambda [y]
                                            ;`[,[- [random 10] 5] 0 ,[- [random 10] 5]]
                                            [list-ref target-list [random [length target-list]]]
                                            ] [iota 5 -20 8]]
                                     ] [iota 5 -20 8]]]]
  [sleep 5]
  [update-targets]
  ]
[thread [lambda []
          [update-targets]
          ]]


[define [do-paint]
  [set! mans (map (lambda (v target colour)
                    (gl-push-matrix)

                    (gl-translate (list-ref v 0) (list-ref v 1)   (list-ref v 2))
                    (apply gl-rotate (fullAngle v target))
                                
                    [gl-scale 0.1 0.1 0.1]
                              
                    [figure colour]

                    (gl-pop-matrix)
                    [map [lambda[e t] [moveTo e t 0.1]] v target]
                    )
                  mans targets colours)]]

(define gears-canvas%
  (class* canvas% ()
    
    (inherit refresh with-gl-context swap-gl-buffers get-parent)
    
    (define rotation 0.0)
    
    (define view-rotx 20.0)
    (define view-roty 30.0)
    (define view-rotz 0.0)
    
   
  
    
    (define step? #f)
    
    (define/public (run)
      (set! step? #t)
      (refresh))
    
    (define/public (move-left)
      (set! view-roty (+ view-roty 5.0))
      (refresh))
    
    (define/public (move-right)
      (set! view-roty (- view-roty 5.0))
      (refresh))
    
    (define/public (move-up)
      (set! view-rotx (+ view-rotx 5.0))
      (refresh))
    
    (define/public (move-down)
      (set! view-rotx (- view-rotx 5.0))
      (refresh))
    
    
    
    (define/override (on-size width height)
      (with-gl-context
          (lambda ()
         
            ;            (unless gear1
            ;              (printf "  RENDERER:   ~A\n" (gl-get-string 'renderer))
            ;              (printf "  VERSION:    ~A\n" (gl-get-string 'version))
            ;              (printf "  VENDOR:     ~A\n" (gl-get-string 'vendor))
            ;              (printf "  EXTENSIONS: ~A\n" (gl-get-string 'extensions))
            ;              )
         
            (gl-viewport 0 0 width height)
            (gl-matrix-mode 'projection)
            (gl-load-identity)
            (let ((h (/ height width)))
              (gl-frustum -1.0 1.0 (- h) h 5.0 60.0))
            (gl-matrix-mode 'modelview)
            (gl-load-identity)
            (gl-translate 0.0 0.0 -40.0)
         
            (gl-light-v 'light0 'position (vector->gl-float-vector
                                           (vector 5.0 5.0 10.0 0.0)))
            (gl-enable 'cull-face)
            (gl-enable 'lighting)
            (gl-enable 'light0)
            (gl-enable 'depth-test)
            (gl-enable 'normalize)
         
   
            ))
      (refresh))
    
   
    
    (define/override (on-paint)
      
        
        
      
      (with-gl-context
          [lambda [] 
  
            (gl-clear-color 0.0 0.0 0.0 0.0)
            (gl-clear 'color-buffer-bit 'depth-buffer-bit)
           
            (gl-push-matrix)
            (gl-rotate view-rotx 1.0 0.0 0.0)
            (gl-rotate view-roty 0.0 1.0 0.0)
            (gl-rotate view-rotz 0.0 0.0 1.0)
            [do-paint]
            (gl-pop-matrix)
            (swap-gl-buffers)
            (gl-flush)
            ;(sleep 0.1)
             
            ])
      
      (when step?
        (set! step? #f)
        (queue-callback (lambda x (send this run)) #f ))
      )
    
    (super-instantiate () (style '(gl no-autoclear)))))

(define (gl-frame)
  (let* ((f (make-object frame% "gears.ss" #f)))
    [set! display-gl (new gears-canvas% (parent win) (min-width pic-width) (min-height pic-height) (stretchable-width #f) (stretchable-height #f))]
    ;(send f create-status-line)
    (send display-gl run)
    
    ))
[thread [thunk

         (gl-frame)
         (send topwin show #t)
         [send topwin enable #t]
         [send topwin focus]
         [displayln (send topwin is-enabled?)]
         ]]

[define null [lambda [] [lambda [] #f]]]


;moves number a towards number b, by c
[define [moveTo a b c]
  [if  [< [* [- a b] [- a b]] 1]
       a
       [if [< a b]
           [+ a c]
           [- a c]]
       ]]

[define [normalise v]
  (match-let ([(list x1 y1 z1) v])
    [let [[mag1 [+ 0.0000001 [sqrt [+ [* x1 x1] [* y1 y1] [* z1 z1]]]]]]
      [list [/ x1 mag1] [/ y1 mag1] [/ z1 mag1]]
      ])]

(define-syntax njoint
  (lambda (x)
    (syntax-case x ()
      [(_ x y z ...)
       (syntax (rot x y z ...))])))

[define [crossprod u v]
  (match-let ([(list u1 u2 u3) u]
              [(list v1 v2 v3) v])
    [list [- [* u2 v3] [* u3 v2]] [- [* u3 v1] [* u1 v3]] [- [* u1 v2] [* u2 v1]]])]

[define [fullAngle v1 v2]
  [letrec [[a1 [vec-angle v1 v2]]
           [rot-axis [normalise [crossprod v1 v2]]]
           ]
    ;[displayln rot-axis]
    [list [* [+ 90  a1] [/ 180 3.1415927]] [list-ref rot-axis 0][list-ref rot-axis 1][list-ref rot-axis 2] ]
    ]]


(define get-gl-pixel (lambda (x y) 
                       (letrec [[rvec (make-cvector _ubyte 1)]
                                [bvec (make-cvector _ubyte  1)]
                                [gvec (make-cvector _ubyte  1)]]

                         (glReadPixels x y  1 1 GL_RED GL_UNSIGNED_BYTE rvec)
                         (glReadPixels x y  1 1 GL_BLUE GL_UNSIGNED_BYTE gvec)
                         (glReadPixels x y  1 1 GL_GREEN GL_UNSIGNED_BYTE bvec))
                       [map [lambda [x] [cvector-ref x 0]]  [list rvec gvec bvec]]))
  



