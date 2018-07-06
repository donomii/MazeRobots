#lang scheme

(require mzlib/defmacro)
(require scheme/class ) ;scheme/gui/base)
[require "simple_figure.rkt"]
[require "mystar.rkt"]
(require (prefix-in arr: math/array))
[require "jobs.rkt"]
[require "scene.rkt"]
[require "utility_functions.rkt"]
(require mred
         ;mzlib/class
         mzlib/math
         sgl
         sgl/gl-vectors)
(require sgl/gl)
[require srfi/1]
(require scheme/foreign)
(define pic-width 800)
(define pic-height 600)
[define display-gl #f]
[define wantpix #f]
[define mouseX 0]
[define mouseY 0]
[define selected -1]
(define topwin (new (class frame%
                      (augment* [on-close (lambda () (exit))])
                      (augment* [can-close? (lambda () #t)])
                      
                      (define/override (on-focus event)
                        [displayln "focus"])
                      (define/override (on-subwindow-char win event)
                        
                        [let [[key [send event get-key-code]]]
                          [verbose [format "Caught key event ~a~n" key]]
                          [when [equal? key 'release]
                            [send display-gl cease-display-movement]]
                          [when [equal? key #\x]
                            [exit]]
                          [when [equal? key #\ ]
                            [set! paused
                                  [if paused
                                      #f
                                      #t]]]
                          [when [equal? key #\a]
                            [send display-gl continuous-display-movement 'slide-left]]
                          [when [equal? key #\d]
                            [send display-gl continuous-display-movement 'slide-right]]
                          
                          [when [equal? key #\w]
                            [send display-gl continuous-display-movement 'zoom-in]]
                          [when [equal? key #\s]
                            [send display-gl continuous-display-movement 'zoom-out]]
                          
                          [when [equal? key #\q]  
                            
                            [send display-gl continuous-display-movement 'spin-left]]
                          [when [equal? key #\e]
                            [send display-gl continuous-display-movement 'spin-right]
                            ]
                          [when [equal? key #\r]
                            [send display-gl continuous-display-movement 'spin-up]
                            ]
                          [when [equal? key #\f]
                            [send display-gl continuous-display-movement 'spin-down]
                            ]
                          [when [equal? key #\t]
                            [send display-gl continuous-display-movement 'slide-up]
                            ]
                          [when [equal? key #\g]
                            [send display-gl continuous-display-movement 'slide-down]
                            ]
                          ])
                      
                      (define/override (on-subwindow-event win event)
                        ;[writeln "Caught mouse event"]
                        [when [equal? [send event get-event-type] 'left-down]
                          ;[printf "Caught mouse click at ~a,~a~n"[send event get-x][send event get-y]]
                          
                          [set! wantpix #t]
                          [set! mouseX [send event get-x]]
                          [set! mouseY [- pic-height [send event get-y]]]
                          ]
                        )
                      (super-new))
                    [label "Game"]
                    [style '(metal)]))
(define win (new horizontal-pane% (parent topwin)))
;(define f win)




[define [drawMiniMap width height amap]
  (for ([i [in-range width]])
    [for [[j [in-range height]]]
      [if [arr:array-ref amap [list->vector [list i j]]]
          [set-gl-pixel i j '[1.0 1.0 1.0 1.0]]
          [set-gl-pixel i j '[0.0 0.0 0.0 1.0]]]
      ]
    )
  ]



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

[define paused #t]
[random-seed 3]



;[define boxes [apply append [map [lambda [x]
;                                   [map [lambda [y]
;                                          `[,[- [random 10] 5] 0 ,[- [random 10] 5]]
;                                          ] [iota 5 -2 1]]
;                                   ] [iota 5 -2 1]]]]





[printf "Starting jobs: ~a~n" [scene-get 'jobs]]


;[define [update-jobs]
;  [when [empty? pending-jobs]
;    [set! pending-jobs [let [[target [random-from-list [scene-get 'walls]]] ;change this to the things list, when we add things
;                             [destination [list 0.0 0.0 0.0]]]
;                         [list
;                          `[fetch ,target ,destination]]]]
;    [printf "Added job, list now: ~a~n" pending-jobs]]
;          
;  [sleep 1]
;  [update-jobs]
;  ]


;[thread [lambda [][update-targets]]]
;[thread [lambda [] [update-jobs] ]]

;[define [drawTargets]
;  (gl-material-v 'front-and-back
;                 'ambient-and-diffuse
;                 (vector->gl-float-vector (apply vector [list 1.0 0.0 0.0 1.0])))
;  [map [lambda [v]
;         [when [not [empty? v]]
;           (gl-push-matrix)
;           (gl-translate (list-ref v 0) (list-ref v 1)   (list-ref v 2))
;           [gl-scale 0.1 0.1 0.1]
;           [cube]
;           (gl-pop-matrix)]]
;       targets]
;  ]

[define [drawBoxes colour positions]
  
  (gl-material-v 'front-and-back
                 'ambient-and-diffuse
                 (vector->gl-float-vector (apply vector colour)))
  [map [lambda [v]
         (gl-push-matrix)
         [gl-scale 0.3 0.3 0.3]
         (gl-translate (list-ref v 0) (list-ref v 1)   (list-ref v 2))
         
         [cube]
         (gl-pop-matrix)]
       positions]
  ]




[define [drawMans]
  
  (map (lambda (v  colour skin i)
         [map [lambda [p]
                    [when [equal? [car p] 'moveTo]
                        [begin
                    [gl-push-matrix]
                    [gl-scale 0.3 0.3 0.3]
                    [apply gl-translate [second p]]
                    [gl-rotate 1.0 0.0 0.0 90.0]
                    [gl-scale 0.3 0.3 0.3]
                    (gl-material-v 'front-and-back
                 'ambient-and-diffuse
                 (vector->gl-float-vector (apply vector [list 1.0 1.0 1.0 1.0])))
                    [cube]
                    [gl-pop-matrix]]]]
                  [second v]]
         (gl-push-matrix)
         [gl-scale 0.3 0.3 0.3]
         (gl-translate (list-ref [car v] 0) (list-ref [car v] 1)   (list-ref [car v] 2))
         
         [letrec [[jobqueue [second v]]
                  [position [first v]]]
           [when [not [empty? jobqueue]]
             
             [letrec [[thisjob [car jobqueue]]
                      [target [second thisjob]]]
               ;[printf "MoveTo: ~a~n" target]
               [scene-set! 'selected i]
               [case [car thisjob]
                 ['moveTo
                  (apply gl-rotate (fullAngle [car v] target))]
                 [else v]]]
             ]
           ]
         
         
                                
         [gl-scale 0.1 0.1 0.1]
         [if [equal? i selected]
             [figure [list 1.0 0.0 0.0 1.0]]
             [if wantpix
                 [figure colour]
             [figure skin]]]

         (gl-pop-matrix)
                 
         )
       [scene-get 'mans]  [scene-get 'colours] [scene-get 'skins] [iota [length [scene-get 'mans]]])
  ]


[define [do-paint]
  ;[drawTargets]
  [drawBoxes [list 0.0 0.0 1.0 1.0] [scene-get 'walls]]
  [drawBoxes [list 0.0 1.0 0.0 1.0] [scene-get 'things]]
  ;[displayln mans]
  [drawMans]

  [when [not paused]
    [scene-tick]
    
;    [printf "Mans: ~a~n" mans]
    ]
  ]

(define gears-canvas%
  (class* canvas% ()
    
    (inherit refresh with-gl-context swap-gl-buffers get-parent)
    
    (define rotation 0.0)

    (define view-slide-horiz 0.0)
    (define view-slide-vert 0.0)
    (define zoom-level 0.0)
    (define view-rotx 20.0)
    (define view-roty 30.0)
    (define view-rotz 0.0)
    [define display-movement #f]
    
   
  
    
    (define step? #f)
    
    (define/public (run)
      (set! step? #t)
      (refresh))

    (define/public (continuous-display-movement action)
      [set! display-movement action]
      (refresh))

    (define/public (cease-display-movement)
      [set! display-movement #f]
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

    (define/public (slide-left)
      (set! view-slide-horiz (+ view-slide-horiz 2.0))
      (refresh))
    
    (define/public (slide-right)
      (set! view-slide-horiz (- view-slide-horiz 2.0))
      (refresh))

        (define/public (slide-up)
      (set! view-slide-vert (+ view-slide-vert 2.0))
      (refresh))
    
    (define/public (slide-down)
      (set! view-slide-vert (- view-slide-vert 2.0))
      (refresh))
    

    (define/public (zoom-in)
      (set! zoom-level (+ zoom-level 2.0))
      (refresh))
    
    (define/public (zoom-out)
      (set! zoom-level (- zoom-level 2.0))
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
            [if wantpix
                [begin
                  (gl-enable 'cull-face)
                  (gl-disable 'lighting)
                  (gl-disable 'light0)
                  (gl-enable 'depth-test)
                  (gl-enable 'normalize)
                  [gl-disable 'color-material]
                  ]
                [begin
                  (gl-enable 'cull-face)
                  (gl-enable 'lighting)
                  (gl-enable 'light0)
                  (gl-enable 'depth-test)
                  (gl-enable 'normalize)
                  [glFogfv GL_FOG_COLOR (vector->gl-float-vector (apply vector [list 0.5 0.5 0.5 0.5]))];
                  [glFogi GL_FOG_MODE GL_LINEAR];
                  [glHint GL_FOG_HINT GL_FASTEST];// GL_NICEST
                  [glFogf GL_FOG_START -1.0];
                  [glFogf GL_FOG_END 1.0];
                  [glFogf GL_FOG_DENSITY 1.0];
                  ]]
            (gl-clear-color 0.0 0.0 0.0 0.0)
            (gl-clear 'color-buffer-bit 'depth-buffer-bit)
            [let [[ui-speed 0.15]]
              [case display-movement
                ['spin-left
                 (set! view-roty (+ view-roty [* ui-speed 5.0]))]

                ('spin-right
                 (set! view-roty (- view-roty [* ui-speed 5.0])))
    
                ('spin-up
                 (set! view-rotx (+ view-rotx [* ui-speed 5.0])))
    
                ('spin-down
                 (set! view-rotx (- view-rotx [* ui-speed 5.0])))

                ('slide-left
                 (set! view-slide-horiz (+ view-slide-horiz [* ui-speed 1.0])))
    
                ('slide-right
                 (set! view-slide-horiz (- view-slide-horiz [* ui-speed 1.0])))

                ('slide-up
                 (set! view-slide-vert (+ view-slide-vert [* ui-speed 1.0])))
    
                ('slide-down
                 (set! view-slide-vert (- view-slide-vert [* ui-speed 1.0])))

                ('zoom-in
                 (set! zoom-level (+ zoom-level [* ui-speed 2.0])))
    
                ('zoom-out
                 (set! zoom-level (- zoom-level [* ui-speed 2.0])))
                ]]
            
            (gl-push-matrix)
            (gl-translate view-slide-horiz  view-slide-vert zoom-level)
            
            (gl-rotate view-rotx 1.0 0.0 0.0)
            (gl-rotate view-roty 0.0 1.0 0.0)
            (gl-rotate view-rotz 0.0 0.0 1.0)
            [do-paint ]
            (gl-pop-matrix)
            (swap-gl-buffers)
            (gl-flush)
            [when wantpix
              [printf "Got pixel: ~a~n"  [get-gl-pixel mouseX mouseY]]
              [set! selected [matchColour [get-gl-pixel mouseX mouseY] [scene-get 'colours]]]
              [printf "figure: ~a~n" selected]
              [set! wantpix #f]
              ]
            
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









;(define get-the-bytes
;  (make-gl-cached-vector
;   'get-the-bytes
;   (λ (n)
;     (log-pict3d-info "<snip> creating temp ARGB bytes of length ~v" n)
;     (make-bytes n))
;   bytes-length))

;(define bs (get-the-bytes (* 4 width height)))
;(glReadPixels 0 0 width height GL_BGRA GL_UNSIGNED_INT_8_8_8_8 bs)

;(define get-gl-pixel (lambda (x y) 
;                       (letrec [[rvec (make-cvector _ubyte 1)]
;                                [bvec (make-cvector _ubyte  1)]
;                                [gvec (make-cvector _ubyte  1)]]
;
;                         (glReadPixels 0 0  1 1 GL_RED GL_UNSIGNED_BYTE rvec)
;                         (glReadPixels x y  1 1 GL_BLUE GL_UNSIGNED_BYTE gvec)
;                         (glReadPixels x y  1 1 GL_GREEN GL_UNSIGNED_BYTE bvec)
;                       [map [lambda [channel] [cvector-ref channel 0]]  [list rvec gvec bvec]])))




(define set-gl-pixel (lambda (x y col)
                       [glWindowPos2f x y]
                       (glDrawPixels  1 1 GL_RGBA GL_UNSIGNED_BYTE [list->gl-float-vector col])
                       ))


(define get-gl-pixel (lambda (x y) 
                       (letrec [[rvec (make-cvector _ubyte (* pic-width (add1 pic-height)))]]
                                
                         ;                         
                         ;[map [lambda [y]
                         ;
                         ;       [map [lambda [x]
                         ;                         (glReadPixels x [- pic-height y]  1 1 GL_RGBA GL_UNSIGNED_INT_8_8_8_8 rvec)
                         ;                        [display [if [> [cvector-ref  rvec 1] 50]
                         ;                                     "#" "."]]]
                         ;                      [iota pic-height]]
                         ;                      [displayln ""]]
                         ;     [iota pic-width]
                         ;       ]

                         (glReadPixels x y  1 1 GL_RGBA GL_UNSIGNED_BYTE rvec)
                         ;[printf "~a,~a : ~a~n" x y [map [lambda [x] [cvector-ref  rvec x]] [iota 4]]]
                         [map [lambda [x] [cvector-ref  rvec x]] [iota 4]])))
;  

;  
;
;(define get-gl-pixel (lambda (x y) 
;                       (letrec [[rvec (make-cvector _ubyte (* pic-width (add1 pic-height)))]
;                                [bvec (make-cvector _ubyte  (* pic-width (add1 pic-height)))]
;                                [gvec (make-cvector _ubyte  (* pic-width (add1 pic-height)))]]
;
;                         [displayln (glReadPixels 0 0  pic-width pic-height GL_RED GL_UNSIGNED_BYTE rvec)]
;                         (glReadPixels 0 0  pic-width pic-height GL_BLUE GL_UNSIGNED_BYTE gvec)
;                         (glReadPixels 0 0  pic-width pic-height GL_GREEN GL_UNSIGNED_BYTE bvec)
;                       [map [lambda [channel] [cvector-ref channel [+ x [* [- pic-height y] pic-width]]]]  [list rvec gvec bvec]])))

[sleep 1]