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
                          [printf "Caught key event ~a~n" key]
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
                            ]])
                      
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
                                         `[ [,x 0 ,y] []]
                                         ] [iota 1 -2 1]]
                                  ] [iota 2 -2 1]]]]

[define boxes [apply append [map [lambda [x]
                                   [map [lambda [y]
                                          `[,[- [random 10] 5] 0 ,[- [random 10] 5]]
                                          ] [iota 5 -2 1]]
                                   ] [iota 5 -2 1]]]]

[define colours  [map [lambda [r]
                        ;`[,[random] ,[random] ,[random]  1.0]
                        [list [/ r 25] 1.0 1.0 1.0]
                        ] [iota 25]]]
[define pending-jobs
  [let [[target [list-ref boxes [random [length boxes]]]]
        [destination [list 0.0 0.0 0.0]]]
    [list
     [list
      [list 'moveTo target]
      [list 'pickUp target]
      [list 'moveTo destination]
      [list 'drop target]
      ]]]]


[define target-list [apply append [map [lambda [x]
                                         [map [lambda [y]
                                                `[,[- [random 10] 5] 0 ,[- [random 10] 5]]
                                                ] [iota 2 -20 8]]
                                         ] [iota 4 -20 8]]]
  ]



[define [random-from-list target-list]
  [list-ref target-list [random [length target-list]]]
  ]

[define [update-targets]
  [let [[newbox `[,[- [random 10] 5] 0 ,[- [random 10] 5]]]
        [destination [list 0.0 0.0 0.0]]]
    [set! boxes [cons  newbox boxes]]
    [set! pending-jobs [cons 
                        [list
                         [list 'moveTo newbox]
                         [list 'pickUp newbox]
                         [list 'moveTo destination]
                         [list 'drop destination]
                         ]
                        pending-jobs]]
    ]
  [sleep 10]
  [update-targets]
  ]

[define [update-jobs]
  [when [empty? pending-jobs]
    
    [set! pending-jobs [let [[target [random-from-list boxes]]
                             [destination [list 0.0 0.0 0.0]]]
                         [list
                          [list
                           [list 'moveTo target]
                           [list 'pickUp target]
                           [list 'moveTo destination]
                           [list 'drop destination]
                           ]]]
          ]
    ]
  [sleep 1]
  [update-jobs]
  ]
[thread [lambda []
          [update-targets]
          ]]
[thread [lambda []
          [update-jobs]
          ]]

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

[define [drawBoxes]
  
  (gl-material-v 'front-and-back
                 'ambient-and-diffuse
                 (vector->gl-float-vector (apply vector [list 0.0 0.0 1.0 1.0])))
  [map [lambda [v]
         (gl-push-matrix)
         (gl-translate (list-ref v 0) (list-ref v 1)   (list-ref v 2))
         [gl-scale 0.3 0.3 0.3]
         [cube]
         (gl-pop-matrix)]
       boxes]
  ]

[define [drawMans] 
  (map (lambda (v  colour i)
         (gl-push-matrix)
         (gl-translate (list-ref [car v] 0) (list-ref [car v] 1)   (list-ref [car v] 2))
         
         [letrec [[jobqueue [second v]]
                  [position [first v]]]
           [when [not [empty? jobqueue]]
             [letrec [[thisjob [car jobqueue]]
                      [target [second thisjob]]]
               [set! selected i]
               [case [car thisjob]
                 ['moveTo
                  (apply gl-rotate (fullAngle [car v] target))
                  ]
                   
                   
                 [else v]]]
               
             ]
           ]
         
         
                                
         [gl-scale 0.1 0.1 0.1]
         [if [equal? i selected]
             [figure [list 1.0 0.0 0.0 1.0]]
             [figure colour]]

         (gl-pop-matrix)
                 
         )
       mans  colours [iota [length mans]])
  ]
[define [replace-in-list old new list]
  [map [lambda [e]
         [if [equal? e old]
             new
             e]]
       list]]
[define [remove-from-list item list]
  [filter [lambda [e]
            [if [equal? e item]
                #f
                e]]
          list]]

[define [do-paint]
  ;[drawTargets]
  [drawBoxes]
  ;[displayln mans]
  [drawMans]
  ;  [set! targets [map [lambda [target man i]
  ;                       
  ;                       ;[second man]
  ;                       target
  ;                       ]
  ;                     targets mans [iota [length targets]]]
  ;        ]
  [when [not paused]
    [set! mans (map (lambda (v  colour i)
                      ;[map [lambda[e t] [moveTo e t [* 0.01 [lengthVec [subVec v target]]]]] v target]
                      ;[displayln jobs]
                      ;[printf "~a, ~a, ~a~n" v target colour]
                    
                      [letrec [[jobqueue [second v]]
                               [position [first v]]]
                        [if [not [empty? jobqueue]]
                            [letrec [[thisjob [car jobqueue]]
                                     [target [second thisjob]]]
                              [set! selected i]
                              [case [car thisjob]
                                ['moveTo
                                 [if [equal? position [second thisjob]]
                                     [let [[newjobs  [cdr jobqueue]]]
                                       ;[set! jobs [replace-in-list [car jobs] newjob jobs]]
                                       [printf "1 Moving to new job ~a because ~a equals ~a~n" newjobs position target]
                                       [list [first v]
                                             newjobs]]
                                     [list [map [lambda[e t] [moveTo e t 0.01]] position [second thisjob]]
                                           jobqueue]]]
                                ['pickUp
                                 [begin
                                   [set! boxes [remove-from-list [second thisjob] boxes]]
                                   [list [first v] [cdr jobqueue]]]]
                                ['drop
                                 [begin
                                   [set! boxes [cons [second thisjob] boxes]]
                                   [list [first v] [cdr jobqueue]]]]
                                [else v]]]
                                
                        
                            [list [car v]
                                  [if [not [empty? pending-jobs]]
                                      [let [[newjob [car pending-jobs]]]
                                
                                        [set! pending-jobs [cdr pending-jobs]]
                                        [printf "pending-jobs: ~a~n" pending-jobs]
                                
                                        [printf "2 Moving to new job ~a~n"  newjob]
                                        newjob]
                                      '[]]
                                  ]
                            ]
                        ])
                    mans  colours [iota [length mans]])]]

  ]

(define gears-canvas%
  (class* canvas% ()
    
    (inherit refresh with-gl-context swap-gl-buffers get-parent)
    
    (define rotation 0.0)

    (define view-slide-horiz 0.0)
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

              ('zoom-in
               (set! zoom-level (+ zoom-level [* ui-speed 2.0])))
    
              ('zoom-out
               (set! zoom-level (- zoom-level [* ui-speed 2.0])))
              ]]
            
            (gl-push-matrix)
            (gl-translate view-slide-horiz  0.0 zoom-level)
            
            (gl-rotate view-rotx 1.0 0.0 0.0)
            (gl-rotate view-roty 0.0 1.0 0.0)
            (gl-rotate view-rotz 0.0 0.0 1.0)
            [do-paint ]
            (gl-pop-matrix)
            (swap-gl-buffers)
            (gl-flush)
            [when wantpix
              [printf "Got pixel: ~a~n"  [get-gl-pixel mouseX mouseY]]
              [set! selected [matchColour [get-gl-pixel mouseX mouseY] colours]]
              [printf "figure: ~a~n" selected]
              [set! wantpix #f]]
            
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
  ;[printf "Moving from ~a to ~a, step ~a~n" a b c]
  [if  [< [* [- a b] [- a b]] c]
       b
       [if [< a b]
           [+ a c]
           [- a c]]
       ]]

[define [diffcolour a b]
  ;[printf "~a vs ~a~n" a b]
  [apply + [map [lambda [aa bb] [abs [- aa bb]]] a b]]
  ]

[define [matchColour colour colours]
  [if [void? colour]
      -1
      [let [[best  999999999]]
        [car [reverse
              [cons -1 [filter positive? [map [lambda [c i] [if [< [diffcolour [map [lambda [x]  [* 256 x]] c] colour] best]
                                                                [begin
                                                                  ;[printf "Matched: ~a against ~a~n" [map [lambda [x] [* 255 x]] c] colour]
                                                                  [set! best [diffcolour [map [lambda [x] [* 256 x]] c] colour]]
                                                                  i]
                                                                -1]]
                                              colours [iota [length colours]]]
                    
                               ]]]]]]]

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

[define [subVec a b]
  [map [lambda [x y] [- x y]] a b]
  ]
[define [lengthVec a]
  [sqrt [apply + [map [lambda [x] [* x x]] a]]]]

[define [fullAngle v1 v2]
  [letrec [[a1 [vec-angle v1 v2]]
           [rot-axis [normalise [crossprod v1 v2]]]
           ]
    ;[displayln rot-axis]
    [list [* [+ 90  a1] [/ 180 3.1415927]] [list-ref rot-axis 0][list-ref rot-axis 1][list-ref rot-axis 2] ]
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