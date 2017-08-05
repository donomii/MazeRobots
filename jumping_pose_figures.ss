#lang scheme

(require mzlib/defmacro)
(require scheme/class ) ;scheme/gui/base)
(require mred
         ;mzlib/class
         mzlib/math
         sgl
         sgl/gl-vectors)
(require sgl/gl)
(require scheme/foreign)
(define pic-width 468)
(define pic-height 495)
(define topwin (new (class frame%
                   (augment* [on-close (lambda () (exit))])
                   (super-new))
                 [label "test"]
                 [style '(metal)]))
(define win (new horizontal-pane% (parent topwin)))
(define f win)
(define pic (make-object bitmap%  10 10 ))
;(send pic load-file "C:/Users/user/Documents/My Dropbox/3danneal/base0.png" )
(define bdc (new bitmap-dc% [bitmap pic]))

(newline)
;(write (send pic get-width))
(define showpic (lambda (c a-dc) (send a-dc draw-bitmap pic 0 0   )))
(define c
  (new canvas% (parent win)
       (min-width pic-width) (min-height pic-height)
       [paint-callback showpic]))
(define rvec (make-cvector _ubyte (* pic-width (add1 pic-height))))
(define bvec (make-cvector _ubyte  (* pic-width (add1 pic-height))))
(define gvec (make-cvector _ubyte  (* pic-width (add1 pic-height))))

(define get-png-pixel (lambda (x y) (let ([ col (new color%)]) 
                          (send bdc get-pixel x y col)
                                      col)))
(define compare-pixel (lambda (a-bitmap-dc x y glred glblue glgreen)
                        (let ([col (get-png-pixel x y)])
                          
                            (+ (abs (- (cvector-ref glred (+ x (* y pic-width))) (send col red)))
                               (abs (- (cvector-ref glblue (+ x (* y pic-width))) (send col blue)))
                               (abs (- (cvector-ref glgreen (+ x (* y pic-width))) (send col green)))
                               
                               ;           (map (lambda (ind) (write (cvector-ref cvec ind))) (build-list 300 values))  
                               
                               ))))
(define count 0)
(define get-gl-data (lambda () (glReadPixels 0 0  pic-width pic-height GL_RED GL_UNSIGNED_BYTE rvec)
                        (glReadPixels 0 0  pic-width pic-height GL_BLUE GL_UNSIGNED_BYTE rvec)
                         (glReadPixels 0 0  pic-width pic-height GL_GREEN GL_UNSIGNED_BYTE rvec)))
(define compare-chunk (lambda (bdc  x  y rvec bvec gvec)
                               (map (lambda (x)
                               (map  (lambda (y ) (compare-pixel bdc  x  y rvec bvec gvec) )  (build-list 10 values))
                                      )(build-list 10 values))))
                               
(define compare-pixels-at (lambda (centered-x centered-y)
                    (begin
                      (car (car (compare-chunk bdc  centered-x  centered-y rvec bvec gvec)))
;                  (foldl + 0 (map  (lambda (y)  
;                         (foldl + 0 (map (lambda (x)  (compare-pixel bdc  x  y rvec bvec gvec)) (build-list pic-width values)) ) )
;                       (build-list pic-height values)) ) 
                  ;(display (format "Count ~a~n" count))
                  )     
                         ))
(define compare (lambda ()
                  
                  (get-gl-data)
                  (compare-pixels-at 100 100)
                    ))

(define build-voxel (lambda (a) 
                      (list 
                       (/ (- (random 11) 5) 1) (/ (- (random 11) 5) 1) (/ (random 10) 10) (/ (random 10) 10) (/ (random 10) 10) (/ (random 10) 10))))
(define build-genome (lambda () (map build-voxel (build-list 2 values))))
(define genome (build-genome))
(define score 999999999999999)
(define new-genome (build-genome))
(define new-score 0)
(define gears-canvas%
  (class* canvas% ()
    
    (inherit refresh with-gl-context swap-gl-buffers get-parent)
    
    (define rotation 0.0)
    
    (define view-rotx 20.0)
    (define view-roty 30.0)
    (define view-rotz 0.0)
    
    (define gear1 #f)
    (define gear2 #f)
    (define gear3 #f)
    
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
         
         ;(unless gear1
         ; (printf "  RENDERER:   ~A\n" (gl-get-string 'renderer))
         ; (printf "  VERSION:    ~A\n" (gl-get-string 'version))
         ; (printf "  VENDOR:     ~A\n" (gl-get-string 'vendor))
         ; (printf "  EXTENSIONS: ~A\n" (gl-get-string 'extensions))
         ; )
         
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
         
         (unless gear1
           
           (set! gear1 (gl-gen-lists 1))
           (gl-new-list gear1 'compile)
           (gl-material-v 'front
                          'ambient-and-diffuse
                          (vector->gl-float-vector (vector 0.8 0.1 0.0 1.0)))
           ;(build-gear 1.0 4.0 1.0 20 0.7)
           (gl-end-list)
           
           (set! gear2 (gl-gen-lists 1))
           (gl-new-list gear2 'compile)
           (gl-material-v 'front
                          'ambient-and-diffuse
                          (vector->gl-float-vector (vector 0.0 0.8 0.2 1.0)))
           ;(build-gear 0.5 2.0 2.0 10 0.7)
           (gl-end-list)
           
           (set! gear3 (gl-gen-lists 1))
           (gl-new-list gear3 'compile)
           (gl-material-v 'front
                          'ambient-and-diffuse
                          (vector->gl-float-vector (vector 0.2 0.2 1.0 1.0)))
           ;(build-gear 1.3 2.0 0.5 10 0.7)
           (gl-end-list)
           
           (gl-enable 'normalize))))
      (refresh))
    
    (define sec (current-seconds))
    (define frames 0)
    
    (define/override (on-paint)
      (when gear1
        (when (>= (- (current-seconds) sec) 5)
          ;(send (get-parent) set-status-text (format "~a fps" (/ (exact->inexact frames) 5)))
          (set! sec (current-seconds))
          (set! frames 0))
        (set! frames (add1 frames))
        
        (when step?
          ;; TODO: Don't increment this infinitely.
          (set! view-roty (+ view-roty 5.0))
          (set! rotation (+ 2.0 rotation)))
        (with-gl-context
         (lambda ()
           
           (gl-clear-color 0.0 0.0 0.0 0.0)
           (gl-clear 'color-buffer-bit 'depth-buffer-bit)
           
           (gl-push-matrix)
           (gl-rotate view-rotx 1.0 0.0 0.0)
           (gl-rotate view-roty 0.0 1.0 0.0)
           (gl-rotate view-rotz 0.0 0.0 1.0)
           (set! new-genome (build-genome))
           (map (lambda (v)
                  (gl-push-matrix)
                  
                  (gl-translate (list-ref v 0) (list-ref v 1)   0.0)
                  ;(gl-rotate (- (* -2.0 0) 25.0) 0.0 0.0 1.0)
                  (gl-material-v 'front-and-back
                                 'ambient-and-diffuse
                                 (vector->gl-float-vector (vector (list-ref v 2) (list-ref v 3) (list-ref v 4) (list-ref v 5))))
                  ;(build-gear 0.5 2.0 2.0 10 0.7)
                  [gl-polygon-mode 'front-and-back 'fill]
                  [picture]

                  (gl-pop-matrix)
                  )
                new-genome)
           
           ;           (gl-push-matrix)
           ;           (gl-translate -3.0 -2.0 0.0)
           ;           (gl-rotate rotation 0.0 0.0 1.0)
           ;           (gl-call-list gear1)
           ;           (gl-pop-matrix)
           ;           
           ;           (gl-push-matrix)
           ;           (gl-translate 3.1 -2.0 0.0)
           ;           (gl-rotate (- (* -2.0 rotation) 9.0) 0.0 0.0 1.0)
           ;           (gl-call-list gear2)
           ;           (gl-pop-matrix)
           ;           
           ;           (gl-push-matrix)
           ;           (gl-translate -3.1 4.2 0.0)
           ;           (gl-rotate (- (* -2.0 rotation) 25.0) 0.0 0.0 1.0)
           ;           (gl-call-list gear3)
           ;           (gl-pop-matrix)
           ;           
           ;           
           ;           (gl-pop-matrix)
           ;           (gl-push-matrix)
           ;           (gl-translate 3.1 -2.0 0.0)
           ;           
           (gl-pop-matrix)
           (swap-gl-buffers)
           (gl-flush)
           (set! new-score (compare ))
           (sleep 1)
           
           (when (< new-score score) (begin
                                       (set! genome new-genome)(set! score new-score)
                                       (display (format "New score: ~a~n" new-score))
                                       (write new-genome)
                                       (newline)))
           ))
        (when step?
          (set! step? #f)
          (queue-callback (lambda x (send this run))))))
    
    (super-instantiate () (style '(gl no-autoclear)))))
(define controls? #t)
(define (gl-frame)
  (let* ((f (make-object frame% "gears.ss" #f))
         (c (new gears-canvas% (parent win) (min-width pic-width) (min-height pic-height) (stretchable-width #f) (stretchable-height #f) )))
    (send f create-status-line)
    (when controls?
      (let ((h (instantiate horizontal-panel% (win)
                 (alignment '(center center)) (stretchable-height #f))))
        (instantiate button%
          ("Start" h (lambda (b e) (send b enable #f) (send c run)))
          (stretchable-width #t) (stretchable-height #t))
        (let ((h (instantiate horizontal-panel% (h)
                   (alignment '(center center)))))
          (instantiate button% ("Left" h (lambda x (send c move-left)))
            (stretchable-width #t))
          (let ((v (instantiate vertical-panel% (h)
                     (alignment '(center center)) (stretchable-width #f))))
            (instantiate button% ("Up" v (lambda x (send c move-up)))
              (stretchable-width #t))
            (instantiate button% ("Down" v (lambda x (send c move-down)))
              (stretchable-width #t)))
          (instantiate button% ("Right" h (lambda x (send c move-right)))
            (stretchable-width #t)))))
    (send topwin show #t) ))
(gl-frame)
(send topwin show #t)
;(send dc show)

[defmacro trans [ xx y z thunk ]
                 ;[lambda []
                  `[ begin
                    [gl-push-matrix]
                   (gl-translate ,xx ,y ,z)
 [,thunk]
 [gl-pop-matrix]
                  ; ]
  [lambda [] "trans"]] ]  
[defmacro left  [thunk] `[trans -1 0 0 ,thunk]]
[defmacro right [thunk] `[trans 1 0 0  ,thunk]]
[defmacro above [thunk] `[trans 0 1 0  ,thunk]]
[defmacro below [thunk] `[trans 0 -1 0 ,thunk]]
[defmacro forwards [thunk] `[trans 0 0 1 ,thunk]]
[defmacro backwards [thunk] `[trans 0 0 -1 ,thunk]]
[define tri [lambda []
              (glBegin GL_TRIANGLES)
              (glVertex3i 0 1 0)				
              (glVertex3i -1 -1  0)			
              (glVertex3i 1 -1  0)
              (glEnd)]]
[defmacro rot [x y z o]
                   `[begin
                   [gl-push-matrix]
                   (glRotated ,x 1 0 0)
                               (glRotated ,y 0 1 0)
                               (glRotated ,z 0 0 1)
                   [,o]
                   
                   [gl-pop-matrix]
                   [lambda [] "rot macro"]]]
[define push-pop [lambda [a-thunk]
                   [lambda []
                     [gl-push-matrix]
                     [a-thunk]
                     [gl-pop-matrix]
                     ]]]
[define square [lambda []
                 [one-sided-square]
                 [rot 0 180 0 one-sided-square]]]
[define one-sided-square [lambda []
                 ;[lambda []
                          ; [gl-push-matrix]
                           ;[gl-scale 0.5 0.5 0.5]
              (glBegin GL_TRIANGLES)
                           
              (glVertex3i -1 1  0)
                               (glVertex3i -1 -1  0)
                               (glVertex3i 1 -1 0)				
              			
              
                               (glVertex3i -1 1  0)
                 (glVertex3i 1 -1  0)              
                 (glVertex3i 1 1  0)
                               
              (glEnd)
                           ;[gl-pop-matrix]
                 [lambda []   "double run on square()" ]
                 
                 ]
                  ; ]
  ]
;[define picture [lambda [] [above [beside [rotate 0 0 45 tri] tri] tri]]]
;[define picture [lambda [] [beside [rotate 0 0 -30 tri] tri]]]
[defmacro h-strip [t1 t2 t3]  `[begin [left ,t1] [,t2] [right ,t3] [lambda [] "h-strip"]]]
[defmacro v-strip [t1 t2 t3]  `[begin [above ,t1] [,t2]  [below ,t3] [lambda [] "v-strip"]]]
[defmacro face [t] 
                  `[begin
                     [v-strip [h-strip ,t ,t ,t]
                           [h-strip ,t ,t ,t]
                            
                           [h-strip ,t ,t ,t]]
                           [lambda [] "face"]
                           ]]
[defmacro box []
  `[begin
     [gl-push-matrix]
     [gl-scale 0.5 0.5 0.5]
     [right [rot 0 90 0 [one-sided-square]]]
     [left [rot 0 -90 0 [one-sided-square]]]
     [above [rot -90 0 0 [one-sided-square]]]
     [below [rot 90 0 0 [one-sided-square]]]
     [backwards [rot 180 0 0 [one-sided-square]]]
     [forwards [one-sided-square]]
     [gl-pop-matrix]
     [lambda [] #f]]]
;[define cube [lambda [t]
;              [rot 45 0 0 [face t]]
;               [rot 0 45 0 [face t]]
;               [rot 0 0 45 [face t]]
               ;[face]
;               ]]

[define null [lambda [] [lambda [] #f]]]


[define prim% [class object%
                [define children '[]]
                [super-new]
                [define/public [render ]
                 [display "rendering..."] 
                  [send this draw]
                  [send this draw-children ]
                  ]
                [define/public [draw] [display "not drawing..."] #f]
                [define/public [draw-children]
                  [map [lambda [c]
                         [send c render]] children]]
                [define/public [add-child a-child]
                  [set! children [cons a-child children]]
                ]]]
  [define thingy% [class prim%
                    ;[define/public [draw-routine] [lambda [] ]]
                    [init draw-routine]
                    [define dr draw-routine]
                    [define/override [draw]
                      [dr]]
                    [super-new]
                    ]]
[define voxel% [class prim%
                ; [inherit render]
  [super-new]
  [define/override [draw]
    [display "drawing"]
    [box]
                 
  ]
                           ]]
[defmacro right-arm-joint [thunk]
  `[begin
    [gl-push-matrix]
   [rot 0  [- 0 [random 90]] [- [random 180] 90] ,thunk]
   [gl-pop-matrix]
   [lambda [] "random-rotate"]
   ]]
[defmacro left-arm-joint [thunk]
  `[begin
    [gl-push-matrix]
   [rot 0  [random 90] [- [random 180] 90] ,thunk]
   [gl-pop-matrix]
   [lambda [] "random-rotate"]
   ]]
[define a-box [new voxel%]]
[define [randomint i]
  [if [< i 0] [- 0 [random [* -1 i]]]
      [random i]]]
[defmacro joint [x y z thunk]
  `[begin
    [gl-push-matrix]
   [rot [randomint ,x]  [randomint ,y] [randomint ,z ] ,thunk]
   [gl-pop-matrix]
   [lambda [] "joint"]
   ]]
(define-syntax njoint
  (lambda (x)
    (syntax-case x ()
      [(_ x y z ...)
       (syntax (rot x y z ...))])))

[defmacro right-arm []
  `[right [right [right-arm-joint [begin [box]  [right box] [right [joint 1 -90 1 [begin [right box] [right [right  box]]]]]]]]]
  ]
[defmacro left-arm []
  `[left [left [left-arm-joint [begin [box]  [left box] [left [joint 1 90 1 [begin [left box]  [left [left  box]]]]]]]]]
  ]
[define body [lambda [] 
               ;torso
               [face [box]]
               ;hips
               [below [below [h-strip box box box]]]
               ;legs
[below [below [below [below [begin 
                              [joint 90 1 1 [right [v-strip box box box]]]
                        [joint 90 1 1 [left [v-strip box box box]]]]]]]]
[above [begin
         ;arms
         [left-arm]
         [right-arm]]]
               
               ;head
             [above [above box]]  ]]
             
[define test  [lambda []
                ;[send [new thingy% [draw-routine [lambda [] [box]]]] render]
                [body]
                  ;[send a-box render]
                ;[below [box]]
  ;[square]
  ;[left [rot 0 -90 0 [face square]]]              
  ;              [right [rot 0 90 0 [face square]]]
  ;              [above [rot -90 0 0 [face square]]]
  ;[above square]
  ;[[above [above [square]]]]
  ;[left [above [above square]]]
                ;[h-strip square square square]
                ;[v-strip square null square]
                ;[face square]
                ]]
[define picture test]