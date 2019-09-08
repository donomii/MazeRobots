#! /usr/bin/env mred
#lang racket
;(require (planet "amb.scm" ("wmfarr" "amb.plt" 1 0)))
;(require (planet dherman/memoize:3:1))
[require "figure.rkt"]

(require mzlib/defmacro)
(require scheme/class scheme/gui/base)
(require 
         
         mzlib/math
         sgl
         sgl/gl-vectors)
(require sgl/gl)
(require scheme/foreign)


(define topwin (new (class frame%
                      (augment* [on-close (lambda () (exit))])
                      (super-new))
                    [label "test"]
                    [style '(metal)]))
(define win (new horizontal-pane% (parent topwin)))
;(define f win)
(define pic (make-object bitmap%  10 10 ))

[display [format "Pic dimensions: ~a ~a~n" [send pic get-width][send pic get-height]]]
;[set! pic-width [sub1 [send pic get-width]]]
;[set! pic-height [sub1 [send pic get-height]]]
(define bdc (new bitmap-dc% [bitmap pic]))
(define diff-pic (make-object bitmap%  10 10 ))

(define diff-pic-dc (new bitmap-dc% (bitmap diff-pic)))
;(send diff-pic-dc draw-bitmap diff-pic 0 0   )

[define [nth pos lst] [list-ref lst [sub1 pos]]]

(newline)
[define drawing-best-display? #f]
;(write (send pic get-width))
(define showpic (lambda (c a-dc) (send a-dc draw-bitmap pic 0 0   )))
(define showdiffpic (lambda (c a-dc) (send a-dc draw-bitmap diff-pic 0 0   )))
[define graph-column (instantiate vertical-panel% (win)
                 (alignment '(center center)) (stretchable-height #t) (stretchable-width #f))]
[define pic-row  (instantiate horizontal-panel% (graph-column)
                 (alignment '(center center)) (stretchable-height #f))]
[define pic-width 256]
[define pic-height 256]
(define c
  (new canvas% (parent pic-row)
       (min-width pic-width) (min-height pic-height)
       [paint-callback showpic]))
(define diffpic-canvas
  (new canvas% (parent pic-row)
       (min-width pic-width) (min-height pic-height)
       [paint-callback showdiffpic]))
(define rvec (make-cvector _ubyte (* pic-width (add1 pic-height))))
(define bvec (make-cvector _ubyte  (* pic-width (add1 pic-height))))
(define gvec (make-cvector _ubyte  (* pic-width (add1 pic-height))))

(define (get-png-pixel  x y) (let ([ col (new color%)]) 
                                      (send bdc get-pixel x y col)
                                      col))

                            
(define count 0)
(define get-gl-data (lambda () 
                      (glReadPixels 0 0  pic-width pic-height GL_RED GL_UNSIGNED_BYTE rvec)
                      (glReadPixels 0 0  pic-width pic-height GL_BLUE GL_UNSIGNED_BYTE gvec)
                      (glReadPixels 0 0  pic-width pic-height GL_GREEN GL_UNSIGNED_BYTE bvec)))





(define build-voxel (lambda (a) 
                      (list 
                       (/ (- (random 11) 5) 1) (/ (- (random 11) 5) 1) (/ (random 10) 10) (/ (random 10) 10) (/ (random 10) 10) (/ (random 10) 10))))
[define [randomint alist]
  [let* [[lower [first alist]]
        [upper [second alist]]
        [bound [- upper lower]]
        [res   [if [equal? bound 0] 0 [if [< bound 0] [* -1 [random [* -1 bound]]] [random bound]]]]
        [final [+ res lower]]]
    final]]
[define extents'[  
          [ 0 -90] [-90 90] [0 0];right shoulder
          [0 0]    [0 -90]  [0 0] ;right elbow
          [ 0 90]  [-90 90] [0 0] ;left shoulder
          [0 0]    [0 90]   [0 0] ;left elbow
          [0 -90 ] [-45 45] [-45 45] ;right leg
          [0 90]   [0 0] [0 0] ;right knee
          [0 -90]  [-45 45] [-45 45] ;left leg
          [0 90]   [0 0] [0 0] ;left knee
          [-45 0] [0 0] [0 0] ; waist
          [-50 50] [-50 50] [-50 50] ; position
          [20 100] [20 100] [20 100] ; a foot
          [20 100] [20 100] [20 100] ; another foot
          ]]
[define best-genome '(0 0 10 0 0 0 0 0 -10 0 0 0 0 0 0 48 0 0 -29 0 0 44 0 0 -6 0 0 -5 17 10 0 0 0 0 0 0)]

[define (save-model) [with-output-to-file "chardata" [lambda [] (write best-genome)(newline)(write best-colours)] #:exists 'truncate/replace ]]
[define (load-model) [with-input-from-file "chardata" [lambda [] (set! best-genome [read])[display "Loaded position data"][newline](set! best-colours [read])[display "Loaded colours"][newline]]  ]]
[define [make-dna extents] [map [lambda [x] [randomint x]] extents]]
(define build-genome (lambda () [make-dna extents]))
;
;(lambda (x) (cvector-set! *texture* x 
;                                       (let* ([s (- (random 512) 256)]
;                                         (t (random 3))
;                                         (byteshift (if (equal? t 0)
;                                                        1
;                                                        (if (equal? t 1)
;                                                            256
;                                                            (* 256 256))))
;                                         (delta (* s byteshift))
;                                         (final (if (< delta 0) 0 delta)))
;                                         final)
;                                       ;(if (odd? x) (random (expt 2 31)) 0)
;                                       ))

(define (force-positive x) (if (< x 0 ) (* x -1) x))
(define (set-nth lst ind new) (append (take lst ind) (list new) (drop lst (+ 1 ind))))
(define adjust-position? #f)
(define small-twiddle? #f)
(define large-twiddle? #f)
(define anneal? #f)
[define calc-masks? #f]
[define compare? #f]
[define [mutate genome] 
  ;[use-best-texture]
;  (map  (lambda (x) (gl-vector-set! *texture* x  (force-positive [+ (gl-vector-ref *texture* x) 
;                                                                    ;(random (expt 2 24))
;                                                                    (+
;                                                                                             (* (* 256 256) [- [random 3] 1])
;                                                                                             (* 256 [- [random 3] 1])
;                                                                                             (* 1 [- [random 3] 1]))
;                                                                    ])))
;                                   (cdr (build-list 10000 values)))
 
     [when (send small-twiddle? get-value)
         (begin  (set-colours! 
        (set-nth best-colours (current-twiddle) [+ (list-ref best-colours (current-twiddle)) (* 1 [- [random 3] 1])]))
        (inc-twiddle))]
     (when (send anneal? get-value)
         (begin [set-colours! [map [lambda [x] [+ x ( * 10 [- [random 3] 1])]] best-colours]]))
  [when (send large-twiddle? get-value )
            (begin [set-colours! 
        (set-nth best-colours (current-twiddle) [+ (list-ref best-colours (current-twiddle)) (if (equal? [random 2] 1) 10 -10)])]
        (inc-twiddle))]
  ;]
         
(if (send adjust-position? get-value)  [map [lambda [x] [+ x [- [random 3] 1]]] genome]  genome) 
  ]
(define genome (build-genome))

[set! genome best-genome]

;[define masks (make-vector [add1 max-colours])]
[define cum-red 0]
[define cum-blue 0]
[define cum-green 0]
[define cum-count 0]
[define max-colours 1032]
[define _colour_pointer 0]


;[define colours (build-list 1000 (lambda (x) (random 101)))]
[define _current_twiddle 0]
[define (current-twiddle) _current_twiddle]
[define inc-twiddle (lambda () (let ((new-index (add1 _current_twiddle)))
                               (if (> new-index max-colours)
                                   (set! _current_twiddle 0)
                                   (set! _current_twiddle new-index))))]
[define read-file (lambda (f) (call-with-input-file f (lambda (p) (read p))))]
[define best-colours [get-colours]]
;[define best-colours (read-file "colourmap")]
[set-colours! best-colours]




[define reset-colour-index (lambda () (display (format "Reset at index ~a~n" _colour_pointer)) (set! _colour_pointer 0))]


                (define vec (make-vector 100))
                      (define *texture* (glGenTextures 10000))
                      (define *texture-ref* (gl-vector-ref *texture* 0))
      [define (upload-texture)
        (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA 1024 3
                      0 GL_RGBA
                      GL_UNSIGNED_INT_8_8_8_8_REV
                     
                      *texture*)
]
                      
                      (define best-texture #f)

                      
                      [define set-best-texture (lambda () (set! best-texture (cvector->list *texture*)))]
                      [define use-best-texture (lambda () (map (lambda (x) (cvector-set! *texture* x (list-ref best-texture x))) (cdr (build-list 10000 values))))]
                      (glEnable GL_TEXTURE_2D )
                      (glDisable GL_TEXTURE_2D )
                      (glPixelStorei GL_UNPACK_ALIGNMENT 1)
                      (glBindTexture GL_TEXTURE_2D *texture-ref*)
                  
 
        
       

        (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)   ; filtrage lineaire
        (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
                 (glTexParameterf GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT );
(glTexParameterf GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT );

                     (begin 
                       ;[map (lambda (x) (cvector-set! *texture* x 
                       ;                (random (expt 2 31))
                       ;                                     ;(if (odd? x) (random (expt 2 31)) 0)
                        ;               )) (cdr (build-list 10000 values))]
                             #t)
 (upload-texture)
                     [set-best-texture]
(define score 99692880 )
(define new-genome (build-genome))
(define new-score 0)
(define gears-canvas%
  (class* canvas% ()
    
    (inherit refresh with-gl-context swap-gl-buffers get-parent)
    [init best-display?]
    [define is-best-display best-display?]
    (define rotation 0.0)
    
    (define view-rotx 0.0)
    (define view-roty 0.0)
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
         ;(gl-disable 'lighting)
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
      [with-gl-context [lambda []
                         (gl-push-matrix)
           (gl-rotate view-rotx 1.0 0.0 0.0)
           (gl-rotate view-roty 0.0 1.0 0.0)
           (gl-rotate view-rotz 0.0 0.0 1.0)
                         [set! new-genome [mutate best-genome]]
                         [set! drawing-best-display? #f]
                         [when is-best-display
                           [set! drawing-best-display? #t]
                           (set-colours! best-colours)
                           (use-best-texture)
                           (set-mask-index! (- [get-mask-index] 4))]
                           
                         [do-paint [if is-best-display best-genome new-genome] is-best-display]
           (gl-pop-matrix)
              (swap-gl-buffers)
           (gl-flush)
                         ;(glFinish)
                        ; (sleep 1)
           ]]
      (when step?
          (set! step? #f)
          (queue-callback (lambda x (send this run)) #f ))
      )
    
    (super-instantiate () (style '(gl no-autoclear)))))
(define controls? #t)
[define draw-row (instantiate horizontal-panel% (graph-column)
                 (alignment '(center center)) (stretchable-height #f))]
(define (gl-frame)
  (let* ((f (make-object frame% "gears.ss" #f))
         (c (new gears-canvas% (parent draw-row) [best-display? #f] (min-width pic-width) (min-height pic-height) (stretchable-width #f) (stretchable-height #f) ))
         
         )
    (send f create-status-line)
    (when controls?
      (let ((h (instantiate horizontal-panel% (win)
                 (alignment '(center center)) (stretchable-height #f))))
        (instantiate button%
          ("Start" h (lambda (b e) (send b enable #f) (send c run)))
          (stretchable-width #t) (stretchable-height #t))
        (instantiate button%
          ("Save" h (lambda (b e) (save-model)))
          (stretchable-width #t) (stretchable-height #t))
        (instantiate button%
          ("Load" h (lambda (b e) (load-model)))
          (stretchable-width #t) (stretchable-height #t))
        (let ((h (instantiate horizontal-panel% (h)
                   (alignment '(center center)))))
          (instantiate button% ("Left" h (lambda x (send c move-left)))
            (stretchable-width #t))
          (let ((v (instantiate vertical-panel% (h)
                     (alignment '(center center)) (stretchable-width #f))))
            (instantiate button% ("Up" v (lambda x (send c move-up) [send best-gl move-up]))
              (stretchable-width #t))
            (instantiate button% ("Down" v (lambda x (send c move-down)))
              (stretchable-width #t)))
          (instantiate button% ("Right" h (lambda x (send c move-right)))
            (stretchable-width #t))
          (let ((opts (instantiate vertical-panel% (h)
                        (alignment '(center center)) (stretchable-width #f))))
            (set! adjust-position? (new check-box% [label "Adjust position"] [parent opts] [value #f] [enabled #t]	 ))
            (set! small-twiddle? (new check-box% [label "Small twiddle"] [parent opts] [value #f] [enabled #t]	 ))
            (set! large-twiddle? (new check-box% [label "Large twiddle"] [parent opts] [value #f] [enabled #t]	 ))
            (set! anneal? (new check-box% [label "Anneal"] [parent opts] [value #f] [enabled #t]	 ))
            (set! calc-masks? (new check-box% [label "Calculate masks"] [parent opts] [value #f] [enabled #t]	 ))
            (set! compare? (new check-box% [label "Compare"] [parent opts] [value #t] [enabled #t]	 ))
            )
          )
        (let ((h (instantiate horizontal-panel% (h)
                   (alignment '(center center)))))
          (instantiate button% ("Slide Left" h (lambda x (set! best-genome [set-nth best-genome 27 [- [nth  28 best-genome] 5]])))
            (stretchable-width #t))
          (let ((v (instantiate vertical-panel% (h)
                     (alignment '(center center)) (stretchable-width #f))))
            (instantiate button% ("Slide Up" v (lambda x (set! best-genome [set-nth best-genome 28 [- [nth  29 best-genome] 5]])))
              (stretchable-width #t))
            (instantiate button% ("Slide Down" v [lambda x (set! best-genome [set-nth best-genome 28 [+ [nth  29 best-genome] 5]])])
              (stretchable-width #t)))
          (instantiate button% ("Slide Right" h (lambda x (set! best-genome [set-nth best-genome 27 [+ [nth  28 best-genome] 5]])))
            (stretchable-width #t))
          
          )))
    [let [[ angles-column1 (instantiate vertical-panel% (win)
                 (alignment '(center center)) (stretchable-height #t) (stretchable-width #f))]
          [ angles-column2 (instantiate vertical-panel% (win)
                 (alignment '(center center)) (stretchable-height #t) (stretchable-width #f))]
          [angles1 [take  [build-list 36 values] 18]]
          [angles2 [drop  [build-list 36 values] 18]]
          [joint-names [list "Right shoulder" "Right shoulder" "Right shoulder" 
                             "Right elbow" "Right elbow" "Right elbow"
                             "Left shoulder" "Left shoulder" "Left shoulder"
                             "Left elbow" "Left elbow" "Left elbow"
                             "Right leg" "Right leg" "Right leg"
                             "Right knee" "Right knee" "Right knee"
                             "Left leg" "Left leg" "Left leg"
                             "Left knee" "Left knee" "Left knee"
                             "Waist" "Waist" "Waist"
                             "Position" "Position" "Position"
                             "Foot" "Foot" "Foot"
                             "Foot" "Foot" "Foot"
                             ]]] 
    [map [lambda [y] 
           [map [lambda [x]
	[letrec [[ scroller (new slider%	 	[label [format "~a ~a" [list-ref  joint-names x] x]]	 
 	 	[min-value -90]	 
 	 	[max-value 90]	 
 	 	[parent [second y]]	 
 	 	[callback [lambda [a b] [set! best-genome 
        (set-nth best-genome x [send scroller get-value])]]]	 
 	 	[init-value 0]	 
 	 	)]]
          #f]] [first y]]
           ] [list [list angles1 angles-column1] [list angles2  angles-column2]]]]
    (send topwin show #t) ))
    


(define (do-paint new-genome best?)
           (gl-clear-color 0.0 0.0 0.0 0.0)
           (gl-clear 'color-buffer-bit 'depth-buffer-bit)
          ;(write new-genome)
           
           
           (let [[v new-genome]]
                  (gl-push-matrix)
                  
                  (gl-translate [/ (list-ref v 27) 10]  [/ (list-ref v 28) 10]   0.0)
                  ;(gl-rotate (- (* -2.0 0) 25.0) 0.0 0.0 1.0)
                  (gl-material-v 'front-and-back
                                 'ambient-and-diffuse
                                 (vector->gl-float-vector (vector [/ (list-ref v 30) 100] [/ (list-ref v 31) 100] [/ (list-ref v 32) 100] [/ (list-ref v 33) 100])))
                  
                  
                  [gl-polygon-mode 'front-and-back 'fill]
                  [picture new-genome]
                  
                  (gl-pop-matrix)
                  
                ))




[define null [lambda [] [lambda [] #f]]]





[defmacro scale [s t]
  `[begin
     [gl-push-matrix]
     [gl-scale ,s ,s ,s]
     [,t]
     [gl-pop-matrix]
     [lambda [] #f]]]
[define mydna [make-dna extents]]


[define test  [lambda [dna]
                ;[send [new thingy% [draw-routine [lambda [] [box]]]] render]
(glBindTexture GL_TEXTURE_2D *texture-ref*)
                  
 
        ;(upload-texture)

        (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)   ; filtrage lineaire
        (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
                 ;(glTexParameterf GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT );
;(glTexParameterf GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT );



;(reset-texel-offset)
                ;[scale 2.5 [body dna]]
                [scale 1.3 [body dna]]
                ;[floor]
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

(gl-frame)
(send topwin show #t)
;(send dc show)
[define best-gl (new gears-canvas% (parent draw-row) [best-display? #t](min-width pic-width) (min-height pic-height) (stretchable-width #f) (stretchable-height #f) )]
;(define best-gl #f)