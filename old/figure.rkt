#lang racket
 (require 
         
         mzlib/math
         sgl
         sgl/gl-vectors)
(require sgl/gl)
(require mzlib/defmacro)

(provide body current-colour-index current-colour current-colour-and-inc inc-colour-index set-colours! get-colours set-mask-index! get-mask-index)

;[define max-colours (* 9 1044)]
[define max-colours 1032]
[define texel-slice (/ 3 1024)]
[define colours (build-list [add1 max-colours] (lambda (x) 70))]
[define [set-colours! val] [set! colours val]]
[define [get-colours] colours]

[define _colour_pointer 0]
[define current-colour-index (lambda () _colour_pointer)]


(define current-texel-offset (lambda () (/ _texel_offset 1024)))
(define _texel_offset 0)
                      (define reset-texel-offset (lambda () (set! _texel_offset 0)))
                      
                      [define inc-texel-offset (lambda () (let ((new-index (add1 _texel_offset)))
                               (if (> new-index 1024)
                                   (set! _texel_offset 0)
                                   (set! _texel_offset new-index))))]

[define mask-index 1]
[define [set-mask-index! val] [set! mask-index val]]
[define [get-mask-index] mask-index]
[define (inc-mask-index) (display (format "Bumped mask index, was: ~a" mask-index)) (newline)
  (let ((new-index (+ 4 mask-index)))
                               (if (> new-index max-colours)
                                   (begin (write "Wrapped mask-index")(newline)(set! mask-index 1))
                                   (set! mask-index new-index)))]


;[define current-colour  (lambda() (if [and [not drawing-best-display?] (send calc-masks? get-value)] (if  (equal? mask-index (current-colour-index)) 100 0)(list-ref colours (current-colour-index))))]
[define current-colour  (lambda() (list-ref colours (current-colour-index)))]
[define (current-colour-and-inc) (inc-colour-index)(current-colour)]
[defmacro inc-colour-index () `(begin (let ((new-index (add1 _colour_pointer)))
                               (if (> new-index max-colours)
                                   (begin (write "Wrapped colour")(newline)(set! _colour_pointer 0))
                                   (set! _colour_pointer new-index))))]


[defmacro h-strip [t1 t2 t3]  `[begin [left ,t1] [,t2] [right ,t3] [lambda [] "h-strip"]]]
[defmacro v-strip [t1 t2 t3]  `[begin [above ,t1] [,t2]  [below ,t3] [lambda [] "v-strip"]]]


[defmacro rcylinder []
  `[begin
     [gl-push-matrix]
     ;[gl-scale 0.5 0.5 0.5]
     [rot 0 0 90 cylinder]
     [gl-pop-matrix]
     [lambda [] #f]]]
;[define cube [lambda [t]
;              [rot 45 0 0 [face t]]
;               [rot 0 45 0 [face t]]
;               [rot 0 0 45 [face t]]
;[face]
;               ]]


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

;Draw face
[defmacro face [t] 
  `[begin
     [v-strip [h-strip ,t ,t ,t]
              [h-strip ,t ,t ,t]
              
              [h-strip ,t ,t ,t]]
     [lambda [] "face"]
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

[define [nth pos lst] [list-ref lst [sub1 pos]]]
[defmacro random-joint [index constraints thunk]
  `[begin
     [gl-push-matrix]
     [let [[index [add1 [* 3 [- ,index 1]]]]]
     [rot [nth index ,constraints]  [nth [add1 index] ,constraints] [nth[add1 [add1 index]] ,constraints ] ,thunk]
     [gl-pop-matrix]
     [lambda [] "joint"]
     ]]]
(define-syntax njoint
  (lambda (x)
    (syntax-case x ()
      [(_ x y z ...)
       (syntax (rot x y z ...))])))

[defmacro right-arm []
  `[push-pop-matrix (right [right [random-joint 1 extents (push-pop-matrix [begin (gl-scale 1 0.66 0.66) [rcylinder]  [right rcylinder] [right [random-joint 2 extents [begin [right rcylinder] [right [right  rcylinder]]]]]])]])]]
  
[defmacro left-arm []
  `[push-pop-matrix [left [left [random-joint 3 extents [push-pop-matrix [begin [gl-scale 1 0.66 0.66 ] [rcylinder]  [left rcylinder] [left [random-joint 4 extents [begin [left rcylinder]  [left [left  rcylinder]]]]]]]]]]]
  ]


[define reset-colour-index (lambda () (display (format "Reset at index ~a~n" _colour_pointer)) (set! _colour_pointer 0))]

;Draw body
[define body [lambda [extents] 
               [reset-colour-index]
               [inc-mask-index]
               [gl-push-matrix]
               [gl-scale 0.65 0.8 1]
               ;torso
               [face [box]]
               ;hips
               [random-joint 9 extents  [begin
                                [below [below [h-strip box box box]]]
                                ;legs
                                
                                 
                                 [below [below [below [below [begin                                                                                    [gl-push-matrix]
                                                         [gl-scale 1 1.5 1]                                                                                                                           
                                                        [random-joint 5 extents  [right [begin [cylinder] [below cylinder]  [below [random-joint 6 extents [begin [below cylinder] [below [below [begin [cylinder] [below [rot 90 0 0 [random-joint 11 extents [above [begin [gl-scale 1 1 0.5 ] [cylinder]]]]]]]]]]]] [lambda [] "v-strip"]]]]
                                                        [random-joint 7 extents [left [begin [cylinder] [below cylinder]  [below [random-joint 8 extents [begin [below cylinder] [below [below [begin [cylinder] [below [rot 90 0 0 [random-joint 12 extents [above [begin [gl-scale 1 1 0.5 ] [cylinder]]]]]]]]]]]] [lambda [] "v-strip"]]]]
                                                        [gl-pop-matrix][lambda () "stuff"]
                                                        ]]]]]
                                 ] ]
               [above [begin
                        ;arms
                        [left-arm]
                        [right-arm]]]
               [gl-pop-matrix]
               ;head
               [push-pop-matrix [above (begin (gl-scale 1 1.0 1 )  [above box])]]]]


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
[defmacro  push-pop-matrix  [a-thunk]
                   `(begin
                     [gl-push-matrix]
                     ,a-thunk
                     [gl-pop-matrix]
                     [lambda () "pushpop"])]
[define square [lambda []
                 [one-sided-square]
                 [rot 0 180 0 one-sided-square]]]
[define one-sided-square [lambda []
                           ;[lambda []
                           ; [gl-push-matrix]
                           ;[gl-scale 0.5 0.5 0.5]
                           (gl-material-v 'front-and-back
                                 'ambient-and-diffuse
                                 (vector->gl-float-vector (vector [/ (current-colour-and-inc) 100] [/ (current-colour-and-inc) 100] [/ (current-colour-and-inc) 100] [/ (current-colour-and-inc) 100])))
                           
                           (glBegin GL_TRIANGLES)
; glVertex2d(0.0,0.0);
; glVertex2d(1.0,0.0);
; glVertex2d(1.0,1.0);
; glVertex2d(0.0,1.0);
                           (let ([o (current-texel-offset)])
                             (inc-texel-offset)
(glTexCoord2d (+ o 0.0) 0.0)                           
                           (glVertex3i -1 1  0)
(glTexCoord2d (+ o texel-slice) 0.0)
                           (glVertex3i -1 -1  0)
(glTexCoord2d (+ o 0.0) 1.0)
                           (glVertex3i 1 -1 0)	
                           
                      (glTexCoord2d (+ o 0.0) 1.0)     
                           (glVertex3i -1 1  0)
                      (glTexCoord2d (+ o texel-slice) 1.0)
                           (glVertex3i 1 -1  0)              
(glTexCoord2d (+ o texel-slice) 0.0)
                           (glVertex3i 1 1  0)
                       )    
                           (glEnd)
                           ;[gl-pop-matrix]
                           [lambda []   "double run on square()" ]
                           
                           ]
  ; ]
  ]
;[define picture [lambda [] [above [beside [rotate 0 0 45 tri] tri] tri]]]
;[define picture [lambda [] [beside [rotate 0 0 -30 tri] tri]]]

[defmacro checker [t u] 
  `[begin
     ;[left [left  ,t]]
     ;[right ,t]
     ;[lambda [] "checker"]
     ;]]
     [gl-push-matrix]
     [gl-scale 1/3 1/3 1/3]
     
     [v-strip [h-strip ,u ,t ,u]
              [h-strip ,t ,u ,t]
              [h-strip ,u ,t ,u]]
     [gl-pop-matrix]
     [lambda [] "checker"]
     ]]
[defmacro scale [s t]
  `[begin
     [gl-push-matrix]
     [gl-scale ,s ,s ,s]
     [,t]
     [gl-pop-matrix]
     [lambda [] #f]]]
[defmacro subdev2 (t)
  `(begin 
     (gl-push-matrix)
  ;(gl-scale 0.5 0.5 0.5)
  (face ,t)
  (gl-pop-matrix)
  (lambda () "subdev")
  )]
[defmacro subdev (t)
  `(begin 
     (gl-push-matrix)
  ;(gl-scale 0.5 0.5 0.5)
  (face (subdev2 ,t))
  [subdev2 ,t]
  (gl-pop-matrix)
  (lambda () "subdev")
  )]
[defmacro box []
  `[begin
     [gl-push-matrix]
     [gl-scale 0.5 0.5 0.5]
     [right [rot 0 90 0  [one-sided-square]]]
     [left [rot 0 -90 0  [one-sided-square]]]
     [above [rot -90 0 0  [one-sided-square]]]
     [below [rot 90 0 0 [one-sided-square]]]
     [backwards [rot 180 0 0 [one-sided-square]]]
     [forwards [one-sided-square]]
     [gl-pop-matrix]
     [lambda [] #f]]]
[defmacro cylinder []
  `[begin
     [gl-push-matrix]
     ;[gl-scale 0.5 0.5 0.5]
     
          [begin
     [map [lambda [angle]
      [rot 0 angle 0  [forwards [begin [gl-scale 0.5 1 1 ] [one-sided-square]]]]]
            [list 45  90 135 180 -45 -90 -135 0]]
     
     [above [rot -90 0 0  [begin [gl-scale 0.5 0.5 0.5 ] [one-sided-square]]]]
     [below [rot 90 0 0 [begin [gl-scale 0.5 0.5 0.5 ] [one-sided-square]]]]]
     [gl-pop-matrix]
     [lambda [] #f]]]

[defmacro swapargs [g f a b]
 `[begin
    [,g
    [,f ,a ,b]
    [,f ,b ,a]]]]

[define [floor ]
  [rot -90 0 0 [below  [scale 20 [swapargs checker checker [swapargs checker checker  one-sided-square null][swapargs checker checker  null one-sided-square ]] ]]]]
