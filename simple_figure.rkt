#lang racket
(require 
         
  mzlib/math
  sgl
  sgl/gl-vectors)
(require sgl/gl)
(require mzlib/defmacro)

(provide body figure cube square one-sided-square)

;An experiment in using racket for procedural generation

; Macros are used in the hope that it helps the optimiser avoid the overhead of function calls
; All this needs to be converted into code that creates a vertex array since these calls are all deprecated now


(define-syntax njoint
  (lambda (x)
    (syntax-case x ()
      [(_ x y z ...)
       (syntax (rot x y z ...))])))

; Draws t at each point defined in a-list
[define [draw-path a-list t]
  [map [lambda [point]
         [trans [first a-list] [second a-list] [third a-list] t]]
       a-list]
  ]

; translate
[defmacro trans [ xx y z thunk ]
  ;[lambda []
  `[ begin
      [gl-push-matrix]
      (gl-translate ,xx ,y ,z)
      [,thunk]
      [gl-pop-matrix]
      ; ]
      [lambda [] "trans"]] ]


; Rotate
[defmacro rot [x y z o]
  `[begin
     [gl-push-matrix]
     (glRotated ,x 1 0 0)
     (glRotated ,y 0 1 0)
     (glRotated ,z 0 0 1)
     [,o]
                   
     [gl-pop-matrix]
     [lambda [] "rot macro"]]]


[defmacro left  [thunk] `[trans -1 0 0 ,thunk]]
[defmacro right [thunk] `[trans 1 0 0  ,thunk]]

; Move up
[defmacro above [thunk] `[trans 0 1 0  ,thunk]]

; Move down
[defmacro below [thunk] `[trans 0 -1 0 ,thunk]]
[defmacro forwards [thunk] `[trans 0 0 1 ,thunk]]
[defmacro backwards [thunk] `[trans 0 0 -1 ,thunk]]

; Draw a triangle
[define tri [lambda []
              (glBegin GL_TRIANGLES)
              (glVertex3i 0 1 0)				
              (glVertex3i -1 -1  0)			
              (glVertex3i 1 -1  0)
              (glEnd)]]

; Wrap a thunk in gl-push and gl-pop
[define push-pop [lambda [a-thunk]
                   [lambda []
                     [gl-push-matrix]
                     [a-thunk]
                     [gl-pop-matrix]
                     ]]]

; Draw a square
[define square [lambda []
                 [one-sided-square]
                 [rot 0 180 0 one-sided-square]]]

;Only one side of the square is visible, which saves some drawing time for boxes etc where you will only ever see one side of each square
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

; Horizontal strip.  Draw thunks t1, t2, t3 side-by-side
[defmacro h-strip [t1 t2 t3]  `[begin [left ,t1] [,t2] [right ,t3] [lambda [] "h-strip"]]]
; Vertical strip.  Draw thunks t1, t2, t3 above each other
[defmacro v-strip [t1 t2 t3]  `[begin [above ,t1] [,t2]  [below ,t3] [lambda [] "v-strip"]]]


[defmacro torso [t] 
  `[begin
     [v-strip [h-strip ,t ,t ,t]
              [h-strip ,t ,t ,t]
                            
              [h-strip ,t ,t ,t]]
     [lambda [] "face"]
     ]]

; Draw a cube
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

; Also draw a cube
[define [cube] [box]]

[defmacro right-arm []
  `[right [right [right-arm-joint [begin [box]  [right box] [right [joint 1 -90 1 [begin [right box] [right [right  box]]]]]]]]]
  ]
[defmacro left-arm []
  `[left [left [left-arm-joint [begin [box]  [left box] [left [joint 1 90 1 [begin [left box]  [left [left  box]]]]]]]]]
  ]
[define body [lambda [] 
               ;torso
               [torso [box]]
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

[define [figure colour]
  ;[printf "Drawing with colour ~a~n" colour]
  [gl-polygon-mode 'front-and-back 'fill]
  [apply glColor4f colour]
  (gl-material-v 'front-and-back
                 'ambient-and-diffuse
                 (vector->gl-float-vector (apply vector colour)))
  
  [body]
  ]
[defmacro joint [x y z thunk]
  `[begin
     [gl-push-matrix]
     [rot [randomint ,x]  [randomint ,y] [randomint ,z ] ,thunk]
     [gl-pop-matrix]
     [lambda [] "joint"]
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
[define [randomint i]
  [if [< i 0] [- 0 [random [* -1 i]]]
      [random i]]]
