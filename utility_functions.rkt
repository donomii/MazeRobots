#lang racket

;Utility Functions

; Typically  pure functions, i.e. that take some values and transform them.  Most notably all the (mathematical) vector functions are here.

[require math/array]
[require srfi/1]

[provide [all-defined-out]]

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

[define [printmap amap]
  [map [lambda [x]
         [map [lambda [y]
                [if [> [array-ref amap [vector x y]] 1]
                    [display "O"]
                    [display "."]
                    ]
                ]
              [iota 100]]
         [displayln ""]]
       [iota 100]]
  ]

[define [falsifyMap width height amap]
  (for ([i [in-range width]])
    [for [[j [in-range height]]]
      [array-set! amap [vector i j] 1]
      ]
    )
  ]

[define obstacle-map 
  [array->mutable-array (build-array #(100 100) [lambda [e] 1])]
  
  ]
[define [build-map mans boxes]
  [set! obstacle-map [array->mutable-array (build-array #(100 100) [lambda [e] 1])]]
  ;[displayln [arr:mutable-array-data obstacle-map]]
  [map [lambda [b] [array-set! obstacle-map [vector [+ 50 [first b]] [+ 50 [third b]]] 9001]] boxes]
  ;  [map [lambda [v]
  ;         [let [[m [first v]]]
  ;           [arr:array-set! obstacle-map [vector [inexact->exact [round [+ 50 [first m]]]] [inexact->exact [round [+ 50 [third m]]]]] #t]
  ;           ]] mans]
  ;[printmap omap]
  ;[drawMiniMap 100 100 obstacle-map]
  obstacle-map
  ]


[define [random-from-list target-list]
  [list-ref target-list [random [length target-list]]]
  ]

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


[define [normalise v]
  (match-let ([(list x1 y1 z1) v])
    [let [[mag1 [+ 0.0000001 [sqrt [+ [* x1 x1] [* y1 y1] [* z1 z1]]]]]]
      [list [/ x1 mag1] [/ y1 mag1] [/ z1 mag1]]
      ])]



[define [crossprod u v]
  (match-let ([(list u1 u2 u3) u]
              [(list v1 v2 v3) v])
    [list [- [* u2 v3] [* u3 v2]] [- [* u3 v1] [* u1 v3]] [- [* u1 v2] [* u2 v1]]])]

[define [subVec a b]
  [map [lambda [x y] [- x y]] a b]
  ]
[define [lengthVec a]
  [sqrt [apply + [map [lambda [x] [* x x]] a]]]]

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

[define [fullAngle v1 v2]
  [letrec [[a1 [vec-angle v1 v2]]
           [rot-axis [normalise [crossprod v1 v2]]]
           ]
    ;[displayln rot-axis]
    [list [* [+ 90  a1] [/ 180 3.1415927]] [list-ref rot-axis 0][list-ref rot-axis 1][list-ref rot-axis 2] ]
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

[define array-height [lambda [an-array]
[vector-ref  [array-shape an-array] 1]
                       ]]

[define array-width [lambda [an-array]
[vector-ref [array-shape an-array] 0]
                       ]]

[define array-depth [lambda [an-array]
[vector-ref [array-shape an-array] 2]
                       ]]


  [define [cartesian-to-weird e]
    ;[printf "In: ~a out: ~a~n" e [vector [third e] [first e] [second e] ]]
[vector [third e] [first e] [second e] ]
    ]

  [define [weird-to-cartesian e]
[list [vector-ref e 1] [vector-ref  e 2] [vector-ref e 1] ]
    ]