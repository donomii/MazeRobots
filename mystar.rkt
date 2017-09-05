#lang racket

;Basic pathfind

;It's half an A* algorithm, it lacks the "open set" or "fringe set" ordered list of possible paths

;Instead, it uses a hash table for the closed set (visited points), and uses the call stack for the possible paths.

;So it doesn't always give the shortest path, but it does run nice and fast, since it doesn't have to maintain a priority queue.

(require (prefix-in arr: math/matrix))
(require math/array)
[require srfi/1]
(define (test-map-1)
  (array-map (lambda (x) [if [> x 1]
                             9001
                             1])
             (arr:matrix [
                          [1 1 1 1 1 1 1 1 1]
                          [1 1 1 1 1 1 2 1 1]
                          [1 1 1 1 2 2 2 1 1]
                          [1 2 1 1 1 1 2 1 1]
                          [1 2 2 2 2 2 2 2 2]
                          [1 2 1 1 1 1 1 1 2]
                          [1 2 1 1 1 1 1 1 1]
                          [1 2 1 1 1 1 1 1 1]
                          [1 1 1 1 1 1 1 1 1]])))

[define [test-map-3]
  (arr:build-matrix 20 20 (λ (x y) [if [> (random 101) 50] 9001 1]))
  ]

(define (test-map-2)
  (array-map (lambda (x) [if [> x 0]
                             9001
                             1])
             
             (arr:matrix [
                          [0 0 0 0]
                          [1 1 1 0]
                          [0 0 0 0]
                          [0 1 1 1]
                          [0 0 0 0]
                          ])))

[define [test-map-4]
  (array-map (lambda (x) [if [equal? x #\*]
                             9001
                             1])
  
             [arr:list->matrix 21 22 [string->list
                                      "
*********************
  *   *   *   *   * *
* * * * * * * * * * *
* * * * * * *   * * *
* * * *** * ***** * *
* * * *   * *     * *
* * * * *** * ***** *
*   * *   * * *   * *
*** * * * * * *** * *
*   *   *   *     * *
* ***************** *
*       * *         *
* ***** * * ***** ***
* *     *   *   * * *
*** * ******* *** * *
*   * *   *     *   *
* *** * * *** * * * *
*   * * *   * * * * *
*** * * *** * * * * *
*   *     *   *   *  
*********************"]
                               ])]


[define closed [make-hash]]

[define [moveTo a b c]
  ;[printf "Moving from ~a to ~a, step ~a~n" a b c]
  [if  [< [* [- a b] [- a b]] c]
       b
       [if [< a b]
           [+ a c]
           [- a c]]
       ]]

[define [lineRec start end]
  [displayln start]
  [if [equal? start end]
      [list end]
      [cons start [lineRec [list [moveTo [car start] [car end] 1] [moveTo [second start] [second end] 1]]end]]
      ]]

[define [basicLine start end]
  [lineRec start end]]

[define [square x] [* x x]]
[define [lineScore scoremap path end]
  ;[printf "linscore ~a: ~a~n" path
  ; [+  [square [- [caar path] [car end]]] [square [- [second [first path]] [second end]]] 
  ;     [fold + 0 [map [lambda [e] [array-ref scoremap [vector [car e] [second e]]]] path]]]]
  
  [+  [square [- [caar path] [car end]]] [square [- [second [first path]] [second end]]] 
      [fold + 0 [map [lambda [e] [array-ref scoremap [vector [car e] [second e]]]] path]]]
  ]

[define [mapScore scoremap path end]
  ;[printf "mapscore ~a: ~a~n" path [+
  ;[sqrt [+  [square [- [caar path] [car end]]] [square [- [second [first path]] [second end]]] ]]
  ;]]
  [+
   [sqrt [+  [square [- [caar path] [car end]]] [square [- [second [first path]] [second end]]] ]]
   ]
  ]

;[basicLine start end]
;[printf "Score: ~a~n" [lineScore [make-map] [basicLine start end] end] ]

[define [improvePath smap path]
  [if [empty? path]
      path
      [if [equal? [car path] [list 5 5]]
          [append [list '[ 10 10] '[11 11]] [improvePath smap [cdr path]]]
          [cons [car path] [improvePath smap [cdr path]]]]
      ]
  ]

;[define [neighbour-list] '[(-1 0) (1 0) (0 -1) (0 1) (-1 -1) (-1 1) (1 -1) (1 1)]]
[define [neighbour-list] '[(-1 0) (1 0) (0 -1) (0 1) ]]

;[improvePath [make-map] [basicLine start end]]
[neighbour-list]

[define [generate-new-paths path neighbours]
  [map [lambda [n]
         [cons [map + n [car path]] path]]
       neighbours]]

[define [doThing smap path start return]
  [hash-set! closed [car path] 1]
  ;[showmap smap path closed][displayln ""]
  
  ;[printf "Score: ~a, path: ~a~n"  [lineScore smap path start] path]
  [if [equal? [car path] start]
      [return path]
      [let [[new-paths [filter [lambda [e]
                                 ;[printf "Position (~a,~a)~n"[caar e] [second [first e]]]
                                 [not
                                  [or
                                   [hash-ref closed [car e] #f]
                                   
                                   [not [< [caar e] [arr:matrix-num-rows smap]]]
                                   [not [< [second [first e]] [arr:matrix-num-cols smap]]]
                                   [< [caar e] 0]
                                   [< [second [first e]] 0]
                                   [> [array-ref smap [vector [caar e] [second [first e]] ]] 9000]
                                   ]]] [generate-new-paths path [neighbour-list]]]]]
   
        [if [empty? new-paths]
            '[]
            [map [lambda [a-path] [doThing smap a-path start return]] [sort  new-paths < #:key [lambda [l] [+ [lineScore smap l start] [/ [mapScore smap l start] 1000000]]]]]
            ]]]]



[define [showmap bmap path closed]
  [let [[amap [mutable-array-copy bmap]]]
    [map [lambda [p] [array-set! amap [apply vector p] -1]] path]
    [if [not [empty? path]]
        [begin
          [array-set! amap [apply vector [car [reverse path]]] -2]
          [array-set! amap [apply vector [car path]] -2]]
        [hash-map closed [lambda [k v] [array-set! amap [apply vector k] -2]] path]
        ]
    [map [lambda [x]
           [map [lambda [y]
                  [begin
                    [if [equal? -2 [array-ref amap [vector x y]]]
                        [display "!"]
                        [begin
                          [when [< [array-ref amap [vector x y]]0]
                            [display "┼"]]
                          [when [> [array-ref amap [vector x y]]9000]
                            [display "*"]]
                          [when [and [<= [array-ref amap [vector x y]]9000] [>= [array-ref amap [vector x y]]0]]
                            [display "."]]]]]]
                [iota [arr:matrix-num-cols amap]
                      ]]
           [displayln ""]]
         [iota [arr:matrix-num-rows amap]]]
    ]]

[define [find-path smap start end]
  [printf "Navigating matrix of size ~ax~a~n" [arr:matrix-num-cols smap] [arr:matrix-num-rows smap]]
  [set! closed [make-hash]]
  [letrec [
           [path [call/cc [lambda [return] [doThing smap [list start] end return]]]]]
    [displayln "calculated path"]
    
    [showmap smap path closed]
    [not [empty? path]]]]

hash-map

;[find-path [array->mutable-array [test-map-2]] '[4 3] '[0 0]]
;[find-path [array->mutable-array [test-map-1]] '[7 7] '[1 1]]
;[find-path [array->mutable-array [test-map-3]] '[19 19] '[0 0]]
;[find-path [array->mutable-array [test-map-4]] '[19 19] '[0 0]]
