#lang racket

;Basic pathfind

;It's half an A* algorithm, it lacks the "open set" or "fringe set" ordered list of possible paths

;Instead, it uses a hash table for the closed set (visited points), and uses the call stack for the possible paths.

;So it doesn't always give the shortest path, but it does run nice and fast, since it doesn't have to maintain a priority queue.


; Use: [find-path map start end]

; start, end: lists of two elements, the coordinates of the start and end points
; map: an array of numbers >0, representing the cost of moving through the square.  If the cost is over 9000, that square can never
;      be crossed.  If there is no path from start to end, find-path will return an empty list

;Example:
;
; (build-array 20 20 (λ (x y) [if [> (random 101) 50] 9001 1]))

[provide find-path showmap]
(require math/array)
[require "utility_functions.rkt"]
[require srfi/1]
[require rackunit]
(define (test-map-1)
  (array-map (lambda (x) [if [> x 1]
                             9001
                             1])
             (array [
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
  (build-array 20 20 (λ (x y) [if [> (random 101) 50] 9001 1]))
  ]

(define (test-map-2)
  (array-map (lambda (x) [if [> x 0]
                             9001
                             1])
             
             (list->array '[
                          0 0 0 0
                          1 1 1 0
                          0 0 0 0
                          0 1 1 1
                          0 0 0 0
                          ])))

[define [test-map-4]
  (array-map (lambda (x) [if [equal? x #\*]
                             9001
                             1])
  
             [list->array 21 22 [string->list
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
  
  ;[+  [sqrt [+ [square [- [caar path] [car end]]] [square [- [second [first path]] [second end]]] ]]
      [fold + 0 [map [lambda [e] [array-ref scoremap [cartesian-to-weird e]]] path]]
  ;]
  ]

[define [mapScore scoremap path end]
  ;[printf "mapscore ~a: ~a~n" path [+
[lengthVec [subVec [first path] end]]
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
[define [neighbour-list] '[ (0 -1 0) (-1 0 0) (1 0 0) (0 0 -1) (0 0 1) (0 1 0)]]

;[improvePath [make-map] [basicLine start end]]


[define [generate-new-paths path neighbours]
  [map [lambda [n]
         [cons [map + n [car path]] path]]
       neighbours]]




[define [doThing smap path start return closed]
  [hash-set! closed [car path] 1]
  ;[showmap smap path closed][displayln ""]


  ;[printf "Score: ~a, path: ~a~n"  [lineScore smap path start] path]
  [if [equal? [car path] start ]
      [return path]
      [let [[new-paths [filter [lambda [e]
                                 ;[printf "Position (~a,~a)~n"[caar e] [second [first e]]]
                                 [not
                                  [or
                                   [hash-ref closed [car e] #f]

                                   [out-of-bounds smap [first e]]
                                   ]]] [generate-new-paths path [neighbour-list]]]]]
   
        [if [empty? new-paths]
            '[]
            [map [lambda [a-path] [doThing smap a-path start return closed]] [sort  new-paths < #:key [lambda [l] [+ [lineScore smap l start] [/ [mapScore smap l start] 100]]]]]
            ]]]]



[define [showmap bmap path closed]
  [let [[amap [mutable-array-copy bmap]]]
    [map [lambda [p] [array-set! amap [cartesian-to-weird p] -1]] path]
    [if [not [empty? path]]
        [begin
          [array-set! amap [cartesian-to-weird [car [reverse path]]] -2]
          [array-set! amap [ cartesian-to-weird [car path]] -2]]
        [hash-map closed [lambda [k v] [array-set! amap [cartesian-to-weird k] -2]] path]
        ]
    [map [lambda [x]
           [map [lambda [y]
                  [begin
                    [if [equal? -2 [array-ref amap [vector x y 0]]]
                        [display "!"]
                        [begin
                          [when [< [array-ref amap [vector x y 0]]0]
                            [display "┼"]]
                          [when [> [array-ref amap [vector x y 0]]9000]
                            [display "*"]]
                          [when [and [<= [array-ref amap [vector x y 0]]9000] [>= [array-ref amap [vector x y 0]]0]]
                            [display "."]]]]]]
                [iota [array-width amap]
                      ]]
           [displayln ""]]
         [iota [array-width amap]]]
    ]]

[define [out-of-bounds smap e]
  
                                  [cond
                                   
                                   [[>= [car e] [array-height smap]] "Point too high"]
                                   [[>= [third  e] [array-width smap]] "Width too large"]
                                   [[>= [second  e] [array-depth smap]] "Point too deep"]
                                   [[< [car e] 0] "Height below zero"]
                                   [[< [second  e] 0] "Point too deep"]
                                   [[< [third  e] 0] "Width below zero"]
                                   [[< 9000 [array-ref smap [cartesian-to-weird e]]] [format "Point ~a is inside a wall of value ~a" e [array-ref smap [cartesian-to-weird e]]]]
                                    [else #f]
                                    ]]

[define [find-path smap start end]
  [verbose [format "Navigating matrix of size ~ax~a from ~a to ~a~n" [array-width smap] [array-width smap] start end]]
  [let [[error [or [out-of-bounds smap start] [out-of-bounds smap end]]]]
  [if [or [equal? start end] error]
      [begin
        [printf "Invalid input because ~a~n" error]
        [printmap smap]
        '[]]
  [let [[closed [make-hash]]]
  [letrec [
           [path [call/cc [lambda [return] [doThing smap [list start]  end return closed]]]]] ;reverse for row-column addressing format
    [verbose [format "calculated path"]]
    
    ;[showmap smap path closed]
    ;[not [empty? path]]
    path]]]]]


