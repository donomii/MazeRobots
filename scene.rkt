#lang racket
[require srfi/1]

; A basic(!) example
(require (prefix-in arr: math/array))
(require math/matrix)
[require "utility_functions.rkt"]
[require "mystar.rkt"]
[require "jobs.rkt"]

; All scenes must provide these functions
[provide scene scene-set! scene-get scene-tick]

; The initial data for the scene.  You must provide data for mans, walls, jobs and things, even if they are empty lists
[define scene-data `[
                     [mans  . ,[apply append [map [lambda [x]
                                                    [map [lambda [y]
                                                           `[ [2 0 2] []]
                                                           ] [iota 1 0 1]]
                                                    ] [iota 1 1 1]]]]
                     [walls . []]
                     [jobs  . []] 
                     [things . []]]]



       

; You must provide this to grant access to the scene data
[define [scene] scene-data]

; You must provide this to grant access to the scene data
[define [scene-get key]
  ;[displayln scene-data]
  [cdr [assoc key scene-data]]]



[define [del-assoc key a-list] [filter [lambda [e] [not [equal? [car e] key]]] a-list]]

; You must provide this to grant access to the scene data
[define [scene-set! key value] [set! scene-data [cons [cons key value] [del-assoc key scene-data]]]]





; Tests (make a proper test suit)
;[scene-set! 'a 'b]
;
;[displayln [scene]]
;
;[scene-set! 'a 'c]
;
;[displayln [scene]]

[define level-map [arr:array->mutable-array[arr:build-array [vector 101 101 3] [lambda [x] 1]]]]
[scene-set! 'level-map level-map]

[define maze
  [arr:array->mutable-array (arr:array-map (lambda (x) [if [equal? x #\*]
                                                           9001
                                                           1])
  
                                           [arr:list->array [vector 21 21 1] [string->list [string-replace
                                                                                            "*********************
  *   *   *   *   *  
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
*   *     *   *   * *
*********************"
                                                                                            [format "~n"] ""]]
                                                            ])]]

[define level2
  [arr:array->mutable-array (arr:array-map (lambda (x) [if [equal? x #\*]
                                                           9001
                                                           1])
  
                                           [arr:list->array [vector 21 21 1] [string->list [string-replace
                                                                                            "*********************
  *   *   *   *   *  
* * * * * * * * * * *
* * * * * * *   * * *
* * * *** * ***** * *
* * * *   * *     * *
* * * * *** * ***** *
*   * *   * * *   * *
*** * * * * * *** * *
*   *   *   *     * *
*                   *
*       * *         *
* ***** * * ***** ***
* *     *   *   * * *
*** * ******* *** * *
*   * *   *     *   *
* *** * * *** * * * *
*   * * *   * * * * *
*** * * *** * * * * *
*   *     *   *   * *
*********************"
                                                                                            [format "~n"] ""]]
                                                            ])]]






[define [add-to-level a-map offset]
  ;[printf "Given array: ~a,~a,~a~n" [array-width a-map] [array-height a-map] [array-depth a-map]]
  [map [lambda [d]
         [map [lambda [w]
                [map [lambda [h]
                       [when [> [arr:array-ref a-map [vector w h d]] 9000]
                         [let [[point [addVec offset [weird-to-cartesian [vector w h d]]]]]
                           ;[printf "Point: ~a~n" point]
                           [scene-set! 'walls [cons point  [scene-get 'walls]]]   ;for now, walls are just a list of the points they occur at
                           [arr:array-set! [scene-get 'level-map] [cartesian-to-weird point] 9001]]
                         ]] [iota  [array-height a-map]]]
                ] [iota  [array-width a-map]]]]
       [iota  [array-depth a-map]]]
  ]

[begin 
  [add-to-level maze [list 1 0 1]]
  [add-to-level level2 [list 1 1 1]]

  [scene-set! 'colours  [map [lambda [r]
                               ;`[,[random] ,[random] ,[random]  1.0]
                               [list [/ r 25] 1.0 1.0 1.0]
                               ] [iota 25]]]

  [scene-set! 'things  [apply append [map [lambda [x]
                                                     [filter [lambda [x] x] [map [lambda [y]
                                                            [let [[x [+ 2 [random 18]]]
                                                                  [y [+ 2 [random 20]]]]
                                                                  [if [> 9000 [arr:array-ref [scene-get 'level-map] [cartesian-to-weird [list x 0 y]]]]
                                                            `[,x 0 ,y ]
                                                            #f]]
                                                            ] [iota 3 -20 8]]]]
                                                   [iota 3 -20 8]]]]

  [scene-set! 'jobs [map [lambda [t p] [list 'fetch t [list 23 0 p ]]] [scene-get 'things] [iota [length [scene-get 'things]]]]]

  #t]

[define [scene-tick]
  [verbose [format "Starting tick~n"]]
  [scene-set! 'mans (map (lambda (v  colour i)
                           
                    
                           [letrec [[old-jobqueue [second v]]
                                    [position [first v]]]
                             ;[printf "Jobqueue: ~a~n"  jobqueue]
                             ;If the robot's jobqueue is not empty
                             [if [not [empty? old-jobqueue]]
                                 ;pick up the first job on the queue
                                 [letrec [
                                          ;expand the first job on the robot's jobqueue
                                          [jobqueue [append [expand-job [car old-jobqueue] position [scene-get 'level-map]] [cdr old-jobqueue]]]
                                          ;select the top job
                                          [thisjob [car jobqueue]]
                                          ;every job has a target
                                          [target [second thisjob]]]
                                   [verbose [when [not [equal? 'moveTo [car thisjob]]]
                                       [format "Expansion complete, jobqueue is now ~a~n" jobqueue]]]
                                   [verbose [format "position: ~a, target: ~a, colour: ~a~n" [first v] target colour]]
                                   [scene-set! 'selected i]
                                   [verbose [format "Doing: ~a~n" thisjob]]
                                   [case [car thisjob]
                                     ['pickUp
                                      [begin
                                        [scene-set! 'things [remove-from-list [second thisjob] [scene-get 'things]]]
                                        [list position [cdr jobqueue]]]]
                                     ['drop
                                      [begin
                                        [scene-set! 'things [cons target [scene-get 'things]]]
                                        [list position [cdr jobqueue]]]]
                                
                                     [else [begin
                                             [default-jobs [list position  jobqueue] scene scene-get scene-set!]]]]]
                                
                        ;If robot job queue is empty
                                 [default-jobs [list [car v]
                                       '[]
                                       ]  scene scene-get scene-set!]
                                 ]
                             ])
                         [scene-get 'mans]  [scene-get 'colours] [iota [length [scene-get 'mans]]])]]