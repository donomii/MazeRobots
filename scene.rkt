#lang racket
[require srfi/1]

; A basic(!) example
(require (prefix-in arr: math/array))
[require "utility_functions.rkt"]
[require "mystar.rkt"]
[require "jobs.rkt"]

; All scenes must provide these functions
[provide scene scene-set! scene-get scene-tick]

; The initial data for the scene.  You must provide data for mans, walls, jobs and things, even if they are empty lists
[define scene-data `[
                [mans  . ,[apply append [map [lambda [x]
                                  [map [lambda [y]
                                         `[ [,x 0 ,y] []]
                                         ] [iota 1 0 1]]
                                  ] [iota 1 1 1]]]]
                [walls . []]
                [jobs  . ,[list [list 'pathTo [list 18 0 19]]
     ]]
                [things . ,[apply append [map [lambda [x]
                                         [map [lambda [y]
                                                `[,[- [random 10] 5] 0 ,[- [random 10] 5]]
                                                ] [iota 2 -20 8]]
                                         ] [iota 4 -20 8]]]]
  ]]



[define [update-things]
  [let [[newthing `[,[- [random 10] 5] 0 ,[- [random 10] 5]]]
        [destination [list 0.0 0.0 0.0]]]
    [scene-set! 'walls [cons  newthing [scene-get 'walls]]]
    [scene-set! 'jobs  [cons
                        `[fetch ,newthing ,destination]
                        [scene-get 'jobs] ]]
    ]
  [sleep 10]
  [update-things]
  ]
              

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

[define maze
  [arr:array->mutable-array (arr:array-map (lambda (x) [if [equal? x #\*]
                                 9001
                                 1])
  
                 [arr:list->array [vector 21 22] [string->list
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
                                  ])]]


[map [lambda [x]
       [map [lambda [y]
              [when [> [arr:array-ref maze [vector x y]] 9000]
                [scene-set! 'walls [cons [list x 0 y] [scene-get 'walls]]]]
              ] [iota 20]]
       ] [iota 21]]

[scene-set! 'maze maze]

[scene-set! 'colours  [map [lambda [r]
                        ;`[,[random] ,[random] ,[random]  1.0]
                        [list [/ r 25] 1.0 1.0 1.0]
                        ] [iota 25]]]



[define [scene-tick]
  ;[printf "Starting tick~n"]
  [scene-set! 'mans (map (lambda (v  colour i)
                      ;[printf "~a, ~a, ~a~n" v target colour]
                    
                      [letrec [[jobqueue [second v]]
                               [position [first v]]]
                        ;[printf "Jobqueue: ~a~n"  jobqueue]
                        [if [not [empty? jobqueue]]
                            [letrec [[thisjob [car jobqueue]]
                                     [target [second thisjob]]]
                              [scene-set! 'selected i]
                              [case [car thisjob]
                                
                                ['pickUp
                                 [begin
                                   ;[set! boxes [remove-from-list [second thisjob] boxes]] change to things, when we add things
                                   [list [first v] [cdr jobqueue]]]]
                                ['drop
                                 [begin
                                   ;[set! boxes [cons [second thisjob] boxes]] change to things, when we add things
                                   [list [first v] [cdr jobqueue]]]]
                                
                                [else [begin
                                        [default-jobs v scene scene-get scene-set!]]]]]
                                
                        
                            [list [car v]
                                  [if [not [empty? [scene-get 'jobs]]]
                                      [letrec [[jobs [scene-get 'jobs]]
                                            [newjob [car jobs]]]
                                        [printf "pending-jobs: ~a~n" jobs]
                                        [scene-set! 'jobs [cdr jobs]]
                                        [printf "pending-jobs: ~a~n" [scene-get 'jobs]]
                                
                                        [printf "2 Moving to new job ~a~n"  newjob]
                                        [expand-job newjob position [scene-get 'maze]]]
                                      '[]]
                                  ]
                            ]
                        ])
                    [scene-get 'mans]  [scene-get 'colours] [iota [length [scene-get 'mans]]])]]