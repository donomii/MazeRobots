#lang racket
[require srfi/1]

(require (prefix-in arr: math/array))
[provide scene scene-set! scene-get]

[define scene-data `[
                [mans  . , [apply append [map [lambda [x]
                                  [map [lambda [y]
                                         `[ [,x 0 ,y] []]
                                         ] [iota 1 0 1]]
                                  ] [iota 1 1 1]]]]
                [walls . []]
                [jobs  . []]
                
                ]]

[define [scene] scene-data]

[define [scene-get key]
  ;[displayln scene-data]
  [cdr [assoc key scene-data]]]

[define [del-assoc key a-list] [filter [lambda [e] [not [equal? [car e] key]]] a-list]]

[define [scene-set! key value] [set! scene-data [cons [cons key value] [del-assoc key scene-data]]]]

[scene-set! 'a 'b]

[displayln [scene]]

[scene-set! 'a 'c]

[displayln [scene]]

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
[displayln maze]
[define stringmaze "
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

[map [lambda [x]
       [map [lambda [y]
              [when [> [arr:array-ref maze [vector x y]] 9000]
                [scene-set! 'walls [cons [list x 0 y] [scene-get 'walls]]]]
              ] [iota 20]]
       ] [iota 21]]

[scene-set! 'maze maze]