#lang racket

[provide expand-job default-jobs]
[require "mystar.rkt"]
[require "utility_functions.rkt"]

[define [expand-job a-job current-loc a-map]
  [printf "Expanding: ~a~n" a-job]
  [case [car a-job]
    ['fetch [let [[target [second a-job]][destination [third a-job]]]
              [list
               [list 'pathTo target]
               [list 'pickUp target]
               [list 'pathTo destination]
               [list 'drop target]
               ]]]
    ['pathTo [begin
               [let [[path  [find-path a-map [list [first current-loc] [third current-loc]] [list [first [second a-job]] [third [second a-job]]]]]]
                 [showmap a-map path [make-hash]]
               [printf "Expanded ~a into ~a~n" a-job path]
                 [map [lambda [p] `[moveTo ,[list [first p] 0 [second p]]]] [reverse path]]]]]
    [else [list a-job]]
    ]
  ]

[define [default-jobs v scene scene-get scene-set!]
  
                      ;[printf "~a, ~a, ~a~n" v target colour]
                    
                      [letrec [[jobqueue [second v]]
                               [position [first v]]]
                        ;[printf "Jobqueue: ~a~n"  jobqueue]
                        [if [not [empty? jobqueue]]
                            [letrec [[thisjob [car jobqueue]]
                                     [target [second thisjob]]]
                              
                              [case [car thisjob]
                                ['moveTo
                                 [if [equal? position [second thisjob]]
                                     [let [[newjobs  [cdr jobqueue]]]
                                       ;[set! jobs [replace-in-list [car jobs] newjob jobs]]
                                       [printf "1 Moving to new job ~a~n" [if [empty? newjobs] "none" [car newjobs]] ]
                                       [list [first v]
                                             newjobs]]
                                     [list [map [lambda[e t] [moveTo e t 0.01]] position [second thisjob]]
                                           jobqueue]]]
                                ['pathTo
                                 [begin
                                   [printf "PathTo - position: ~a pathTo: ~a~n" position target]
                                   [if [equal? target position]
                                       [begin
                                         [printf "Reached pathTo goal at ~a, moving to next job~n" target]
                                         [list [first v] [cdr jobqueue]]]
                                       [letrec [[amap [build-map [scene-get 'mans] [scene-set! 'walls]]]
                                                [path [reverse [find-path amap [map [lambda [e] [+ 50 e]] [map round [list [first position] [third position]]]] [map [lambda [e] [+ 50]] [map round [list [first target] [third target]]]]]]]]
                                         [let [
                                               [firstStep [if [> [length path] 1]
                                                              [second path]
                                                              [first path]]]
                                               ;[waypoint [car path]]
                                               ]
                                           [printf "From: ~a to: ~a~n" [map round position] [map round target]]
                                           [printf "path ~a~n" path]
                                           [showmap amap path [make-hash]]
                                           [list [first v] [cons `[moveTo ,[list [- [first firstStep] 50] 0 [- [second firstStep] 50]]] jobqueue]]
                                           ]]

                                       ]]]
                                [else [begin
                                        ;
                                        [printf "I don't know how to do job: ~a~n" [car thisjob]]
                                        v]]]]
                                
                        
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
                        ]
                    
 ]