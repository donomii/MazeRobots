#lang racket

[provide expand-job]
[require "mystar.rkt"]

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