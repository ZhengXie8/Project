;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname tubes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; **********************************************
;; Zheng Xie (20994611)
;; CS 135 Fall 2022
;; Assignment 10, Problem 1
;; **********************************************
;;


(require "lib-tubes.rkt")

;; A Game is (make-game Nat Nat (listof (listof Sym)))
(define-struct game (tubesize maxcolours tubes))

;;; Constants

(define emptygame
  (make-game 0 5
             (list empty empty empty empty empty)))

(define emptygame2
  (make-game 10 3 empty))

(define emptygame3
  (make-game 10 3 (list empty empty)))

(define smallgame1
  (make-game 2 2
             (list (list 'blue 'red)
                   (list 'blue 'red)
                   (list))))

(define smallgame2
  (make-game 2 3
             (list (list 'blue 'red)
                   (list 'blue 'red)
                   (list))))

(define smallinvalidgame1
  (make-game 2 1
             (list (list 'blue 'red)
                   (list 'blue 'red)
                   (list))))


(define smallinvalidgame2
  (make-game 2 2
             (list (list 'blue 'red)
                   (list 'blue 'blue)
                   (list))))

(define smallinvalidgame3
  (make-game 2 2
             (list (list 'blue 'red 'blue)
                   (list 'red)
                   (list))))


(define smallgamefinal
  (make-game 2 2
             (list (list)
                   (list 'blue 'blue)
                   (list 'red 'red))))


(define mediumgame
  (make-game 2 3
             (list (list 'blue 'red)
                   (list 'red 'yellow)
                   (list 'yellow 'blue)
                   (list))))

(define mediumgamestuck
  (make-game 2 3
             (list (list 'blue 'red)
                   (list 'red 'yellow)
                   (list 'yellow 'blue)
                   )))

(define largergame
  (make-game 3 3
             (list (list 'blue 'red 'red)
                   (list 'yellow 'blue 'yellow)
                   (list 'red 'yellow 'blue)
                   (list))))

(define biggame
  (make-game 5 3
             (list (list 'blue 'blue 'red 'red 'yellow)
                   (list 'red 'red 'yellow 'blue 'red)
                   (list 'yellow 'blue 'blue 'yellow 'yellow)
                   (list)
                   (list))))

(define biggame2
  (make-game 5 3
             (list (list 'yellow 'blue 'blue 'yellow 'yellow)
                   (list 'red 'red 'yellow 'blue 'red)
                   (list)
                   (list 'blue 'blue 'red 'red 'yellow)
                   (list))))

(define biggamesolve
  (make-game 5 3
             (list (list 'blue 'blue 'blue 'blue 'blue)
                   (list 'red 'red 'red 'red 'red)
                   (list 'yellow 'yellow 'yellow 'yellow 'yellow)
                   (list)
                   (list))))

(define hugegame
  (make-game 4 9
             (list (list 'purple 'pink 'yellow 'blue)
                   (list 'blue 'green 'purple 'white)
                   (list 'orange 'yellow 'black 'blue)
                   (list 'white 'orange 'orange 'pink)
                   (list 'pink 'red 'red 'black)
                   (list 'yellow 'green 'orange 'blue)
                   (list 'white 'purple 'red 'yellow)
                   (list 'green 'red 'green 'black)
                   (list 'purple 'black 'white 'pink)
                   (list)
                   (list))))

;; a
;; (check-colour? size num los) consumes two natural numbers, size and num,
;; and a list of symbols los, and produces true if each symbol in the
;; list appears exactly size times and if there are at most num different
;; symbols; otherwise, check-colour? will produce false.

;; Example
(check-expect (check-colour? 3 2 (list 'yellow 'green 'red)) false)


;; check-colour?: Nat Nat (listof Sym) -> Bool
(define (check-colour? size num los)
    (cond [(empty? los) true]
          [(zero? num) false]
          [else (local [;; (delete-colour size colour los) produces false if the
                        ;; appearance of colour is more than the size, or else
                        ;; it returns the los with all the elements that's symbol=
                        ;; to colour removed
                        ;; delete-colour: Nat Sym (listof Sym) -> (Anyof false (listof Sym))
                        (define (delete-colour size colour los)
                          (cond [(and (empty? los) (zero? size)) empty]
                                [(empty? los) false]
                                [(not (symbol=? (first los) colour))
                                 (local [(define delete-rest (delete-colour size colour (rest los)))]
                                   (cond [(false? delete-rest) false]
                                         [else (cons (first los) delete-rest)]))]
                                [(zero? size) false]
                                [else (delete-colour (sub1 size) colour (rest los))]))
                        (define deleted-list (delete-colour size (first los) los))]
                  (cond [(false? deleted-list) false]
                        [else (check-colour? size (sub1 num) deleted-list)]))]))

;; Tests
(check-expect (check-colour? 0 0 empty) true)
(check-expect (check-colour? 0 1 (list 'yellow)) false)
(check-expect (check-colour? 1 0 (list 'green)) false)
(check-expect (check-colour? 3 3 (list 'yellow 'green 'yellow 'blue 'blue 'yellow
                                       'green 'green 'blue)) true)
(check-expect (check-colour? 3 2 (list 'red 'red 'red 'yellow 'yellow 'yellow
                                        'blue 'blue 'blue)) false)
(check-expect (check-colour? 4 2 (list 'blue 'blue 'blue 'blue)) true)

;; b
;; (valid-game? gm) consumes a Game, gm and produces true if gm is a valid game,
;; and false otherwise.

;; Example
(check-expect (valid-game? emptygame) true)

;; valid-game?: Game -> Bool
(define (valid-game? gm)
  (cond [(foldr (lambda (first rest)
                  (and (<= (length first) (game-tubesize gm)) rest))
                true
                (game-tubes gm))
         (check-colour? (game-tubesize gm)
                       (game-maxcolours gm)
                       (foldr (lambda (first rest) (append first rest))
                              empty
                              (game-tubes gm)))]
        [else false]))

;; Tests
(check-expect (valid-game? smallgamefinal) true)
(check-expect (valid-game? mediumgame) true)
(check-expect (valid-game? emptygame2) true)
(check-expect (valid-game? smallinvalidgame1) false)
(check-expect (valid-game? smallinvalidgame2) false)
(check-expect (valid-game? smallinvalidgame3) false)
(check-expect (valid-game? hugegame) true)
(check-expect (valid-game? biggame) true)

;; c
;; (remove-completed gm) consumes a Game, gm, and produces a Game which
;; is similar to gm but has any completed tubes removed

;; Example
(check-expect (remove-completed smallgame1) smallgame1)

;; remove-completed: Game -> Game
(define (remove-completed gm)
  (local [;; (tube-complete? tube) consumes a single tube and determines if it's
          ;; completed
          ;; tube-complete?: (listof Sym) -> Bool
          (define (tube-complete? tube)
            (cond [(not (= (length tube) (game-tubesize gm))) false]
                  [(empty? tube) false]
                  [else (local [;; (tube-complete/ne tube) consumes a tube that's
                                ;; not empty, also having the amount of symbols
                                ;; matching the length of the tube, and determines
                                ;; if all symbols of the tube are same to the symbol
                                ;; given
                                ;; tube-complete/ne: (listof Sym) Sym-> Bool
                                ;; reqruies: tube is not empty
                                ;;           (length tube) = the required tubesize
                                (define (tube-complete/ne tube sym)
                                  (cond [(empty? tube) true]
                                        [else (and (symbol=? (first tube) sym)
                                                   (tube-complete/ne (rest tube) sym))]))]
                          (tube-complete/ne tube (first tube)))]))
          ;; (remove-tubes list-tubes) consumes a list of tubes and remove the tubes
          ;; that are completed
          ;; remove-tubes: (listof (listof Sym)) -> (listof (listof Sym))
          (define (remove-tubes list-tubes)
            (foldr (lambda (first rest)
                     (cond [(tube-complete? first) rest]
                           [else (cons first rest)]))
                   empty
                   list-tubes))
          (define tubes-removed (remove-tubes (game-tubes gm)))]
    (make-game (game-tubesize gm)
               (local ;; (count-colours list-tubes) consumes a list of tubes and
                 ;; return the amount of colours
                 ;; count-colours: (listof (listof Sym)) -> Nat
                 [(define (count-colours list-tubes)
                         (cond [(empty? list-tubes) 0]
                               [else (add1
                                      (count-colours (filter
                                                      (lambda (x)
                                                        (not (symbol=? (first list-tubes) x)))
                                                      list-tubes)))]))]
                 (count-colours (foldr (lambda (x y) (append x y))
                                       empty
                                       tubes-removed)))
               tubes-removed)))

;; Tests
(check-expect (remove-completed smallgame1) smallgame1)
(check-expect (remove-completed biggamesolve) (make-game 5 0
             (list (list)
                   (list))))
(check-expect (remove-completed
               (make-game 2 3
                          (list (list 'blue 'red)
                                (list 'red 'blue)
                                (list 'yellow 'yellow))))
              (make-game 2 2
                          (list (list 'blue 'red)
                                (list 'red 'blue))))
(check-expect (remove-completed emptygame2)
              (make-game 10 0 empty))
(check-expect (remove-completed (make-game 3 3 (list
                                                (list 'yellow 'blue 'yellow)
                                                (list 'red 'red 'red)
                                                (list 'blue 'yellow 'blue)       
                                                (list))))
              (make-game 3 2 (list
                              (list 'yellow 'blue 'yellow)
                              (list 'blue 'yellow 'blue)       
                              (list))))
(check-expect (remove-completed largergame) largergame)


;; d
;; (finished-game? gm) consumes a Game, gm, and produces true if the game is finished
;; and false otherwise.

;; Example
(check-expect (finished-game? emptygame) true)

;; finished-game?: Game -> Bool
(define (finished-game? gm)
  (foldr (lambda (first rest) (and (cond [(empty? first) true]
                                         [else false])
                                   rest))
         true
         (game-tubes (remove-completed gm))))

;; Tests
(check-expect (finished-game? emptygame2) true)
(check-expect (finished-game? emptygame3) true)
(check-expect (finished-game? smallgame1) false)
(check-expect (finished-game? smallgame2) false)
(check-expect (finished-game? smallgamefinal) true)
(check-expect (finished-game? mediumgame) false)
(check-expect (finished-game? largergame) false)
(check-expect (finished-game? biggame) false)
(check-expect (finished-game? biggamesolve) true)
(check-expect (finished-game? hugegame) false)

;; e
;;(num-blocks llos) consumes a list of lists of symbols, and produces the number of
;; "blocks" contained in llos.

;; Examples
(check-expect (num-blocks empty) 0)

;; num-blocks: (listof (listof Sym)) -> Nat
(define (num-blocks llos)
  (local [;; (num-blocks/list los first) consumes a list of symbols without
          ;; the first one, and the first one separated as first, and count the
          ;; number of "blocks" in the los
          ;; num-blocks/list: (listof Sym) Sym -> Nat
          (define (num-blocks/list los sym)
            (cond [(empty? los) 1]
                  [(symbol=? (first los) sym)
                   (num-blocks/list (rest los) sym)]
                  [else (add1 (num-blocks/list (rest los) (first los)))]
                  ))]
    (foldr (lambda (x y) (+ (cond [(empty? x) 0]
                                         [else (num-blocks/list (rest x) (first x))])
                                   y))
           0
           llos)))

;; Tests
(check-expect (num-blocks (list empty empty empty empty empty)) 0)
(check-expect (num-blocks (list empty empty)) 0)
(check-expect (num-blocks (list (list 'blue 'red)
                                (list 'blue 'red)
                                (list))) 4)
(check-expect (num-blocks   (list (list)
                                  (list 'blue 'blue)
                                  (list 'red 'red))) 2)
(check-expect (num-blocks (list (list 'blue 'red)
                                (list 'red 'yellow)
                                (list 'yellow 'blue)
                                (list))) 6)
(check-expect (num-blocks (list (list 'blue 'red 'red)
                                (list 'yellow 'blue 'yellow)
                                (list 'red 'yellow 'blue)
                                (list))) 8)
(check-expect (num-blocks (list (list 'blue 'blue 'red 'red 'yellow)
                                (list 'red 'red 'yellow 'blue 'red)
                                (list 'yellow 'blue 'blue 'yellow 'yellow)
                                (list)
                                (list))) 10)

;; f
;; (equiv-game? gm1 gm2) consumes two Games and produces true if gm1 and gm2 are
;; equivalent, and false otherwise

;; Examples
(check-expect (equiv-game? emptygame emptygame2) false)

;; equiv-game?: Game Game -> Bool
(define (equiv-game? gm1 gm2)
  (local [;; (compare-tube tube1 tube2) consumes two tubes and determine if they are identical
          ;; compare-tube: (listof Sym) (listof Sym) -> Bool
          (define (compare-tube tube1 tube2)
            (cond [(not (= (length tube1) (length tube2))) false]
                  [(empty? tube1) true]
                  [(not (symbol=? (first tube1) (first tube2)))
                   false]
                   [else (compare-tube (rest tube1) (rest tube2))]))
          ;; (compare-list tube1 list-tube2) consumes a tube and a list of tubes and determine
          ;; if there's a tube identical to tube1 in the list-tube2. If there is, then the list-tube2
          ;; with the first tube identical to tube1 removed is returned, or else return false
          ;; compare-list: (listof Sym) (listof (listof Sym)) -> (Anyof (listof (listof Sym)) False)
           (define (compare-list tube1 list-tube2)
             (cond [(empty? list-tube2) false]
                   [(compare-tube tube1 (first list-tube2))
                    (rest list-tube2)]
                   [else (local [(define compare-rest (compare-list tube1 (rest list-tube2)))]
                           (cond [(false? compare-rest) false]
                                 [else (cons (first list-tube2) compare-rest)]))]))
           ;; (compare list-tube1 list-tube2) consumes two list of tubes and determine if they
           ;; have identical tubes, maybe in different ordering.
           ;; comapre: (listof (listof Sym)) (listof (listof Sym)) -> Bool
           (define (compare list-tube1 list-tube2)
             (cond [(empty? list-tube1) true]
                   [else (local [(define compare-first (compare-list (first list-tube1)
                                                                     list-tube2))]
                           (cond [(false? compare-first) false]
                                 [else (compare (rest list-tube1) compare-first)]))]))]
    (and (= (game-maxcolours gm1) (game-maxcolours gm2))
         (= (game-tubesize gm1) (game-tubesize gm2))
         (= (length (game-tubes gm1)) (length (game-tubes gm2)))
         (compare (game-tubes gm1) (game-tubes gm2)))))

;; Tests
(check-expect (equiv-game? emptygame2 emptygame3) false)
(check-expect (equiv-game? smallgame1 smallgame2) false)
(check-expect (equiv-game? smallgame1 smallgamefinal) false)
(check-expect (equiv-game? biggame biggame2) true)
(check-expect (equiv-game? biggame biggame) true)
(check-expect (equiv-game? mediumgamestuck mediumgame) false)
(check-expect (equiv-game? biggame biggamesolve) false)

;; g
;; (all-equiv? log1 log2) consume two lists of Games, log1 and log2, and produces
;; true if every game in log1 has one equivalent game in log2, else produces false.

;; Examples
(check-expect (all-equiv? empty empty) true)

;; all-equiv?: (listof Game) (listof Game) -> Bool
(define (all-equiv? log1 log2)
  (cond [(not (= (length log1) (length log2))) false]
        [(empty? log1) true]
        [else (local [;; (equiv-first gm log2) consumes a game and a list of games,
                      ;; determines if there's an identical game in the list of games.
                      ;; equiv-first: Game (listof Game) -> Bool
                      (define (equiv-first gm log2)
                        (cond [(empty? log2) false]
                              [(equiv-game? gm (first log2)) (rest log2)]
                              [else (local [(define others (equiv-first gm (rest log2)))]
                                      (cond [(false? others) false]
                                            [else (cons (first log2) others)]))]))
                      (define first? (equiv-first (first log1) log2))]
                (cond [(false? first?) false]
                      [else (all-equiv? (rest log1) first?)]))]))

;; Tests
(check-expect (all-equiv? (list emptygame2) empty) false)
(check-expect (all-equiv? empty (list emptygame2)) false)
(check-expect (all-equiv? (list biggame smallgame1) (list smallgame1 biggame2)) true)
(check-expect (all-equiv? (list smallgame1) (list smallgame2)) false)
(check-expect (all-equiv? (list biggame mediumgame) (list smallgame1 smallgame2 emptygame3)) false)


;; h
;; (next-games gm) consumes a Game, and produces a list of Games that can happen
;; by moving one ball from gm.

;; Example
(define (test-next-games gm expected) (all-equiv? (next-games gm) expected))
(check-expect (test-next-games smallgame1 (list (make-game 2 2
                                                           (list (list 'blue 'red)
                                                                 (list 'red)
                                                                 (list 'blue)))
                                                (make-game 2 2
                                                           (list (list 'red)
                                                                 (list 'blue 'red)
                                                                 (list 'blue))))) true)

;; next-games: Game -> (listof Game)
(define (next-games gm)
  (local [(define game (game-tubes gm))
          ;; (insert-list-tube list-tube size origin) determines all the possible
          ;; shape of the game by inserting the first element of any tube into
          ;; all the other non-empty tubes
          ;; insert-list-tube: (listof (listof Sym)) Nat Nat -> (listof (listof (listof Sym)))
          (define (insert-list-tube lst-tube size origin)
            (cond [(empty? lst-tube) empty]
                  [(empty? (first lst-tube))
                   (insert-list-tube (rest lst-tube) size (add1 origin))]
                  [else (append (insert-tube (first lst-tube) game size origin 0)
                                (insert-list-tube (rest lst-tube) size (add1 origin)))]))
          ;;(insert-tube tube list-tubes size origin dest) determines all the possible
          ;; ways of inserting the first element of a tube into any of the other non-empty
          ;; tubes
          ;; insert-tube: (listof Sym) (listof (listof Sym)) Nat Nat Nat ->
          ;;              (listof (listof (listof Sym)))
          (define (insert-tube tube list-tubes size origin dest)
            (cond [(empty? list-tubes) empty]
                  [(=  origin dest)
                   (insert-tube tube (rest list-tubes) size origin (add1 dest))]
                  [(= (length (first list-tubes)) size)
                   (insert-tube tube (rest list-tubes) size origin (add1 dest))]
                  [else (cons (insert (first tube) game (length game) origin dest 0)
                              (insert-tube tube (rest list-tubes) size origin (add1 dest)))]))
          ;; (insert sym list-tubes len origin dest acc) determines the shape generated by inserting
          ;; the sym into the tube with the number corresponding to dest, while removing the first
          ;; element from the tube with number corresponding to origin.
          ;; insert: Sym (listof (listof Sym)) Nat Nat Nat Nat -> (listof (listof Sym))
          (define (insert sym list-tubes len origin dest acc)
            (cond [(= acc origin)
                   (cons (rest (first list-tubes))
                         (insert sym (rest list-tubes) len origin dest (add1 acc)))]
                  [(= acc len) empty]
                  [(= dest acc)
                   (cons (cons sym (first list-tubes))
                         (insert sym (rest list-tubes) len origin dest (add1 acc)))]
                  [else (cons (first list-tubes)
                              (insert sym (rest list-tubes) len origin dest (add1 acc)))]))
          (define list-possible (insert-list-tube (game-tubes gm)
                                                  (game-tubesize gm)
                                                  0))
          ;; (make-list lst) consumes a list of possible shapes for the game and produces a list of
          ;; games corresponding the the shapes
          ;; make-list: (listof (listof (listof Sym))) -> (listof Game)
          (define (make-list lst)
            (cond [(empty? lst) empty]
                  [else (cons (make-game (game-tubesize gm)
                                         (game-maxcolours gm)
                                         (first lst))
                              (make-list (rest lst)))]))]
    (make-list list-possible)
    ))


(check-expect (test-next-games emptygame (list smallgame1)) false)
(check-expect (test-next-games largergame (list
                                           (make-game
                                            3
                                            3
                                            (list
                                             (list 'red 'red)
                                             (list 'yellow 'blue 'yellow)
                                             (list 'red 'yellow 'blue)
                                             (list 'blue)))
                                           (make-game
                                            3
                                            3
                                            (list
                                             (list 'blue 'red 'red)
                                             (list 'blue 'yellow)
                                             (list 'red 'yellow 'blue)
                                             (list 'yellow)))
                                           (make-game
                                            3
                                            3
                                            (list
                                             (list 'blue 'red 'red)
                                             (list 'yellow 'blue 'yellow)
                                             (list 'yellow 'blue)
                                             (list 'red))))) true)




;;;;;

;; (solve gm draw-option) determines if the game gm is solveable,
;; and will also draw each possible move depending on the draw-option

;; Examples:
;; students should provide some here, or just in tests

;; solve: Game (anyof 'off 'norm 'slow 'fast) -> Bool

(define (solve gm draw-option)
  (local
    [(define setup (puzzle-setup gm draw-option))                
     (define (solve-helper to-visit visited)
       (cond
         [(empty? to-visit) false]
         [else
          (local
            [(define draw (draw-board (first to-visit) draw-option))] 
            (cond
              [(finished-game? (first to-visit)) true]
              [(member? (first to-visit) visited)
               (solve-helper (rest to-visit) visited)]
              [else
               (local [(define nbrs (next-games (first to-visit)))
                       (define new (filter (lambda (x) (not (member? x visited))) nbrs))
                       (define new-to-visit (append new (rest to-visit)))
                       (define new-visited (cons (first to-visit) visited))]
                 (solve-helper new-to-visit new-visited))]))]))]
    (solve-helper (list gm) empty)))

;; Test cases that can be uncommented as the solution is completed

;(check-expect (solve smallgame1 'slow) true)
;(check-expect (solve mediumgamestuck 'slow) false)
;(check-expect (solve mediumgame 'slow) true)
;(check-expect (solve biggame 'off) true)

;; Below is the format for testing and timing the solution:
;; be sure to remove any other check-expects when measuring your timing

;(check-expect (time (solve mediumgame 'off)) true)
;(check-expect (time (solve largergame 'off)) true)
;(check-expect (time (solve biggame 'off)) true)
;(check-expect (time (solve monstergame 'off)) true)