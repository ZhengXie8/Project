;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname tubes-bonus) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; **********************************************
;; Zheng Xie
;; CS 135 Fall 2022
;; Assignment 10, Bonus
;; **********************************************
;;


(require "lib-tubes.rkt")

;; A Game is (make-game Nat Nat (listof (listof Sym)))
(define-struct game (tubesize maxcolours tubes))

;;; Constants
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
(define hugegame2
  (make-game 4 9
             (list (list 'purple 'pink 'yellow 'blue)
                   (list 'blue 'green 'purple 'white)
                   (list 'white 'yellow 'black 'blue)
                   (list 'pink 'orange 'red 'pink)
                   (list 'purple 'red 'orange 'black)
                   (list 'yellow 'green 'orange 'blue)
                   (list 'white 'purple 'red 'yellow)
                   (list 'green 'red 'green 'black)
                   (list 'black 'white 'pink)
                   (list 'orange)
                   (list ))
             ))
(define mediumgamestuck
  (make-game 2 3
             (list (list 'blue 'red)
                   (list 'red 'yellow)
                   (list 'yellow 'blue)
                   )))

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
(define (finished-game? gm)
  (foldr (lambda (first rest) (and (cond [(empty? first) true]
                                         [else false])
                                   rest))
         true
         (game-tubes (remove-completed gm))))

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

(define (next-games gm)
  (local [;; (change-tubes list-tubes size origin) determines all the possible ways
          ;; that a new list-tubes can be made by removing a ball from the top
          ;; of one tube and add it to another (respecting the size).
          ;; change-tubes: (listof (listof Sym)) Nat -> (listof (listof (listof Sym)))
          (define game (game-tubes gm))
          (define (insert-list-tube lst-tube size origin)
            (cond [(empty? lst-tube) empty]
                  [(empty? (first lst-tube))
                   (insert-list-tube (rest lst-tube) size (add1 origin))]
                  [(and (= (length (first lst-tube)) (game-tubesize gm))
                        (tube-complete/ne (first lst-tube) (first (first lst-tube))))
                   (insert-list-tube (rest lst-tube) size (add1 origin))]
                  [else (append (insert-tube (first lst-tube) game size origin 0)
                                (insert-list-tube (rest lst-tube) size (add1 origin)))]))
          (define (insert-tube tube list-tubes size origin dest)
            (cond [(empty? list-tubes) empty]
                  [(=  origin dest)
                   (insert-tube tube (rest list-tubes) size origin (add1 dest))]
                  [(= (length (first list-tubes)) size)
                   (insert-tube tube (rest list-tubes) size origin (add1 dest))]
                  [else (cons (insert (first tube) game (length game) origin dest 0)
                              (insert-tube tube (rest list-tubes) size origin (add1 dest)))]))
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
          (define (make-list lst)
            (cond [(empty? lst) empty]
                  [else (cons (make-game (game-tubesize gm)
                                         (game-maxcolours gm)
                                         (first lst))
                              (make-list (rest lst)))]))]
    (make-list list-possible)
    ))


(define (solve gm draw-option)
  (local
    [(define setup (puzzle-setup gm draw-option))
     (define (solve-helper to-visit visited) 
       (cond
         [(empty? to-visit) visited]
         [else
          (local 
            [(define draw (draw-board (first to-visit) draw-option))
             (define sorted (my-sort to-visit))]
            (cond [(finished-game? (first sorted)) true]
                  [(member? (first sorted) visited)
                   (solve-helper (rest sorted) visited)]
                  [else
                   (local [(define nbrs (next-games (first sorted)))
                           (define new (filter (lambda (x) (not (member? x visited))) nbrs))
                           (define result (solve-helper new (cons (first sorted)
                                                                  visited)))]
                     (cond [(boolean? result) true]
                           [else (solve-helper (rest sorted) result)]))]))]))]
     
     (cond [(list? (solve-helper (list gm) empty)) false]
           [else true])
     ))

(define (my-sort lst-gm)
  (local [(define (count-bottom tube sym acc)
            (cond [(empty? tube) (add1 acc)]
                  [(= 2 acc) (count-bottom tube sym (* 1.5 acc))]
                  [(= 4 acc) (count-bottom tube sym (* 3 acc))]
                  [(symbol=? sym (first tube))
                   (count-bottom (rest tube) sym (add1 acc))]
                  [else (count-bottom (rest tube) (first tube) 0)]))
          (define (count-bottom/lst list-tubes)
            (foldr (lambda (x y) (+ (cond [(empty? x) 0]
                                          [else (count-bottom (rest x) (first x) 0)])
                                    y))
                   0
                   list-tubes))
          (define (sort-helper lst-gm)
            (quicksort lst-gm (lambda (x y)
                                (cond [(<= (count-bottom/lst (game-tubes x))
                                          (count-bottom/lst (game-tubes y)))
                                       false]
                                      [else true]))))
          ]
    (sort-helper lst-gm)
    ))

(define (tube-complete/ne tube sym)
  (cond [(empty? tube) true]
        [else (and (symbol=? (first tube) sym)
                   (tube-complete/ne (rest tube) sym))]))


;(check-expect (solve smallgame1 'slow) true)
;(check-expect (solve mediumgamestuck 'slow) false)
;(check-expect (solve mediumgame 'fast) true)
;(check-expect (solve biggame 'fast) true)
(check-expect (solve hugegame 'fast) true)


;; Below is the format for testing and timing the solution:
;; be sure to remove any other check-expects when measuring your timing

;(check-expect (time (solve mediumgame 'off)) true)
;(check-expect (time (solve largergame 'off)) true)
;(check-expect (time (solve biggame 'off)) true)
;(check-expect (time (solve hugegame2 'off)) true)
