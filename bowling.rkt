#lang racket

; Take list of strings, turn all valid elements into numbers
(define (list->int-list lst)
  (map (λ (val)
         (if (string->number val) (string->number val) val)) ; if the string can be converted to a number, do so
       lst))

; input file -> lines of text as list of lists
(define (file->int-lists filename)
  (let
      ([lines (file->lines filename)])
    (map list->int-list (map string-split lines))))

; Prepare data for use (this is the format used by every other function)
(define (parse-file filename)
  (define (iter lst new-list team-name) ; move all team names in-line with player games
    (cond
      [(empty? lst) new-list]
      [(= (length (car lst)) 1) (iter (cdr lst) new-list (caar lst))]
      [else (iter (cdr lst) (append new-list `((,team-name ,@(car lst)))) team-name)]))
  (iter (file->int-lists filename) '() "NoName"))

(define (strike? val)
  (equal? val "X"))

(define (spare? val)
  (equal? val "/"))

; Score the next half-frame (single roll)
(define (calculate-next-roll score-list) ; score whatever is at the front of the list
  (define (bowling-value score) ; Raw score used for strike and spare calculation
    (cond [(number? score) score]
          [(strike? score) 10]
          [else 0]))
  (cond [(empty? score-list) 0] ; if the game list is empty, return 0
        [(and (> (length score-list) 1) (spare? (second score-list))) 0] ; if the next symbol is a spare, we will count that as 10, so the current number is 0
        [(number? (first score-list)) (first score-list)] ; if element is a number, this is the roll score
        [(spare? (first score-list)) (+ 10 (bowling-value (second score-list)))] ; if it is a spare, the score is 10 plus the next score
        [(strike? (first score-list)) (if (spare? (third score-list)) ; if it is a strike, look at next two scores
                                          20 ; if the next frame is a spare, only add 10
                                          (+ 10 ; if there is not a spare in the next frame, calculate normally                     
                                             (bowling-value (second score-list))
                                             (bowling-value (third score-list))))]
        [else 0]))

; Score a single game
(define (score-game game-list)
  (define extra-frames (> (length (cdddr game-list)) 10))
  (define (iter game-scores total)
    (if (empty? game-scores)
        total
        (if (and extra-frames ; if there are extra frames, then watch out for the spare and strikes towards the end
                 (or (and (= (length game-scores) 2) (spare? (first game-scores)))
                     (and (= (length game-scores) 3) (strike? (first game-scores)))))
            (+ total (calculate-next-roll game-scores))
            (iter (rest game-scores) (+ total (calculate-next-roll game-scores)))))) ; otherwise, continue to next score (I believe this is tail call optimization)
  (iter (cdddr game-list) 0)) ; start without team name, fname, and lname

; Calculate all game scores
(define (score-players game-list)
  (sort (map (λ (game) `(,@(take game 3) ,(score-game game)))
             game-list)
        (λ (g1 g2) (string<? (string-append (first g1) (third g1) (second g1))
                             (string-append (first g2) (third g2) (second g2))))))

; Get total score grouped by elements as specified by group-func
(define (total-by lst group-func score-func) ; group func is a function that specifies which elements of each sublist of lst to group by
  (map (λ (player-games)                     ; score-func is a function that specifies the position of the score within each sublist of lst
         (foldl (λ (next-game current-score) `(,@(group-func current-score) ,(+ (score-func next-game) (last current-score)))) ; add score to last element of current-score list
                `(,@(group-func (car player-games)) 0) ; initialize with group-func elements and total of 0
                player-games))
       (group-by group-func lst)))

; Group player scores
(define (player-totals game-list)
  (total-by (score-players game-list)
            (λ (game) (take game 3))
            last))

; Get total scores for each team
(define (score-teams game-list)
  (total-by (score-players game-list)
            (λ (game) `(,(first game)))
            last))

; Get top-scoring player(s) and number of points where scores is the output from score-all
(define (find-top-score-player game-list)
  (let* ; bring this out so it isn't recalculated every time the filter λ is called
      ([scores (player-totals game-list)]
       [top-player-score (foldl (λ (next-player-list top-score) (if (> top-score (last next-player-list)) top-score (last next-player-list)))
                                (fourth (first scores)) ; find largest score among players
                                scores)])
    (filter (λ (player-list) (= (fourth player-list) top-player-score)) ; then, filter all players with that score
            scores)))

; Put all desired output into list
(define (get-stats filename)
  (define game-list (parse-file filename))
  (if (empty? game-list)
      "Game file empty"
      (let* ([team-scores (score-teams game-list)])
        `(("Game Scores" ,(score-players game-list))
          ("Player Totals" ,(player-totals game-list))
          ("Top players" ,(find-top-score-player game-list))
          ("Team scores" ,team-scores)
          ("Winner" ,(first (foldl (λ (team-info winning-team) (if (> (second team-info) (second winning-team)) team-info winning-team))
                                   (first team-scores)
                                   team-scores)))))))

(get-stats "scores.txt")

(module+ test
  (require rackunit)
  (check-equal? (score-game '("NoTeam" "John" "Doe" "X" 7 2 4 5 8 "/" 3 6 "X" "X" 5 "/" 9 "/" 1 8)) 143)
  (check-equal? (score-game '("NoTeam" "John" "Doe" 7 "/" "X" 5 4 "X" "X" 7 "/" 5 4 8 "/" "X" 8 "/" "X")) 179))