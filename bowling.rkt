#lang racket
; input file -> lines of text as list of lists
(define (process-file filename)
  (let
      ([lines (file->lines filename)])
    (map (lambda (lst)
           (map (lambda (val) ; map a map to each sublist
                  (if (string->number val) (string->number val) val)) ; if the string can be converted to a number, do so
            lst))
          (map string-split lines)))) 

(define (strike? val)
  (equal? val "X"))

(define (spare? val)
  (equal? val "/"))

; Score the next half-frame (single roll)
(define (calculate-next-roll game)
  (define (bowling-value score) ; Basically, convert X to 10
    (if (number? score)
        score
        (if (strike? score) 10 0)))
  (cond [(empty? game) 0] ; if the game list is empty, return 0
        [(and (> (length game) 1) (spare? (second game))) 0] ; if the next symbol is a spare, we will count that as 10, so the current number is 0
        [(number? (first game)) (first game)] ; if element is a number, this is the roll score
        [(spare? (first game)) (+ 10 (bowling-value (second game)))] ; if it is a spare, the score is 10 plus the next score
        [(strike? (first game)) (if (spare? (third game)) ; if it is a strike, look at next two scores
                                    20 ; if the next frame is a spare, only add 10
                                    (+ 10 ; if there is not a spare in the next frame, calculate normally                     
                                       (bowling-value (second game))
                                       (bowling-value (third game))))]
        [else 0]))

; Score a single game
(define (score-game game [total 0])
    (if (empty? game)
        total
        (if (or (and (= (length game) 2) (spare? (first game)))
                (and (= (length game) 3) (strike? (first game))))
            (+ total (calculate-next-roll game)) ; if spare is second-to-last or strike is third-to-last, only calculate spare/strike score and ignore the extra frames
            (score-game (rest game) (+ total (calculate-next-roll game)))))) ; otherwise, continue to next score

; Calculate all individual player scores
(define (score-players game-list [scores '()] [team-name "NoTeam"])
  (if (empty? game-list)
      scores
      (if (= (length (first game-list)) 1) ; if the top line is the team name, continue running with this as the current team name
          (score-players (rest game-list) scores (first (first game-list)))
          (let*
              ([fname (first (first game-list))]
               [lname (second (first game-list))]
               [game-matches-player? (lambda (line) (and (equal? (first line) fname) (equal? (second line) lname)))]) ; test if name on game matches given name
            (score-players (filter-not game-matches-player? game-list)
                           (append scores (list (list fname
                                                      lname
                                                      (foldl + 0 (map (lambda (lst) ; calculate player score
                                                                        (score-game (rest (rest lst))))
                                                                      (filter game-matches-player? game-list)))
                                                      team-name)))
                           team-name)))))

; Get top-scoring player(s) and number of points where scores is the output from score-all
(define (find-top-score-player scores)
  (if (empty? scores)
      '()
      (let ; bring this out so it isn't recalculated every time the filter lambda is called
          ([top-player-score (foldl (lambda (next-player-list top-score) ; find largest score among players
                                      (if (> top-score (third next-player-list)) top-score (third next-player-list)))
                                    (third (first scores))
                                    scores)])
        (filter (lambda (player-list) ; then, filter all players with that score
                  (= (third player-list) top-player-score))
                scores))))

; Get total scores for each team
(define (score-teams scores [team-scores '()])
  (if (empty? scores)
      team-scores
      (let
          ([team-name (fourth (first scores))])
        (score-teams (filter-not (lambda (player-list) (equal? (fourth player-list) team-name)) scores) ; remove team being currently scored
                     (append team-scores
                             (list (list team-name
                                         (foldl (lambda (player-list total) (+ total (third player-list))) ; add all scores from matching team
                                                0
                                                (filter (lambda (player-list) (equal? (fourth player-list) team-name)) scores)))))))))

; Put all desired output into list
(define (get-stats open-file)
  (let*
      ([player-scores (score-players open-file)]
       [team-scores (score-teams player-scores)])
    (list (list "Player Scores" player-scores)
          (list "Top players" (find-top-score-player player-scores))
          (list "Team scores" team-scores)
          (list "Winner" (first (foldl (lambda (team-info winning-team) ; name of winner
                                         (if (> (second team-info) (second winning-team)) team-info winning-team))
                                       (first team-scores) team-scores))))))
 
(get-stats (process-file "scores.txt"))

(module+ test
  (require rackunit)
  (check-equal? (score-game '("X" 7 2 4 5 8 "/" 3 6 "X" "X" 5 "/" 9 "/" 1 8)) 143)
  (check-equal? (score-game '(7 "/" "X" 5 4 "X" "X" 7 "/" 5 4 8 "/" "X" 8 "/" "X")) 179))