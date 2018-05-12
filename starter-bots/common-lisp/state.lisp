(in-package :bot)

(define-poclo state ((game-details () game-details) 
                     (players () player)
                     (game-map () game-cell)) camel-case)

(define-poclo game-details ((game-round "round" ()) map-width map-height 
                            (building-prices () building-prices)) camel-case)

(define-poclo building-prices (energy defense attack) screaming-snake-case)

(define-poclo player (player-type energy health hits-taken score) camel-case)

(define-poclo game-cell (x y (buildings () building-state) 
                           (missiles () missile-state) cell-owner) camel-case)

(define-poclo building-state (health construction-time-left price weapon-damage
                              weapon-speed weapon-cooldown-time-left weapon-cooldown-period
                              destroy-multiplier construction-score energy-generated-per-turn
                              building-type x y player-type) camel-case)

(define-poclo missile-state ((missile-speed "speed" ()) damage x y player-type) camel-case)

(defun read-state (filename)
  (with-open-file (file filename)
    (when file
      (parse-state file))))
