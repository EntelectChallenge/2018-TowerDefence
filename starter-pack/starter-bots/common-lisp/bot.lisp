(in-package :bot)

(defparameter defense 0)
(defparameter attack 1)
(defparameter energy 2)

(defparameter state-path "state.json")
(defparameter command-path "command.txt")

(define-data-class command (x y building))

(defun seed-random ()
  (setf *random-state* (make-random-state t)))

(defmethod write-command-to-stream ((command command) out)
  (with-slots (x y building) command
      (format out "~a,~a,~a~%" x y building)))

(defmethod energy ((state state))
  (let ((a-player (find-if (lambda (player) (string= (player-type player) "A"))
                           (players state))))
    (or (and a-player (energy a-player)) 0)))

(defmethod can-afford-attack-building ((state state))
  (>= (energy state) 
      (attack (building-prices (game-details state)))))

(defmethod can-afford-defence-building ((state state))
  (>= (energy state)
      (defense (building-prices (game-details state)))))

(defmethod can-afford-energy-building ((state state))
  (>= (energy state)
      (energy (building-prices (game-details state)))))

(defmethod can-afford-all-buildings ((state state))
  (and (can-afford-attack-building state)
       (can-afford-defence-building state)
       (can-afford-energy-building state)))

(defmethod is-opponent-attack-building ((building building-state))
  (and (string= (player-type building) "B")
       (string= (building-type building) "ATTACK")))

(defmethod is-my-defence-building ((building building-state))
  (and (string= (player-type building) "A")
       (string= (building-type building) "DEFENSE")))

(defmethod is-under-attack ((state state) y)
  (and (some (lambda (cell) (some #'is-opponent-attack-building (buildings cell)))
             (aref (game-map state) y))
       (not (some (lambda (cell) (some #'is-my-defence-building (buildings cell)))
                  (aref (game-map state) y)))))

(defmethod is-occupied ((state state) x y)
  (not (= (length (buildings (aref (aref (game-map state) y) x))) 0)))

(defmethod unoccupied-in-row ((state state) y)
  (coerce (loop for x from 0 to (- (/ (map-width (game-details state)) 2) 1)
             when (not (is-occupied state x y))
             collect x) 'vector))

(defmethod unoccupied-cells ((state state))
  (let (unoccupied)
    (loop for x from 0 to (- (/ (map-width (game-details state)) 2) 1)
       do (loop for y from 0 to (- (map-height (game-details state)) 1)
             when (not (is-occupied state x y))
             do (push (cons x y) unoccupied)))
    (coerce unoccupied 'vector)))

(defmethod defence-building ((state state))
  (and (can-afford-defence-building state)
       (loop for y from 0 to (- (map-height (game-details state)) 1)
          until (> (length found) 0)
          when (and (is-under-attack state y)
                    (unoccupied-in-row state y))
          collect it into found
          finally (let ((unoccupied (car found)))                 
                    (if (> (length unoccupied) 0)
                      (let ((x (aref unoccupied (random (length unoccupied)))))
                        (return (make-instance 'command :x x :y (- y 1) :building defense)))
                      (return nil))))))
            
(defmethod random-building ((state state))
  (and (can-afford-all-buildings state)
       (let ((unoccupied (unoccupied-cells state)))
         (when (> (length unoccupied) 0)
           (let* ((choice (aref unoccupied (random (length unoccupied))))
                    (building (aref (vector defense attack energy) (random 3))))
               (make-instance 'command :x (car choice) :y (cdr choice) 
                              :building building))))))

(defmethod choose-move ((state state))
  (or (defence-building state) (random-building state)))

(defmethod write-command (filename (command command))
  (with-open-file (file filename :direction :output :if-does-not-exist :create 
                        :if-exists :supersede)
    (when file
      (write-command-to-stream command file))))

(defun write-empty ()
  (with-open-file (file command-path :direction :output
                        :if-does-not-exist :create 
                        :if-exists :supersede)
    (when file (format file " "))))

(defun take-turn ()
  (seed-random)
  (let* ((state (read-state state-path))
         (command (choose-move state)))
    (if command
        (write-command command-path command)
        (write-empty))))
