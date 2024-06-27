(defparameter intial-state (random-board))
(defparameter initial-node (create-node initial-state 0))
(defparameter second-state (set-initial-value (clone-table initial-state)))
(defparameter second-node (create-node second-state [board_max_score] 1 initial-node))