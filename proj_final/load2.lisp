(defparameter var-name (random-board))
 (defparameter initial-node (create-node var-name 0))
 (defparameter clone-var (copy-board var-name))
(setf clone-var (set-highest-of-first-row-to-T clone-var))
 (defparameter initial-node2 (create-node (first clone-var) (second clone-var) 1 initial-node))
(horse-can-move-to (first clone-var))
(print-board (first clone-var))