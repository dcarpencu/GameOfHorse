(load "/Users/carpencudavid/POLI/Erasmus/CLASSES/AI class/proj 1/proj_final/202301022_project_phase1/search.lisp")
(load "/Users/carpencudavid/POLI/Erasmus/CLASSES/AI class/proj 1/proj_final/202301022_project_phase1/puzzle.lisp")


;;; Project launch

;;; Program Initialization
;; start

(defun start ()
  "Allows starting the program, reading the initial state and the algorithm to use for finding the solution (either depth-first search or breadth-first search)."
  (let* ((algorithm (read-algorithm))
         (table (read-table)))
    (funcall algorithm (funcall table))))


;;; not in use
(defun start-menu ()
  (load (read-parameter "Enter the path of the Puzzle.lisp script file"))
  (load (read-parameter "Enter the path of the Search.lisp script file"))
  (write-line "Welcome to the horse game!")
  (let* ((param4 (read-parameter "Enter the search algorithm to use: "))
	 (param5 (read-parameter "Enter the heuristic to use: ")))
    (start)
    )
  )

;; read-algorithm
(defun read-algorithm ()
  "Reads the algorithm to use."
  (progn
    (format t "Which algorithm do you want to use for search? ~%")
    (format t "1- Breadth-first search ~%")
    (format t "2- Depth-first search ~%")
    (format t "3- A* ~%")
    (let ((response (read)))
      (cond ((= response 1) #'bfs)
            ((= response 2) #'dfs)
            (T #'a-star)))
    )
  )

;;; read-table 
(defun read-table ()
  "Reads the table to use."
  (progn
    (format t "Which table do you want to use for search? ~%")
    (format t "1- Table a ~%")
    (format t "2- Table b ~%")
    (format t "3- Table c ~%")
    (format t "4- Table d ~%")
    (format t "5- Table e ~%")
    (format t "6- Random Table ~%")
    (let ((response (read)))
      (cond ((= response 1) #'prob-a)
            ((= response 2) #'prob-b)
            ((= response 3) #'prob-c)
            ((= response 4) #'prob-d)
            ((= response 5) #'prob-e)
            (T #'prob-r)))
    )
  )

;;; 44 with 20
(defun prob-a ()
  (let* ((lst '((02 44 20 NIL NIL NIL NIL NIL NIL NIL)
                (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
                (NIL 03 30 NIL NIL NIL NIL NIL NIL NIL)
                (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
                (NIL NIL NIL 22 NIL NIL NIL NIL NIL NIL)
                (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
                (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
                (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
                (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
                (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)))
         (initial-node (create-node lst 0))
         (clone-var (set-highest-of-first-row-to-T (copy-board lst)))
         (initial-node2 (create-node (first clone-var) (second clone-var) 1 initial-node)))
    (defparameter goal 70)
    initial-node2)
  )



;;; 02 10
(defun prob-b ()
  (let* ((lst '(
		(10 NIL 04 NIL 06 NIL 08 NIL 02 NIL)
		(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
		(NIL 03 NIL 05 NIL 07 NIL 09 NIL 11)
		(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
		(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
		(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
		(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
		(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
		(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
		(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
		))
         (initial-node (create-node lst 0))
         (clone-var (set-highest-of-first-row-to-T (copy-board lst)))
         (initial-node2 (create-node (first clone-var) (second clone-var) 1 initial-node)))
    (defparameter goal 60)
    initial-node2)
  )

(defun prob-c ()
  (let* ((lst '(
		(01 12 03 23 NIL 88 NIL NIL NIL NIL)
		(21 45 43 NIL NIL NIL NIL NIL NIL NIL)
		(NIL 56 NIL 78 NIL NIL NIL NIL NIL NIL)
		(89 NIL 99 54 NIL NIL NIL NIL NIL NIL)
		(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
		(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
		(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
		(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
		(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
		(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
		))
         (initial-node (create-node lst 0))
         (clone-var (set-highest-of-first-row-to-T (copy-board lst)))
         (initial-node2 (create-node (first clone-var) (second clone-var) 1 initial-node)))
    (defparameter goal 270)
    initial-node2)
  )

(defun prob-d ()
  (let* ((lst '(
		(NIL 05 NIL NIL NIL 15 NIL NIL NIL 15)
		(NIL NIL NIL 06 NIL NIL NIL 16 NIL NIL)
		(NIL 04 NIL NIL NIL 14 NIL NIL NIL 24)
		(NIL NIL NIL 07 NIL NIL NIL 17 NIL NIL)
		(NIL 03 NIL NIL NIL 13 NIL NIL NIL 23)
		(NIL NIL NIL 08 NIL NIL NIL 18 NIL NIL)
		(NIL 02 NIL NIL NIL 12 NIL NIL NIL 22)
		(NIL NIL NIL 09 NIL NIL NIL 19 NIL NIL)
		(NIL 01 NIL NIL NIL 11 NIL NIL NIL 21)
		(NIL NIL NIL 10 NIL NIL NIL 20 NIL NIL)
		))
         (initial-node (create-node lst 0))
         (clone-var (set-highest-of-first-row-to-T (copy-board lst)))
         (initial-node2 (create-node (first clone-var) (second clone-var) 1 initial-node)))
    (defparameter goal 600)
    initial-node2)
  )

(defun prob-e ()
  (let* ((lst '(
		(NIL 05 NIL NIL NIL 15 NIL NIL NIL 15)
		(NIL NIL NIL 06 NIL NIL NIL 16 NIL NIL)
		(NIL 04 NIL NIL NIL 14 NIL NIL NIL 24)
		(NIL NIL NIL 07 NIL NIL NIL 17 NIL NIL)
		(NIL 03 NIL NIL NIL 13 NIL NIL NIL 23)
		(NIL NIL NIL 08 NIL NIL NIL 18 NIL NIL)
		(NIL 02 NIL NIL NIL 12 NIL NIL NIL 22)
		(NIL NIL NIL 09 NIL NIL NIL 19 NIL NIL)
		(NIL 01 NIL NIL NIL 11 NIL NIL NIL 21)
		(NIL NIL NIL 10 NIL NIL NIL 20 NIL NIL)
		))
         (initial-node (create-node lst 0))
         (clone-var (set-highest-of-first-row-to-T (copy-board lst)))
         (initial-node2 (create-node (first clone-var) (second clone-var) 1 initial-node)))
    (defparameter goal 300)
    initial-node2)
  )


(defun prob-r ()
  (let* ((lst (random-board))
         (initial-node (create-node lst 0))
         (clone-var (set-highest-of-first-row-to-T (copy-board lst)))
         (initial-node2 (create-node (first clone-var) (second clone-var) 1 initial-node)))
    (defparameter goal 500)
    initial-node2)
  )