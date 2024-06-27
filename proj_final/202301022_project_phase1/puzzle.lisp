;;;Stuff for the table

(defparameter objective 2000)

(defun played-board ()
  "Test board similar to the previous one but with a knight placed at position: i=0 and j=0"
  '(
    (T 25 54 89 21 8 36 14 41 96) 
    (78 47 56 23 5 49 13 12 26 60) 
    (0 27 17 83 34 93 74 52 45 80) 
    (69 9 77 95 55 39 91 73 57 30) 
    (24 15 22 86 1 11 68 79 76 72) 
    (81 48 32 2 64 16 50 37 29 71) 
    (99 51 6 18 53 28 7 63 10 88) 
    (59 42 46 85 90 75 87 43 20 31) 
    (3 61 58 44 65 82 19 4 35 62) 
    (33 70 84 40 66 38 92 67 98 97)
    )
  )

;;; create copy of the board
(defun copy-board (lst)
  "Recursively clones the board."
  (if (null lst)
      nil
      (if (listp (car lst))
          (cons (copy-board (car lst)) (copy-board (cdr lst)))
          (cons (car lst) (copy-board (cdr lst))))))

(defun row (index board)
  "Function that receives an index and the board and returns a list that represents that line of the board."
  (cond
    ((and (integerp index) (<= 0 index (- (length board) 1)))
     (nth index board))
    (T (error "Invalid index or board"))))

(defun cell (i j board)
  "Function that receives two indexes and the board and returns the value present in this cell of the board."
  (cond
    ((and (integerp i) (integerp j)
          (<= 0 i (- (length board) 1))
          (<= 0 j (- (length (first board)) 1)))
     (nth i (nth j board)))
    (T (error "Invalid indices or board"))))

(defun list-numbers-recursively (n &optional (accumulator '()))
  "Function that receives a positive number n and creates a list with all the numbers between 0 (inclusive) and the number passed as an argument (exclusive). By default, n is 100."
  (cond ((and (integerp n) (>= n 0))
         (cond ((= n 0) accumulator)
               (t (list-numbers-recursively (- n 1) (cons (- n 1) accumulator)))))
        (t
         (error "Invalid input. Please provide a non-negative integer."))))

(defun list-numbers (&optional (n 100))
  (list-numbers-recursively n))


(defun remover-se (pred lista)
  "Removes elements from the list based on the given predicate."
  (cond
    ((null lista) nil)
    ((funcall pred (car lista)) (remover-se pred (cdr lista)))
    (t (cons (car lista) (remover-se pred (cdr lista))))))

(defun shuffle (lst)
  "Randomly shuffles the elements of a list."
  (if (endp lst)
      nil
      (let* ((random-index (random (length lst)))
             (random-element (nth random-index lst)))
        (cons random-element (shuffle (remover-se (lambda (x) (eql x random-element)) lst))))))

(defun random-board (&optional (list (shuffle (list-numbers))) (n 10))
  "Creates sublists of n elements from a shuffled list with each row shuffled independently."
  (cond
    ((null list) nil)
    (T (cons (shuffle (subseq list 0 n)) (random-board (subseq list n) n)))))

(defun replace-position (index list &optional (value nil))
  "Replaces the element at the specified index with the given value in the list."
  (if (and (integerp index) (<= 0 index (1- (length list))))
      (append (subseq list 0 index) (list value) (subseq list (1+ index)))
      (error "Invalid index or list")))

(defun replace-cell (i j board &optional (value nil))
  "Replaces the cell at the specified indices with the given value in the board."
  (cond ((and (integerp i) (integerp j)
	      (<= 0 i (1- (length board)))
	      (<= 0 j (1- (length (first board)))))
	 (if (null (cell i j board)) nil
	     (replace-position i (replace-position j (row i board) value) value))
	 )
	(T NIL)))

(defun find-horse (board i j)
  "Recursively find the position of the horse on the board."
  (cond
    ((>= i (length board)) nil)  ; If we reached the end of rows, return nil
    ((>= j (length (first board))) (find-horse board (1+ i) 0))  ; Move to the next row
    ((eql (cell i j board) T) (list i j))  ; Horse found, return position
    (T (find-horse board i (1+ j)))))  ; Move to the next column

(defun horse-position (board)
  "Returns the position (i j) of the horse on the board. Returns NIL if the horse is not on the board."
  (find-horse board 0 0))


(defun horse-can-move-to (board)
  "Check for all possible moves."
  (let* ((horse-position (horse-position board))
	 (x (first horse-position))
	 (y (second horse-position))
	 (moves '()))
    (dolist (offset '((1 . 2) (2 . 1) (2 . -1) (1 . -2)
		      (-1 . -2) (-2 . -1) (-2 . 1) (-1 . 2)))
      (let ((new-x (+ x (car offset))) (new-y (+ y (cdr offset))))
	(cond ((and (>= new-x 0) (<= new-x 9) (>= new-y 0) (<= new-y 9) (cell new-x new-y board))
	       (push (list new-x new-y) moves))
	      (t (push nil moves)))))
    (remove-nil(reverse moves)))
  )

(defun remove-nil (lst)
  "Removes all the nil values of the list."
  (let ((values '()))
    (mapcar #'(lambda(value) (if (not (null value)) (push value values))) lst)
    (reverse values)))





;;;v2 with proper checks

(defun move-horse (board new-i new-j)
  "Helper function for moving the horse on the board."
  (let ((horse-position (horse-position board)))
    (if horse-position
        (let* ((i (first horse-position))
               (j (second horse-position)))
          (if (and (<= 0 new-i (1- (length board))) (<= 0 new-j (1- (length (first board))))) ; Check if the new position is within the board
              (let ((cell-value (cell new-i new-j board)))
                (setf (nth i (nth j board)) NIL) ; Clear the current position
                (setf (nth new-i (nth new-j board)) T) ; Move the horse to the new position
                cell-value) ; Return the updated value
              (error "New position is out of bounds")))
	(error "Horse not found on the board"))))

(defun compare-division (number)
  (let ((whole-part (floor number 10))
        (remainder (mod number 10)))
    (if (= whole-part remainder)
        t
        nil)))

(defun find-and_nil_max_double (board)
  (labels ((process-row (row)
             (if (null row)
                 nil
                 (let ((element (car row)))
                   (when (and (numberp element) (floatp element))
                     (when (or (not max-double) (> element max-double))
                       (setf max-double element)))
                   (process-row (cdr row)))))
           (process-board (board)
             (if (null board)
                 nil
                 (progn
                   (process-row (car board))
                   (process-board (cdr board))))))
    (let ((max-double nil))
      (process-board board)
      (setf max-double nil))))


;;; Node creation

;;; Constructor
(defun create-node (board score &optional (depth 0) (parent nil) (heuristic depth))
  (list board score depth parent heuristic)
  )

(defun create-seccessor-nodes (node)
  (let* ((moves-available (horse-can-move-to (node-board node))))
    (mapcar
     (lambda (coordinates)
       (let* ((x (first coordinates))
	      (y (second coordinates))
	      (copy (copy-board (node-board node)))
	      (score (move-horse copy x y)))
	 (create-node copy (+ (node-score node) score) (1+ (node-depth node)) node 
		      )))
     moves-available)))


;;; current board
(defun node-board (node)
  "Returns the board from the given node."
  (first node))

;;; score
(defun node-score (node)
  "Returns the score from the given node."
  (second node))

;;; depth
(defun node-depth (node)
  "Returns the g value from the given node."
  (third node))

;;; parent
(defun node-parent (node)
  "Returns the parent from the given node."
  (fourth node))

;;; heuristic
(defun node-heuristic (node)
  "Returns the heuristic from the given node."
  (fifth node))


(defun set-highest-of-first-row-to-T (list-of-lists)
  "Sets the highest value of the first row to T in a list of lists."
  (if (null list-of-lists)
      list-of-lists
      (let* ((first-row (first list-of-lists))
             (max-value (apply #'max (remove-nil first-row)))
             (new-first-row (mapcar
			     #'(lambda (x) (if x (if (= x max-value) T x) nil)) first-row)))
        (list (cons new-first-row (rest list-of-lists)) max-value))))


(defun generate-next-nodes (node)
  (if (null node)
      (error "Invalid node"))
  (let* ((possible-moves (horse-can-move-to (node-board node))))
    (mapcar (lambda (coordinates)
              (let ((x (first coordinates))
		    (y (second coordinates)))
		(let ((new-state (copy-board (node-board node)))
		      (score (cell x y (node-board node))))
		  (move-horse new-state x y)
		  (list new-state (+ (node-score node) score) (1+ (node-depth node)) node 
                        0))))
            possible-moves)))


(defun print-board (board)
  "Prints the board"
  (labels
      ((print-row(line)
	 "Prints a string"
	 (cond ((null line) nil)
	       ((null (first line))
		(format t "-- ")
		(print-row (rest line)))
	       (t (format t "~2,'0D " (first line))
		  (print-row (rest line))))))
    (cond ((null board) nil)
	  ((equal 'RANDOM (first board)) (format t "RANDOM~%"))
	  (t (print-row (first board))
	     (format t "~%")
	     (print-board (rest board))))))