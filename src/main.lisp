(defpackage tic-tac-toe
  (:nicknames :ttt)
  (:use :cl))
(in-package :tic-tac-toe)

;; game state
(defun make-board ()
  "Returns a board for tic-tac-toe."
  (make-array '(3 3) :initial-element "-"))

(defun b-get (board coords)
  "Returns the board values specified in coords. If more than one coord, returns a list."
  (let ((coords-count (list-length coords)))
    (cond
     ((oddp coords-count) (error "b-get requires x and y coordinates for each position."))
     ((= coords-count 2) (aref board (first coords) (second coords)))
     (t (loop for x-y on coords by #'cddr 
                collect (aref board (first x-y) (second x-y)))))))

(defun b-set (board coords player)
  "Given valid coordinates, plays into a board position."
  (loop for x-y on coords by #'cddr 
                do (setf (aref board (first x-y) (second x-y)) player)))

(defun show-board (board)
  "Displays the borad for the text interface user."
  (dotimes (x 3)
    (dotimes (y 3)
      (if (= y 2)
        (format t "~a~%" (b-get board (list x y)))
        (format t "~a | " (b-get board (list x y)))))))

(defmacro player-x ()
  "X")

(defmacro player-o ()
  "O")

(defmacro unplayed ()
  "-")

;; predicates
(defun valid-player-p (p)
  (or (equal p (player-x)) (equal p (player-o))))

(defun valid-coord-p (z)
  (and z (integerp z) (>= z 0) (<= z 2)))

(defun valid-coords-p (x y)
  "If the coordinates are valid returns true, false otherwise."
  (and (valid-coord-p x) (valid-coord-p y)))

(defun unplayed-p (board x y)
  "If a position is valid and empty, returns true, false otherwise."
  (and
    (valid-coords-p x y)
    (equal (b-get board (list x y)) (unplayed))))

(defun valid-move-p (board x y player)
  (and (valid-player-p player) (unplayed-p board x y)))

(defun line-win-draw-play-p (board line-coords)
  (let ((line (b-get board line-coords)))
    (cond
    ;; Check if the line-coords contains "-". Not a winning line, not a draw.
     ((member (unplayed) line :test 'string=) (unplayed))
     ;; Check if all elements are (player-x). Winning line.
    ((every (lambda (element) (string= element (player-x))) line) (player-x))
    ;; Check if all elements are (player-o). Winning line.
    ((every (lambda (element) (string= element (player-o))) line) (player-o))
    ;; If none of the above, return NIL (only (player-x) and (player-o) but no "-"). Draw line.
    (t nil))))

(defun board-win-draw-play-p (board)
  (let* ((lines 
         '((0 0 0 1 0 2)
           (1 0 1 1 1 2)
           (2 0 2 1 2 2)
           (0 0 1 0 2 0)
           (0 1 1 1 2 1)
           (0 2 1 2 2 2)
           (0 0 1 1 2 2)
           (0 2 1 1 2 0)))
         (evaluated-lines 
          (map 'list #'(lambda (l) (line-win-draw-play-p board l)) lines)))
    (cond ; assumes turn based play
     ; game over, player-x won 
     ((some #'(lambda (s) (string= (player-x) s)) evaluated-lines) (player-x))
     ; game over, player-o won
     ((some #'(lambda (s) (string= (player-o) s)) evaluated-lines) (player-o))
     ; at least one line is still playable
     ((some #'(lambda (s) (string= (unplayed) s)) evaluated-lines) (unplayed))
     ; game over, it's a draw, neither player won, no plays are left on the board
     (t nil))))

;; parsing
(defun prompt-line (text-prompt)
  (format *query-io* "~a: " text-prompt)
  (force-output *query-io*))

(defun message-line (text-message)
  (format *query-io* "~a~%" text-message)
  (force-output *query-io*))

(defun prompt-read (text-prompt)
  (prompt-line text-prompt)
  (read-line *query-io*))

(defun parse-coord (input-coordinate)
  (let* ((input-coordinate
          (nth-value 0 (parse-integer input-coordinate :junk-allowed t))))
    (if (valid-coord-p input-coordinate)
        input-coordinate
        nil)))


;; game play
(defun read-coord (text-prompt)
  (loop
   (let ((coord (parse-coord (prompt-read text-prompt))))
     (when coord (return coord)))))

(defun change-player (player)
  (cond
   ((string= player (player-x)) (player-o))
   ((string= player (player-o)) (player-x))))

(defun play (&optional (board (make-board)) (player (player-x)))
  (loop
   (show-board board)
   (message-line (concatenate 'string player "'s turn."))
   (let ((x (parse-coord (prompt-read "Enter row (0-2)")))
         (y (parse-coord (prompt-read "Enter column (0-2)"))))
     (if (valid-move-p board x y player)
         (b-set board (list x y) player)
         (message-line "Invalid play. Try again."))
     (let ((game-state (board-win-draw-play-p board)))
       (cond
        ((equal game-state nil)
          (progn
           (message-line "Good game! It's a draw.")
           (show-board board)
           (return nil)))
        ((equal game-state player)
          (progn
           (message-line (concatenate 'string player " won!"))
           (show-board board)
           (return player)))
        ((equal game-state (unplayed)) 
          (progn
           (message-line "The game is still a contest!")
           (setf player (change-player player)))))))))


