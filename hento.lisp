;(asdf:operate 'asdf:load-op :lispbuilder-sdl)
;(asdf:operate 'asdf:load-op :lispbuilder-sdl-ttf)
(ql:quickload "lispbuilder-sdl")
(ql:quickload "lispbuilder-sdl-ttf")

(defun surface-paint (surface window x y)
  (when surface
    (let* ((tw (sdl:width surface))
	   (th (sdl:height surface))
	   (mx (* x tw))
	   (my (* y th)))
      (sdl:draw-surface-at-* surface mx my :surface window))))

(defun tile-for-color (color)
  (when color
    (string-downcase (string color))))

(defstruct tile-cache
  tiles)

(defun tile-cache-get (cache key)
  (unless (null key)
    (let ((a (assoc key (tile-cache-tiles cache) :test 'string=)))
      (when (null a)
	(setq a (cons key
		      (sdl::convert-to-display-format
		       :surface (sdl:load-image (format nil "~A.png" key))
		       :pixel-alpha t)))
	(push a (tile-cache-tiles cache)))
      (cdr a))))

(defclass piece ()
  ((x :accessor piece-x
      :initarg :x)
   (y :accessor piece-y
      :initarg :y)
   (color :accessor piece-color
	  :initarg :color)))

;;; Attempt to move the chain using anchor BLK that is to be moved to
;;; position XN YN.
(defgeneric piece-move-chain (board chain blk xn yn))

;;; Paint a piece BLK on WINDOW.  PREV and NEXT pieces are given as
;;; helpers.
(defgeneric piece-paint (prev blk next window cache))

;;; Return a surface for piece BLK.
(defgeneric piece-surface (blk cache))

(defmethod piece-move-chain (board chain (blk piece) xn yn)
  nil)

(defmethod piece-project ((blk piece))
  blk)

(defmethod piece-paint (prev (cur piece) next window cache)
  (surface-paint (piece-surface cur cache) window
		 (piece-x cur) (piece-y cur))

  (let ((color (piece-color cur)))
    (when color
      (surface-paint (tile-cache-get cache (tile-for-color color))
		     window (piece-x cur) (piece-y cur)))))


(defclass clamped-piece (piece) ())

(defmethod piece-paint :after (prev (cur clamped-piece) next window cache)
  (let* ((clamp-up (tile-cache-get cache "clamp-up"))
	 (clamp-down (tile-cache-get cache "clamp-down"))
	 (clamp-left (tile-cache-get cache "clamp-left"))
	 (clamp-right (tile-cache-get cache "clamp-right")))

    (labels ((paint-clamp (cur other dx dy clamp)
			  (let* ((blk-x (piece-x cur))
				 (blk-y (piece-y cur))
				 (other-x (piece-x other))
				 (other-y (piece-y other))
				 (rdx (- other-x blk-x))
				 (rdy (- other-y blk-y)))
			    (when (and (= dx rdx) (= dy rdy))
			      (surface-paint clamp window
					     (piece-x cur) (piece-y cur))))))
      (loop for other in (list prev next)
	    do (unless (null other)
		 (paint-clamp cur other 1 0 clamp-right)
		 (paint-clamp cur other 0 1 clamp-down)
		 (paint-clamp cur other -1 0 clamp-left)
		 (paint-clamp cur other 0 -1 clamp-up))))))

(defclass plain-piece (clamped-piece) ())

(defmethod piece-surface ((blk plain-piece) cache)
  (tile-cache-get cache "tile"))

(defclass puller-piece (clamped-piece) ())

(defmethod piece-surface ((blk puller-piece) cache)
  (tile-cache-get cache "puller"))

(defmethod piece-move-chain (board chain (blk puller-piece) xn yn)
  (unless (board-has-block-at board xn yn)
    (hento-chain-pull chain blk xn yn)))


(defclass pusher-piece (clamped-piece) ())

(defmethod piece-surface ((blk pusher-piece) cache)
  (tile-cache-get cache "pusher"))

(defmethod piece-move-chain (board chain (blk pusher-piece) xn yn)
  (multiple-value-bind (other-chain other-blk) (board-has-block-at board xn yn)
    (if (null other-chain)
	(hento-chain-pull chain blk xn yn)

      ;; We can only push our own chain...
      (when (eq other-chain chain)
	;; ...and only if the collision block is our direct neighbor.
	(let ((my-pos (position blk (hento-chain-blocklist chain)))
	      (other-pos (position other-blk
				   (hento-chain-blocklist chain))))

	  ;; Push is like pull of the pushed head in the direction
	  ;; of the last chain link.
	  (when (= (abs (- my-pos other-pos)) 1)
	    ;; Turn the chain such that the new head is in the
	    ;; beginning.
	    (let ((blocklist (hento-chain-blocklist chain)))
	      (when (> other-pos my-pos)
		(setf (hento-chain-blocklist chain) (nreverse blocklist))))
	    (let* ((lst (hento-chain-blocklist chain))
		   (blk (car lst))
		   (nxt (cadr lst))
		   (dx (- (piece-x blk) (piece-x nxt)))
		   (dy (- (piece-y blk) (piece-y nxt))))
	      ;; If there's a discontinuity, assume that what happens
	      ;; is that the chain is in the process of being pulled
	      ;; out of the loading area.
	      (when (or (< dx -1) (> dx 1) (< dy -1) (> dy 1))
		(setf dx 1)
		(setf dy 0))
	      (let ((xn (+ (piece-x blk) dx))
		    (yn (+ (piece-y blk) dy)))
		(unless (board-has-block-at board xn yn)
		  (hento-chain-pull chain blk xn yn))))))))))

(defclass nonproj-piece (piece) ())

(defmethod piece-project ((blk nonproj-piece))
  nil)

(defclass brick-piece (nonproj-piece) ())

(defmethod piece-surface ((blk brick-piece) cache)
  (tile-cache-get cache "brick"))


(defclass wooden-piece (nonproj-piece) ())

(defmethod piece-surface ((blk wooden-piece) cache)
  (tile-cache-get cache "wood"))


(defun piece-at-p (blk x y)
  (and (= x (piece-x blk))
       (= y (piece-y blk))))

(defstruct hento-chain
  blocklist)

(defun hento-chain-has-block-at (chain x y)
  (let ((blocklist (hento-chain-blocklist chain)))
    (labels ((block-at (lst)
		       (if (null lst) nil
			 (if (piece-at-p (car lst) x y)
			     (car lst)
			   (block-at (cdr lst))))))
      (block-at blocklist))))

(defun map-neighbors (f l)
  (do* ((pprev NIL prev)
	(prev NIL (car lst))
	(lst l (cdr lst))
	(ret NIL (push (funcall f pprev prev (car lst)) ret)))
      ((null (car lst)) (nreverse ret))))

(defun hento-chain-map (f chain)
  (mapcar f (hento-chain-blocklist chain)))

(defun hento-chain-paint (chain window cache)
  (map-neighbors #'(lambda (prev cur next)
		     (piece-paint prev cur next window cache))
		 (hento-chain-blocklist chain)))

(defun chain-remove-piece (chain piece)
  (setf (hento-chain-blocklist chain)
	(delete-if #'(lambda (other) (eq piece other))
		   (hento-chain-blocklist chain))))

(defun hento-chain-pull (chain blk x y)
  (let ((blocklist (hento-chain-blocklist chain)))
    (unless (eq blk (car blocklist))
      (setf (hento-chain-blocklist chain) (nreverse blocklist))))
  (labels ((shift (lst x y)
	     (unless (null lst)
	       (let* ((blk (car lst))
		      (x0 (piece-x blk))
		      (y0 (piece-y blk)))
		 (when (or (/= x x0) (/= y y0))
		   (setf (piece-x blk) x)
		   (setf (piece-y blk) y)
		   (shift (cdr lst) x0 y0))))))
    (shift (hento-chain-blocklist chain) x y)))

(defun random-choice (lst)
  (elt lst (random (length lst))))

(defun random-color ()
  (let ((colors '(:red :green :blue :white))); :orange :gray NIL)))
    (random-choice colors)))

(defun random-color-list (len)
  (loop for i below len collecting (random-color)))

(defun layout-list (lst xys dx dy)
  (do* ((xys xys (cdr xys))
	(lst lst (cdr lst))
	(xy (car xys) (if xys (car xys) (list (+ x dx) (+ y dy))))
	(x (first xy) (first xy))
	(y (second xy) (second xy))
	(chain))

      ((null lst) (nreverse chain))

    (push (list (car lst) x y) chain)))

(defun make-hento-chain-from-layout (lst)
  (labels ((puller-chain-piece (i len)
	     (declare (ignorable len))
	     (if (zerop i) 'puller-piece 'plain-piece))
	   (pusher-chain-piece (i len)
	     (declare (ignorable len))
	     (if (zerop i) 'pusher-piece 'plain-piece))
	   (double-puller-chain-piece (i len)
	     (if (or (zerop i) (= i (1- len)))
		 'puller-piece 'plain-piece)))
    (let* ((templates (list #'puller-chain-piece
			    #'pusher-chain-piece
			    #'double-puller-chain-piece))
	   (template (random-choice templates))
	   (len (length lst))
	   (pcs (loop for emt in lst
		      for i by 1
		      collecting (destructuring-bind (color x y) emt
				   (make-instance (funcall template i len)
						  :color color :x x :y y)))))
      (make-hento-chain :blocklist pcs))))


(defstruct board
  w h tw th
  cx cy
  chains)

(defun board-tx (board mx)
  (floor (/ mx (board-tw board))))

(defun board-ty (board my)
  (floor (/ my (board-th board))))

(defun board-add-chain (board chain)
  (push chain (board-chains board)))

(defun board-has-block-at (board x y)
  (dolist (chain (board-chains board))
    (let ((piece (hento-chain-has-block-at chain x y)))
      (when piece
	(return (values chain piece))))))

(defun board-proximity-block (board mx my)
  (let* ((x (board-tx board mx))
	 (y (board-ty board my)))
    (board-has-block-at board x y)))

(defun board-paint (board window cache)
  (let ((bg (tile-cache-get cache "background")))
    (sdl:draw-surface-at-* bg 0 0 :surface window))

  (let* ((port (tile-cache-get cache "port"))
	 (tw (board-tw board))
	 (th (board-th board))
	 (x (- (* (board-cx board) tw)
	       (floor (/ (- (sdl:width port) tw) 2))))
	 (y (- (* (board-cy board) (board-th board))
	       (floor (/ (- (sdl:height port) th) 2)))))
    (sdl:draw-surface-at-* port x y :surface window))

  (mapcar #'(lambda (chain)
	      (hento-chain-paint chain window cache))
	  (board-chains board)))

(defun board-move-chain (board chain blk mx my)
  (labels ((bound-dir (d)
	     (cond ((> d 1) 1)
		   ((< d -1) -1)
		   (T d))))
    (let* ((x (board-tx board mx))
	   (y (board-ty board my))
	   (x0 (piece-x blk))
	   (y0 (piece-y blk))
	   (dx (bound-dir (- x x0)))
	   (dy (bound-dir (- y y0)))
	   (xn (+ x0 dx))
	   (yn (if (/= dx 0) y0 (+ y0 dy))))
      (when (or (/= x0 xn) (/= y0 yn))
	(piece-move-chain board chain blk xn yn)))))

(defun board-add-initial-chain (board)
  (let ((cx (board-cx board))
	(cy (board-cy board)))
    (unless (board-has-block-at board cx cy)
      (let* ((len (+ 3 (random 4)))
	     (chain (make-hento-chain-from-layout
		     (layout-list (random-color-list len)
				  (list (list cx cy)
					(list 1 (- (board-h board) 2)))
				  1 0))))
	(board-add-chain board chain)))))

(defun board-project (board)
  "Project pieces in chain to a 2D array and return it.  Each cell of
  the array contains either nil or a piece."
  (let* ((w (board-w board))
	 (h (board-h board))
	 (arr (make-array (list w h) :initial-element nil)))
    (loop for chain in (board-chains board)
	  do (hento-chain-map #'(lambda (piece)
				  (let ((piece (piece-project piece)))
				    (when piece
				      (setf (aref arr (piece-x piece)
						  (piece-y piece))
					    piece))))
			      chain))
    arr))

(defun board-remove-piece (board piece)
  (let ((x (piece-x piece))
	(y (piece-y piece)))
    (multiple-value-bind (chain piece) (board-has-block-at board x y)
      (when chain
	(chain-remove-piece chain piece)))))

(defun score-of (group)
  (let ((l (length group)))
    (* l l)))

(defun board-explode (board color)
  (labels ((uniq-enque (que seen x y)
		       (let ((xy (cons x y)))
			 (if (member xy seen :test 'equal)
			     que
			   (cons xy que)))))
    (let ((arr (board-project board))
	  (seen)
	  (score 0))
      (dotimes (x (board-w board))
	(dotimes (y (board-h board))
	  ;;(format t "~A considering ~Ax~A seen ~A~%" color x y (length seen))
	  (unless (member (cons x y) seen :test 'equal)
	    (do ((que (list (cons x y)))
		 (group))

		((null que)
		 (when (>= (length group) 3)
		   (dolist (piece group)
		     (board-remove-piece board piece))
		   (incf score (score-of group))))

	      (let ((xy (pop que)))
		(push xy seen)
		(destructuring-bind (x . y) xy
		  (let ((piece (aref arr x y)))
		    (when piece
		      (when (eq (piece-color piece) color)
			;;(format t "~A found ~A~%" color piece)
			(push piece group)
			;; We need to consider all surrouding
			;; directions, even up, to handle *-pieces in
			;; shapes like this:  #  *
			;;                   *####
			(setf que (uniq-enque que seen x (1- y)))
			(setf que (uniq-enque que seen x (1+ y)))
			(setf que (uniq-enque que seen (1- x) y))
			(setf que (uniq-enque que seen (1+ x) y)))))))))))
      score)))

(defun open-window (bw bh)
  (let ((tile (sdl:load-image "tile.png")))
    (sdl:window (* (sdl:width tile) bw)
		(* (sdl:height tile) bh)
		:bpp 32
		:title-caption "hento"
		:fullscreen NIL
		:double-buffer T
		:sw T)))

(defstruct clock
  prev)

(defun clock-delta (clock)
  (let ((now (get-internal-real-time)))
    (float (/ (- now (clock-prev clock))
	      internal-time-units-per-second))))

(defun clock-reset (clock)
  (let ((now (get-internal-real-time)))
    (setf (clock-prev clock) now)))


(defstruct score-display
  (score 0)
  (shown 0))

(defun score-inc (score add)
  (incf (score-display-score score) add))

(defun score-tick (score)
  (when (< (score-display-shown score) (score-display-score score))
    (incf (score-display-shown score))))


(sdl:with-init
 (sdl:sdl-init-video sdl:sdl-init-audio sdl:sdl-init-noparachute)

 (let* ((board-w 11)
	(board-h 13)
	(window (open-window board-w board-h))
	(cache (make-tile-cache))
	(tile (tile-cache-get cache "tile"))
	(board (make-board
		:tw (sdl:width tile) :th (sdl:height tile)
		:cx 5 :cy 5
		:w board-w :h board-h))
	(hold nil)
	(next-color (random-color))
	(game-over nil)
	(clock (make-clock))
	(score (make-score-display)))

   (labels ((add-edge (x0 y0 dx dy steps &optional (cls 'brick-piece))
	      (do ((i 0 (1+ i))
		   (x x0 (+ x dx))
		   (y y0 (+ y dy))
		   (chain NIL))
		  ((= i steps)
		   (board-add-chain board
				    (make-hento-chain :blocklist chain)))
		(push (make-instance cls :x x :y y :color NIL)
		      chain))))
     (add-edge 0 0 +1 +0 board-w)
     (add-edge 0 (- board-h 3) +1 +0 board-w)
     (add-edge 0 (1- board-h) +1 +0 board-w)
     (add-edge 0 1 0 +1 (- board-h 2))
     (add-edge (1- board-w) 1 0 +1 (- board-h 2)))

   (clock-reset clock)
   (board-add-initial-chain board)

   (sdl:clear-display sdl:*black*) ;(sdl:color :r 255 :g 255 :b 255)
   (sdl:update-display)

   (sdl:with-events ()
     (:quit-event () t)

     (:mouse-button-down-event (:x mousex :y mousey :button btn)
       (when (= btn sdl:sdl-button-left)
	 (multiple-value-bind (chain piece)
	     (board-proximity-block board mousex mousey)
	   (when chain
	     (setf hold (cons chain piece))))))

     (:key-down-event (:key key)
       (cond ((sdl:key= key :sdl-key-escape)
	      (sdl:push-quit-event))))

     (:mouse-button-up-event ()
       (setq hold nil))

     (:mouse-motion-event (:x mousex :y mousey)
       (unless (null hold)
	 (destructuring-bind (chain . blk) hold
	   (board-move-chain board chain blk mousex mousey))))

     (:idle ()
       (sdl:clear-display sdl:*black*)
       (score-tick score)
       (let ((clk (clock-delta clock)))
	 (when (> clk 10)
	   (if (board-has-block-at board
				   (board-cx board)
				   (board-cy board))
	       (setf game-over t)
	     (progn
	       (board-explode board next-color)
	       (clock-reset clock)
	       (board-add-initial-chain board)
	       (setf next-color (random-color)))))

	 (board-paint board window cache)
	 (let* ((gauge (tile-cache-get cache "gauge-base"))
		(meter (tile-cache-get cache "gauge-meter"))
		(bead (tile-cache-get cache (tile-for-color next-color)))
		(w (sdl:width gauge))
		(ww (round (/ (* w clk) 10)))
		(h (sdl:height gauge))
		(y0 (- 16 (floor (/ h 2)))))

	   (unless (null gauge)
	     (sdl:draw-surface-at-* gauge y0 y0 :surface window))

	   (unless (null meter)
	     (sdl:set-clip-rect (sdl:rectangle :x y0 :y y0 :w ww :h h)
				:surface window)
	     (sdl:blit-surface meter window)
	     ;;(sdl:draw-surface-at-* gauge y0 y0 :surface window)
	     (sdl:clear-clip-rect window))

	   (surface-paint bead window 3 0)
	   (sdl:update-display)))))))

(quit)
