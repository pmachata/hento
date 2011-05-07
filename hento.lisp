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


(defstruct board
  w h tw th
  cx cy
  chains)

(defstruct chain
  blocklist)

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

;;; Return a piece that will be used for discovering explosion areas.
(defgeneric piece-project (blk))

;;; A hook called when piece enters the game
(defgeneric piece-materialized-hook (blk game))


(defun piece-at-p (blk x y)
  (and (= x (piece-x blk))
       (= y (piece-y blk))))

(defun chain-has-block-at (chain x y)
  (let ((blocklist (chain-blocklist chain)))
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

(defun chain-map (f chain)
  (mapcar f (chain-blocklist chain)))

(defun chain-paint (chain window cache)
  (map-neighbors #'(lambda (prev cur next)
		     (piece-paint prev cur next window cache))
		 (chain-blocklist chain)))

(defun list-paint (lst window cache)
  (mapcar #'(lambda (piece)
	      (piece-paint nil piece nil window cache))
	  lst))

(defun chain-remove-piece (chain piece)
  (setf (chain-blocklist chain)
	(delete-if #'(lambda (other) (eq piece other))
		   (chain-blocklist chain))))

(defun chain-pull (chain blk x y)
  (let ((blocklist (chain-blocklist chain)))
    (unless (eq blk (car blocklist))
      (setf (chain-blocklist chain) (nreverse blocklist))))
  (labels ((shift (lst x y)
	     (unless (null lst)
	       (let* ((blk (car lst))
		      (x0 (piece-x blk))
		      (y0 (piece-y blk)))
		 (when (or (/= x x0) (/= y y0))
		   (setf (piece-x blk) x)
		   (setf (piece-y blk) y)
		   (shift (cdr lst) x0 y0))))))
    (shift (chain-blocklist chain) x y))
  (values chain blk))

(defun random-choice (lst)
  (elt lst (random (length lst))))

(defun random-color ()
  (let ((colors '(:red :green :blue :white :orange))); :gray NIL)))
    (random-choice colors)))

(defun color-to-sdl (color)
  (ecase color
    (:red (sdl:color :r 255 :g 0 :b 0))
    (:green (sdl:color :r 0 :g 255 :b 0))
    (:blue (sdl:color :r 0 :g 0 :b 255))
    (:white (sdl:color :r 255 :g 255 :b 255))
    (:orange (sdl:color :r 255 :g 127 :b 0))
    (:gray (sdl:color :r 127 :g 127 :b 127))))

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

(defun make-chain-from-layout (lst)
  (labels ((puller-chain-piece (i len)
	     (declare (ignorable len))
	     (if (zerop i) 'puller-piece 'plain-piece))

	   (pusher-chain-piece (i len)
	     (declare (ignorable len))
	     (if (zerop i) 'pusher-piece 'plain-piece))

	   (double-puller-chain-piece (i len)
	     (if (or (zerop i) (= i (1- len)))
		 'puller-piece 'plain-piece))

	   (attacher-chain-piece (i len)
	     (declare (ignorable len))
	     (when (zerop i) 'attacher-piece))

	   (pause-chain-piece (i len)
	     (declare (ignorable len))
	     (when (zerop i) 'pause-piece)))

    (let* ((templates (list #'puller-chain-piece
			    #'pusher-chain-piece
			    #'double-puller-chain-piece
		            #'attacher-chain-piece
			    #'pause-chain-piece))
	   (template (random-choice templates))
	   (len (length lst))
	   (pcs (remove-if #'null
			   (loop for emt in lst
				 for i by 1
				 collecting (destructuring-bind (color x y) emt
					      (let ((cls (funcall template i len)))
						(when cls
						  (make-instance cls
								 :color color
								 :x x :y y))))))))
      (make-chain :blocklist (remove-if #'null pcs)))))


(defun board-tx (board mx)
  (floor (/ mx (board-tw board))))

(defun board-ty (board my)
  (floor (/ my (board-th board))))

(defun board-add-chain (board chain)
  (push chain (board-chains board)))

(defun board-has-block-at (board x y)
  (dolist (chain (board-chains board))
    (let ((piece (chain-has-block-at chain x y)))
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
	      (chain-paint chain window cache))
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
      (let* ((len (+ 2 (random 5)))
	     (chain (make-chain-from-layout
		     (layout-list (random-color-list len)
				  (list (list cx cy)
					(list (board-w board) 0))
				  1 0))))
	(board-add-chain board chain)))))

(defun board-project (board)
  "Project pieces in chain to a 2D array and return it.  Each cell of
  the array contains either nil or a piece."
  (labels ((initial-piece-p (piece)
			    (or (and (= (board-cx board) (piece-x piece))
				     (= (board-cy board) (piece-y piece)))
				(>= (piece-x piece) (board-w board))
				(< (piece-x piece) 0)
				(>= (piece-y piece) (board-h board))
				(< (piece-y piece) 0)))
	   (initial-chain-p (chain)
			    (some #'initial-piece-p
				  (chain-blocklist chain))))
    (let* ((w (board-w board))
	   (h (board-h board))
	   (arr (make-array (list w h) :initial-element nil)))
      (loop for chain in (board-chains board)
	    if (not (initial-chain-p chain))
	    do (chain-map #'(lambda (piece)
			      (let ((piece (piece-project piece)))
				(when piece
				  (setf (aref arr (piece-x piece)
					      (piece-y piece))
					piece))))
			  chain))
      arr)))

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
	  (score 0)
	  (exploded nil))
      (dotimes (x (board-w board))
	(dotimes (y (board-h board))
	  ;;(format t "~A considering ~Ax~A seen ~A~%" color x y (length seen))
	  (unless (member (cons x y) seen :test 'equal)
	    (do ((que (list (cons x y)))
		 (group))

		((null que)
		 (when (>= (length group) 3)
		   (dolist (piece group)
		     (board-remove-piece board piece)
		     (push piece exploded))
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
      (values score exploded))))



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
    (chain-pull chain blk xn yn)))


(defclass pusher-piece (clamped-piece) ())

(defmethod piece-surface ((blk pusher-piece) cache)
  (tile-cache-get cache "pusher"))

(defmethod piece-move-chain (board chain (blk pusher-piece) xn yn)
  (multiple-value-bind (other-chain other-blk) (board-has-block-at board xn yn)
    (if (null other-chain)
	(chain-pull chain blk xn yn)

      ;; We can only push our own chain...
      (when (eq other-chain chain)
	;; ...and only if the collision block is our direct neighbor.
	(let ((my-pos (position blk (chain-blocklist chain)))
	      (other-pos (position other-blk
				   (chain-blocklist chain))))

	  ;; Push is like pull of the pushed head in the direction
	  ;; of the last chain link.
	  (when (= (abs (- my-pos other-pos)) 1)
	    ;; Turn the chain such that the new head is in the
	    ;; beginning.
	    (let ((blocklist (chain-blocklist chain)))
	      (when (> other-pos my-pos)
		(setf (chain-blocklist chain) (nreverse blocklist))))
	    (let* ((lst (chain-blocklist chain))
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
		  (chain-pull chain blk xn yn))))

	    ;; Don't change the hold
	    (values chain blk)))))))


(defclass attacher-piece (clamped-piece) ())

(defmethod piece-surface ((blk attacher-piece) cache)
  (tile-cache-get cache "wood"))

(defmethod piece-move-chain (board chain (blk attacher-piece) xn yn)
  (multiple-value-bind (other-chain other-blk) (board-has-block-at board xn yn)
    (if (null other-chain)
	(chain-pull chain blk xn yn)
      ;; If the place is taken, and the other block is not ephemeral
      ;; (this is to disallow attaching to the border, but it may
      ;; later be changed if ephemeral blocks are needed apart from
      ;; border blocks), and it's one of the ends of the other block,
      ;; then attach.
      (unless (null (piece-project other-blk))
	(labels ((make-new-blk ()
			       (make-instance 'puller-piece
					      :color (piece-color blk)
					      :x (piece-x blk)
					      :y (piece-y blk))))
	  (cond ((eq other-blk
		     (car (chain-blocklist other-chain)))
		 (let ((nblk (make-new-blk)))
		   (push nblk (chain-blocklist other-chain))
		   (chain-remove-piece chain blk)
		   (values other-chain nblk)))

		((eq other-blk
		     (car (last (chain-blocklist other-chain))))
		 (let ((nblk (make-new-blk)))
		   (push nblk (cdr (last (chain-blocklist other-chain))))
		   (chain-remove-piece chain blk)
		   (values other-chain nblk)))))))))


(defclass pause-piece (clamped-piece) ())

(defmethod piece-surface ((blk pause-piece) cache)
  (tile-cache-get cache "pause"))

(defmethod piece-move-chain (board chain (blk pause-piece) xn yn)
  (multiple-value-bind (other-chain other-blk) (board-has-block-at board xn yn)
    (declare (ignorable other-blk))
    (when (null other-chain)
      (let ((nblk (make-instance 'puller-piece
				 :color (piece-color blk)
				 :x (piece-x blk)
				 :y (piece-y blk))))
	(setf (chain-blocklist chain) (list nblk))
	(chain-pull chain nblk xn yn)
	(values chain nblk)))))


(defclass nonproj-piece (piece) ())

(defmethod piece-project ((blk nonproj-piece))
  nil)

(defclass brick-piece (nonproj-piece) ())

(defmethod piece-surface ((blk brick-piece) cache)
  (tile-cache-get cache "brick"))


(defclass wooden-piece (nonproj-piece) ())

(defmethod piece-surface ((blk wooden-piece) cache)
  (tile-cache-get cache "wood"))


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


(defclass particle ()
  ((x :accessor particle-x
      :initarg :x)
   (y :accessor particle-y
      :initarg :y)
   (dx :accessor particle-dx
       :initarg :dx)
   (dy :accessor particle-dy
       :initarg :dy)))

(defgeneric particle-tick (particle))
(defgeneric particle-paint (particle window))

(defmethod particle-tick ((p particle))
  (incf (particle-x p) (particle-dx p))
  (incf (particle-y p) (particle-dy p)))

;; Particle influenced by gravity
(defclass gravity-particle (particle) ())

(defmethod particle-tick :after ((p gravity-particle))
  (incf (particle-dy p) 1))

(defclass debris-particle (gravity-particle)
  ((color :accessor debris-color
	  :initarg :color)))

(defmethod particle-paint ((p particle) window)
  (let ((x (particle-x p))
	(y (particle-y p)))
    (sdl:draw-rectangle-* (1- x) (1- y) 3 3
			  :surface window
			  :color (debris-color p))))

(defstruct particles
  (lst nil))

(defun particles-paint (particles window)
  (loop for p in (particles-lst particles)
	do (particle-paint p window))

  (labels ((outside (p) (let ((x (particle-x p))
			      (y (particle-y p)))
			  (or (< x -10)
			      (< y -10)
			      (> x (sdl:width window))
			      (> y (sdl:height window))))))
    (setf (particles-lst particles)
	  (delete-if #'outside (particles-lst particles)))))

(defun particles-tick (particles)
  (loop for p in (particles-lst particles)
	do (particle-tick p)))

(defun particle-add (particles p)
  (push p (particles-lst particles)))

(defun create-explosion (board particles piece)
  (let* ((tw (board-tw board))
	 (th (board-th board))
	 (x0 (* tw (piece-x piece))) ; edge x
	 (y0 (* th (piece-y piece))) ; edge y
	 (cw (floor (/ tw 3))) ; count w
	 (ch (floor (/ th 3))) ; count h
	 (cw2 (floor (/ cw 2)))
	 (ch2 (floor (/ ch 2))))
    (dotimes (x cw)
      (dotimes (y ch)
	(let ((p (make-instance 'debris-particle
				:x (+ x0 (* 3 x)) :dx (- x cw2)
				:y (+ y0 (* 3 y)) :dy (- y ch2)
				:color (color-to-sdl (piece-color piece)))))
	  (particle-add particles p))))))


(sdl:with-init
 (sdl:sdl-init-video sdl:sdl-init-audio sdl:sdl-init-noparachute)

 (let* ((board-w 13)
	(board-h 13)
	(window (open-window board-w board-h))
	(cache (make-tile-cache))
	(tile (tile-cache-get cache "tile"))
	(board (make-board
		:tw (sdl:width tile) :th (sdl:height tile)
		:cx 6 :cy 6
		:w board-w :h board-h))
	(hold nil)
	(next-color (random-color))
	(game-over nil)
	(clock (make-clock))
	(x-clock (make-clock))
	(score (make-score-display))
	(font (sdl:initialise-default-font sdl:*font-10x20*))
	(particles (make-particles))
	(explosion-que nil))

   (labels ((add-edge (x0 y0 dx dy steps &optional (cls 'brick-piece))
	      (do ((i 0 (1+ i))
		   (x x0 (+ x dx))
		   (y y0 (+ y dy))
		   (chain NIL))
		  ((= i steps)
		   (board-add-chain board
				    (make-chain :blocklist chain)))
		(push (make-instance cls :x x :y y :color NIL)
		      chain))))
     (add-edge 0 0 +1 +0 board-w)
     ;;(add-edge 0 (- board-h 3) +1 +0 board-w)
     (add-edge 0 (1- board-h) +1 +0 board-w)
     (add-edge 0 1 0 +1 (- board-h 2))
     (add-edge (1- board-w) 1 0 +1 (- board-h 2)))

   (clock-reset clock)
   (clock-reset x-clock)
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
       (setf hold nil))

     (:mouse-motion-event (:x mousex :y mousey)
       (unless (null hold)
	 (destructuring-bind (chain . blk) hold
	   (multiple-value-bind (chain blk)
	       (board-move-chain board chain blk mousex mousey)
	     (when chain
	       (setf hold (cons chain blk)))))))

     (:idle ()
       (sdl:clear-display sdl:*black*)
       (score-tick score)
       (particles-tick particles)
       (let ((clk (clock-delta clock)))
	 (when (> clk 5)
	   (if (board-has-block-at board
				   (board-cx board)
				   (board-cy board))
	       (progn
		 (setq game-over t)
		 (format t "GAME OVER~%"))

	     (multiple-value-bind (x-score x-ploded)
		 (board-explode board next-color)
	       (setf explosion-que (nconc explosion-que x-ploded))
	       (score-inc score x-score)
	       (board-add-initial-chain board)

	       ;; if what we are holding disappeared, drop it
	       (unless (null hold)
		 (let ((blk (cdr hold)))
		   (multiple-value-bind (chain piece)
		       (board-has-block-at board (piece-x blk) (piece-y blk))
		     (declare (ignorable chain))
		     (unless (eq piece blk)
		       (setf hold nil)))))

	       (clock-reset clock)
	       (setf next-color (random-color)))))

	 (board-paint board window cache)
	 (list-paint explosion-que window cache)
	 (let* ((gauge (tile-cache-get cache "gauge-base"))
		(meter (tile-cache-get cache "gauge-meter"))
		(bead (tile-cache-get cache (tile-for-color next-color)))
		(w (sdl:width gauge))
		(ww (round (/ (* w clk) 5)))
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

	   (surface-paint bead window 3 0)))

       (unless (null explosion-que)
	 (let ((clk (clock-delta x-clock)))
	   (when (> clk 0.05)
	     (clock-reset x-clock)
	     (let ((x-blk (pop explosion-que)))
	       (create-explosion board particles x-blk)))))

       (sdl:draw-string-solid-* (format nil "~A"
					(score-display-shown score))
				(- (sdl:width window) 10) 5
				:font font
				:surface window
				:color sdl:*white*
				:justify :right)
       (particles-paint particles window)
       (sdl:update-display)))))

(quit)
