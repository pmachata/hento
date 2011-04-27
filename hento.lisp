;(asdf:operate 'asdf:load-op :lispbuilder-sdl)
;(asdf:operate 'asdf:load-op :lispbuilder-sdl-ttf)
(ql:quickload "lispbuilder-sdl")
(ql:quickload "lispbuilder-sdl-ttf")

(defclass piece ()
  ((x :accessor piece-x
      :initarg :x)
   (y :accessor piece-y
      :initarg :y)
   (color :accessor piece-color
	  :initarg :color)))

(defmethod piece-surface ((blk piece) cache)
  (tile-cache-get cache "tile"))

(defmethod piece-move-chain (board chain (blk piece) xn yn)
  nil)


(defclass puller-piece (piece) ())

(defmethod piece-surface ((blk puller-piece) cache)
  (tile-cache-get cache "puller"))

(defmethod piece-move-chain (board chain (blk puller-piece) xn yn)
  (unless (hento-board-has-block-at board xn yn)
    (hento-chain-pull chain blk xn yn)))


(defclass pusher-piece (piece) ())

(defmethod piece-surface ((blk pusher-piece) cache)
  (tile-cache-get cache "pusher"))

(defmethod piece-move-chain (board chain (blk pusher-piece) xn yn)
  (let ((other (hento-board-has-block-at board xn yn)))
    (if (null other)
	(hento-chain-pull chain blk xn yn)

      ;; We can only push our own chain...
      (when (eq (car other) chain)
	;; ...and only if the collision block is our direct neighbor.
	(let ((my-pos (position blk (hento-chain-blocklist chain)))
	      (other-pos (position (cdr other)
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
		   (dy (- (piece-y blk) (piece-y nxt)))
		   (xn (+ (piece-x blk) dx))
		   (yn (+ (piece-y blk) dy)))
	      (unless (hento-board-has-block-at board xn yn)
		(hento-chain-pull chain blk xn yn)))))))))

(defclass brick-piece (piece) ())

(defmethod piece-surface ((blk brick-piece) cache)
  (tile-cache-get cache "brick"))


(defun piece-at-p (blk x y)
  (and (= x (piece-x blk))
       (= y (piece-y blk))))

(defun piece-paint (prev cur next window cache)
  (let* ((tile (piece-surface cur cache))
	 (clamp-up (tile-cache-get cache "clamp-up"))
	 (clamp-down (tile-cache-get cache "clamp-down"))
	 (clamp-left (tile-cache-get cache "clamp-left"))
	 (clamp-right (tile-cache-get cache "clamp-right"))
	 (tw (sdl:width tile))
	 (th (sdl:height tile))
	 (x0 (piece-x cur))
	 (y0 (piece-y cur))
	 (mx0 (* x0 tw))
	 (my0 (* y0 th)))

    (sdl:draw-surface-at-* tile mx0 my0 :surface window)

    (labels ((paint-clamp (cur other dx dy clamp)
	       (let* ((blk-x (piece-x cur))
		      (blk-y (piece-y cur))
		      (other-x (piece-x other))
		      (other-y (piece-y other))
		      (rdx (- other-x blk-x))
		      (rdy (- other-y blk-y)))
		 (when (and (= dx rdx) (= dy rdy))
		   (sdl:draw-surface-at-* clamp mx0 my0 :surface window)))))
      (loop for other in (list prev next)
	    do (unless (null other)
		 (paint-clamp cur other 1 0 clamp-right)
		 (paint-clamp cur other 0 1 clamp-down)
		 (paint-clamp cur other -1 0 clamp-left)
		 (paint-clamp cur other 0 -1 clamp-up))))

    (let ((color (piece-color cur)))
      (when color
	(let ((surface (tile-cache-get cache (string-downcase (string color)))))
	  (sdl:draw-surface-at-* surface mx0 my0
				 :surface window))))))

(defstruct hento-chain
  blocklist)

(defun hento-chain-has-block-at (chain x y)
  (let* ((blocklist (hento-chain-blocklist chain)))
    (labels ((block-at (lst)
	       (if (null lst) NIL
		 (if (piece-at-p (car lst) x y)
		     (cons chain (car lst))
		   (block-at (cdr lst))))))
      (block-at blocklist))))

(defun map-neighbors (f l)
  (do* ((pprev NIL prev)
	(prev NIL (car lst))
	(lst l (cdr lst))
	(ret NIL (push (funcall f pprev prev (car lst)) ret)))
      ((null (car lst)) (nreverse ret))))

(defun hento-chain-paint (chain window cache)
  (map-neighbors #'(lambda (prev cur next)
		     (piece-paint prev cur next window cache))
		 (hento-chain-blocklist chain)))

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

(defun make-hento-chain-colors (lst x0 y0 dx dy)
  (do* ((x x0 (+ x dx))
	(y y0 (+ y dy))
	(i 0 (1+ i))
	(chain))

      ((= i (length lst)) (make-hento-chain :blocklist chain))

    (let* ((cls (if (> i 0)
		    'piece
		  (if (= 0 (random 5))
		      'pusher-piece
		    'puller-piece)))
	   (blk (make-instance cls :x x :y y :color (elt lst i))))
      (push blk chain))))

(defun make-hento-chain-random (len x0 y0 dx dy)
  (let ((colors '(:white :gray :red :green :blue :orange)))
    (make-hento-chain-colors (loop for i below len
				   collecting (elt colors
						   (random (length colors))))
			     x0 y0 dx dy)))

(defstruct hento-board
  w h tw th
  chains)

(defun hento-board-tx (board mx)
  (floor (/ mx (hento-board-tw board))))

(defun hento-board-ty (board my)
  (floor (/ my (hento-board-th board))))

(defun hento-board-add-chain (board chain)
  (setf (hento-board-chains board)
	(cons chain (hento-board-chains board))))

(defun hento-board-has-block-at (board x y)
  (labels ((find-chain (chains)
	     (if (null chains) NIL
	       (let ((ret (hento-chain-has-block-at (car chains) x y)))
		 (if (null ret) (find-chain (cdr chains))
		   ret)))))
    (find-chain (hento-board-chains board))))

(defun hento-board-proximity-block (board mx my)
  (let* ((x (hento-board-tx board mx))
	 (y (hento-board-ty board my)))
    (hento-board-has-block-at board x y)))

(defun hento-board-paint (board window cache)
  (let ((bg (tile-cache-get cache "background")))
    (sdl:draw-surface-at-* bg 0 0 :surface window))

  (let* ((port (tile-cache-get cache "port"))
	 (x (- (floor (/ (sdl:width window) 2))
	       (floor (/ (sdl:width port) 2))))
	 (y (- (floor (/ (sdl:height window) 2))
	       (floor (/ (sdl:height port) 2)))))
    (sdl:draw-surface-at-* port x y :surface window))

  (mapcar #'(lambda (chain)
	      (hento-chain-paint chain window cache))
	  (hento-board-chains board)))

(defun hento-board-move-chain (board chain blk mx my)
  (labels ((bound-dir (d)
	     (cond ((> d 1) 1)
		   ((< d -1) -1)
		   (T d))))
    (let* ((x (hento-board-tx board mx))
	   (y (hento-board-ty board my))
	   (x0 (piece-x blk))
	   (y0 (piece-y blk))
	   (dx (bound-dir (- x x0)))
	   (dy (bound-dir (- y y0)))
	   (xn (+ x0 dx))
	   (yn (if (/= dx 0) y0 (+ y0 dy))))
      (when (or (/= x0 xn) (/= y0 yn))
	(piece-move-chain board chain blk xn yn)))))

(defstruct tile-cache
  tiles)

(defun tile-cache-get (cache key)
  (let ((a (assoc key (tile-cache-tiles cache) :test 'string=)))
    (when (null a)
      (setq a (cons key
		    (sdl::convert-to-display-format
		     :surface (sdl:load-image (concatenate 'string key ".png"))
		     :pixel-alpha t)))
      (push a (tile-cache-tiles cache))
      (format T "caching tile ~S: ~S~%" key a))
    (cdr a)))

(defun open-window (bw bh)
  (let ((tile (sdl:load-image "tile.png")))
    (sdl:window (* (sdl:width tile) bw)
		(* (sdl:height tile) bh)
		:bpp 32
		:title-caption "hento"
		:fullscreen NIL
		:double-buffer T
		:sw T)))

(sdl:with-init
 (sdl:sdl-init-video sdl:sdl-init-audio sdl:sdl-init-noparachute)

 (let* ((board-w 13)
	(board-h 13)
	(window (open-window board-w board-h))
	(cache (make-tile-cache))
	(tile (tile-cache-get cache "tile"))
	(board (make-hento-board
		:tw (sdl:width tile)
		:th (sdl:height tile)
		:w board-w :h board-h))
	(hold NIL))

   (labels ((add-edge (x0 y0 dx dy steps)
	      (do ((i 0 (1+ i))
		   (x x0 (+ x dx))
		   (y y0 (+ y dy))
		   (chain NIL))
		  ((= i steps)
		   (hento-board-add-chain board
					  (make-hento-chain :blocklist chain)))
		(push (make-instance 'brick-piece
				     :x x :y y :color NIL)
		      chain))))
     (add-edge 0 0 +1 +0 board-w)
     (add-edge 0 (1- board-h) +1 +0 board-w)
     (add-edge 0 1 0 +1 (- board-h 2))
     (add-edge (1- board-w) 1 0 +1 (- board-h 2)))

   (sdl:clear-display sdl:*black*) ;(sdl:color :r 255 :g 255 :b 255)
   (sdl:update-display)

   (sdl:with-events ()
     (:quit-event () t)

     (:mouse-button-down-event
      (:x mousex :y mousey :button btn)
      (let ((hld (hento-board-proximity-block board mousex mousey)))
	(unless (null hld)
	  (destructuring-bind (chain . blk) hld
	    (when (= btn sdl:sdl-button-left)
	      (setq hold hld))))))

     (:mouse-button-up-event
      ()
      (setq hold NIL))

     (:mouse-motion-event
      (:x mousex :y mousey)
      (unless (null hold)
	(destructuring-bind (chain . blk) hold
	  (hento-board-move-chain board chain blk mousex mousey))))

     (:idle ()
       (sdl:clear-display sdl:*black*)
       (let ((cx (floor (/ board-w 2)))
	     (cy (floor (/ board-h 2))))
	 (unless (hento-board-has-block-at board cx cy)
	   (let ((chain (make-hento-chain-random 5 cx cy 50 0)))
	     (hento-board-add-chain board chain))))
       (hento-board-paint board window cache)
       (sdl:update-display)))))

(quit)
