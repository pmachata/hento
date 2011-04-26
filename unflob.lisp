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
    (hento-chain-move chain blk xn yn)))


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

(defun hento-chain-move (chain blk x y)
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
	(blks))

      ((= i (length lst))
       (make-hento-chain :blocklist blks))

    (let* ((cls (if (or (= i 0) (= i (1- (length lst))))
		    'puller-piece 'piece))
	   (blk (make-instance cls :x x :y y :color (elt lst i))))
      (push blk blks))))

(defun make-hento-chain-random (len x0 y0 dx dy)
  (let ((colors '(:white :gray :red :green :blue :orange)))
    (do ((i 0 (1+ i))
	 (lst NIL (push (elt colors
			     (random (length colors)))
			lst)))
	((= i len) (make-hento-chain-colors lst x0 y0 dx dy)))))

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
      (piece-move-chain board chain blk xn yn))))

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

   (when NIL
     (hento-board-add-chain board
			    (make-hento-chain-random 7 1 1 +1 0))
     (hento-board-add-chain board
			    (make-hento-chain-random 9 1 2 +1 0))
     (hento-board-add-chain board
			    (make-hento-chain-random 11 1 3 +1 0)))

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
      (:x mousex :y mousey)
      (let ((hld (hento-board-proximity-block board mousex mousey)))
	(unless (null hld)
	  (destructuring-bind (chain . blk) hld
	    (setq hold hld)
	    (format T "hold=~A~%" hold)))))

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
	   (let ((chain (make-hento-chain-random 5 cx cy 0 0)))
	     (hento-board-add-chain board chain))))
       (hento-board-paint board window cache)
       (sdl:update-display)))))

(quit)
