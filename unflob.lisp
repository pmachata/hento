;(asdf:operate 'asdf:load-op :lispbuilder-sdl)
;(asdf:operate 'asdf:load-op :lispbuilder-sdl-ttf)
(ql:quickload "lispbuilder-sdl")
(ql:quickload "lispbuilder-sdl-ttf")

(defstruct hento-font
  face
  w h)

(defstruct hento-block
  (x 0) (y 0)
  color)

(defun hento-block-at-p (blk x y)
  (and (= x (hento-block-x blk))
       (= y (hento-block-y blk))))

(defun hento-block-paint (prev blk next window font cache)
  (when NIL
    (let ((color (if (eq (hento-block-color blk) 'white)
		     sdl:*white*
		   (sdl:color :r 160 :g 160 :b 160))))))
  (let* ((tile (tile-cache-get cache "tile"))
	 (clamp-up (tile-cache-get cache "clamp-up"))
	 (clamp-down (tile-cache-get cache "clamp-down"))
	 (clamp-left (tile-cache-get cache "clamp-left"))
	 (clamp-right (tile-cache-get cache "clamp-right"))
	 (tw (sdl:width tile))
	 (th (sdl:height tile))
	 (x0 (hento-block-x blk))
	 (y0 (hento-block-y blk))
	 (mx0 (* x0 tw))
	 (my0 (* y0 th)))

    (sdl:draw-surface-at-* tile mx0 my0 :surface window)

    (labels ((paint-clamp (blk other dx dy clamp)
	       (let* ((blk-x (hento-block-x blk))
		      (blk-y (hento-block-y blk))
		      (other-x (hento-block-x other))
		      (other-y (hento-block-y other))
		      (rdx (- other-x blk-x))
		      (rdy (- other-y blk-y)))
		 (when (and (= dx rdx) (= dy rdy))
		   (sdl:draw-surface-at-* clamp mx0 my0 :surface window)))))
      (loop for other in (list prev next)
	    do (unless (null other)
		 (paint-clamp blk other 1 0 clamp-right)
		 (paint-clamp blk other 0 1 clamp-down)
		 (paint-clamp blk other -1 0 clamp-left)
		 (paint-clamp blk other 0 -1 clamp-up))))

    (let ((color (hento-block-color blk)))
      (when color
	(let ((surface (tile-cache-get cache (string-downcase (string color)))))
	  (sdl:draw-surface-at-* surface mx0 my0
				 :surface window))))

    (when NIL
      (labels ((center (coord t-size f-size)
		       (floor (- (+ coord (/ t-size 2)) (/ f-size 2)))))
	(sdl:draw-string-solid-* (hento-block-letter blk)
				 (center mx0 tw (hento-font-w font))
				 (center my0 th (hento-font-h font))
				 :color sdl:*black*
				 :font (hento-font-face font)
				 :surface window)))))

(defstruct hento-chain
  blocklist)

(defun hento-chain-has-block-at (chain x y)
  (let* ((blocklist (hento-chain-blocklist chain)))
    (labels ((block-at (lst)
	       (if (null lst) NIL
		 (if (hento-block-at-p (car lst) x y)
		     (cons chain (car lst))
		   (block-at (cdr lst))))))
      (block-at blocklist))))

(defun map-neighbors (f l)
  (do* ((pprev NIL prev)
	(prev NIL (car lst))
	(lst l (cdr lst))
	(ret NIL (push (funcall f pprev prev (car lst)) ret)))
      ((null (car lst)) (nreverse ret))))

(defun hento-chain-paint (chain window font cache)
  (map-neighbors #'(lambda (prev cur next)
		     (hento-block-paint prev cur next window font cache))
		 (hento-chain-blocklist chain)))

(defun hento-chain-move (chain blk x y)
  (let ((blocklist (hento-chain-blocklist chain)))
    (unless (eq blk (car blocklist))
      (setf (hento-chain-blocklist chain) (nreverse blocklist))))
  (labels ((shift (lst x y)
	     (unless (null lst)
	       (let* ((blk (car lst))
		      (x0 (hento-block-x blk))
		      (y0 (hento-block-y blk)))
		 (when (or (/= x x0) (/= y y0))
		   (setf (hento-block-x blk) x)
		   (setf (hento-block-y blk) y)
		   (shift (cdr lst) x0 y0))))))
    (shift (hento-chain-blocklist chain) x y)))

(defun make-hento-chain-colors (lst x0 y0 dx dy)
  (do* ((x x0 (+ x dx))
	(y y0 (+ y dy))
	(i 0 (1+ i))
	(blks))

      ((= i (length lst))
       (make-hento-chain :blocklist blks))

    (push (make-hento-block :x x :y y
			    :color (elt lst i))
	  blks)))

(defun make-hento-chain-random (len x0 y0 dx dy)
  (let ((colors '(:white :gray :red :green :blue :orange)))
    (do ((i 0 (1+ i))
	 (lst NIL (push (elt colors
			     (random (length colors)))
			lst)))
	((= i len) (make-hento-chain-colors lst x0 y0 dx dy)))))

(defstruct hento-board
  w h tw th
  font
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
  (mapcar #'(lambda (chain)
	      (hento-chain-paint chain window (hento-board-font board) cache))
	  (hento-board-chains board)))

(defun hento-board-move-chain (board chain blk mx my)
  (labels ((bound-dir (d)
	     (cond ((> d 1) 1)
		   ((< d -1) -1)
		   (T d))))
    (let* ((x (hento-board-tx board mx))
	   (y (hento-board-ty board my))
	   (x0 (hento-block-x blk))
	   (y0 (hento-block-y blk))
	   (dx (bound-dir (- x x0)))
	   (dy (bound-dir (- y y0)))
	   (xn (+ x0 dx))
	   (yn (if (/= dx 0) y0 (+ y0 dy))))
      (unless (hento-board-has-block-at board xn yn)
	(hento-chain-move chain blk xn yn)))))

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

(sdl:with-init
 (sdl:sdl-init-video sdl:sdl-init-audio sdl:sdl-init-noparachute)

 (let* ((window (sdl:window 640 480 :bpp 32
			    :title-caption "unflob"
			    :fullscreen NIL
			    :double-buffer T
			    :sw T))
	(cache (make-tile-cache))
	(tile (tile-cache-get cache "tile"))
	(font (make-hento-font
	       :face (sdl:initialise-default-font sdl:*font-10x20*)
	       :w 10 :h 20))
	(board (make-hento-board
		:font font
		:tw (sdl:width tile)
		:th (sdl:height tile)
		:w (floor (/ (sdl:width window) (sdl:width tile)))
		:h (floor (/ (sdl:height window) (sdl:height tile)))))
	(hold NIL)) ; (block . chain)

   (hento-board-add-chain board
			  (make-hento-chain-random 7 1 1 +1 0))
   (hento-board-add-chain board
			  (make-hento-chain-random 9 1 2 +1 0))
   (hento-board-add-chain board
			  (make-hento-chain-random 11 1 3 +1 0))

   ;(format T "board=~A~%" board)
   (format T "cache=~A~%" cache)

   (sdl:clear-display sdl:*black*) ;(sdl:color :r 255 :g 255 :b 255)
   (sdl:update-display)

   (sdl:with-events ()
     (:quit-event () t)

     (:mouse-button-down-event
      (:x mousex :y mousey)
      (let ((hld (hento-board-proximity-block board mousex mousey)))
	(unless (null hld)
	  (destructuring-bind (chain . blk) hld
	    (when (or (eq blk (car (hento-chain-blocklist chain)))
		      (eq blk (car (last (hento-chain-blocklist chain)))))
	      (setq hold hld)
	      (format T "hold=~A~%" hold))))))

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
       (hento-board-paint board window cache)
       (sdl:update-display)))))

(quit)
