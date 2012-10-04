;;;
;;; A simple graph layout thingy
;;;

; SVG might be a good approach to drawing graphics to the reports. I just 
; don't know it yet =) Gotta start with something simple. Something derived
; from this will probably be an important part of the next version of 
; platypus. 

(import lib-random)

(define graph-size-factor 2000) ; node private space

;;; from platypus.scm, share later
(define hex-digits '(48 49 50 51 52 53 54 55 56 57 97 98 99 100 101 102))

(define (hexen n tail)
   (cond
      ((< n 0) (hexen 0 tail))
      ((> n 255) (hexen 255 tail))
      (else
         (ilist
            (lref hex-digits (>> n 4))
            (lref hex-digits (band n #b11))
            tail))))

(define (rgb-strengths->color r g b) ; 0-100
   (bytes->string
      (cons 35
         (foldr
            (lambda (x tail)
               (hexen (div (* x 255) 100) tail))
            null
            (list r g b)))))

; n in -100 .. 100
(define (score-color n)
   (let
      ((r (div (- n 100) -2))
       (g (div (+ n 100) 2))
       (b 0)) 
      (rgb-strengths->color r g b)))

; n is 0 .. 100
(define (edge-color n)
	(score-color (* 2 (- 50 n))))

	

; graph = ff of node -> ((link-weight . node) ...)

; edges based on example from zvon.org

; define graph
(define (graph-prelude fd w h)
	(mail fd 
		(foldr render null
			(list 
"<svg width=\"" w "pt\" height=\"" h "pt\"
  viewBox = \"0 0 " w " " h "\"
  xmlns=\"http://www.w3.org/2000/svg\" 
  xmlns:xlink=\"http://www.w3.org/1999/xlink\">

<g id=\"graph\" class=\"graph\" style=\"fill:#000;font-family:Times-Roman;font-size:11.00;\">

<defs>
	<g id=\"arrowMarker\">
		<g stroke=\"black\" stroke-width=\"1\">
			<line x1=\"8\" y1=\"-2\" x2=\"3\" y2=\"0\"/>
			<line x1=\"9\" y1=\"+2\" x2=\"3\" y2=\"0\"/>
		</g>
	</g>
	<marker id=\"ee\" markerWidth=\"48\" markerHeight=\"24\" viewBox=\"-4 -4 25 5\" orient=\"auto\" refX=\"0\" refY=\"0\" markerUnits=\"strokeWidth\">
 	 	<g><use xlink:href=\"#arrowMarker\" transform=\"rotate(180)\" stroke-width=\"1\" stroke=\"black\"/></g>
	</marker>
</defs>

"))))

(define (graph-finale fd)
	(mail fd (render
"
</g>
</svg>
" null)))

(define (draw-node fd node id x y)
	(mail fd
		(foldr render null
			(list
"<g id=\"n" id "\" class=\"node\">
<ellipse style=\"fill:#afa;stroke:#8a8;\" cx=\"" x "\" cy=\"" y "\" rx=\"4\" ry=\"4\"/>
</g>
"))))

(define homeless "bug, unplaced node")

; use the node position as the unique identifier
(define (graph-draw-nodes fd graph poss)
	(ff-fold
		(lambda (nth node links)
			(lets 
				((location (get poss node homeless))
				 (x y location))
				(draw-node fd node nth x y)
				(+ nth 1)))
		0 graph))

(define (draw-edge fd from to weight id poss)
	;(show " drawing edge " (list from " -> " to))
	(lets
		((fromp (get poss from homeless))
		 (top   (get poss to   homeless))
		 (fx fy fromp)
		 (tx ty top))
		(mail fd
			(foldr render null
				(list
"<g id=\"e" id "\" class=\"edge\">
<path style=\"fill:none;stroke:" (edge-color weight) "\" d=\"M" fx "," fy "L" tx "," ty "\" marker-end=\"url(#ee)\"/>
</g>
")))))

(define (graph-draw-edges fd graph poss)
	(ff-fold
		(lambda (nth node links)
			(fold
				(lambda (nth link)
					(draw-edge fd node (cdr link) (car link) nth poss)
					(+ nth 1))
				nth links))
		0 graph))
		
(define (output-graph graph poss w h fd)
	(graph-prelude fd w h)
	(graph-draw-edges fd graph poss)
	(graph-draw-nodes fd graph poss)
	(graph-finale fd)
	(close-port fd)
)

(define (plot-nodes-randomly graph w h)
	(lets
		((nodes (map car (ff->list graph)))
		 (secs rst (clock))
		 (mid-w (div w 2))
		 (mid-h (div h 2)))
		(let loop ((nodes nodes) (poss False) (rst rst))
			(if (null? nodes)
				poss
				(lets
					((rst x (rand rst 20))
					 (rst y (rand rst 20)))
					(loop 
						(cdr nodes)
						(put poss (car nodes) 
							(cons (+ mid-w (- 10 x)) (+ mid-h (- 10 y))))
						rst))))))

; I'm not really sure how graph layouting usually works, or how well the 
; algorithms fit to a functional setting.. let's first try to make 
; something simple:

;;;
;;; Use the force
;;;

; vectors would be a proper solution, but I'll just first use
; (dx . dy) to represent the forces. vectors would need trigonometric 
; functions, which for now would be approximated/precomputed using 
; Taylor series...

(define (distance p1 p2)
	(lets
		((x1 y1 p1)
		 (x2 y2 p2)
		 (d1 (- x2 x1))
		 (d2 (- y2 y1)))
		(sqrt (+ (* d1 d1) (* d2 d2)))))

(define strong-force-threshold 20)

(define max-strong-force 100)

(define (strong-scalar dist)
	(if (< dist 2)
		max-strong-force
		(min max-strong-force
			(/ 1 (/ strong-force-threshold dist)))))
	
; -> (dx . dy) affecting p1, obviously symmetric, but for now
(define (strong-force p1 p2) 
	(let ((d (distance p1 p2)))
		;(show "dist is " d)
		(if (< d strong-force-threshold)
			(lets 
				((x y p1)
				 (xp yp p2)
				 (dx (- 0 (- xp x))) ; mirror dir (non-normalized)
				 (dy (- 0 (- yp y)))
				 (f (strong-scalar d)))
				(cons
					(floor (* dx f))
					(floor (* dy f))))
			'(0 . 0))))
				
(define (sum f1 f2)
	(lets
		((dx1 dy1 f1)
		 (dx2 dy2 f2))
		(cons (+ dx1 dx2) (+ dy1 dy2))))

(define (weak-force p1 p2 strength)
	(let ((d (distance p1 p2)))
		(cond
			((= d 0)
				'(0 . 0)) ; just repulsion using charge
			((eq? strength 0)
				'(0 . 0))
			(else
				(lets 
					((x y p1)
					 (xp yp p2)
					 (dx (- xp x))
					 (dy (- yp y))
					 (optimal-dir (+ 30 (/ 200 strength)))
					 (f (/ (- d optimal-dir) (expt (/ d 4) 2))))
					(cons
						(floor (* dx f))
						(floor (* dy f))))))))

(define (compute-strong-forces graph poss)
	(ff-fold
		(lambda (forces n1 p1)
			(ff-fold
				(lambda (forces n2 p2)
					(if (eq? n1 n2)
						forces
						(put forces n1
							(sum
								(get forces n1 '(0 . 0))
								(strong-force p1 p2)))))
				forces poss))
		False poss))

(define (add-weak-forces graph poss forces)
	(ff-fold
		(lambda (forces n1 links)
			(fold
				(lambda (forces link)
					(lets ((str n2 link))
						(put forces n1
							(sum
								(get forces n1 '(0 . 0))
								(weak-force 
									(get poss n1 'bug)
									(get poss n2 'bug)
									str)))))
				forces links))
		forces graph))

(define (compute-forces graph poss)
	(lets
		((forces (compute-strong-forces graph poss))
		 (forces (add-weak-forces graph poss forces)))
		forces))

(define (bound a x b)
	(cond
		((< x a) a)
		((> x b) b)
		(else x)))

(define (apply-forces forces poss w h)
	(ff-fold
		(lambda (poss node force)
			(put poss node
				(lets
					((new (sum force (get poss node 'bug)))
					 (x y new))
					(cons (bound 10 x (- w 10)) (bound 10 y (- h 10))))))
					
		poss forces))

; grap x poss -> poss'
(define (sproing graph poss w h)
	(apply-forces
		(compute-forces graph poss)
		poss w h))

(define (num->string i)
	(bytes->string
		(map (lambda (x) (+ x 48))
			(let loop ((i i) (out null))
				(if (= i 0) 
					(if (null? out) (list 0) out)
					(loop (div i 10) (cons (rem i 10) out)))))))

(define (pad i)
	(cond
		((< i 10) "00000")
		((< i 100) "0000")
		((< i 1000) "000")
		((< i 10000) "00")
		((< i 100000) "0")
		(else (error "too big lol: " i))))

(define (name i)
	(foldr string-append ""
		(list "/tmp/level-" (pad i) (num->string i) ".svg")))

(define (plot-nodes graph w h)
	(print "tallyho:")
	(let ((poss (plot-nodes-randomly graph w h)))
		(let loop ((poss poss) (n 0))
			(show " sproing " n)
			(output-graph graph poss w h 
				(open-output-file (name n)))
			(if (= n 100)
				poss
				(loop (sproing graph poss w h) (+ n 1))))))

(define (render-graph graph path)
	(call/cc
		(lambda (fail)
			(lets
				((fail 
					(lambda (reason) (show "error rendering graph: " reason) False))
				 (fd (or (open-output-file path) (fail "could no open file")))
				 (n-nodes (ff-fold (lambda (n k v) (+ n 1)) 0 graph))
				 (graph-height (max 40 (sqrt (* n-nodes graph-size-factor)))) ; maybe
				 (graph-width (floor (* graph-height 1.618033))) ; certainly
				 (poss (plot-nodes graph graph-width graph-height)))
				(output-graph graph poss graph-width graph-height fd)))))

(define (random-graph size) 
	(lets
		((secs ms (clock))
		 (rst (+ ms (* secs 1000))))
		(let loop ((rst rst) (graph False) (node 0))
			(if (= node size)
				graph
				(lets
					((rst n-peers (rand rst 3))
					 (n-peers (+ n-peers 1))
					 (peers (random-numbers rst size n-peers))
					 (weights (random-numbers rst 100 n-peers)))
					(loop (rand-succ rst)
						(put graph node (zip cons weights peers))
						(+ node 1)))))))
			
(render-graph 
	;(list->ff
	;	'((a (50 . b) (50 . c))
	;	  (b (50 . d))
	;	  (c (50 . b))
	;	  (d)))
	(random-graph 10)
	"test.svg")






