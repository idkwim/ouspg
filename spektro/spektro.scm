;;;
;;; Spektro -- a simple tool to assist in analysis of mass spectrometry runs
;;;

;;; Todo
;
; - allow missing elements in series?
; - extra constraints?
; - could apply modular arithmetic
; - allowed ranges
; - remove syshalts
; - ask if charges are handled correctly for series
; - solution formatting code is ugly
;   + element format name,mass,charge[,min[,max]]
;   + how about in series? just apply to base elements and base molecule?
;

;;;
;;; Dependencies
;;;

(import lib-args)

;;;
;;; Utils
;;;

(define (syshalt)
	; wait for a while to let outputs to finish and then terminate
	; halt will be deprecated soon
	(fold * 1 (iota 1 1 1000))
	(halt 1))

;;;
;;; Single goal solver 
;;; 



; find matching factors to satisfy the mass and charge with given elems
;  success: (report factors n) -> n-1
;  failure: n

(define (find-factors mass charge elems moves report n)
	;(print (list 'find-factors 'mass mass 'charge charge 'elems elems 'moves moves))
	(cond
		((eq? n 0) n)
		((eq? mass 0) 
			(if (eq? charge 0) (report moves n) n))
		((null? elems) n)
		((< mass 0) n)
		(else
			(bind (car elems)
				(lambda (name this-mass this-charge min)
					(let try-factors ((count min) (n n))
						(let ((next-mass (- mass (* count this-mass))))
							(if (< next-mass 0)
								n
								(try-factors (+ count 1)
									(find-factors next-mass 
										(- charge (* count this-charge)) 
										(cdr elems)
										(cons (cons count name) moves)
										report n))))))))))

; why not in math.scm?

(define (abs x)
	(if (< x 0) (- 0 x) x))

; a solution is interesting if it is not a multiple of a smaller solution

(define (gcdl lst)
	(if (null? lst) 
		0
		(fold gcd (car lst) (cdr lst))))

(define (interesting? factors div)
	(= 1 (gcdl (cons div (map (o abs car) factors))))) 

(define (solve-goal mass charge elems n cook)
	(let loop ((divisor 1) (n n))
		(if (> n 0)
			(loop (+ divisor 1)
				(find-factors 
					(* mass divisor)
					(* charge divisor) 
					elems
					null
					(lambda (factors n)
						(cook mass charge factors divisor n))
					n))
			(begin
				(show "Enough goals computed for m/z " mass)
				0))))





;;;
;;; Interval finder
;;;

(define (try-interval this step masses picked)
	(let 
		((picked (cons this picked))
		 (next (+ this step)))
		(cond
			((get masses next False)
				(try-interval next step masses picked))
			((>= (length picked) 3)
				(reverse picked))
			(else False))))

(define (walk-intervals lst step masses max)
	(cond
		((= step max)
			null)
		((try-interval (car lst) step masses null) =>
			(lambda (matched)
				(cons
					(cons step matched)
					(walk-intervals lst (+ step 1) masses max))))
		(else
			(walk-intervals lst (+ step 1) masses max))))

(define (find-intervals masses)
	(if (< (length masses) 3)
		(begin 
			(show "Too few masses for meaningful intervals: " masses)
			1)
		(let*
			((masses (sort < masses))
			 (max (car (reverse masses)))
			 (massff (fold (lambda (all this) (put all this this)) False (sort < masses))))
			(let loop ((lst masses))
				(if (> (length lst) 2)
					(append
						(walk-intervals lst 1 massff (- max (car lst)))
						(loop (cdr lst)))
					null)))))

(define (all-intervals masses)
	(let* 
		((all (find-intervals masses))
		 (lend (map (lambda (x) (cons (length x) x)) all))
		 (all (sort (lambda (a b) (< (car a) (car b))) lend))
		 (all (map cdr all)))
		(if (null? all)
			(print "No intervals found")
			(for-each 
				(lambda (lst)
					(print*
						(list "Interval with delta " (car lst) " and length " (length (cdr lst)) ": " (cdr lst))))
				all))))




;;;
;;; Formula formatting 
;;;

; signer = abs, lead or 

(define (format-formula factors signer)
	(cond
		((null? factors)
			null)
		((string? (car factors))
			(cons (car factors)
				(format-formula (cdr factors) 'lead)))
		((eq? (caar factors) 0)
			(format-formula (cdr factors) signer))
		((eq? signer 'abs)	; show the absolute value
			(let* 
				((this (car factors))
				 (n (abs (car this))))
				(cond
					((= n 1)
						(cons (cdr this)
							(format-formula (cdr factors) 'normal)))
					(else
						(ilist n
							(cdr this)
							(format-formula (cdr factors) 'normal))))))
		((eq? signer 'lead)	; show the sign in the number
			(let* ((this (car factors)) (n (car this)))
				(cond
					((= n 1)
						(cons (cdr this)
							(format-formula (cdr factors) 'normal)))
					((= n -1)
						(ilist "-" (cdr this)
							(format-formula (cdr factors) 'normal)))
					(else
						(ilist n
							(cdr this)
							(format-formula (cdr factors) 'normal))))))
		; emit a separate sign and continue as abs
		(else
			(let ((next (car factors)))
				(if (and (pair? next) (< (car next) 0))
					(cons " - "
						(format-formula factors 'abs))
					(cons " + "
						(format-formula factors 'abs)))))))

(define (format-solution mass charge factors divisor n)
	(print*
		(append
			(list "m/z " mass " = ")
			(append
				(format-formula factors 'lead)
				(if (> divisor 1)
					(list " / " divisor)
					null))))
	(- n 1))

(define (maybe-format-solution mass charge factors divisor n)
	(if (interesting? factors divisor)
		(format-solution mass charge factors divisor n)
		n))



;;;
;;; Series solver
;;;

(define (try-intervals lst-in)
	(let loop ((step 1) (this (car lst-in)) (lst (cdr lst-in)))
		(cond
			((null? lst)
				step)
			((eq? (+ this step) (car lst))
				(loop step (car lst) (cdr lst)))
			((> (+ this step) (car lst))
				(show "No interval applicable, stopping at " step)
				False)
			(else
				(loop (+ step 1) (car lst-in) (cdr lst-in))))))

(define (get-interval lst)
	(if (< (length lst) 2)
		(begin
			(print "Series is too short")
			False)
		(try-intervals lst)))

; factors = ((n . name) ...)
;; fixme, what is this then?

(define (adjust-elements elems factors steps)
	(fold 
		(lambda (elems factor)
			(map
				(lambda (elem)
					(bind elem
						(lambda (name mass charge min)
							(if (eq? name (cdr factor))
								(tuple name mass charge (div (- 0 (car factor)) steps))	
								; <- negative factors to allow removing originals in the iterations
								elem))))
				elems))
		elems factors))
			
; (cook mass charge factors divisor n) -> n'

(define (solve-series masses charge elems nequs)
	(let* 
		((masses (sort < masses))
		 (interval (get-interval masses))
		 (series-length (- (length masses) 1)))
		(if interval
			(begin
				(print* (list "Solving mass series " masses " with interval " interval))
				(print* (list "Charge is " charge " and elements are " elems))
				(let 
					((n-left
						(solve-goal (car masses) charge elems nequs
							(lambda (mass charge factors divisor n)
								; may not be interesting but the extension can be
								(find-factors 
									(* interval divisor)
									0
									(adjust-elements elems factors series-length)
									null
									(lambda (chain-factors n)
										; check the full molecule for interestingness
										(if (interesting? (append chain-factors factors) divisor)
											(format-solution (car masses) charge 
												(append factors 
													(cons " + [" (append chain-factors (list "]"))))
												divisor
												n)
											n))
									n)))))
					(begin
						(if (= n-left 0)
							'ok
							(show "Unable to compute enough solutions for m/z " (car masses)))
						0)))
			(show "No repeating increment in " masses))))


;;;
;;; Command line processing 
;;;

(define (zap-elem fns args)
	(cond
		((null? args) null)
		((null? fns)
			(print "Too many arguments in element.")
			(syshalt))
		(else
			(cons ((car fns) (car args))
				(zap-elem (cdr fns) (cdr args))))))


(define (split-by elem lst)
	(let loop ((lst lst) (buff null))
		(cond
			((null? lst)
				(if (null? buff) 
					null
					(list (reverse buff))))
			((eq? (car lst) elem)
				(cons (reverse buff)
					(loop (cdr lst) null)))
			(else
				(loop (cdr lst)
					(cons (car lst) buff))))))

(define (parse-element str)
	(let*
		((lst (split-by 44 (string->bytes str)))
		 (lst (map bytes->string lst))
		 (lst 
			(zap-elem
				(list
					(lambda (name-str) name-str)
					(lambda (mass-str) 
						(let ((mass (string->integer mass-str)))
							(cond
								((not mass)	
									(show "Bad mass definition: " mass-str)
									(syshalt))
								((< mass 1)
									(show "Rather strange mass: " mass)
									(syshalt))
								(else mass))))
					(lambda (charge-str)
						(let ((charge (string->integer charge-str)))
							(if charge charge
								(begin
									(show "Bad charge for element: " charge-str)
									(syshalt))))))
				lst))
		 (len (length lst)))
		(cond
			((eq? len 1)
				(show "No mass given for " (car lst))
				(syshalt))
			((eq? len 2)
				(show "No charge given for " (car lst))
				(syshalt))
			((eq? len 3)
				(tuple (car lst) (cadr lst) (caddr lst) 0))			; min element lisatty
			(else
				(show "Bad element definition: " str)
				(syshalt)))))

(define command-line-rules
	(cl-rules
		`((charge "-c" "--charge" cook ,string->integer default "+1"
				comment "desired total charge")
		  (elem "-e" "--element" cook ,parse-element plural
				comment "define a new element, like H,1,+1")
		  (count "-n" "--count" cook ,string->integer default "10"
				comment "maximum solutions per given mass")
		  (series "-s" "--series" 
				comment "solve a mass-series by growing a submolecule")
		  (intervals "-i" "--intervals"
				comment "find interesting intervals in masses")
		  (about "-A" "--about")
		  (help "-h" "--help"))))

(define usage-text "Usage: spektro [args] [mass] ...")

; the rest is now autogenerated from command-line-rules

(define about-spektro
"Spektro -- a simple tool to assist in analysis of mass-spectrometry results
Copyright (c) 2009 Aki Helin

This unrelated tool was written as part of the Protos Genome 
Project at Oulu University Secure Programming Group (OUSPG).

Spektro was written after it turned out some chemists still
solve these problems manually.")


;;;
;;; Startup
;;;

(define (spektro args)
	(or
		(process-arguments args command-line-rules usage-text
			(lambda (dict others)
				(let* 
					((elems (get dict 'elem null))
					 (elems (if (tuple? elems) (list elems) elems))
					 (charge (get dict 'charge 1)))
					(cond
						((get dict 'help False)
							(print usage-text)
							(print-rules command-line-rules)
							0)
						((get dict 'about False)
							(print about-spektro)
							0)
						((null? others)
							(print "No masses?")
							0)
						((get dict 'intervals False)
							(let ((masses (map string->integer others)))
								(if (all number? masses)
									(all-intervals masses)
									(begin
										(show "Cannot check intervals: bad masses: " others)
										1))))
						((null? elems)
							(print "No elements given. Hard to solve goals without them.")
							0)
						((get dict 'series False)
							(let ((masses (map string->integer others)))
								(if (all number? masses)
									(solve-series masses charge elems (get dict 'count 10))
									(show "Bad series masses: " masses))))
						(else
							(for-each
								(lambda (goal)
									(let ((mass (string->integer goal)))
										(cond
											((not mass)
												(show "Bad goal: " goal))
											((< mass 1)
												(show "Goal too small: " goal))
											(else 
												(fork-named (tuple 'mass mass)
													(lambda ()
														(solve-goal mass charge elems (get dict 'count 10) maybe-format-solution)))))))
								others))))))
		1))

; (spektro (list "-c" "0" "-s" "-e" "A,1,0" "5" "7" "9"))

;(dump (lambda (args) (set-signal-action 'halt) (spektro (cdr args))) "spektro.c")

(lambda (args) (set-signal-action 'halt) (spektro (cdr args)))




