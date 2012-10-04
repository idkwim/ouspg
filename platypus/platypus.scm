;;;
;;; Platypus -- a simple plagiarism/similarity analysis tool
;;;

; status:
;	- the reports are starting to be useful (though they proper descriptions what
;    they mean, how to use them and how to not interpret them)
;	- when the report structure is ok, improve the analysis accuracy (layering)
;	- when this is done, start cleaning up the code
;	- and then improve the efficiency
;    
; todo: (currently throwing together new features, cleanups later)
;	- clusters
;		- render cluster reports on separate pages
;			- export the graph data as a JSON object and draw a visualization out of it
;				- would remove the separate dotty/neato run we used earlier
;			- or just draw the graph as an image in owl using some library (which does 
;          not exist yet ;) and include it to the html. would work even with IE.
;	- automatically adjusting clustering weight limit
;	- automatic autocleaning threshold
;	- clean up the code after it works properly again. this is still a prototype.
;	- detect clean data separately and replace the nodes with strings in a separate pass
;		+ could score to 1 or sqrt(len(s))
;	- abstract out the analyzer to allow --similarity-algorithm madam|substring|...
;	  + or --algorithms madam=2,substring
;	- switch from a plain global analysis to optional component- and pairwise reanalysis
;		- ie, purge changes *up to clean data nodes* and run the same code again
;	- if processing larger data sets, make the first pass(es) with heuristic versions
;		- madam -> O(n log n) to bounded depth by always replacing 1 of 2 digrams?
;		- substring -> grow improbable substrings from a suffix array or -tree, and mark 
;		 the links where others also have it?
;	- --input-encoding ascii | binary | utf8 <- has an effect on reading and reports only
;	- madam related: switch to iterators later
;	- pairwise comparison progress meter
;	- bugs
;		- no utf8 normalization (no matter for now, since there is not even an --encoding option =)
;		- could allow preproecessing regexps
;			--translate lowercase
;			--translate s/\n/* /g

(import lib-args)
;(import lib-lazy)

(define min-weight 40)
(define cluster-weight 90) 

; given a link of weight 0 <= w <= 100 scale the score to represent the suspiciousness
; maybe something like n/(10-n/10) would be more sensible? just prototyping here..

(define (link-suspiciousness w) 
	(cond
		((< w 40) (div w 7))
		((< w 50) (div w 6))
		((< w 60) (div w 5))
		((< w 70) (div w 4))
		((< w 80) (div w 3))
		((< w 90) (div w 2))
		(else w)))
	
; a better solution would be to sort the links and 'burn fuel' when selecting them.
; we'll be using about 5 cents worth of fuel

(define clean-page-text 
"<p>This page lists structures which were frequent enough to be considered as typical expressions. use of these, and any structure within them, carries a smaller weight than using less frequent ones. Remember that the program operates actually on trees, and the strings below are just leaves of some of the common tree nodes.

<p>If the autocleaning went well you should see a few longer strings on top, and something like lexical symbols (words, variable names etc) in the middle, and a moderately common letter sequences on the bottom.

<p>If large non-typical strings appear on the top, run the analysis with a lower autocleaning threshold. 
")

(define file-list-page-text 
"<p>This page contains a listing of all the files sorted and coloured by one suspiciousness metric. The analysis builds a graph in which there are weighted links between the files based on the computed similarity. This page ranks the files by substracting the weight of the heaviest link (largest similarity with another file) from the uniqueness score (percentage of structures which are unique to the file) of each file.

<p>The list contains:<ul>
	<li>A link to the cluster in which the file occurs. The is the similarity graph component in which the file occurs. 
	<li>Name of the file coloured by the score.
	<li>Numbers used in scoring (uniqueness - heaviest link = score)
	<li>Link to page with all used links to/from the document in the similarity graph.
	<li>Link to file contents
</ul>
")

(define file-image '(71 73 70 56 57 97 15 0 15 0 161 2 0 18 112 0 42
255 0 255 255 255 255 255 255 33 249 4 1 10 0 2 0 44 0 0 0 0 15 0 15
0 0 2 36 148 143 169 155 225 207 68 0 20 56 54 43 84 153 190 192 85 34
216 136 90 104 90 168 73 34 221 184 194 101 218 30 223 29 69 5 0 59))

(define links-image '(71 73 70 56 57 97 15 0 15 0 161 3 0 37 219 1 42
255 0 246 226 0 255 255 255 33 249 4 1 10 0 3 0 44 0 0 0 0 15 0 15 0 0
2 38 156 63 130 139 226 12 21 100 114 90 11 114 60 32 120 96 72 206 211
125 201 152 112 94 0 166 80 214 94 242 82 201 181 133 206 183 81 0 0 59))

(define (note x) 
	(display x)
	(flush-port 1))

(define (k a b) a)

(define (efold op st ff)
	(ff-fold
		(lambda (st a bs)
			(ff-fold
				(lambda (st b val)
					(op st a b val))
				st bs))
		st ff))

; an owl->c compilation timestamp actually..

(define platypus-version (div (time) 1000))

(define platypus-index 
	(bytes->string
		(foldr render null 
			(list "Platypus v0." platypus-version "
:: <a href=\"index.html\">index</a> 
:: <a href=\"file-list.html\">files</a> 
:: <a href=\"clusters.html\">clusters</a> 
:: <a href=\"cleaned.html\">cleaned</a> 
<hr>
"))))

;;; polite file reading

(define (extend-lst data pos tail)
	(if (eq? pos 0)
		(cons (refb data pos) tail)
		(extend-lst data (- pos 1)
			(cons (refb data pos) tail))))

(define (get-blocks port)
	(lets ((block (interact port #xffff)))
		(cond
			((not block)
				(show "Failed to read from port " port)
				(close-port port)
				null)
			((eof? block)
				(close-port port)
				null)
			(else
				(extend-lst block (- (sizeb block) 1) (get-blocks port))))))

(define (file->bytes fail path)
	(let ((port (open-input-file path)))
		(if port
			(get-blocks port)
			(fail "Unable to read " path))))

;;; 2-level index

(define-syntax put2 
	(syntax-rules ()
		((put2 ff a b v)
			(let ((sub (get ff a True)))
				(if (eq? sub True)
					(put ff a (put False b v))
					(let ((val (get sub b False)))
						(if val
							(ff-update ff a (ff-update sub b v))
							(ff-update ff a (put sub b v)))))))))

(define (put2 ff a b v)
	(let ((sub (get ff a False)))
		(if sub
			(ff-update ff a (put sub b v))
			(put ff a (put False b v)))))

(define-syntax get2
	(syntax-rules ()
		((get2 ff a b d)
			(get (get ff a False) b d))))

(define (ff-union-dig a b)
	(efold (lambda (all a b val) (put2 all a b val)) a b))

(define (flatten a tail)
	 (cond
		 ((null? a) tail) 
		 ((pair? a)
			(flatten (car a)
				(flatten (cdr a) tail)))
		 (else (cons a tail))))

(define (flatten-div a tail)
	 (cond
		 ((null? a) tail) 
		 ((eq? a 32) 
			(append (string->bytes "&nbsp;") tail))
		 ((eq? a 10) 
			(append (string->bytes "\\n") tail))
		 ((eq? a 9) 
			(append (string->bytes "\\t") tail))
		 ((eq? a 60) 
			(append (string->bytes "&lt;") tail))
		 ((eq? a 62) 
			(append (string->bytes "&gt;") tail))
		 ((pair? a)
			(append 
				;(string->bytes "<div class=\"sub\">")
				null
				(flatten-div (car a)
					(flatten-div (cdr a)
						(append 
							;(string->bytes "</div>")
							null
							tail)))))
		 (else (cons a tail))))

(define (merge-dups idx dups)
	(put idx 'dup
		(ff-fold 
			(lambda (dups a n)
				(let ((n (+ (get dups a 0) n)))
					(if (eq? n 0)
						(del dups a)
						(put dups a n))))
			(get idx 'dups False)
			dups)))

(define (merge idx delta)
	(ff-fold
		(lambda (idx a bs)
			(if (eq? a 'dup)
				(merge-dups idx bs)
				(ff-fold
					(lambda (idx b count)
						(let ((ab (+ (get2 idx a b 0) count)))
							(if (lesser? ab 2)
								(let ((bs (del (get idx a False) b)))
									(if bs
										(put idx a bs)
										(del idx a)))
								(put2 idx a b ab))))
					idx bs)))
		idx delta))

(define blank (put False 'dup False))

,load "../radamsa/lib/madam.scm"

(import lib-madam)

(define (import fail) 
	(lambda (path)
		(show " - reading " path)
		(file->bytes fail path)))


;;;
;;; Global analysis
;;;

; nodes = ff of node -> weight

(define (grab seen x)
	(if (pair? x)
		(receive (grab seen (car x))
			(lambda (seen carw)
				(receive (grab seen (cdr x))
					(lambda (seen cdrw)
						(let ((sum (+ carw cdrw)))
							(values (put seen x sum) sum))))))
		(values seen 1)))

(define (grab-nodes x)
	(lets ((seen weight (grab False x)))
		seen))

(define (nodes-of lst)
	(for False lst
		(lambda (seen this)
			(if (pair? this)
				(receive (grab seen this)
					(lambda (seen weight) seen))
				seen))))

(define (intersect-nodes a b)
	(ff-fold
		(lambda (both node weight)
			(if (get b node False)
				(put both node weight)
				both))
		False a))

(define (diff-nodes a b)
	(ff-fold
		(lambda (ap ak av)
			(if (get b ak False)
				ap 
				(put ap ak av)))
		False a))

(define (sum-weights nodes)
	(ff-fold
		(lambda (sum ak weight) (+ sum weight))
		0 nodes))

(define (count-nodes seen lst)
	(ff-fold
		(lambda (seen node weight)
			(put seen node
				(+ 1 (get seen node 0))))
		seen (nodes-of lst)))

(define (uniqueness lst suspicious)
	(lets
		((all (nodes-of lst))
		 (area (ff-fold (lambda (sum n w) (+ sum w)) 0 all))
		 (bad  (ff-fold (lambda (sum n w) (if (get suspicious n False) (+ sum w) sum)) 0 all)))
		(if (= area 0)
			0
			(div (* (- area bad) 100) area))))

;; add all nodes to clean nodes which occur in 
;; at least threshold% of the data sources
;; ... -> clean'

(define (add-common-nodes clean counts total threshold)
	(note " - cleaning common nodes: ")
	(ff-fold
		(lambda (clean node count)
			(if (>= (div (* count 100) total) threshold)
				(begin
					(note "*")
					(put clean node count))
				clean))
		clean counts))



; (data ...) x clean-datas -> ((score . data) ...) x clean-nodes

(define (uniquenesses nodes clean autoclean-threshold)
	(lets
		((clean-nodes 
			(fold 
				(lambda (a b) (ff-union a b k)) 
				False
				(map nodes-of clean)))
		 (node-counts
			(fold count-nodes False nodes))
		 (clean-nodes
			(add-common-nodes clean-nodes node-counts (length nodes) autoclean-threshold))
		 (shared-nodes
			(ff-fold
				(lambda (shared node count)
					(if (and (> count 1) (not (get clean-nodes node False)))
						(put shared node count)
						shared))
				False node-counts)))
		(print "")
		(print " - computing uniquenesses")
		(values
			(map
				(lambda (node)
					(cons (uniqueness node shared-nodes) node))
				nodes)
			clean-nodes)))

; a's overlap with b
; could actually be relative difference, but check out the graph before 
; going back to prototype-like comparison

(define (pair-overlap a b clean)
	(let*
		((area
			(ff-fold 
				(lambda (sum n w) 
					(if (get clean n False)
						(+ sum 1)
						(+ sum w)))
				0 a))
		 (bad 
			(ff-fold 
				(lambda (sum n w)
					(if (get b n False)
						(if (get clean n False)
							(+ sum 1)
							(+ sum w))
						sum))
				0 a))
		 (good (- area bad))
		 (perc (if (= area 0) 0 (div (* bad 100) area))))

		(cond
			;((< perc 40) 0)
			; could use different distributions
			;((< perc 95)
			;	(/ (* (- perc 40) 100) 40))
			(else perc)))) ; plain percent now

; ((name . data) ..) x clean-set -> ff of name -> ((edge-weight . name) ...)
; store all edges with weight above cluster-weight, and heaviest one of none pass it

; sources x clean => a -> b -> edge-weight (0-100)
; later just sources => ...
(define (similarity-graph sources clean)
	(lets
		((nodes
			(map 
				(lambda (node) 
					(cons (car node) 
						(ff-diff (nodes-of (cdr node)) clean)))
				sources)))
		; nodes are (name . nonclean-nodes)
		(for False nodes
			(lambda (links node)
				(show " * at " (car node))
				(put links (car node)
					; note, similarities are in a digraph, so you need O(n*(n-1)) comparisons to get them all
					; it would only improve to O(n*(n-1)/2) by switching to an undirected one
					(list->ff
						(for False nodes
							(lambda (best pair)
								(if (eq? node pair)
									best ; here comes the -1
									(let ((score (pair-overlap (cdr node) (cdr pair) clean)))
										(cond
											((not best)
												(list (cons (car pair) score)))
											((> score cluster-weight)
												(let ((this (cons (car pair) score)))
													(if (> (cdar best) cluster-weight)
														(cons this best)
														(list this))))
											((> score (cdar best))
												(list (cons (car pair) score)))
											(else best))))))))))))

; a -> b and a <- b, meaning they happen to be most similar with eachother, or they 
; are in a cluster with strong links (more common)

(define (bisimilarity sims a b) ; -> False | total-strength
	(let ((ab (get2 sims a b False)))
		(if ab
			(let ((ba (get2 sims b a False)))
				(if ba (+ ab ba) False))
			False)))

(define bisimilar? bisimilarity)


;;;
;;; Platypus entry code
;;; 

;;; Command Line Stuff

(define (self x) x)

(define about-platypus

"Platypus -- a simple plagiarism detection tool.
Copyright (c) 2009 Aki Helin
This program is being written as part of the Protos Genome
Project at Oulu University Secure Programming Group (OUSPG).
Documentation and a newer version may be available at 
http://code.google.com/p/ouspg/.")


(define usage-text "Usage: platypus [args] -o /report/here [sample] ...")

(define command-line-rules
	(cl-rules
		`((clean "-c" "--clean" has-arg plural)
		  (help  "-h" "--help")
		  (version "-V" "--version")
		  (about	"-A" "--about")
		  (auto  "-a" "--autoclean" 
		  		cook ,string->integer 
				check ,(lambda (x) (and (<= 0 x) (<= x 100)))
				default "15"
				comment "occurrence percentage after which patterns are marked clean")
		  (output "-o" "--output" has-arg
		  		comment "directory where to generate the report"))))

(define usage-examples 
"Examples: 
 $ platypus -o /tmp works/*.c
 $ platypus -o /tmp -a 40 -c lecture-samples.lsp -c submissions/*/*.lsp
 $ for foo in 2009-Q1-sudoku-*/; do cat $foo/*.[ch] > /tmp/$foo.soln; done; platypus -o /tmp /tmp/*.soln
")

(define (listen x)
	(if (list? x)
		x
		(list x)))

; FIXME, remove operlapping names later

; actually, more proper would be to drop the shared prefix and convert
; all slashes to underscores. 

(define (drop-shared-prefixes lsts)
	(let ((first (caar lsts)))
		(if (all (lambda (x) (eq? (car x) first)) lsts)
			(drop-shared-prefixes (map cdr lsts))
			lsts)))

(define path-conversions
	(list->ff
		'((46 . 95) ; . -> _
		  (47 . 124)) ; / -> |
		 ))

(define (proper-names paths)
	(lets
		((charss (map string->runes paths))
		 (charss (drop-shared-prefixes charss))
		 (charss 
			(map (λ (l) (map (λ (char) (get path-conversions char char)) l)) charss)))
		(map runes->string charss)))

;;;
;;; Dotty output, no longer used
;;;

(define (link-target a b)
	(if (lesser? a b)
		(foldr string-append "" (list a "-vs-" b ".html"))
		(link-target b a)))

(define (file-link name)
	(foldr string-append "" (list "file-" name ".html")))

(define (linked-back? name lst)
	(cond
		((null? lst) False)
		((eq? name (cdar lst)) (caar lst))
		(else (linked-back? name (cdr lst)))))

; sims -> (ff of a -> b -> (weight . type)) x (orphan ...)
; sims = ((name (strength . target) ...) ...)



(define html-prelude 
"<html>
<head>
<link rel=\"stylesheet\" type=\"text/css\" href=\"platypus.css\">
</head>
<body>
")

(define html-finale "
<hr>
</body>
</html>
")

;; should give nice round borders for firefox and safari

(define platypus-css "
body { color: #0e0; background-color: #000; }
.shared { color: #f00; }
.shclean { color: #f40; }
.clean { color: #af0; }
hr { background-color: #050; border: 0px; height: 1px; }
a { color: #bbaa00; text-decoration: none; }
a.bad { color: #ee8800; text-decoration: none; }
a.suspicious { color: #998800; text-decoration: none; }
a:hover { color: #ffee00; text-decoration: underline; }
.cluster {
	border: solid #aa0000 2px;
	-moz-border-radius: 10px;
	-webkit-border-radius: 10px;
	padding: 8px;
	background-color: #220000;
	clear: left;
	float: left;
	margin-bottom: 20px;
}
.flyingcolours {
	border: solid #00aa00 2px;
	-moz-border-radius: 10px;
	padding: 8px;
	background-color: #002200;
	clear: left;
	float: left;
	margin-bottom: 20px;
}
div.bidirectionals {
	border: solid #aa0000 2px;
	-moz-border-radius: 10px;
	padding: 8px;
	background-color: #220000;
	clear: left;
	float: left;
	margin-bottom: 20px;
}
div.rights {
	border: solid #aa8800 2px;
	-moz-border-radius: 10px;
	padding: 8px;
	background-color: #442200;
	clear: left;
	float: left;
	margin-bottom: 20px;
}
div.lefts {
	border: solid #88aa00 2px;
	-moz-border-radius: 10px;
	padding: 8px;
	background-color: #224400;
	clear: left;
	float: left;
	margin-bottom: 20px;
}
.cleanstrings {
	color: #bbbbbb;
	border: solid #0000aa 2px;
	-moz-border-radius: 10px;
	padding: 8px;
	background-color: #000022;
	clear: left;
	float: left;
	margin-bottom: 20px;
}
.sub {
	border: solid #44a 1px;
	border: 2px;
	float: left;
}
")

(define (render-content forest shared clean)

	(define (render-between start node end state tail)
		(render start
			(walk (car node) state
				(walk (cdr node) state
					(render end tail)))))

	(define (walk node state tail)
		(cond
			((eq? node 9)
				(ilist 32 32 32 tail))
			((eq? node 60)
				(render "&lt;" tail))
			((eq? node 62)
				(render "&gt;" tail))
			((null? node) 
				tail)
			((pair? node)
				(cond
					;; writing shared data
					((eq? state 'shared)	
						(if (get clean node False)
							;; switch to shread AND clean
							(render-between "<font class=\"shclean\">" node "</font>" 'clean tail)
							;; resume shared data
							(walk (car node) state (walk (cdr node) state tail))))
					;; continue clean
					((eq? state 'clean)
						(walk (car node) state 
							(walk (cdr node) state tail)))
					;; start clean data
					((get clean node False)
						(render "<font class=clean>"
							(walk (car node) 'clean
								(walk (cdr node) 'clean 
									(render "</font>" tail)))))
					;; start shared data
					((get shared node False)
						(render-between "<font class=shared>" node "</font>" 'shared tail))
					(else
						(walk (car node) state (walk (cdr node) state tail)))))
			(else
				(cons node tail))))

	(let ((data (walk forest False null)))
		(if (all number? data)
			data
			(show " BAD DATA: " data))))

(define (shared-nodes al bl) 
	(intersect-nodes
		(nodes-of al)
		(nodes-of bl)))

(define (render-file-contents out-dir name data clean)
	(lets
		((path
			(foldr string-append "" (list out-dir "/" (file-link name))))
		 (port (open-output-file path)))
		(if port
			(begin
				(mail port (string->bytes html-prelude))
				(mail port (string->bytes platypus-index))
				(mail port (string->bytes "file: <i>"))
				(mail port (render name null))
				(mail port (render "</i><hr><pre>" null))
				(mail port (render-content data False clean))
				(mail port (render "</pre>" null))
				(mail port (string->bytes html-finale))
				(close-port port))
			(show "Error: could not open file for writing: " path))))

; files are (name . data) 

(define (render-pairwise-comparison out-dir a b clean)
	(show " + " (cons (car a) (car b)))
	(lets 
		((path 
			(foldr string-append "" 
				(list out-dir "/" (link-target (car a) (car b)))))
		 (port (open-output-file path)))
		(if port
			(let ((shared (shared-nodes (cdr a) (cdr b))))
				(mail port (string->bytes html-prelude))
				(mail port (string->bytes platypus-index))
				(mail port 
					(foldr render null
						(list "<table><tr><td valign=top><b>" (car a) "</b><hr><pre>")))
				(mail port
					(render-content (cdr a) shared clean))
				(mail port 
					(string->bytes "</pre></td><td valign=top><pre>"))
				(mail port 
					(foldr render null
						(list "</pre></td><td valign=top><b>" (car b) "</b><hr><pre>")))
				(mail port
					(render-content (cdr b) shared clean))
				(mail port (string->bytes "</pre></td></tr></table>"))
				(mail port (string->bytes html-finale))
				(close-port port))
			(begin
				(show " *** failed to open file for writing report: " path)
				False))))

(define (render-source out-dir a clean)
	(let* 
		((path 
			(foldr string-append "" 
				(list out-dir "/" (car a) ".html")))
		 (port (open-output-file path)))
		(if port
			(let ((shared False))
				(mail port (string->bytes html-prelude))
				(mail port 
					(foldr render null (list "<b>" (car a) "</a><hr>")))
				(mail port 
					(string->bytes "<pre>"))
				(mail port
					(render-content (cdr a) False clean))
				(mail port 
					(string->bytes "</pre>"))
				(mail port (string->bytes html-finale))
				(close-port port))
			(begin
				(show " *** failed to open file for writing report: " path)
				False))))


;;;
;;; Component separation and sorting
;;;

(define (first-node ff)
	(call/cc
		(lambda (ret)
			(ff-fold (lambda (x a _) (ret a)) False ff))))

(define (component-nodes edges taken)
	(define next 
		(efold
			(lambda (taken a b val)
				(cond
					((get taken a False)
						(if (get taken b False)	
							taken
							(put taken b True)))
					((get taken b False)
						(if (get taken a False)
							taken
							(put taken a True)))
					(else taken)))
			taken edges))
	(if (eq? next taken)
		taken
		(component-nodes edges next)))

(define (component-edges edges nodes)
	(efold
		(lambda (out a b val)
			(if (get nodes a False)
				(put2 out a b val)
				out))
		False edges))

(define (edges-diff edges comp)
	(efold
		(lambda (out a b val)
			(if (get2 comp a b False)
				out
				(put2 out a b val)))
		False edges))

(define (separate-components edges)
	(if edges
		(lets
			((nodes (component-nodes edges (put False (first-node edges) True)))
			 (comp-edges (component-edges edges nodes))
			 (edges (edges-diff edges comp-edges)))
			(cons comp-edges
				(separate-components edges)))
		null))


(define (component-suspiciousness edges)
	(efold
		(lambda (sum a b weight) 
			(+ sum (link-suspiciousness weight)))
		0 edges))

(define (index i l)
	(if (null? l)
		null
		(cons (cons i (car l))
			(index (+ i 1) (cdr l)))))

(define (sorted-components edges) ; graph → ((id weight . component) ...)
	(index 1
		(sort (λ a b (> (car a) (car b)))
			(map (λ c (cons (component-suspiciousness c) c))
				(separate-components edges)))))

(define (render-clean-text clean port)
	(mail port (render "<div class=cleanstrings><ul>" null))

	(fold
		(lambda (done node)
			(if (get done node False)
				done
				(begin
					(mail port (render "<li>`" (flatten-div node '(96 10))))
					(ff-union done (grab-nodes node) k))))
		False
		(map cdr
			(sort (lambda (a b) (> (car a) (car b)))
				(ff-fold
					(lambda (out node _)
						(cons (cons (length (flatten node null)) node) out))
					null clean))))
		
	(mail port (render "</ul></div>" null))
	)



(define (render-clean-strings-report clean out-dir)
	(let ((port (open-output-file (string-append out-dir "/cleaned.html"))))
		(mail port (string->bytes html-prelude))
		(mail port (string->bytes platypus-index))
		(mail port (string->bytes clean-page-text))
		(render-clean-text clean port)
		(mail port (string->bytes html-finale))
		(close-port port)))

(define (generate-index names scores autoclean out-dir)
	(let ((port (open-output-file (string-append out-dir "/index.html"))))
		(if port
			(begin
				(mail port (string->bytes html-prelude))
				(mail port (string->bytes platypus-index))
				(mail port (foldr render null 
					(list "<p>There are a total of " (length names) " files.")))
				(lets
					((sl (ff-fold (lambda (out node score) (cons score out)) null scores))
					 (sl (sort < sl)))
					(mail port 
						(foldr render null
							(list "<p>The similarity scores range from " (car sl) " to " (car (reverse sl)) " and have a median of " (div (fold + 0 sl) (length sl)) "."))))
				(mail port 
					(foldr render null
						(if (< autoclean 100)
							(list "<p>All structures (roughly substrings) occurring in at least " autoclean "% of files were added to the extended grammar. Each occurrence counts as one symbol. The list of autocleaned strings is available <a href=\"cleaned.html\">here</a>")
							(list "<p>Autocleaning was disabled."))))
				(mail port
					(foldr render null
						(list
							"<p>The <a href=\"file-list.html\">file list</a> page contains the files sorted by suspiciousness. Line has a link to the contents of the file or a comparison of the file with the most similar case if it appears to be possibly interesting."
							"<p>The <a href=\"clusters.html\">cluster list</a> page contains clusters of documents sorted by their total weight. A cluster contains files and all similarity links above the give threshold. The comparisons point to the same ones which are in the file list"
							"<p>And grammar here."
							)))
				(mail port (string->bytes html-finale))
				(close-port port))
			(print "ERROR: failed to open index.html for writing"))))


(define (render-pairwise-comparisons datas links clean-nodes out-dir)
	(print "Rendering pairwise comparisons.")
	(ff-fold
		(lambda (x a bs)
			(ff-fold	
				(lambda (x b strength)
					(render-pairwise-comparison out-dir 
						(cons a (get datas a False)) 
						(cons b (get datas b False))
						clean-nodes))
				x bs))
		42 links)
	(print "Pairwise done"))

(define (render-all-files datas clean out-dir)
	(print "Rendering plain files: ")
	(ff-fold
		(lambda (_ name data)
			(render-file-contents out-dir name data clean))
		42 datas))

; sims = name -> ((edge-weight . peer-name) ...)

(define (heaviest-link sims name def)
	(ff-fold
		(lambda (best a str)
			(if (> str (car best))
				(cons str a)
				best))
		def
		(get sims name False)))

; fixme, should be string->runes, but it seems to be still missing from lib-string (just a str-iterr..)

(define (short-name name)
	(if (> (string-length name) 50)
		(bytes->string 
			(append (take (string->bytes name) 47) '(46 46 46)))
		name))


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

; sort by (uniqueness - weight of heaviest link) 

(define (generate-cluster-report wcomps out-dir)
	(let 
		((port 
			(open-output-file 
				(string-append out-dir "/clusters.html"))))
		(if port
			(begin
				(mail port (string->bytes html-prelude))
				(mail port (string->bytes platypus-index))
				
				(for-each
					(lambda (iwc)
						(mail port (foldr render null 
							(list 
								"<p>Cluster <a name=cluster-" (car iwc)">#" 
								(car iwc) "</a> with suspiciousness " (cadr iwc) ":<table style=\"margin: 25px;\">")))
						(let
							((total-linkage
								(sort 
									(λ (a b) (> (cdr a) (cdr b)))
									(ff->list
										(efold
											(λ (all a b weight)
												(lets
													((all (put all a (+ weight (get all a 0))))
													 (all (put all b (+ weight (get all b 0)))))
													all))
											False (cddr iwc))))))
							(for-each
								(λ (node)
									(lets 
										((name linkage node)
										 (data (foldr render null (list "<tr>" 
											"<td><font color=" (score-color (- 100 linkage)) ">"
											name "</font></td><td>(" linkage ")</td>"
											"<td><a href=\"" (string-append "links-of-" (file-link name)) 
											"\"><img src=links.gif></a>"
											"<a href=\"" (file-link name) "\"><img src=file.gif></a>"
											"</td>"))))
										(mail port data)))
								total-linkage))
						(mail port (render "</table>" null)))
					wcomps)
				(mail port (string->bytes html-finale)))
			(print "ERROR: failed to open cluster report page for writing"))))

(define nl (bytes->string (list 10)))

(define (compute-scores sims uniqs)
	(ff-fold
		(lambda (out name uness)
			(put out name
				(- uness (car (heaviest-link sims name '(0 . ""))))))
		False uniqs))

(define (comparison-links a bs)
	(foldr
		(lambda (node tail)
			(lets ((b str node))
				(foldr render tail
					(list "<a href=\"" (link-target a b) "\">" b "</a> <font color=" (score-color (- 0 str)) ">(" str ")</font><br>"))))
		null bs))

(define (render-file-links a sims out-dir)
	(lets
		((bis ; bisimilar nodes (b -> strength)
		 	(ff-fold
				(lambda (out b str)
					(let ((bs (bisimilarity sims a b)))
						(if bs (put out b bs) out)))
				False (get sims a False)))
		 (rights ; what a is similar to (but not bisimlar) 
		 	(ff-fold
				(lambda (out b str)
					(if (get bis b False)
						out
						(put out b str)))
				False (get sims a False)))
		 (lefts ; which are similar with a, but not vice versa
		 	(ff-fold
				(lambda (out b bs)
					(if (get bis b False)
						out
						(let ((str (get bs a False)))
							(if str
								(put out b str)
								out))))
				False sims))
		 (bis (sort (lambda (a b) (> (cdr a) (cdr b))) (ff->list bis)))
		 (rights (sort (lambda (a b) (> (cdr a) (cdr b))) (ff->list rights)))
		 (lefts (sort (lambda (a b) (> (cdr a) (cdr b))) (ff->list lefts)))
		 (port 
		 	(open-output-file 
				(foldr string-append ""
					(list out-dir "/links-of-" (file-link a))))))
		(mail port (string->bytes html-prelude))
		(mail port (string->bytes platypus-index))
		(mail port 
			(string->bytes 
				(foldr string-append ""
					(list "<p>Links related to " a))))
		(if (pair? bis)
			(begin
				(mail port (render "<p><div class=bidirectionals>Bisimilar files:<br>" null))
				(mail port (comparison-links a bis))
				(mail port (render "</div>" null))))
		(if (pair? rights)
			(begin
				(mail port (render "<p><div class=rights>This is similar with:<br>" null))
				(mail port (comparison-links a rights))
				(mail port (render "</div>" null))))
		(if (pair? lefts)
			(begin
				(mail port (render "<p><div class=lefts>Files which have are partially contained in this:<br>" null))
				(mail port (comparison-links a lefts))
				(mail port (render "</div>" null))))
		(close-port port)))

(define (cluster-id clusters node)
	(cond
		((null? clusters)
			"?")
		((get (cddr (car clusters)) node False)
			(caar clusters))
		(else
			(cluster-id (cdr clusters) node))))

(define (render-file-list datas sims uniqs score-ff clean-nodes clusters out-dir)
	(print "Rendering file list page")
	(let ((port (open-output-file (string-append out-dir "/file-list.html"))))
		(if port 
			(let 
				;; sort scores to get printing order
				((scores
					(sort (lambda (a b) (< (car a) (car b)))
						(ff-fold 
							(lambda (out name score) (cons (cons score name) out))
							null score-ff))))

				(mail port (string->bytes html-prelude))
				(mail port (string->bytes platypus-index))
				(mail port (string->bytes file-list-page-text))
				(mail port (string->bytes "<table style=\"margin: 30px;\">"))
			
				(mail port
					(foldr render null
						(foldr
							(lambda (node tail)
								(lets
									((cid (cluster-id clusters (cdr node)))
									 (name (cdr node))
									 (my-score (get uniqs name 0))
									 (decrement (heaviest-link sims name '(0 . ""))))
									(ilist nl
										"<tr>"
										"<td><a href=clusters.html#cluster-" cid ">#" cid "</a></td>"
										"<td><font color=\"" (score-color (car node)) "\">"  name "</font></td>"
										"<td style=\"text-align: left;\"><font color=\"" (score-color my-score) "\">(" my-score "</font></td><td> - </td><td style=\"text-align: right\"><font color=\"" (score-color (- 0 (car decrement))) "\">" (car decrement) "</font></td><td> = </td><td style=\"text-align: right;\"><font color=\"" (score-color (car node)) "\">" (car node)") </td>" 
										"<td>"
											" <a href=\"" (string-append "links-of-" (file-link name)) "\"><img src=links.gif></a>"
											" <a href=\"" (file-link name) "\"><img src=file.gif></a>"
										"</td>"
										tail)))
							null scores)))

				(mail port (string->bytes "</table>"))
				(mail port (string->bytes html-finale))

				(close-port port))
			(print " ** Failed to open file list page for writing ** "))))

(define (try-write path data fail)
	(let ((port (open-output-file path)))
		(if port
			(begin
				(mail port data)
				(close-port port))
			(fail "could not write to " path))))

(define (platypus paths clean-paths out-dir autoclean)
	(print "You see a platypus.")
	(print "Reading files.")
	(call/cc
		(lambda (abort)
			(define (fail reason info)
				(show reason info)
				(abort 1))
			(lets
				((files (map (import fail) paths))
				 (cleans (map (import fail) clean-paths))
				 (names (proper-names paths))	
				 (port (open-output-file (string-append out-dir "/index.html"))))
				(if (not port)
					(fail "cannot write to output directory: " out-dir))
				(if (null? clean-paths)
					(print " - no clean sample files")
					(show " - clean files: " clean-paths))
				(if (> autoclean 100)
					(print "Autoclean disabled (over 100%)")
					(show " - autoclean threshold: " autoclean))
				(show " - output directory: " out-dir)
				(print "")
				(print "Growing a global forest.")
				(lets
					((data (compress-verbose (append files cleans) (lambda (msg) (show " - " msg))))
					 (_ (print "Cleaning compressed"))
					 (clean-compressed (take (reverse data) (length clean-paths)))
					 (_ (print "Computing uniquenesses"))
					 (uniqs clean-nodes  (uniquenesses data clean-compressed autoclean))
					 (_ (print "Computing uniquenesses"))
					 (uniqs ; ff of name -> score	
						(list->ff
							(map
								(lambda (node)
									(cons (car node) (cadr node)))
								(zip cons names uniqs))))
					 (_ (print "Making similarity graph (O(nfiles^2), may take a while)"))
					 (sims (similarity-graph (zip cons names data) clean-nodes))
					 (wcomps (sorted-components sims)) ; ((weight . graph) ...)
					 (datas (list->ff (zip cons names data)))
					 (_ (print "Computing scores"))
					 (scores (compute-scores sims uniqs)))

					(print "Rendering pairwise")

					; write pairwise comparison htmls
					(render-pairwise-comparisons datas sims clean-nodes out-dir)
					
					; write plain content htmls for all files
					(render-all-files datas clean-nodes out-dir)

					; write a sorted file index page
					(render-file-list datas sims uniqs scores clean-nodes wcomps out-dir)

					(generate-cluster-report wcomps out-dir)
					
					; write link pages for each file
					(ff-fold
						(lambda (_ a subs)
							(render-file-links a sims out-dir))
						42 sims)

					(print "Writing report to index.html")
					(generate-index names scores autoclean out-dir)

					;;; support-files
					(try-write 
						(string-append out-dir "/platypus.css")
						(string->bytes platypus-css)
						fail)

					(try-write (string-append out-dir "/file.gif") file-image fail)
					(try-write (string-append out-dir "/links.gif") links-image fail)

					(render-clean-strings-report clean-nodes out-dir)

					(print "Platypus out")
					0)))))

(define (output-path-ok? path)
	(let ((port (open-output-file (string-append path "/index.html"))))
		(if port
			(begin
				(close-port port)
				True)
			False)))

(define (platypus-main args)
	(process-arguments args command-line-rules "Try playtypus -h for help."
		(lambda (dict subjects)
			(cond
				((get dict 'help False)
					(print usage-text)
					(print-rules command-line-rules)
					(print "")
					(print usage-examples)
					0)
				((get dict 'version False)
					(show "platypus v0." platypus-version)
					0)
				((get dict 'about False)
					(print about-platypus)
					0)
				((< (length subjects) 2)
					(print "I need several files to work on.")
					0)
				((not (output-path-ok? (get dict 'output ".")))
					(show "Cannot write to " (get dict 'output "."))
					1)
				(else	 
					(platypus 
						(reverse subjects)
						(listen (get dict 'clean null))
						(get dict 'output ".")
						(get dict 'auto 25)))))))

(lambda (vm-args) 
	;(set-signal-action 'halt)
	(platypus-main (cdr vm-args)))




