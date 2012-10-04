;;;
;;; Platypus 2000 - time for a rewrite
;;;

;; overview of plans:
;
;  $ platy -o <file|dir> --format <json|csv|html> --preprocess "s/[ \t\n]/ /" --idiom 50% --partition 1000 --similarity <cover|levenshtein|?>
;
; Would be nice to have the comparison generate output report as it goes, and
; only fill the results requiring global knowledge at the end of the run.
;  

; algorithm : preprocess o cluster o compare o render
; preprocessing:
;  - if requested, apply regular expressions 
;     + include builting ones like -e merge-whitespaces
;     + these *change the data*, but would be useful for the json use (?)
;     + default none
;  - if requested, do idiomatization(tm)
;     + using parallel suffix array walks
;     + #[codepoint ..] → #[codepoint|id ...]
;  - compute suffix arrays 
; clustering:
;  - if number of samples > n
;     + perform < O(n^2) rounds of heuristic pass to partition the samples to 
;       sets of < n files (for example by reading random strings in parallel)
;     + platypus aimed to do essentially just this
; compare:
;  - have a set of n files
;  - choice to be made: use reflexive similarity or not?
;     - reflexive: O(n*(n-1)/2) comparisons
;     - non-reflexive: O(n^2), but can detect A <- B
;  - collect O(n^2) similarity graph, matrix or equivalent
; render:
;  - json, (ask)
;  - csv, <n>,<m>,<imilarity> (or accept a format string?)
;  - html, much like platypus now
;     + allow highlighting shared sections by hovering cursor?

(import lib-args)

(define usage-text "Usage: platy ...")

(define command-line-rules
   (cl-rules
      `((help  "-h" "--help")
        (about "-A" "--about")
        (min-len "-l" "--min-len" cook ,string->integer comment "default auto (maxr(4,sqrt(length)/2))")
        (cutoff "-c" "--cutoff" cook ,string->integer default "40")
        )))

(define (sequence env data ops)
   (if (null? ops)
      ;; passed through
      0
      (lets ((env data ((car ops) env data)))
         (if data
            (sequence env data (cdr ops))
            (+ 1 (length ops))))))

(define bug "bug: missing value")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,,,
;;
;; Comparisons
;;

; node node → similarity X, being a renderer etc. need to know more about output 
;                           requirements, but probably (X fd output-type) → side-effects 
;; options here:
;  - compute levenshtein distance?
;    + pros, simple (though haven't done it functionally yet..)
;    - cons, easy to cheat (for example just reorder functions)
;  - pairwise compression ratio vs each separately?
;    + pros, hard to cheat given a good compression algorithm
;    - cons, answers not especially meaningful as such
;  - cover?
;    + pros, nice meaningful answers
;            similarity can be defined intuitively as covered%
;            report can show exactly this
;    - cons, optimal solution is rather hard to find...
;  - greedy cover?
;    + pros, simple to implement given a suffix array
;    - cons, not sure how robust this is against changes
;            needs a (manual?) baseline probability or a minimum length

;; get the length of longest string starting from iter, which is also in sarr

;; output ((n . m) ...), n+min-len <= m
(define (cover vec tvec sarr min-len)

   (define fini (vec-len vec))

   ;; return False | n, being (pos + m) which is also in sarr
   ;; need pos in original -> iter
   ;; for target, range in sarray (initially 0/end)
   ;; first bisect-range the area matching current
   ;; then if range is non-empty, step further and n++

   ;; use plain bisect, since the range ends will probably be changed soon
   (define target-byte 
      (let ((end (vec-len tvec)))
         (λ (pos)
            (if (< pos end) 
               (vec-ref tvec pos)
               -1))))

   ;; read char code at target data in sarray at given position (in range)
   (define (read pos depth)
      (target-byte (+ depth (vec-ref sarr pos))))

   ; pos → n, length of longest shared string starting at pos
   (define (step pos start end depth)
      (if (= pos fini) ;; end of input data
         depth
         (lets
            ((char (vec-ref vec pos))
             (new-start (bisect (λ (p) (>= (read p depth) char)) start end)))
            (if new-start 
               (let ((new-end (or (bisect (λ (p) (< char (read p depth))) new-start end) end)))
                  (step (+ pos 1) new-start new-end (+ depth 1)))
               depth))))
            
   ;; walk 0..n, and collect (n . m) steps to a sorted list
   (let loop ((pos 0))
      (if (= pos fini)
         null
         (let ((step (step pos 0 (vec-len sarr) 0)))
            (if (< step min-len) ; ignore too small steps
               (loop (+ pos 1))
               (let ((end (+ pos step)))
                  ;; leave a mark
                  (cons (cons pos end) (loop end))))))))

;(lets
;   ((original  (list->vector (string->list "lammasxxxlammasxxxxxlammas")))
;    (reference (list->vector (string->list "lammas"))))
;   (cover original reference 
;      (suffix-array reference)
;      2))
; (exit-owl 1)

;; note, could keep the number of uncovered bytes in closest sample, and use that to bail out of covering if cannot get a better one
; → percent ((pos . end) ...), where end is first position *not* in covered after pos (or length of vector)
(define (greedy-cover node target min-len)
   (lets
      ((path data sarr node) ;; <- file being compared 
       (min-len (or min-len (max 4 (>> (sqrt (vec-len data)) 1))))
       (tpath tdata tsarr target) ;; <- node being compared against
       (covered 
         (cover data tdata tsarr min-len)) ; <- vector of covered bytes (1 = covered, 0 = not covered)
       (total (fold (λ (n pair) (+ n (- (cdr pair) (car pair)))) 0 covered))
       (covered-perc
         (if (= total 0) 0 (floor (* 100 (/ total (vec-len data)))))))
      ;(print* (list " - total cover " covered-perc " in " data " vs " covered))
      ;(print* (list " - covered " total " out of " (vec-len data) " bytes with " covered))
      (values covered-perc covered)))

(define (nop env data) (values env data))
(define preprocess nop)
(define cluster nop)

(define (add-file path tail)
   (if (not tail)
      False
      (begin
         (for-each display (list " - " path ": "))
         (flush-port stdout)
         (let ((vec (file->vector path)))
            (if vec 
               (begin
                  (for-each display (list (vec-len vec) "b"))
                  (flush-port stdout) ;; show before computing the suffix array
                  (let ((sarray (suffix-array vec)))
                     (print "")
                     (cons (tuple path vec sarray) tail)))
               (begin
                  (print " *** READ ERROR ***")
                  False))))))

;; ff x (path ..) → ff x (#(path data sarray) ...)
(define (read-data dict paths)
   (print "Reading and preprocessing files:")
   (values dict
      (foldr add-file null paths)))

(define (compare-against env node nodes)
   (fold
      (λ (ress target)
         (if (eq? target node) ;; don't compare against self
            ress
            (lets ((perc pairs (greedy-cover node target (get env 'min-len False))))
               ;(print* (list "  " perc "% in " (ref target 1)))
               (cons (tuple perc target pairs) ress))))
      null nodes))

(define (show-similarities limit ns n)
   (if (or (< n 1) (>= (ref (car ns) 1) limit)) ;; first or similar enough
      (begin
         (print* (list "   " (ref (car ns) 1) "% is in " (ref (ref (car ns) 2) 1)))
         (show-similarities limit (cdr ns) (+ n 1)))))

(define (compare env data) 
   (show "Minimum length " (get env 'min-len "auto"))
   (for-each
      (λ (node)
         (print* (list "File " (ref node 1) ":"))
         ;; a simple animation here
         (lets
            ((others (compare-against env node data)) ;; → (#(perc node' pairs) ...), not including self
             (others (sort (λ (a b) (> (ref a 1) (ref b 1))) others)))
            ;; take the most similar, and all having at least cutoff% 
            (show-similarities (get env 'cutoff bug) others 0)))
      data)
   (values env data))

(define (render env data)
   (print "Got up to renderer.")
   (values env data))



(define (platypus args)
   (process-arguments args 
      command-line-rules 
      "To maximize your success rate, try $ platy --help"
      (λ (dict paths)
         (cond
            ((get dict 'help F)
               (print-rules command-line-rules)
               0)
            ((get dict 'about F)
               (print "Yes.")
               0)
            ((null? paths)
               (print "I need data.")
               1)
            (else
               (sequence dict paths
                  (list read-data preprocess cluster compare render)))))))

(λ (argl)
   (if (null? argl)
      (print "That's odd.")
      (platypus (cdr argl)))) ;; drop executable name
   

