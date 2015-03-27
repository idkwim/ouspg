## Abstract ##

This page describes a below quadratic algorithm for sorting suffixes purely functionally. All of the functional suffix sorting algorithms we have seen previously have had at least O(n^2) worst case time complexity. Even though the imperative algorithms are already doing O(n), it still would be nice to get at least something with O(n log n) worst case complexity purely functionally, to be able to construct these data structures in functional programs. This page is working towards one such solution.

Index:


# Introduction #

Suffix arrays and suffix trees useful and in many respects exchangeable data structures for indexing data. They can be used for example to find all occurrences of a substring from a piece of data in time linear to the length of the pattern instead of to the amount of the data. Obviously these data structures can be constructed only once, and then shared for use in many places.

Using the obvious underlying data structures, a suffix array gives O(n) space requirement and O(n log n) substring search time, whereas a suffix tree usually takes more space but gives O(n) search. We are here (for now) working with suffix arrays.

Suffix arrays and trees can be constructed in O(n) time using various algorithms which scale up to processing several gigabytes of data in a few hours. For some reason however, all the purely functional algorithms I've seen have used the obvious O(n^2) algorithms. This is probably because the faster algorithms rely heavily on mutations, making them much less pleasant to convert to a functional language. Like quicksort, they have a good enough average behavior for many uses.


# Suffix array #

A suffix array is simply an array of all the suffixes of a string, which are sorted by the lexicographic order of the suffixes. For example the string _banana_ has, if you include the last empty one, suffixes starting at positions (0 1 2 3 4 5 6), which as strings are ("banana" "anana" "nana" "ana" "na" "a" ""). Sorting the suffixes gives:

```
 0 - banana      6 - 
 1 - anana       5 - a
 2 - nana   --\ 3 - ana
 3 - ana    --/ 1 - anana
 4 - na          0 - banana
 5 - a           4 - na
 6 -             2 - nana
```

so the suffix array of "banana" is (6 5 3 1 0 4 2). Depending on your need, a separate end-of-string marker can be used, and if used it is convenient to make it either the smallest or largest value. We are here using it as the smallest one. Notice that as a result, the end of string (or it's length) is always the first value of the suffix array.

Why is this data structure useful? Suppose you need to find all the occurrences of the string "ana". Since the suffix array is sorted, all suffixes at which some string occurs are clustered together. To find a string, all you have to is find, usually with binary search, the first and last position in the suffix array where it occurs. All the positions at which it occurs are then the values between the first and last position in the suffix array. Finding strings either exactly or with errors, finding most or frequently overlapping substrings, sequence alignment and several other problems become much simpler if you have a suffix tree or an array at hand.


# Implementations #

To construct the suffix array, all you need to do is sort the suffixes. There are many ways one can sort a list of strings, and taking advantage of the fact that the strings are related gives some extra speed in the sensible sorting options. Even the simplest algorithms have O(n log n) expected speed when given normal real-world data, and close to linear if the data is random. However, a string with enough repetition will clearly show the upper bound.


## String sort (bad) ##

The simplest option is to simply sort the suffixes using a normal sorting function and string comparison. As we all know, comparison based sorting has a bound at O(n log n) comparisons, which is achievable for example by using mergesort. This is the compelexity we are aiming for, but we have not yet counted the comparisons. Woops, there comes another O(n), making string sorting way above O(n^2) for sorting a string of size n.


## Radix string sort (bad for suffixes) ##

A more sensible approach to arbitrary string sorting uses ternnary partitioning. On each step a pivot character occurring as the first character in some string is selected. The strings are then partitioned into those starting with a lower character, those starting with a higher character, and the ones having the same character. The lower and higher lists can be sorted recursively. For the list having the same character, one can sort starting from the second character, since the first ones are already known to be the same. This avoids many unnecessary comparisons of same characters, and can be implemented for example by carrying the sorting depth as one variable in the sorting function, or if lists are used as input, simply dropping the first values in the middle partition.

Notice that this function constructs a tree while the sorting proceeds, and then flattens in while appending the results.


## Iterative tree building (bad) ##

For suffix trees, one approach is to start from the beginning or end of the tree, and store each suffix to it. Simple, but also leads to O(n^2) if no special tricks are used.


## Top-down suffix tree building (bad) ##

Like in radix string sort, one can construct a suffix tree during the sorting phase. The most natural solution is to use a radix sort at each step to cluster suffixes starting with a given character, and build a tree node having a link from each node to similarly constructed child nodes.


## Functional qsufsort (better) ##

The qsufsort algorithms (due to Larsson and Sadakane) iteratively sorts all strings to depth n, but instead of growing the depth linearly, it is doubled in each step. The sorting starts by first sorting the data according to the first character of each suffix, but then only the positions of the suffixes are used to sort the rest. Like the naive algorithms, this has only slightly above linear expected running time, but the upper bound is O(n log n).

A first sketch of a functional version of the algorithm follows:

```
(define (car< a b) (< (car a) (car b)))
(define (cdr< a b) (< (cdr a) (cdr b)))

(define (invert bs) (map car (sort cdr< (iff->list bs))))

(define (get-run x vs out)
   (cond
      ((null? vs) (values out vs))
      ((eq? (caar vs) x) (get-run x (cdr vs) (cons (cdar vs) out)))
      (else (values out vs))))

(define (chunk vs bs tl n) ; -> ls' + bs'
   (if (null? vs)
      (values tl bs)
      (lets
         ((l vs (get-run (car (car vs)) vs null))
          (ln (length l))
          (lid (+ n ln))
          (bs (fold (lambda (bs p) (iput bs p lid)) bs l))
          (vs bs (chunk vs bs tl (+ n ln))))
         (if (null? (cdr l))
            (values vs bs)
            (values (cons l vs) bs)))))

(define (ssort-bucket l bs tl n)
   (chunk
      (sort car< (map (lambda (p) (cons (iget bs (+ p n) -1) p)) l))
         bs tl (- (iget bs (car l) False) (length l))))

(define (ssort-step ss bs n) ; -> ls' + bs'
   (if (null? ss)
      (values ss bs)
      (lets
         ((bucket (car ss))
          (tl bs (ssort-step (cdr ss) bs n)))
         (ssort-bucket bucket bs tl n))))

(define (ssort-steps ls bs n)
   (if (null? ls)
      (invert bs)                               ; O(n), read results
      (lets ((ls bs (ssort-step ls bs n)))
         (ssort-steps ls bs (* n 2)))))

(define (ssort lst)
   (lets
      ((sb (lzip cons lst (lnums 0)))
       (sb (cons (cons -1 (length lst)) sb))
       (sb (sort car< sb))
       (ls bs (chunk sb False null -1)))
      (cdr (ssort-steps ls bs 1))))
```

## Functional Ukkonen (not here yet) ##

This might be the next target for conversion.


# Benchmarks #

not here yet.