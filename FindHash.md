# Introduction #

This page discusses the idea of automatically analyzing a string of data for finding embedded [hash function digests](http://en.wikipedia.org/wiki/Hash_algorithm) (digests colloquially known as hashes). For this task we apply [suffix arrays](http://en.wikipedia.org/wiki/Suffix_array) to skip redundant checks. Overlooking some implementation details, the result should be theoretically somewhat optimal, in terms of algorithmical complexity.


# Rationale #

When inferring structural model from a chunk of data it may be useful to know whether the data contains embedded hash function digests. By embedded hash function digest we mean, in this context, a digest created with a hash function (such as [MD5](http://en.wikipedia.org/wiki/MD5)) from some other piece of the data in question. This practice is used e.g. in some network protocols, to provide checks against data corruption. A string of symbols (often bytes) _a<sub>0</sub>a<sub>1</sub>...a<sub>n</sub>_ is be fed to a hash function, and the resulting digest _d<sub>0</sub>d<sub>1</sub>...d<sub>m</sub>_ appended into the original data. The resulting string _a<sub>0</sub>a<sub>1</sub>...a<sub>n</sub>d<sub>0</sub>d<sub>1</sub>...d<sub>m</sub>_ then contains a means to check, with high probability, that the original piece of data (_a<sub>0</sub>a<sub>1</sub>...a<sub>n</sub>_) has survived intact through the tubes.

If the general case was to just append the data, looking for potential hashes would be easy. Just split the data into a suffix and prefix and check whether the suffix is a digest of the prefix. But often the hash is embedded in the middle of the data. For example a network protocol header may contain a checksum of either it itself, of the container payload. Quite a pickle.


# Brute Force Approach #

The way to find all the potential places for embedded digests, really, is to go through all unique substrings, find out their digest, and check whether the digest appears in the data. Even if we knew some shortcuts for a hash function X, the above approach is the only general way that applies for all typical hash functions. Or, well, if you find out a better general shortcut, you'll be rich and famous. Or as rich and famous as one can become with mathematics.

Anyway, the brute force way is to go through all the data's substrings, hash them and check whether some resulting digest is contained in the data. Unfortunately a text of length _n_ contains _(n+1)n/2_ substrings (or, as scientists put it, _O(n<sup>2</sup>)_). And let's just say that the digest length is constant, so checking whether a digest exists in the text is _O(n)_. So, together the naive approach has the time complexity of _O(n<sup>3</sup>)_. Quite a (pickle)<sup>3</sup>.


# Suffix Arrays! #

Let's first get rid of the _O(n)_ time complexity of finding a given digest from the data. There has been a lot of work done in the area of making searches faster by pre-indexing the searched data. One fruit of such research is the concept of _suffix arrays_. A suffix array for data _D_ is basically an array of _D_'s substrings, sorted alphabetically. There's an naive and easy way to create such an array (list the substrings, sort the list using some general sorting algorithm), but it takes _O(n<sup>2</sup> log n)_ time. The real kicker is that you can create a suffix array in just _O(n)_ time (we use a _O(n log n_) time algorithm, but it's quite a bit faster than the naive case anyway). You really can't get lower pre-processing time complexity bounds than that.

Using just the list of sorted suffixes and a basic binary search algorithm you can search for a substring of length _m_ (a digest in this case) on _O(m log n)_ time, or in our case, _m_ being a constant, _O(log n)_. If you augment your suffix array with longest common prefixes (LCPs) you can shave some constants off to make it in _O(m + log n)_ time. Our implementation uses a basic _O(m log n)_ time search. It's good enough, and simple to boot. If we really wanted to be optimal, there are ways to make the search _O(m)_ time with _O(n)_ time preprocessing step (such as [suffix trees](http://en.wikipedia.org/wiki/Suffix_tree)). They tend to be harder to implement and take more memory, though.


# Redundancy Redundancy #

When we said earlier that a text of length _n_ contains _n(n-1)/2_ substrings we were being sly. The problem was that we didn't take into account that there might be repeated substrings. You can look at it this way: How many substrings does string _abba_ have? Well, _a_, _ab_, _abb_, _abba_, _b_, _bb_, _bba_, _b_, _ba_, _a_, forgetting the empty substring, so _(n+1)n/2 = 5\*4/2 = 10_ so we're correct! Kind of. There are only 8 _unique_ substrings! That doesn't seem much of an improvement. But when we're dealing with structured data (which tends to contain patterns and repetitions) with a limited alphabet (if you have, say, 1000 symbol string that contains only 2 symbols, it's bound to contain repetitions), it all starts adding up.

LCP augmented suffix arrays (or suffix trees) can be used to walk through all unique substrings. Also, if two substrings _s<sub>1</sub>_ and _s<sub>2</sub>_ share the same prefix _p_ and we have the hash function state saved for _p_, we can use that state for calculating both _s<sub>1</sub>_ and _s<sub>2</sub>_ and save the trouble of calculating _p_ twice. Therefore, finding out the hash for one suffix has a constant amortized time complexity. Calculating the substring digests for a text containing _S_ unique substrings has the time complexity of _O(S)_, which has to be optimal in the general case.


# Conclusion #

Combining these two optimizations (calculating digests only for unique substrings in optimal time and checking whether the digest appears in the text in optimal time) we get the combined time complexity of _O(n + S)_ for data of length n and with S unique substrings. This assuming that the preprocessing step can be done in _O(n)_ time and searching for a digest in constant time (assuming the digest length is constant). Suffix trees can do that. We should probably check whether suffix arrays can, too. At the moment our [current implementation](http://code.google.com/p/ouspg/source/browse/trunk/findhash) uses a very simple implementation of suffix arrays and digest searching, having total _O((n + S) log n)_ time complexity.