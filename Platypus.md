## About ##

Platypus is a tool fir digging similarities from a set of files and reporting the findings in a human-readable form. It is not exactly a security-related application and thus has a fairly low development priority. It does however seem to come in handy occasionally, so we decided to make it public and will be making some improvement later on.

## History ##

Even the hackers of OUSPG cannot devote their full attentio to meditating the Zen of programming. We also have to take part in various mundane chores involved in running a world-class university, one of which is teaching. We have been envisioning AI-based solutions to this issue, but for now only several individual bits of the process have been automated. Platypus is a small such bits, https://www.raippa.fi/ a bit larger.

Platypus was designed to be a simple way to quickly check programming assignments. Teaching elementary programming is one hard nut to crack, because the base skill levels of students vary greatly, and somewhat independently about 20% of students seem to grok the ideas much faster than the rest. These combined make planning the difficulty levels of assignments exams pretty hard. Plagiarism is a natural consequence when the amount of work does not scale with the benefits.

We decided to roll our own plagiarism detector when a large and particularly troublesome set of submissions rolled in. Many of the works with poorer scores were obviously original and probably had measurable quantities of sweat, tears, frustration, anger and eventually experience and enlightenment in their development history. On the other hand, clusters of many of the fairly well scored submissions had similar quirky features and the writers often had trouble explaining the higher and lower level ideas their solutions. We decided to apply some of our black-box structure inference techniques (algorithms, in our project-local dialect) to weed out the worst copycats and correspondingly reward some of the originally unappreciated originality.

## Plagiarism ##

So, what is this plagiarism thing? In theory there is one kind of plagiarism, namely that where A copies something from B, so you need to just check all the information in the world to see if B has been plagiarized from some A. It probably has.

In practice there are two kinds, the one where A and B are in an available set of documents which is of manageable size, and the case where you probably only get one of them. In the latter case you want to understand the structure of the data and report sections which are somehow stylistically too different from the rest of the data. When the full data is available, such as the 0.1-10Mb data sets in our programming assignments, you can run a global algorithm. The current version of Platypus will probably handle 10-100Mb data sets for now and larger ones later.

So, to detect plagiarism in a set of documents there are two obvious things you want to keep in mind:
  * Plagiarism manifests as redundancy.
  * Redundancy is the difference between the size of a chunk of data and it's Kolmogorov complexity.

Sadly Kolmogorov complexity is uncomputable, so we had to pick an approximation. The Madam algorithm (maximal digram abstraction), which we developed a long time ago, liked for it's simplicity, also found useful, keliaakikkoman implemented it in O(n), and found had all been done before under a the sexy name RE-PAIR, was selected. The algorithm operates as follows:
  1. Read in a set of data
  1. Replace all occurrences of the most frequent pair (digram) of symbols with a new symbol if it occurs at least twice.
  1. Repeat if necessary.

This captures sharing remarkably efficiently. You can then do cool stuff with the resulting grammar, which is a fairly good approximation of the optimal deterministic CFG for the data, which is a fairly good approximation of the Kolmogorov complexity of the data. It also makes it easy to render human-readable reports instead of blurting out "SIMILARITY 0.4273313" from a pairwise compression ratio, which would be another obvious option.

Furthermore, instead of computing the O(n²) pairwise similarities separately, a global approach makes sense, because it is useful to try to learn some of the features of the language used in the documents, because different often idiomatic expressions cause different amounts of redundancy. Our approach is to add new nonterminals from the grammar effectively into an extended alphabet (by setting their weight in coparisons to 1) if they occur in sufficiently many of the documents. There are however still O(n²) performed at the end of the run, or O(n(n+1)/2) if you allow similarity to be symmetric, which will be addressed in later versions.

In our experience this approach has the good features of being theoretically sane and hard to fool, like pairwise compression with a good algorithm, and it also makes results that are useful for humans, like n longest shared substrings by multiple sequence alignment using suffix trees or arrays, neither of which naturally has both features.

**Note:** Instead of developing more and more thorough plagiarism detection techniques to penalize the students, we would like to encourage splitting programming assignments into smaller and more manageable bits to ensure each student has at least some level of understanding on the subject, and then devoting extra focus into trying to allow and encourage working as groups. Copying existing code and modifying it to do something else is a natural way to learn and often a useful skill in real life.

## Requirements ##

The current system requires:
  * A UNIX-like system like Linux or a BSD.
  * Gcc

## Installation ##

The following instructions should work for getting Platypus up and running in Debian or Ubuntu Linux distributions.

```
 $ sudo apt-get install gcc wget
 $ wget http://ouspg.googlecode.com/files/platypus.c.gz
 $ gunzip platypus.c.gz
 $ gcc -O2 -o platypus platypus.c
 $ ./platypus --help
```

Issues arising in the installation or use can be reported to http://code.google.com/p/ouspg/issues/entry.

## Usage ##

The intention is to create a tool which would give a useful summary for files, no matter what the programming- or natural language used, using the same `platypus -o /tmp works/*` command. There is also some ability to fine-tune the operation, for example by providing course material which is ok to share usign `platypus -o /tmp --clean code-samples.txt works/*`.

For example a report built from all .c and .h files of Linux kernel version 2.2.26 is available at http://www.ee.oulu.fi/~aki/linux-2.2.26-report/. A run for the current stable kernel sources took a bit more memory than the test machine had available.

The similarity analysis builds a graph where each node represents a file and each edge, having a score, represents the similarity (a weighted score of shared nonterminals).The weak links are first removed leaving a more or less connected graph. The resulting components are then displayed in the red boxes, which are sorted by the total weight of the links in the edges. An arrow can be read as 'much of my structure is also in', and the size of the arrow and text correspond to the weight of the edge(s) in the graph. Therefore a box with only one pair of files with a bidirectional arrow between them means they are quite probably probably related. Notice that very small files tend to be contained in larger ones with a high probability, which may lift less interesting clusters up in the list, but does not affect the pairwise scores.

After the red boxes comes a usually bigger green box containing all the components of size 1, meaning they were not especially similar with any other files. These are sorted by a score giving the percentage of structures (nonterminals in the grammar) which are only present in the particular file. A higher score means more relative originality.

The last blue box lists all the rules which were frequent enough to be marked as typical content. For source code this usually contains a few licenses or large pieces of them, followed by lots of keywords and idiomatic expressions. This list essentially gives an extended alphabet for viewing the data.


## Todo ##

A rough sketch of next moves:
  * make recursive divide and conquers passes on the data recursively
  * use less memory
  * use less time
  * write reports differently when the data is binary
  * handle at least UTF-8 encoded data differently
  * as with other tools, start providing plain binary- or C-code downloads when the tool is good enough