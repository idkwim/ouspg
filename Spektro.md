# Spektro #

Spektro is a simple tool to assist in analysis of mass-spectrometry
runs. It computes a stream of candidate molecules satisfying given
charge- and mass requirements from given elements or compounds. Spektro
was developed after we learned that there is a time-consuming mechanical
problem our local chemists occasionally have to solve. We wrote a few
solutions to the problem, this being one of them.

## The problem ##

A mass-spectrometer is used to analyze the chemical composition of some
sample material or structure of a molecule. A run basically diverts
a stream of ionized molecules with a magnetic field and measures the
resulting mass to charge ratios based on different trajectories caused
by different masses. A run produces a list of mass and intensity pairs,
from which a poor chemist may have to mentally to deduce the structure
of the (sub)molecule causing each spike based on the known mass, charge
and possibly list of elements possibly occurring in the molecule.

Abstracting away the possible mass-spectrometer calibration issues, what
remains is a constraint satisfaction problem. A chemist picks all or
some of the highest intensity masses and rounds the masses to integers
(molecules with fractional masses tend to be rather rare) giving a set
of masses {m<sub>1</sub>, m<sub>2</sub> .. m<sub>n</sub>}. The known
facts are that each of these is an ion with a known charge c (typically
-1 or +1), and it is usually known or conjectured to be composed
of some elements {E<sub>1</sub>, E<sub>2</sub> .. E<sub>i</sub>}
each having a known mass E<sub>i</sub><sup>m</sup> and charge
E<sub>i</sub><sup>c</sup>. The basic problem is now to find for
each mass m<sub>j</sub> a set of integral factors {f<sub>1</sub>,
f<sub>2</sub> .. f<sub>i</sub>} and an integer k so that Σ
f<sub>x</sub>E<sub>x</sub><sup>m</sup> = km<sub>j</sub> and the charges
correspondingly satisfy Σ f<sub>x</sub>E<sub>x</sub><sup>c</sup> = kc.

Another related problem is to find a similar solution for an arithmetic
progression of masses where one wishes to have a sub-molecule repetition
of which causes the progression while preserving the constraints.

Spektro currently receives the data from command line arguments and tries to find as many results as requested for each mass or the given progression of masses. It is intended to also incorporate some other useful features later. The remaining task for the chemist is to pick the solution which makes sense.

## Getting Spektro ##

You need to have a working C-compiler and `make` get spektro running. In Debian-based Linux distributions you can get suitable ones with:
```
 $ sudo apt-get install gcc make
```

Installation:
```
 $ wget http://ouspg.googlecode.com/files/spektro-0.1.tar.gz
 $ tar -zxvf spektro-0.1.tar.gz
 $ cd spektro-0.1
 $ make
 $ sudo make install # or just run ./spektro <arguments>
```

## Basic usage ##

For example to find one solution to an equation summing to charge 0 and
mass 18, using (O)xygen having mass 16 and charge -2,
and (H)ydrogen having mass 1 and charge +1, one could run:

```
 $ spektro --charge 0 -e O,16,-2 -e H,1,+1 -n 1 18
 m/z 18 = 2H + O
 Enough goals computed for m/z 18. Use -n &lt;limit&gt; to get more if needed.
```

Each element is given as a 3-tuple having name, mass and charge
separated by commas. And now a more realistic example. Notice that the
problem may have an infinite number of solutions. The solutions where k
(shown as usual as the divisor in the results) is small are usually more
interesting, so the program operates by finding all solutions for each
k in order. Also, all multiples of found solutions are solutions but not
interesting ones, so they are not printed. The multiples are easy to find,
because they are simply the ones where none of the factors and k are
coprime.

```
 $ spektro -c -1 -e A,27,+3 -e P,31,+5 -e O,16,-2 -e H,1,+1 -n 20 341
 m/z 341 = 37H + 19O
 m/z 341 = 2H + 12O + 3P + 2A
 m/z 341 = 20H + 15O + 3A
 m/z 341 = 3H + 11O + 6A
 m/z 341 = 3H + 25O + 9P / 2
 m/z 341 = 21H + 28O + 6P + A / 2
 m/z 341 = 39H + 31O + 3P + 2A / 2
 m/z 341 = 57H + 34O + 3A / 2
 m/z 341 = 22H + 27O + 3P + 5A / 2
 m/z 341 = 5H + 23O + 3P + 8A / 2
 m/z 341 = 23H + 26O + 9A / 2
 m/z 341 = 40H + 44O + 9P / 3
 m/z 341 = 58H + 47O + 6P + A / 3
 m/z 341 = 76H + 50O + 3P + 2A / 3
 m/z 341 = 5H + 37O + 12P + 2A / 3
 m/z 341 = 94H + 53O + 3A / 3
 m/z 341 = 23H + 40O + 9P + 3A / 3
 m/z 341 = 41H + 43O + 6P + 4A / 3
 m/z 341 = 59H + 46O + 3P + 5A / 3
 m/z 341 = 77H + 49O + 6A / 3
 Enough goals computed for m/z 341
```

## Solving series ##

In addition to solving a single mass, it is often useful to solve an
arithmetic progression of masses where a part of a molecule is repeated
several several times. This arises when molecules are fragmented for some
reason causing netral chunks to be chopped off. These kind of solutions
are searched for if the -s or --series flag is given and the masses are
suitable. The repeated part of the molecule is shown in square brackets.

```
 $ spektro -c 0 -n 1 -s -e H,1,0 5 7 9 11 
 Solving mass series (5 7 9 11) having interval 2.
 Total charge must be 0.
 Elements are:
    name: H, mass: 1, charge: 0
 
 m/z 5 = 5H + [2H]
 Enough goals computed for m/z 5. Use -n &lt;limit&gt; to get more if needed.
```

And again a more realistic example. First suitable intervals are searched
using -i from a set of masses obtained from spikes in the spectrometry
results, and then suitable solution candidates are searched for.

```
 $ spektro -i 79 97 99 101 105 113 121 127 129 136 149 153 157 159 163 173 
 177 185 187 193 199 207 209 210 212 213 215 217 219 237 249 251 253 265 273 281 
 282 285 289 295 297 309 311 317 325 326 339 340 341 369 370 383 384 385 413 414 
 427 457 458 471 501 502 515 545 546  589 590 633 677
 ..
 Interval with delta 44 and length 14: (105 149 193 237 281 325 369 413 457 501 
 545 589 633 677)
 $ bin/spektro --charge +1 -e A,27,+3 -n 10 -e P,31,+5 -e O,16,-2 -e H,1,+1 -s 
 105 149 193 237 281 325 369 413 457 501 545 589 633 677 
 Solving mass series (105 149 193 237 281 325 369 413 457 501 545 589 633 677) having interval 44.
 Total charge must be 1.
 Elements are:
    name: A, mass: 27, charge: 3
    name: P, mass: 31, charge: 5
    name: O, mass: 16, charge: -2
    name: H, mass: 1, charge: 1

 m/z 105 = 14H + 13O + 3P + [9H + 6O + A] / 3
 m/z 105 = 32H + 16O + A + [9H + 6O + A] / 3
 m/z 105 = 15H + 12O + 4A + [9H + 6O + A] / 3
 m/z 105 = 13H + 16O + 4P + A + [6H + 7O + P + A] / 4
 m/z 105 = 31H + 19O + P + 2A + [6H + 7O + P + A] / 4
 m/z 105 = 14H + 15O + P + 5A + [6H + 7O + P + A] / 4
 m/z 105 = 47H + 26O + 2P + [3H + 8O + 2P + A] / 5
 m/z 105 = 12H + 19O + 5P + 2A + [3H + 8O + 2P + A] / 5
 m/z 105 = 30H + 22O + 2P + 3A + [3H + 8O + 2P + A] / 5
 m/z 105 = 13H + 18O + 2P + 6A + [3H + 8O + 2P + A] / 5
 Enough goals computed for m/z 105. Use -n &lt;limit&gt; to get more if needed.
```