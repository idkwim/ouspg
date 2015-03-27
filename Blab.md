# Blab - a Data Production System #

Blab is a small tool for generating data according to grammars. You can think of it as a reverse grep or yacc. It is intended to be used to generate data which has a known context-free structure, usually in order to be able to test programs or produce interesting sample data for fuzzers. It could also be useful for modern poetry.

Nutshell:
```
 $ sudo apt-get install gcc curl
 $ curl https://ouspg.googlecode.com/files/blab-0.1.4.tar.gz \
   | tar -zxvf - && cd blab-0.1.4 && make && sudo make install
 $  blab -e '"hello" 32 "world\n" json "\ntro" "lo"+ 10'  
 hello world
 {"-wIs":-2,"1 8":[76,-24.3851e-1        ,-5811.4]}
 trololololo
```


## About ##

Blab is a tool for generating data. Data is a valuable asset in this day and age, but often not just any data will do. Often the data used by programs has a fairly simple structure. Some typical classes of structure are defined by regular expressions, which can describe things like phone numbets, and context free grammars, which capture structure such as syntax of programming languages and HTML documents. Grammars and regular expressions are used to accept and process these kinds of inputs, but conversely they can naturally be used to generate data. Blab does just that.

In robustness testing, which is largely about trying to find errors in programs and devices by feeding them data which they may fail to process in interesting ways, one of the easiest options is to mutate known valid data and see if the usually broken version cause issues. However, it is often the case that the data to be generated has fairly simple structure for which it is easier to write a grammar than to find samples or write a program generate it. Blab was written after running into this situation sufficiently many times. Rolling a simple program to produce this kind of data is trivial, but for example making a nice variance, handling repetition, limiting output size without breaking the structure and other points always end up taking a bit more effort than expected.

The most important problem for a tool like blab is to be simple enough to be useful. To this end, it only supports regexp-like operations, context free grammar rules and a simple way to make modular definitions. At least in internal use this has been the maximal set of features after which it starts to be easier to roll a program to generate data, but before which blab is significantly more pleasant to work with than a quick and dirty generator which may carp out an occasional infinitely large file while having mostly dominant structure in 95% of the outputs.


## Blab by Example ##

The language supported by blab is heavily influenced, but not equal, with common regular expressions. In particular, numbers in the input refer to bytes, and only parts enclosed in quotation marks are treated as string data. Strings not enclosed in quotation marks refer to rules of the builtin or user-defined grammar.

When blab starts up it reads some instructions what to do from the user, and then generates one or more output according to the instructions. It never generates an output that doesn't match the requirements, or to put it differently, it's not a fuzzer.

The grammar data can be given either either from command line, standard input or files. A simple example is generating a few bytes, like 42 and 10, which is '`*`\n' in ASCII and UTF-8.
```
 $ echo 42 10 | blab 
 *
 $ blab -e '42 10'
 *
 $ echo 42 10 > foo
 $ blab foo
 *
```

In case you're wondering, there is currently a delay in blab startup because it processes the builtin library every time it is started. This will be fixed soon.

The usual regexp quantifiers (`*`, + and {}) can be used with all structures, and they always refer to the thing on the left. Parenthesis can be used to group things together.

```
 $ blab -e '42* 10'
 
 $ blab -e '42+ 10'
 *****
 $ blab -e '42{80} 10'
 ********************************************************************************
 $ blab -e '(42 10){3}'
 *
 *
 *
```

A sequence of ASCII characters (or UTF-8) enclosed in quotation marks is equal to the same bytes (with UTF-8 encoding) enclosed in parenthesis.

```
 $ blab -e '"Hello, world!!1" 10'
 Hello, world!!1
```

All things in the grammar are implicitly catenated together. The | operation can be used make a union, that is, give more than one option. This is the first thing that introduces nondeterminism so far.

```
 $ blab -e '("foo" | "bar") 10'
 bar
 $ blab -e '"c" ("a" | "d"){3} "r"'
 cadar
```

Character classes can be used, but they are first class objects like bytes and strings, so they are not placed withín strings. A character class equals a union of all the options, after possibly UTF-8 encoding them.

```
 $ blab -e '(([wrstp][aeiouy]{1,2}){1,4} 32){5} 10'
 soty wypisi tisyro to patu 
```

All things so far have been doable within the realm of simple regular languages. Context free grammars are a slightly more expressive class of simple languages. To use this power, all you need to do is define named rules and use them. All of the examples so far have also been context free grammars, because the first rule blab encounters is always the one it also generates, or in other terms it is made the start symbol after implictly naming it if necessary. Naming it explicitly has no effect.

```
 $ blab -e 'foo = 42 10'
 *
```

To define multiple rules, just add multiple definitions. Newline is just a whitespace so you can put the definitions to same or separate lines.

```
 $ blab -e 'foo = a b+ 10
 a = "tro"
 b = "lo"
 '
trololololololo
 $ blab -e 'S = "You have my " W (" and my " W)+ "." 10 
    W = "axe" | "sword" | "bow" | "ballista" | "catapult" | "dice"'
 You have my bow and my catapult.
```

Blab has a builtin library (std.blab in sources) which defines many commonly needed things. You can try it out for example with:

```
 $ blab -e html -o test-%n.html -n 10
 $ firefox test-*.html
```

Notice that here we write the output to files instead of standard output. The output pattern can be given with -o, %n is replaced with the number of the output and -n specifies how many outputs to read from the grammar. Writing many outputs at a time has the benefit of needing to process the builtin library at startup only once.

Remembering which label names are used starts to get tedious if there are lots of them. Blab allows grouping definitions together with simple namespaces to make sure there are no accidental overlaps in the definitions. Anything defined in a namespace can be accessed with a dot-syntax much like in many programming languages and all such definitions see all the ones below them.

```
 $ echo '
 output = country.foo nl
 nl = 10
 country {
   foo = nl
   nl = "netherlands"
 } ' | blab
netherlands
```

## Blab Grammar Grammar ##

The incomplete grammar below describes most of the language currently used by Blab. There will likely be very few changes in the following versions. Incompleteness mainly refers to not specifying how everything apart from chars in strings are separated by arbitrary whitespaces.

The first Rhs or Defn is the output, or in other words, is the start symbol of the grammar, which is implicitly named in case of a plain Rhs.

```
Gram  = (Rhs | Defn) (Dict | Defn)*
Defn  = Label "=" Rhs 
Label = [a-z0-9-]+ ("." Label)*
Byte  = [0-9]+                  -- 0 <= Byte < 256
Str   = "\"" Char* "\""
Char  = (a valid UTF-8 representation of a code point)
Dict  = Label "{" Gram "}"
Num   = [0-9]+
CharC = "[" ((Char "-" Char) | Char)* "]"
Rhs   = Byte 
      | Label
      | Str 
      | CharC
      | Rhs+ 
      | "(" Rhs ")"
      | Rhs "|" Rhs
      | Rhs "*"
      | Rhs "+"
      | Rhs "{" Num "}"         -- == A{n} = {n,n}
      | Rhs "{" Num "," Num "}" -- {n,m}, n <= m
```


## Use and Abuse ##

Blab has been useful and profitable in testing many programs, either by generating data by itself or by acting as a sample generator for fuzzing.


## Troubleshooting ##

Having trouble with Blab? You can pick one or more out of:
  * file a bug at http://code.google.com/p/ouspg/issues/entry
  * complain to ouspg@ee.oulu.fi
  * pester aoh on #freenode


## Examples ##

Some bugs found using Blab:
  * CVE-2011-0075 (OUSPG)
  * CVE-2011-0074 (OUSPG)
  * CVE-2011-1434 (OUSPG)
  * CVE-2011-1186 (OUSPG)
  * CVE-2011-3232 (OUSPG)
  * CVE-2011-3661 (OUSPG)
  * CVE-2013-2844 (cons0ul)