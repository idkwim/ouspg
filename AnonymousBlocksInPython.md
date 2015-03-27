

Get your fix of kludgy code with no context whatsoever by using the following command:
```
svn checkout http://ouspg.googlecode.com/svn/trunk/blocks blocks-read-only
```

# Introduction #

Anonymous blocks are the new black. [Ruby's](http://allaboutruby.wordpress.com/2006/01/20/ruby-blocks-101/) got them, [Smalltalk's](http://en.wikipedia.org/wiki/Smalltalk#Code_blocks) got them, and by golly, now even [C's](http://en.wikipedia.org/wiki/Blocks_(C_language_extension)) got them! The small Dutch web programming language Python with slow whitespaces has a habit of borrowing features from C (recently, [buffer overflows](https://www.cert.fi/en/reports/2009/vulnerability2009085.html)), so blocks must be only a matter of time.

Smart-assery aside, about a month ago an [interesting blog post](http://billmill.org/multi_line_lambdas.html) by Bill Mill started circulating the tubes. In the post Bill presented how to emulate simple multi-line lambdas with Python's with-statements, the result somewhat resembling Ruby's nice block syntax. He built on ideas by [Richard Jones](http://www.mechanicalcat.net/richard/log/Python/Something_I_m_working_on.3) and Alex Martelli.

With Bill's accepts\_block-decorator you could write stuff like this:

```
>>> @accepts_block
... def each(iterable, block):
...     for i in iterable:
...         block(i)
...
>>> with each([1, 2, 3]):
...     def _(x):
...         print x
...
1
2
3
```

The construct could naturally be used for other less dull purposes, such as auto-retrying file or database operations. To use with-statements in Python 2.5 you need to write `from __future__ import with_statement` at the beginning of your module.

On this page we introduce a horrible hack (or rather a triplet horrible hacks) to take the concept a bit further:

```
>>> with each([1, 2, 3]) as x:
...     print x
...
1
2
3
```

The captured pseudo-block also operates in the with-statement's local namespace, like so:

```
>>> with each([1, 2, 3]) as x:
...     foo = x
...
>>> print foo
3
```

The blocks are reusable, so you can also do some callback-ish stuff:

```
>>> callback = None
>>> 
>>> @takes_block
... def capture_block(block):
...     global callback
...     callback = block
... 
>>> with capture_block() as x:
...     print x
... 
>>> callback("hello")
hello
```

Or just... I don't know.

```
>>> @takes_block
... def recursor_of_death(block):
...     block(block)
... 
>>> with recursor_of_death() as block:
...     block(block)
... 
```

# Outline #

According to [PEP 343 -- The "with" Statement](http://www.python.org/dev/peps/pep-0343/) code like:

```
with EXPR as VAR:
    BLOCK
```

it's translated to something resembling:

```
mgr = (EXPR)
exit = mgr.__exit__
value = mgr.__enter__()
exc = True
try:
    try:
        VAR = value  # Only if "as VAR" is present
        BLOCK
    except:
        exc = False
        if not exit(*sys.exc_info()):
            raise
finally:
    if exc:
        exit(None, None, None)
```

We can abuse that! The process goes roughly like this:

  1. `mgr.__enter__` sets a tracing function for the calling frame.
  1. The tracer strategically raises a special exception just before `BLOCK` starts executing. It also stores the frame's instruction pointer for later use.
  1. Upon the exception execution jumps automatically to `mgr.__exit__`.
  1. `mgr.__exit__` silentrly captures the special exception. It then uses the previously stored instruction pointer and the calling frame's current one to capture the bytecode representing `BLOCK`.
  1. Some bytecode-munging later we have a callable function which should do same things as `BLOCK` when called.
  1. Every time the block function is called its (potentially modified) locals are fed back to the original frame's locals.

The specifics differ, but that's the gist of it. Next sections go through the code, dividing it roughly by its three main hacks.

# Hack number 1: Trace-barfing #

To start off, let's define the dependencies for our hack.

```
from __future__ import with_statement
import sys
import new
import inspect
import byteplay
```

Now the real implementation starts. The first task is to make sure that `BLOCK` won't be prematurely executed, and to somehow capture its bytecode. Our `__enter__` function starts the game by setting a very special `trace` function/method that raises a very special `EndTracing` exception just before `BLOCK` would start executing. Raising `EndTracing` essentially skips over the whole block, right into the point where our `__exit__` function catches the exception.

`__enter__` also stores the current instruction pointer value of the calling frame (ie. the frame containing the with-statement). The value is then used to split the frame's bytecode string at the right point, and capture the bytecodes representing `BLOCK` in the next section.

```
# The special exception class for exceptions raised by the first
# tracing function, and captured by our __exit__ method.
class EndTracing(Exception):
    pass

class BlockContext(object):
    def __init__(self, func, *args, **keys):
        # Instruction pointer for later use.
        self.start = None

        # The actual context manager into which the block function
        # will be fed.
        self.func = func
        self.args = args
        self.keys = keys

    def __enter__(self):
        # Enable tracing by setting the globals tracing func to
        # anything true-ish. Otherwise setting the calling frame's
        # tracing function does nothing.
        sys.settrace(lambda *args, **keys: None)

        # Capture the calling frame, its current instruction pointer
        # value and set the magical tracing function.
        frame = inspect.currentframe(1)
        frame.f_trace = self.trace
        self.start = frame.f_lasti

    def trace(self, frame, event, arg):
        # If our calculations are correct, we should just be starting
        # to execute the block. Raise the magical exception,
        # essentially skipping over the block, straight to __exit__.
        raise EndTracing
```

# Hack number 2: Bytecode-munging #

For the next step we use the previously imported Noam Raphael's [byteplay](http://code.google.com/p/byteplay/) module, which eases the pain of playing with Python bytecodes. The code inside the nested `block` function is heavily influenced by (ie. stolen from) a [neat article](http://www.voidspace.org.uk/python/articles/code_blocks.shtml) by Michael Foord. Thanks, Noam and Michael!

`__exit__` uses the the previously captured instruction pointer value to splice, modify and reflect on the `BLOCK` bytecode string. This whole method is a behemoth in both size and complexity, but as they say: Release early, release often. The magical `force_locals` will be explained in the next section.

```
    def __exit__(self, type, value, traceback):
        # If all this nonsense was caused by some other exception than
        # EndTracing, reraise it (and if value is None, just quit).
        if not isinstance(value, EndTracing):
            return False

        # Capture the calling frame's bytecodes.
        frame = inspect.currentframe(1)
        code = frame.f_code

        # Store the BLOCK bytecodes (plus some extra ones left from
        # the original with-statement - we'll get rid of those later).
        code_str = frame.f_code.co_code[self.start:frame.f_lasti]
        new_code = new.code(0, code.co_nlocals, 
                            code.co_stacksize, code.co_flags, 
                            code_str, code.co_consts,
                            code.co_names, code.co_varnames, 
                            code.co_filename, "<block>", 
                            frame.f_lineno, code.co_lnotab)

        # Define the block function that runs the stored BLOCK
        # bytecode with the original frame's globals and locals and
        # stores the locals back.
        def block(*args):
            # Kudos, Noam Raphael.
            c = byteplay.Code.from_code(new_code)
            
            # Make giving arguments resemble returning values (block()
            # sends None as an argument, block("x") sends "x" and
            # block("x", "y") sends ("x", "y")).
            if not args:
                args = None
            elif len(args) == 1:
                args = args[0]

            # Find the first CALL_FUNCTION instruction and replace it
            # with LOAD_CONST and inject the block argument to imitate
            # __enter__'s return value.
            for i, (opcode, param) in enumerate(list(c.code)):
                if opcode == byteplay.CALL_FUNCTION:
                    c.code[i] = byteplay.LOAD_CONST, args
                    break

            # Get rid of the first SETUP_FINALLY instruction, an
            # artifact of the original with-statement.
            for i, (opcode, param) in enumerate(list(c.code)):
                if opcode == byteplay.SETUP_FINALLY:
                    del c.code[i]
                    break

            # Cut away some instructions in the end that are artifacts
            # of the original with-statement. The number or these
            # instructions differs in Python 2.5 and 2.6, but for both
            # the last POP_BLOCK seems to be a good marker.
            for i, (opcode, param) in reversed(list(enumerate(c.code))):
                if opcode == byteplay.POP_BLOCK:
                    c.code[i:] = []

            # For safety, ensure that the block ends with at least
            # some kind of return.
            c.code.append((byteplay.LOAD_CONST, None))
            c.code.append((byteplay.RETURN_VALUE, None))

            # Ensure the access to the original frame's locals. The
            # LOAD_, STORE_ and DELETE_ bytecodes accessing locals
            # need to be modified to use LOAD_NAME, STORE_NAME and
            # DELETE_NAME only. Kudos, Michael Foord.
            for i, (opcode, param) in enumerate(c.code):
                if opcode in (byteplay.LOAD_FAST, byteplay.LOAD_DEREF):
                    opcode = byteplay.LOAD_NAME
                elif opcode in (byteplay.STORE_FAST, byteplay.STORE_DEREF):
                    opcode = byteplay.STORE_NAME
                elif opcode == byteplay.DELETE_FAST:
                    opcode = byteplay.DELETE_NAME
                c.code[i] = opcode, param
            c.newlocals = False
            c.freevars = ()

            locals = dict(frame.f_locals)
            globals = frame.f_globals     
            try:
                return eval(c.to_code(), globals, locals)
            finally:
                self.force_locals(frame, locals)

        # Run the context manager with the given arguments and the
        # block function.
        result = self.func(*(self.args + (block,)), **self.keys)

        # Ensure that the magical exception won't be reraised.
        return True
```

# Hack number 3: Locals-forcing #

The last piece of the puzzle is how to force the block operate again in the local context it was defined in. Frame objects do have the attribute `f_locals`, which can be accessed - but not updated in the general case. Hm.

Turns out the key lies inside Richie Hindle's April Fools joke which with an implementation: [Goto for Python](http://entrian.com/goto/). The module actually manages to add `goto` and `comefrom` syntax into python with some creative trace trickery. Inside the Richie's module implementation lies something peculiar. The magical trace function modifies the frame's `f_lineno` attribute on the fly. Why can one modify _that_ but not `f_locals`? Not fair!

Wrong. Turns out tracing functions are special. Inside them you can modify stuff you usually can't, including the `f_locals`. Now we can define a function/method that takes a frame and updates its locals by trace trickery similar to earlier.

```
    def force_locals(self, old_frame, new_locals):
        def trace(frame, event, arg):
            # Update the locals and unset the frame's local trace function.
            frame.f_locals.clear()
            frame.f_locals.update(new_locals)
            del frame.f_trace

        # Force tracing on with setting the global tracing function to 
        # something non-false and set the frame's local trace function.
        sys.settrace(lambda *args, **keys: None)
        old_frame.f_trace = trace
```

As a finish, let's add a friendly touch and define the `takes_block` function decorator.

```
# Some fairy-dust sprinkles to wrap all this heresy into a decorator.
def takes_block(func):
    def _takes_block(*args, **keys):
        return BlockContext(func, *args, **keys)
    return _takes_block
```

# Bugs, limitations #

The code is riddled with all kinds of nasty corner cases and bugs. As an example, defining lambdas referring to locals in captured blocks seems to crash consistently:

```
>>> def func():
...     with each([1, 2, 3]) as x:
...         lambda: x
... 
>>> func()
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
  File "<stdin>", line 3, in func
  File "hack.py", line 67, in __exit__
    s = byteplay.Code.from_code(start_code)
  File "build/bdist.macosx-10.3-i386/egg/byteplay.py", line 346, in from_code
IndexError: tuple index out of range
```

Whether it's a flaw in our thinking or `byteplay` (probably our thinking) - no idea. Maybe someone with more mad Python skillz can look into that.

One big limitation in this approach is that you have to be extra careful to keep the parser happy and cooperative. So when run at module-level, this doesn't really work as intended:

```
>>> with each([1, 2, 3]):
...     return
... 
  File "<stdin>", line 2
SyntaxError: 'return' outside function
```

Neither does this:

```
>>> def func():
...     with each([1, 2, 3]) as x:
...         yield x
...
>>> func()
<generator object at 0xc0288>
```

Nor this a tad more subtle one:

```
>>> def func(foo):
...     with each([1, 2, 3]) as x:
...         print x
...         del foo
... 
>>> func("whatever")
1
2
3
```

And so on, and so on. But that's what you get by playing with fire. Moreover, this hack is likely very CPython 2.x specific. No bonus for Jython, IronPython etc. Or maybe it does work for other implementations. That would be something.

# A plea for help #

There you go. We can't commit into taking this thing any further. But everyone can grab the code and build on it, should the need arise. It's MIT licensed.

Oh, and almost forgot: We are sorry, Guido. So sorry.