# License: MIT <http://www.opensource.org/licenses/mit-license.php>

from __future__ import with_statement
import sys
import new
import inspect
import byteplay

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

    def __exit__(self, type, value, traceback):
        # If all this nonsense was caused by some other exception than
        # EndTracing, reraise it (and if value is None, just quit).
        if not isinstance(value, EndTracing):
            return False

        # Capture the calling frame's bytecodes.
        frame = inspect.currentframe(1)
        code = frame.f_code

        # Store the BLOCK bytecodes (plus some extra ones left from
        # the original with statement - we'll get rid of those later).
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
            
            # Make giving arguments resemble returning values
            # (e.g. block() sends None as an argument, block("x")
            # sends "x" and block("x", "y") sends ("x", "y")).
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

# Some fairy-dust sprinkles to wrap all this heresy into a decorator.
def takes_block(func):
    def _takes_block(*args, **keys):
        return BlockContext(func, *args, **keys)
    return _takes_block
