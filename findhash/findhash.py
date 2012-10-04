import suffixarray

def find_hash(text, hash_func):
    suffixes = suffixarray.sort_suffixes(text)

    initial_hash = hash_func()
    hash_stack = [initial_hash]
    hash_size = len(initial_hash.digest())
    
    for text, start, lcp in suffixes:
        hash_stack[lcp+1:] = []
        current_hash = hash_stack[-1].copy()

        digest = current_hash.digest()
        for position in find_digest(digest, suffixes):
            yield position, start, start+lcp

        for index in xrange(start+lcp, len(text)):
            current_hash.update(text[index])
            hash_stack.append(current_hash.copy())
            
            digest = current_hash.digest()
            for position in find_digest(digest, suffixes):
                yield position, start, index+1

def find_digest(digest, suffixes):
    if not suffixes:
        return

    lo = 0
    hi = len(suffixes)-1
    size = len(digest)

    lo_text, lo_start, _ = suffixes[lo]
    lo_text = lo_text[lo_start:lo_start+size]

    hi_text, hi_start, _ = suffixes[hi]
    hi_text = hi_text[hi_start:hi_start+size]
    
    if lo_text > digest:
        return
    if hi_text < digest:
        return

    while hi > lo+1:
        mid = (lo + hi) // 2
        mid_text, mid_start, _ = suffixes[mid]
        mid_text = mid_text[mid_start:mid_start+size]
        
        if mid_text < digest:
            lo, lo_text, lo_start = mid, mid_text, mid_start
        else:
            hi, hi_text, hi_start = mid, mid_text, mid_start

    for index in xrange(lo, len(suffixes)):
        text, start, _ = suffixes[index]
        text = text[start:start+size]
        if text > digest:
            break
        if text == digest:
            yield start

if __name__ == "__main__":
    import hashlib
    import unittest

    def check_hash(text, pos, start, end, hash_func):
        digest = hash_func(text[start:end]).digest()
        assert text[pos:pos+len(digest)] == digest

    class FindHashTest(unittest.TestCase):
        def test_basic(self):
            text = "abba"
            end = len(text)
            text += hashlib.md5(text).digest()

            assert list(find_hash(text, hashlib.md5)) == [(end, 0, end)]

        def test_random_strings(self):
            import random

            for size in (1, 10, 100, 500):
                text = [random.randint(0, 255) for _ in xrange(size)]
                text = "".join(map(chr, text))

                start = random.randint(0, size-1)
                end = random.randint(start+1, size)

                digest = hashlib.md5(text[start:end]).digest()
                
                text = text[:end] + digest + text[end+1:] + text
                results = list(find_hash(text, hashlib.md5))

                assert (end, start, end) in results
                for position, start, end in results:
                    check_hash(text, position, start, end, hashlib.md5)

    unittest.main()
