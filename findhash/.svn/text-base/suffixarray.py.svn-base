def shared_prefix(left_text, left, right_text, right, shared=0):
    end = min(len(left_text)-left, len(right_text)-right)

    for i in xrange(shared, end+1):
        left_ch = left_text[left+i:left+i+1]
        right_ch = right_text[right+i:right+i+1]

        if left_ch != right_ch:
            return i
    return end

def _combine(left, right):
    current = list(reversed(left))
    other = list(reversed(right))

    result = list()

    other_text, other_offset, other_prefix = other.pop()
    while current:
        current_text, current_offset, current_prefix = current.pop()

        prefix = shared_prefix(current_text, current_offset, 
                               other_text, other_offset, 
                               min(current_prefix, other_prefix))
        current_ch = current_text[current_offset+prefix:current_offset+prefix+1]
        other_ch = other_text[other_offset+prefix:other_offset+prefix+1]

        if current_ch <= other_ch:
            result.append((current_text, current_offset, current_prefix))
            other_prefix = prefix
        else:
            result.append((other_text, other_offset, other_prefix))
            other_text = current_text
            other_offset = current_offset
            other_prefix = prefix
            current, other = other, current

    other.reverse()
    result.append((other_text, other_offset, other_prefix))
    result.extend(other)
    return result

def _sort_suffixes(text, start, end):
    if start == end:
        return []
    if end-start == 1:
        return [(text, start, 0)]

    cut = (start + end) / 2
    left = _sort_suffixes(text, start, cut)
    right = _sort_suffixes(text, cut, end)

    return _combine(left, right)
    
def sort_suffixes(text):
    return _sort_suffixes(text, 0, len(text))

def combine(*suffix_arrays):
    if not suffix_arrays:
        return list()

    if len(suffix_arrays) == 1:
        return suffix_arrays[0]

    cut = len(suffix_arrays) / 2
    left = combine(*suffix_arrays[:cut])
    right = combine(*suffix_arrays[cut:])
    return _combine(left, right)

if __name__ == "__main__":
    import unittest

    def check_suffix_array(suffix_array):
        from itertools import izip, islice

        pairs = izip(suffix_array, islice(suffix_array, 1, None))
        for (prev_text, prev_offset, _), (text, offset, shared) in pairs:
            prefix = shared_prefix(prev_text, prev_offset, text, offset)

            assert prefix == shared, "incorrect shared prefix"
            
            prev_ch = prev_text[prev_offset+prefix:prev_offset+prefix+1]
            ch = text[offset+prefix:offset+prefix+1]
            assert prev_ch <= ch, "incorrect ordering"

    class SharedPrefixTest(unittest.TestCase):
        def test_short_string(self):
            text = "abba"
            assert shared_prefix(text, 0, text, 3, 1) == 1

        def test_same_strings(self):                                 
            text = "abba"
            assert shared_prefix(text, 0, text, 0) == len(text)

    class SortSuffixesTest(unittest.TestCase):
        def test_short_string(self):
            suffixes = sort_suffixes("abba")
            check_suffix_array(suffixes)

        def test_random_strings(self):
            import random

            for size in (1, 10, 100, 1000, 10000):
                text = [random.randint(0, 255) for _ in xrange(size)]
                suffixes = sort_suffixes(text)
                check_suffix_array(suffixes)

    class CombineTest(unittest.TestCase):
        def test_combine_zero(self):
            assert combine() == list()

        def test_combine_one(self):
            suffixes = sort_suffixes("abba")
            assert combine(suffixes) == suffixes

        def test_same_string(self):
            suffixes = sort_suffixes("abba")
            check_suffix_array(combine(suffixes, suffixes, suffixes))

        def test_random_strings(self):
            import random

            for size in (1, 10, 100, 1000, 10000):
                text = [random.randint(0, 255) for _ in xrange(size)]
                left = sort_suffixes(text)

                text = [random.randint(0, 255) for _ in xrange(size)]
                right = sort_suffixes(text)

                suffixes = combine(left, right)

                check_suffix_array(suffixes)

    unittest.main()
