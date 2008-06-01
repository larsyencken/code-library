def reverse(s):
    result = []
    for i in xrange(len(s) - 1, -1, -1):
        result.append(s[i])
    return ''.join(result)