from math import log

def bin(x):
    """Returns a binary string representation of the given integer.""" 
    result = []
    for i in range(int(1 + log(x)/log(2))):
        if x & (1 << i):
            result.append('1')
        else:
            result.append('0')

    result.reverse()

    return ''.join(result)
