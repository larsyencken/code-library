import random

def shuffle_word(word):
    if len(word) <= 3:
        return word

    chars = list(word)
    first = chars.pop(0)
    last = chars.pop()
    random.shuffle(chars)
    return ''.join([first] + chars + [last])

def shuffle_words(input):
    output = []
    for word in input.split():
        output.append(shuffle_word(word))
    return ' '.join(output)

if __name__ == '__main__':
    x = raw_input('> ')
    while x:
        print shuffle_words(x)
        x = raw_input('> ')

