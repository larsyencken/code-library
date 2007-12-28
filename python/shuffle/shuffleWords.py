import random

def shuffleWord(word):
    if len(word) <= 3:
        return word

    wordChars = list(word)
    first = wordChars.pop(0)
    last = wordChars.pop()
    random.shuffle(wordChars)
    return ''.join([first] + wordChars + [last])

def shuffleWords(input):
    output = []
    for word in input.split():
        output.append(shuffleWord(word))
    return ' '.join(output)

if __name__ == '__main__':
    x = raw_input('> ')
    while x:
        print shuffleWords(x)
        x = raw_input('> ')

