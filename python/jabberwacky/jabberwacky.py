import httplib
httplib.HTTPConnection.debuglevel = 0
import urllib
import re
import os

baseUrl = "http://www.jabberwacky.com/chat-joan"

class JabberWackyBot(object):
    def __init__(self):
        self.history = []
        pass

    def respond(self, text=None):
        if not text:
            data = urllib.urlopen(baseUrl).read()
            response = self._responseFromPage(data)
            self.history.append(response)
            return response

        self.history.append(text)
        urlDict = {}
        for i, text in zip(range(len(self.history), 0, -1), self.history):
            urlDict['vText%d' % i] = text
        urlDict = urllib.urlencode(urlDict)
        data = urllib.urlopen(baseUrl, urlDict).read()
        response = self._responseFromPage(data)
        self.history.append(response)
        return response

    def chat(self):
        botLine = self.respond()
        print botLine
        sayVictoria(botLine)
        while True:
            try:
                line = raw_input("> ")
                sayBruce(line)
                botLine = self.respond(line)
                print botLine
                sayVictoria(botLine)
            except EOFError:
                break

        botLine = "Goodbye!"
        print botLine
        sayVictoria(botLine)
        return

    def _responseFromPage(self, data):
        match = re.search('<INPUT NAME=vText2 TYPE=hidden VALUE="([^"]+?)">',
                data, re.MULTILINE)
        return match.group(1)

def sayBruce(text):
    if os.uname()[0] == 'Darwin':
        os.system('say -v Bruce "%s"' % text)

def sayVictoria(text):
    if os.uname()[0] == 'Darwin':
        os.system('say -v Victoria "%s"' % text)

j = JabberWackyBot()
j.chat()
