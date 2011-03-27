# -*- coding: utf-8 -*-
#
#  hello.py
#  code
#
#  Created by Lars Yencken on 2011-03-28.
#  Copyright 2011 Lars Yencken. All rights reserved.
#

"""
Hello world from Flask micro-framework.
"""

from flask import Flask
app = Flask(__name__)

@app.route('/')
def hello():
    return 'Hello World!'

if __name__ == '__main__':
    app.run()
