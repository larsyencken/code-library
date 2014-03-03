#!/usr/bin/env hy

(import flask)

(def app (flask.Flask __name__))

((app.route "/")
    (defn index []
        (flask.make_response "Hello World!")))

(app.run)
