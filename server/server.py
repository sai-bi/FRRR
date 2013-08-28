#!/usr/bin/python

import tornado.ioloop
import tornado.web
from subprocess import Popen, PIPE, STDOUT

class FaceDetector(tornado.web.RequestHandler):
    def post(self):
        print self.request.body
        p = Popen('./detect/detect.py', stdout=PIPE, stdin=PIPE, stderr=STDOUT)
        faces = (p.communicate(input=self.request.body))[0]
        print faces
        self.write(faces)

application = tornado.web.Application([
    (r'/detect', FaceDetector)
])

if __name__ == '__main__':
    application.listen(9000)
    tornado.ioloop.IOLoop.instance().start()
