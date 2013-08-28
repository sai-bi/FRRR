#!/usr/bin/python

# ------------------------------------------------- ---------------------------
# Face Detection Test (OpenCV)
#
# Thanks to:
# http://japskua.wordpress.com/2010/08/04/detecting-eyes-with-python-opencv
# ------------------------------------------------- ---------------------------

import cv
import time
import Image
import urllib2
from   cStringIO import StringIO
import time
import math

def detect_faces(image, face_cascade):

    min_size      = (20, 20)
    image_scale   = 1 # <- 2
    haar_scale    = 1.1
    min_neighbors = 3
    haar_flags    = 0

    # Allocate temporary images
    grayscale  = cv.CreateImage((image.width, image.height), 8, 1)
    small_image = cv.CreateImage(
                      (cv.Round (image.width  / image_scale),
                      cv.Round (image.height / image_scale)
                  ), 8, 1)

    # Convert input color image to grayscale
    cv.CvtColor(image, grayscale, cv.CV_BGR2GRAY)

    # Scale input image for faster processing
    cv.Resize(grayscale, small_image, cv.CV_INTER_LINEAR)

    # Equalize histogram
    cv.EqualizeHist(small_image, small_image)

    # Detect faces
    faces = cv.HaarDetectObjects(
                small_image, face_cascade, cv.CreateMemStorage(0),
                haar_scale, min_neighbors, haar_flags, min_size
            )

    # If faces are found
    acc = []
    if faces:
        for ((x, y, w, h), n) in faces:
            # The input to cv.HaarDetectObjects was resized, so the stairs
            # Bounding box of each face and convert it to two CvPoints
            acc += map(lambda x: x * image_scale, [x, y, w, h])

    return acc

def detect(url):

    face_cascade = cv.Load('../resources/haarcascade_frontalface_alt.xml')
    img_file = urllib2.urlopen(url)

    im = StringIO(img_file.read())
    source = Image.open(im).convert('RGB')
    bitmap = cv.CreateImageHeader(source.size, cv.IPL_DEPTH_8U, 3)
    cv.SetData(bitmap, source.tostring())
    cv.CvtColor(bitmap, bitmap, cv.CV_RGB2BGR)

    faces = detect_faces(bitmap, face_cascade)

    return faces

if __name__ == '__main__':
    url = raw_input()
    faces = detect(url)
    print ','.join([str(i) for i in faces])
