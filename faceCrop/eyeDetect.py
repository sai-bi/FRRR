#!/usr/bin/python

# ------------------------------------------------- ---------------------------

# Face Detection Test (OpenCV)

#

# Thanks to:

# http://japskua.wordpress.com/2010/08/04/detecting-eyes-with-python-opencv

# ------------------------------------------------- ---------------------------

import  cv
import  time
import  Image

def DetectEye (image):

    faceCascade = cv.Load("frontalEyes35x16.xml")


    min_size = (20, 20)
    image_scale = 1
    haar_scale =  1.1
    min_neighbors = 3
    haar_flags =  0

    # Allocate the temporary images

    grayscale = cv.CreateImage ((image.width, image.height), 8 , 1 )
    SmallImage = cv.CreateImage (
            (cv.Round (image.width / image_scale),
             cv.Round (image.height / image_scale)
            ), 8, 1)

    # Convert input color image to grayscale
    cv.CvtColor (image, grayscale, cv.CV_BGR2GRAY)

    # Scale input image for faster processing

    cv.Resize (grayscale, SmallImage, cv.CV_INTER_LINEAR)



    # Equalize the histogram

    cv.EqualizeHist (SmallImage, SmallImage)



    # Detect the faces

    faces =  cv.HaarDetectObjects (

            SmallImage, faceCascade, cv.CreateMemStorage ( 0 ),

            haar_scale, min_neighbors, haar_flags, min_size

        )



    # If faces are found

    eye = []

    if  faces:


        for  ((x, y, w, h), n) in  faces:

            # The input to cv.HaarDetectObjects was resized, so the stairs

            # Bounding box of each face and convert it to two CvPoints

            pt1 =  ( int (x *  image_scale), int ((y+h/2) *  image_scale))

            pt2 =  ( int ((x +  w) *  image_scale), int ((y +  h/2) *  image_scale))

            eye.append(pt1)
            eye.append(pt2)

    return eye

            #cv.Rectangle (image, pt1, pt2, cv.RGB ( 255 , 0 , 0 ), 5 , 8 , 0 )
            



   







