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
import  urllib2
from  cStringIO import StringIO
import time
import math
import sys
def DetectFace (image, faceCascade):

    min_size = (20, 20)
    image_scale = 2
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
    faceCrop = []
    if  faces:
        for  ((x, y, w, h), n) in  faces:

            # The input to cv.HaarDetectObjects was resized, so the stairs

            # Bounding box of each face and convert it to two CvPoints

                pt1 =  ( int (x *  image_scale), int (y *  image_scale))

                pt2 =  ( int ((x +  w) *  image_scale), int ((y +  h) *  image_scale))

                #cv.Rectangle (image, pt1, pt2, cv.RGB ( 255 , 0 , 0 ), 5 , 8 , 0 )
                #cvSetImageROI(image, cvRect(int(x*image_scale), int(y*image_scale), w*image_scale, h*image_scale));
                #IplImage *img2 = cvCreateImage(cvGetSize(image),
                
                #while  (cv.WaitKey ( 15 ) == - 1 ):
                cropImage =  image[int(y*image_scale):int((y+h)*image_scale), int(x*image_scale):int((x+w)*image_scale)]
                #cv.ShowImage("cropped", cropImage)

                #cropImage.save("cropImage.jpg")
                #cv.SaveImage("cropImage.jpg",cropImage)

                faceCrop.append(cropImage)

                    

                    

                #return (pt1,pt2);
    return faceCrop  



#---------- #

# MAIN

#---------- #





def faceDetect(url):
    faceCascade = cv.Load("haarcascade_frontalface_alt.xml")
    #faceCascade = cv.Load("haarcascade_profileface.xml")
    
    img_file = urllib2.urlopen(url)

    im = StringIO(img_file.read())
    source = Image.open(im).convert("RGB")
    bitmap = cv.CreateImageHeader(source.size, cv.IPL_DEPTH_8U, 3)
    cv.SetData(bitmap, source.tostring())
    cv.CvtColor(bitmap, bitmap, cv.CV_RGB2BGR)
    
    #while  (cv.WaitKey ( 15 ) == - 1 ):
    faceCrop =  DetectFace(bitmap, faceCascade)
    #cv.ShowImage ( "face detection test" , image)
    return faceCrop





'''
if __name__ == "__main__":
    faceDetect("http://upload.wikimedia.org/wikipedia/commons/thumb/3/33/Arnold_Schwarzenegger_edit%28ws%29.jpg/528px-Arnold_Schwarzenegger_edit%28ws%29.jpg")
'''











