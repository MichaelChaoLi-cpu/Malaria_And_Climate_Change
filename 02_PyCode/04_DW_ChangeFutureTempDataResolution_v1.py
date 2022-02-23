# -*- coding: utf-8 -*-
"""
Change the resolution of Future Temperature

from 5 min to 0.25 degree

Created on Wed Feb 23 14:50:56 2022

@author: li.chao.987@s.kyushu-u.ac.jp
"""

from osgeo import gdal
import os
import numpy as np

## List input raster files
os.chdir('F:/13_Article/09_TempPrediction/')
rasterFilesRaw = os.listdir(os.getcwd())
rasterFiles = []
for raster in rasterFilesRaw:
    if raster[-3:] == "tif":
        rasterFiles.append(raster)
        
loop = 0 

while (loop < len(rasterFiles)):
    dataset = gdal.Open(rasterFiles[loop], gdal.GA_ReadOnly)
    outputFolder = 'F:/13_Article/09_TempPrediction/Res025/'
    
    outputRaster = outputFolder + rasterFiles[loop] 
    gdal.Warp(outputRaster, dataset, dstSRS = "EPSG:4326", xRes = 0.25, yRes = 0.25,
              resampleAlg = "average")
    loop = loop + 1
