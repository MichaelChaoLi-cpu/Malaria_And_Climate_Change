# -*- coding: utf-8 -*-
"""
Change the resolution of Malaria Data

From 0.0416666666666667 to 0.25

Mehtod: average

Aim: This processing could improve the speed of R raster::extract

Created on Mon Feb 14 12:15:59 2022

@author: M.L.
"""

from osgeo import gdal
import os
import numpy as np

## List input raster files
os.chdir('F:/13_Article/PfPR/Raster Data/PfPR_rmean/')
rasterFilesRaw = os.listdir(os.getcwd())
rasterFiles = []
for raster in rasterFilesRaw:
    if raster[-3:] == "tif":
        rasterFiles.append(raster)
        
loop = 0 

while (loop < len(rasterFiles)):
    ## Open HDF file
    tifflayer = gdal.Open(rasterFiles[loop], gdal.GA_ReadOnly)
    
    outputFolder = "F:/13_Article/PfPR/Raster Data/PfPR_rmean_025/R025_"
   
    outputRaster = outputFolder + rasterFiles[loop]   
    
    gdal.Warp(outputRaster, tifflayer, dstSRS = "EPSG:4326", xRes = 0.25, yRes = 0.25,
              resampleAlg = "average")
    loop = loop + 1
