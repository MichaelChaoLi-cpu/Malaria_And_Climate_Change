# -*- coding: utf-8 -*-
"""
Change the resolution of Population 

From lkm to 0.25 Deg

Mehtod: average

Source: WorldPop Unconstrained

Created on Tue Feb 15 15:04:29 2022

@author: M.L.
"""

from osgeo import gdal
import os
import numpy as np

## List input raster files
os.chdir('F:/13_Article/07_Population/WorldPop1km/')
rasterFilesRaw = os.listdir(os.getcwd())
rasterFiles = []
for raster in rasterFilesRaw:
    if raster[-3:] == "tif":
        rasterFiles.append(raster)
        
loop = 0 

while (loop < len(rasterFiles)):
    ## Open HDF file
    tifflayer = gdal.Open(rasterFiles[loop], gdal.GA_ReadOnly)
    
    outputFolder = "F:/13_Article/07_Population/WorldPop025Deg/"
   
    outputRaster = outputFolder + rasterFiles[loop]   
    
    gdal.Warp(outputRaster, tifflayer, dstSRS = "EPSG:4326", xRes = 0.25, yRes = 0.25,
              resampleAlg = "average")
    loop = loop + 1
