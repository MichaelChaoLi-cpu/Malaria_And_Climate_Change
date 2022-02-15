# -*- coding: utf-8 -*-
"""
Convert nc4 to GeoTiff 

For DP_13 GLDAS Noah Land Surface Model L4 monthly 0.25 x 0.25 degree V2.1 (GLDAS_NOAH025_M)

ReadMe file: https://hydro1.gesdisc.eosdis.nasa.gov/data/GLDAS/GLDAS_NOAH025_M.2.1/doc/README_GLDAS2.pdf

Resolution: 0.25 arc degree

Temporal Resolution: Monthly

Period: 2000.01 - 2019.12

Data Output: Tair_f_inst -> Temperature
             Psurf_f_inst -> Air Pressure

Created on Mon Feb 14 16:37:44 2022

@author: li.chao.987@s.kyushu-u.ac.jp
"""

import numpy as np
import os
import netCDF4
from osgeo import gdal

inputNc4FileFolder = 'F:/13_Article/00_RawClimateData'

src_dataset = gdal.Open("D:/10_Article/09_TempOutput/07_MonthlyPrecipitationTif/totalPrecipitationRate201501.tif", gdal.GA_ReadOnly)
geotransform = src_dataset.GetGeoTransform()
spatialreference = src_dataset.GetProjection()
ncol = 1440
nrow = 600
nband = 1

outputTempGeoTiffFileFolder = 'F:/13_Article/01_Temperature'
outputAirPressureGeoTiffFileFolder = 'F:/13_Article/03_AirPressure'
outputHumidityGeoTiffFileFolder = 'F:/13_Article/04_Humidity'
outputPrecipitationGeoTiffFileFolder = 'F:/13_Article/05_Precipitation'
outputWindSpeedGeoTiffFileFolder = 'F:/13_Article/06_WindSpeed'

## List input raster files
os.chdir(inputNc4FileFolder)
rasterFilesRaw = os.listdir(os.getcwd())
rasterFiles = []
for raster in rasterFilesRaw:
    if raster[-3:] == "nc4":
        rasterFiles.append(raster)
#print(rasterFiles)

for raster in rasterFiles:
    nc4File = inputNc4FileFolder + '/' + raster
    readNc4File = netCDF4.Dataset(nc4File)
    
    monthlyTemp = readNc4File["Tair_f_inst"][:] 
    monthlyTemp = np.nanmean(monthlyTemp, axis = 0)
    monthlyTempOutputRaster = outputTempGeoTiffFileFolder + '/Temp_' + raster[17:23] + ".tif"

    driver = gdal.GetDriverByName("GTiff")
    dst_dataset = driver.Create(monthlyTempOutputRaster, ncol, nrow, nband, gdal.GDT_Float32)
    dst_dataset.SetGeoTransform(geotransform)
    dst_dataset.SetProjection(spatialreference)
    dst_dataset.GetRasterBand(1).WriteArray(monthlyTemp)
    dst_dataset = None
    
    monthlyAirPressure = readNc4File["Psurf_f_inst"][:] 
    monthlyAirPressure = np.nanmean(monthlyAirPressure, axis = 0)
    monthlyAirPressureOutputRaster = outputAirPressureGeoTiffFileFolder + '/AirPressure_' + raster[17:23] + ".tif"

    driver = gdal.GetDriverByName("GTiff")
    dst_dataset = driver.Create(monthlyAirPressureOutputRaster, ncol, nrow, nband, gdal.GDT_Float32)
    dst_dataset.SetGeoTransform(geotransform)
    dst_dataset.SetProjection(spatialreference)
    dst_dataset.GetRasterBand(1).WriteArray(monthlyAirPressure)
    dst_dataset = None
    
    monthlyHumidity = readNc4File["Qair_f_inst"][:] 
    monthlyHumidity = np.nanmean(monthlyHumidity, axis = 0)
    monthlyHumidityOutputRaster = outputHumidityGeoTiffFileFolder + '/Humidity_' + raster[17:23] + ".tif"

    driver = gdal.GetDriverByName("GTiff")
    dst_dataset = driver.Create(monthlyHumidityOutputRaster, ncol, nrow, nband, gdal.GDT_Float32)
    dst_dataset.SetGeoTransform(geotransform)
    dst_dataset.SetProjection(spatialreference)
    dst_dataset.GetRasterBand(1).WriteArray(monthlyHumidity)
    dst_dataset = None
    
    monthlyPrecipitation = readNc4File["Rainf_f_tavg"][:] 
    monthlyPrecipitation = np.nanmean(monthlyPrecipitation, axis = 0)
    monthlyPrecipitationOutputRaster = outputPrecipitationGeoTiffFileFolder + '/Precipitation_' + raster[17:23] + ".tif"

    driver = gdal.GetDriverByName("GTiff")
    dst_dataset = driver.Create(monthlyPrecipitationOutputRaster, ncol, nrow, nband, gdal.GDT_Float32)
    dst_dataset.SetGeoTransform(geotransform)
    dst_dataset.SetProjection(spatialreference)
    dst_dataset.GetRasterBand(1).WriteArray(monthlyPrecipitation)
    dst_dataset = None
    
    monthlyWindSpeed = readNc4File["Wind_f_inst"][:] 
    monthlyWindSpeed = np.nanmean(monthlyWindSpeed, axis = 0)
    monthlyWindSpeedOutputRaster = outputWindSpeedGeoTiffFileFolder + '/WindSpeed_' + raster[17:23] + ".tif"

    driver = gdal.GetDriverByName("GTiff")
    dst_dataset = driver.Create(monthlyWindSpeedOutputRaster, ncol, nrow, nband, gdal.GDT_Float32)
    dst_dataset.SetGeoTransform(geotransform)
    dst_dataset.SetProjection(spatialreference)
    dst_dataset.GetRasterBand(1).WriteArray(monthlyWindSpeed)
    dst_dataset = None