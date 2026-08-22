# -*- coding: utf-8 -*-
"""
Created on Sat Feb 27 22:10:01 2021

@author: cgdwo
"""

from osgeo import gdal
import glob
raster_list = glob.glob("*.tif")



for i in raster_list:
    
    File_name = i.split('.tif')[0]
    print("Successfully converted " + File_name)
    Tiff_FileName = i
    ds = gdal.Open(Tiff_FileName, 1) # 1 means you want to edit the file
    rb = ds.GetRasterBand(1) #assuming your raster has 1 band. 
    rb.SetNoDataValue(-9999)
    rb = None 
    ds = None
    ASC_FileName = File_name + '.asc'
    ds = gdal.Translate(ASC_FileName, Tiff_FileName, )
    