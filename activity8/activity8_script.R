library(raster)
library(ggplot2)
library(rgdal)

dirR <- "C:\\Users\\guozh\\Desktop\\ENVDS\\activity8\\a08\\oneida"

rdatB2 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B02_20m.tif"))
rdatB3 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B03_20m.tif"))
rdatB4 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B04_20m.tif"))
rdatB8 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B08_20m.tif"))


#Q1#####
plot(rdatB2/10000)

rgbS <- stack(rdatB4,rdatB3,rdatB2)/10000
plotRGB(rgbS, scale=2)
plotRGB(rgbS, stretch="lin")

#Q2#####
plotRGB(rgbS, stretch="lin", maxpixels = rgbS@ncols * rgbS@nrows)

#Q3#####
rgbS@ncols * rgbS@nrows

#Q4#####
plot(rdatB8, maxpixels = rgbS@ncols * rgbS@nrows)
rgbS1 <- stack(rdatB8,rdatB3,rdatB2)/10000
plotRGB(rgbS1, stretch="lin", maxpixels = rgbS@ncols * rgbS@nrows)


#calculate NDVI
#NIR-red/(NIR + RED)
NDVI <- (rdatB8 - rdatB4) / (rdatB8 + rdatB4)
#visualize NDVI across the Oneida lake area
plot(NDVI)

