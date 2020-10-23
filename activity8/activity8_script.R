library(raster)
library(ggplot2)
library(rgdal)

dirR <- "C:\\Users\\guozh\\Desktop\\ENVDS\\activity8\\a08\\oneida"

rdatB2 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B02_20m.tif"))
rdatB3 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B03_20m.tif"))
rdatB4 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B04_20m.tif"))
rdatB8 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B08_20m.tif"))

plot(rdatB2/10000)

rgbS <- stack(rdatB4,rdatB3,rdatB2)/10000
plotRGB(rgbS, scale=2)
plotRGB(rgbS, stretch="lin")

plotRGB(rgbS, stretch="lin", maxpixels = rgbS@ncols * rgbS@nrows)


#calculate NDVI
#NIR-red/(NIR + RED)
NDVI <- (rdatB8 - rdatB4) / (rdatB8 + rdatB4)
#visualize NDVI across the Oneida lake area
plot(NDVI)

