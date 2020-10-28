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

#Q5#####
NDVI <- (rdatB8 - rdatB4) / (rdatB8 + rdatB4)
plot(NDVI, maxpixels = rgbS@ncols * rgbS@nrows)

#Q6####
algae <- readOGR(paste0(dirR,"/Oneida/algae.shp"), verbose=FALSE)
agri <- readOGR(paste0(dirR,"/Oneida/agriculture.shp"), verbose=FALSE)
forest <- readOGR(paste0(dirR,"/Oneida/forest.shp"), verbose=FALSE)
water <- readOGR(paste0(dirR,"/Oneida/water.shp"), verbose=FALSE)
wetlands <- readOGR(paste0(dirR,"/Oneida/wetlands.shp"), verbose=FALSE)

plotRGB(rgbS, stretch="lin",maxpixels=2297430)
plot(algae, add=TRUE, col=rgb(0.5,0.5,0.5,0.5), pch=19)
plot(agri, add=TRUE, col=rgb(0.75,0.5,0.5,0.5), pch=19)
plot(forest, add=TRUE, col=rgb(0.75,0.75,0.25,0.5), pch=19)
plot(water, add=TRUE, col=rgb(0.33,0.75,0.75,0.5), pch=19)
plot(wetlands, add=TRUE, col=rgb(0.33,0.33,0.65,0.5), pch=19)
legend("bottomleft", c("algae","agri","forest","water","wetlands"),
       pch=19, col=c(rgb(0.5,0.5,0.5,0.5),rgb(0.75,0.5,0.5,0.5),
                     rgb(0.75,0.75,0.25,0.5),rgb(0.33,0.75,0.75,0.5),
                     rgb(0.33,0.33,0.65,0.5)),
                bty="n", cex=0.75)

landExtract <-  data.frame(landcID = as.factor(rep(c("algae","water","agri","forest","wetland"),each=120)),
                           x=c(algae@coords[,1],water@coords[,1],agri@coords[,1],forest@coords[,1],wetlands@coords[,1]),
                           y=c(algae@coords[,2],water@coords[,2],agri@coords[,2],forest@coords[,2],wetlands@coords[,2]))

