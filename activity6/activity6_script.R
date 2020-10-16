####setup####
library(sp)
library(rgdal)
library(dplyr)
library(tidyverse)

####Q1####
g1966 <- readOGR("C:\\Users\\guozh\\Desktop\\ENVDS\\activity6\\GNPglaciers\\GNPglaciers_1966.shp")
g2015 <- readOGR("C:\\Users\\guozh\\Desktop\\ENVDS\\activity6\\GNPglaciers\\GNPglaciers_2015.shp")

str(g2015)

plot(g1966, col = "green", border=NA)
plot(g2015, col = "black", border=NA, add=TRUE)


head(g2015@data)


####Q2####

g1966@data$GLACNAME

g2015@data$GLACNAME

setdiff(g1966@data$GLACNAME, g2015@data$GLACNAME)
setdiff(g2015@data$GLACNAME, g1966@data$GLACNAME)

g2015@data$GLACNAME <- ifelse(g2015@data$GLACNAME == "North Swiftcurrent Glacier",
                              "N. Swiftcurrent Glacier",
                              ifelse(   g2015@data$GLACNAME ==  "Miche Wabun", 
                                        "Miche Wabun Glacier",
                                        as.character(g2015@data$GLACNAME)))



####Q3####
####Q4####
####Q5####
gdf66 <- data.frame(GLACNAME = g1966@data$GLACNAME, area66 = g1966@data$Area1966)
gdf15 <- data.frame(GLACNAME = g2015@data$GLACNAME, area15 = g2015@data$Area2015)
gAll <- full_join(gdf66,gdf15, by="GLACNAME")

####Q6####
####Q7####
gAll$gdiff <- ((gAll$area66-gAll$area15)/gAll$area66)*100

gl.chg <- ggplot(gAll, aes(x = area66, y = gdiff)) + geom_point(size=2)
gl.chg


