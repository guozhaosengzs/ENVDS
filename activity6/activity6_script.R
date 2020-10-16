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

####Q8####
g1966@data <- left_join(g1966@data, gAll, by="GLACNAME")
spplot(g1966, "gdiff", main="% change in area", col="transparent")

discription1 = summary(gAll$gdiff)
sd(gAll$gdiff)

discription2 = summary(gAll$area66)


min_diff = gAll[gAll$gdiff == discription1[1], "GLACNAME"]
max_diff =gAll[gAll$gdiff == discription1[6], "GLACNAME"]

min_area66 = gAll[gAll$area66 == discription2[1], "GLACNAME"]
max_area66 =gAll[gAll$area66 == discription2[6], "GLACNAME"]

gAll[gAll$GLACNAME == min_area66, "gdiff"]
gAll[gAll$GLACNAME == max_area66, "gdiff"]


g1966@polygons

#####9####
min66 <- g1966[g1966@data$GLACNAME == min_diff,]
min66_15 <- g2015[g2015@data$GLACNAME == min_diff,]
plot(min66, main = "Pumpelly Glacier Area Change from 1966 to 2015 (10.30%)", col="Blue")
plot(min66_15, col="dodgerblue2", add=TRUE)
legend("bottomright", legend=c("1996", "2015"), title = "Years", fill=c("Blue", "dodgerblue2"), horiz=TRUE)


max66 <- g1966[g1966@data$GLACNAME == max_diff,]
max66_15 <- g2015[g2015@data$GLACNAME == max_diff,]
plot(max66, main = "Boulder Glacier Area Change from 1966 to 2015 (84.72%)",border="Black",col="turquoise4")
plot(max66_15, col="cyan2", border="Black",add=TRUE)
legend("bottomleft", legend=c("1996", "2015"), title = "Years", fill=c("turquoise4", "cyan2"), horiz=TRUE)
