library(tidyverse)

data <- read_csv('https://raw.githubusercontent.com/guozhaosengzs/ENVDS/master/activity5/noaa2011124.csv')
data$NAME<- as.factor(data$NAME)
nameS <- levels(data$NAME)

#Q1####
data_p <- na.omit(data.frame(NAME=data$NAME, year=data$year, PRCP=data$PRCP))

precip <- aggregate(data_p$PRCP, by=list(data_p$NAME,data_p$year), FUN="sum", na.rm=TRUE)
colnames(precip) <- c("NAME","year","totalP")

precip$ncount <- aggregate(data_p$PRCP, by=list(data_p$NAME,data_p$year), FUN="length")$x

#Q2####
pr <- precip[precip$ncount >=364,]
ca <- pr[pr$NAME == nameS[2], ]
ny <- pr[pr$NAME == nameS[5], ]

plot(ca$year, ca$totalP)

plot(ca$year, ca$totalP,
     type = "b",
     pch = 19,
     ylab = "Annual precipitation (mm)",
     xlab = "Year", 
     yaxt = "n",
     ylim =c(0, 1600))
axis(2, seq(0,1600, by=400), las=2 )
points(ny$year, ny$totalP,
       type = "b",
       pch = 19,
       col="tomato3")

legend("topleft", 
       c("California", "New York"), 
       col= c("black", "tomato3"), 
       pch=19, 
       lwd=1, 
       bty="n")

#Q3####
data_t <- na.omit(data.frame(NAME=data$NAME, year=data$year, PRCP=data$PRCP))