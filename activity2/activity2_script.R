library(tidyverse)

Data = read_csv(file = "noaa2011124.csv")
Data$NAME = as.factor(Data$NAME)

# Question1 
nrow(Data)
ncol(Data)

# Question2
vchar = c("hey", "hello", "hi", "howdy", "greetings")
vnum = c(0, 1, pi, -5.6, 2^10)
vint = (-2:2)
fdata = factor(c(T,F,T,T,F))

# Question 3
levels(Data$NAME)
mean(Data$TMAX[Data$NAME == "ABERDEEN, WA US"], na.rm=T)
sd(Data$TMAX[Data$NAME == "ABERDEEN, WA US"], na.rm=TRUE)

Data$TAVE = Data$TMIN + ((Data$TMAX-Data$TMIN)/2)

averageTemp <- aggregate(Data$TAVE, by=list(Data$NAME), FUN="mean", na.rm=TRUE)
averageTemp

Data$siteN <- as.numeric(Data$NAME)


hist(Data$TAVE[Data$siteN == 1],
     freq=FALSE, 
     main = paste(levels(Data$NAME)[1]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey75",
     border="white")

# Question 4

m = mean(Data$TAVE[Data$siteN == 5], na.rm=TRUE)
s = sqrt(var(Data$TAVE[Data$siteN == 5], na.rm=TRUE))

hist(Data$TAVE[Data$siteN == 5],
     freq=FALSE, 
     main = paste(levels(Data$NAME)[5]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey75",
     border="white")
curve(dnorm(x, mean=m, sd=s), col="blue", lwd = 2, add=TRUE)
abline(v = m, lwd = 2, col = "red", lty = 1)
abline(v = m + s, lwd = 2, col = "red", lty = 2)
abline(v = m - s, lwd = 2, col = "red", lty = 2)
abline(v = m - 2*s, lwd = 2, col = "red", lty = 3)
abline(v = m + 2*s, lwd = 2, col = "red", lty = 3)

# Question 5

extrm_hot = qnorm(0.95, 
                   mean(Data$TAVE[Data$siteN == 1],na.rm=TRUE), 
                   sd(Data$TAVE[Data$siteN == 1],na.rm=TRUE))
1 - pnorm(extrm_hot,
          mean(Data$TAVE[Data$siteN == 1],na.rm=TRUE) + 4, 
          sd(Data$TAVE[Data$siteN == 1],na.rm=TRUE))

# Question 6
prcp1 = Data$PRCP[Data$siteN == 1]

hist(prcp1,
     freq=FALSE, 
     main = paste(levels(Data$NAME)[1]),
     xlab = "Precipitation", 
     ylab="Relative frequency",
     col="white",
     breaks = 50)
lines(density(prcp1, na.rm = TRUE), col = "red", lwd = 2)

# Question 7
yearly_prcp = aggregate(PRCP ~ year + NAME + siteN, Data, FUN = "sum", na.rm=TRUE)
yearly_prcp

# Question 8
yearly_prcp1 = subset(yearly_prcp, siteN == 1)
yearly_prcp3 = subset(yearly_prcp, siteN == 3)


par(mfrow = c(1,2), cex = 0.7)

hist(yearly_prcp1$PRCP,
     freq=FALSE, 
     main = paste(levels(Data$NAME)[1]),
     xlab = "Annual Precipitation", 
     ylab="Relative frequency",
     col="grey75",
     border="white")
curve(dnorm(x, 
            mean(yearly_prcp1$PRCP, na.rm=TRUE), 
            sd(yearly_prcp1$PRCP, na.rm=TRUE)), 
      col="red", lwd = 1.5, add=TRUE)


hist(yearly_prcp3$PRCP,
     freq=FALSE, 
     main = paste(levels(Data$NAME)[3]),
     xlab = "Annual Precipitation", 
     ylab="Relative frequency",
     col="grey75",
     border="white")
curve(dnorm(x, 
            mean(yearly_prcp3$PRCP, na.rm=TRUE), 
            sd(yearly_prcp3$PRCP, na.rm=TRUE)), 
      col="red", lwd = 1.5, add=TRUE)

# Question 9
WA700p = pnorm(700,
               mean(yearly_prcp1$PRCP), 
               sd(yearly_prcp1$PRCP))
ND700p = pnorm(700,
               mean(yearly_prcp3$PRCP), 
               sd(yearly_prcp3$PRCP))

cat("WA: ", WA700p, " vs ", "ND: ", ND700p)
