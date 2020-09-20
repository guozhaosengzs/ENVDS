library(tidyverse)
library(ggplot2)
library(ggpubr)


beaver_data <- read_csv('https://raw.githubusercontent.com/guozhaosengzs/ENVDS/master/activity4/beaver_dam.csv')

#1
plot(beaver_data$dams.n, beaver_data$area.ha, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Surface water area (ha)",
     xlab =  "Number of beaver dams")

#2
beaver_lm <- lm(beaver_data$area.ha ~ beaver_data$dams.n)
beaver_rs <- rstandard(beaver_lm)

qqnorm(beaver_rs)
qqline(beaver_rs)
shapiro.test(beaver_rs)

plot(beaver_data$dams.n, beaver_rs, 
     xlab = "beaver damns", 
     ylab = "standardized residual")
abline(h=0)

plot(beaver_data$dams.n, beaver_data$area.ha, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Surface water area (ha)",
     xlab =  "Number of beaver dams")

abline(beaver_lm, lwd=2)
summary(beaver_lm)

#3
pheno <- read_csv("https://raw.githubusercontent.com/guozhaosengzs/ENVDS/master/activity4/red_maple_pheno.csv")

par(mfrow=c(1,2))
plot(pheno$Tmax,pheno$doy, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab =  "Maximum temperature (C)")
plot(pheno$Prcp,pheno$doy, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab =  "Precipitation (mm)")

doy_lat <- ggplot(pheno, aes(x = Lat, y = doy)) + geom_point()
        
doy_tmax <- ggplot(pheno, aes(x = Tmax, y = doy)) + geom_point(colour = 'red', na.rm = TRUE)
        
doy_elev <- ggplot(pheno, aes(x = elev, y = doy)) + geom_point(colour = 'blue')
        
doy_site <- ggplot(pheno, aes(x = siteDesc, y = doy)) + geom_dotplot(binaxis='y', stackdir='center', stackratio=1.1, dotsize=0.4)

ggarrange(doy_lat, doy_tmax, doy_elev, doy_site, 
          labels = c("Latitutde", "Max Temperature", "Elevation", 'Urban/Rural'),
          ncol = 2, nrow = 2)

#4
plot( ~  pheno$Lat + pheno$Tmax+ pheno$Tmin +pheno$Prcp + pheno$elev + pheno$siteDesc)

#5
pheno$urID <- ifelse(pheno$siteDesc == "Urban",1,0)

#6
mlr <- lm(pheno$doy ~  pheno$Tmax  + pheno$Prcp + pheno$elev + pheno$urID)
mlFitted <- fitted(mlr)
mlFitted 

#7
summary(mlr)

