library(tidyverse)

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
