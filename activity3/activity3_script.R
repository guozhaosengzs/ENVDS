library(tidyverse)

lemming = read_csv('/Users/gzs/Desktop/ENVST 206/ENVDS/activity3/lemming_herbivory.csv')
lemming$herbivory <- as.factor(lemming$herbivory)

plot(lemming$CH4_Flux ~ lemming$herbivory, xlab ="Treatment", 
     ylab="CH4 fluxes (mgC m –2 day–1) ")
hist(lemming$CH4_Flux[lemming$herbivory == "Ctl"])
hist(lemming$CH4_Flux[lemming$herbivory == "Ex"])

shapiro.test(lemming$CH4_Flux[lemming$herbivory == "Ctl"])
shapiro.test(lemming$CH4_Flux[lemming$herbivory == "Ex"])

bartlett.test(lemming$CH4_Flux ~ lemming$herbivory)

t.test(lemming$CH4_Flux ~ lemming$herbivory)


