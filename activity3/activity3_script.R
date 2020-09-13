library(tidyverse)

lemming = read_csv('https://raw.githubusercontent.com/guozhaosengzs/ENVDS/master/activity3/lemming_herbivory.csv')
lemming$herbivory <- as.factor(lemming$herbivory)

plot(lemming$CH4_Flux ~ lemming$herbivory, xlab ="Treatment", 
     ylab="CH4 fluxes (mgC m –2 day–1) ")
hist(lemming$CH4_Flux[lemming$herbivory == "Ctl"])
hist(lemming$CH4_Flux[lemming$herbivory == "Ex"])

shapiro.test(lemming$CH4_Flux[lemming$herbivory == "Ctl"])
shapiro.test(lemming$CH4_Flux[lemming$herbivory == "Ex"])

bartlett.test(lemming$CH4_Flux ~ lemming$herbivory)

# Q1
t.test(lemming$CH4_Flux ~ lemming$herbivory)
mean(lemming$CH4_Flux[lemming$herbivory == ])

