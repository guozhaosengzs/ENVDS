library(tidyverse)

lemming <- read_csv('https://raw.githubusercontent.com/guozhaosengzs/ENVDS/master/activity3/lemming_herbivory.csv')
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
mean(lemming$CH4_Flux[lemming$herbivory == "Ctl"])
mean(lemming$CH4_Flux[lemming$herbivory == "Ex"])

# Q2 -- see word doc

# Q3
insect <- read_csv('https://raw.githubusercontent.com/guozhaosengzs/ENVDS/master/activity3/insect_richness.csv')
insect$urbanName <- as.factor(insect$urbanName)
insect$urbanType <- as.factor(insect$urbanType)

# Q4
shapiro.test(insect$Richness[insect$urbanType == 1])
shapiro.test(insect$Richness[insect$urbanType == 3])
shapiro.test(insect$Richness[insect$urbanType == 8])
shapiro.test(insect$Richness[insect$urbanType == 9])

bartlett.test(insect$Richness ~ insect$urbanType)

# Q5 -- see word doc

# Q6
lm_ins <- lm(insect$Richness ~ insect$urbanName)

aov_ins <- aov(lm_ins)

summary(aov_ins)

tukey_t = TukeyHSD(aov_ins)
tukey_t

plot(tukey_t, cex.axis=0.56)

ggplot(data = insect, aes(x=urbanName, y=Richness)) + 
  geom_boxplot() + 
  stat_summary(fun=mean, geom="point", size=5, color="red")

# Q7
tapply(insect$Richness, insect$urbanName, "mean")

# Q8
species <- matrix(c(18,8,15,32), ncol=2, byrow = TRUE) 
colnames(species) <- c("Not protected", "Protected")
rownames(species) <- c("Declining", "Stable/Increase")

mosaicplot(species, xlab="population status", ylab="legal protection",
           main="Legal protection impacts on populations")

chisq.test(species)

# Q9 -- see word doc

# Q10 -- see word doc