library(tidyverse)
library(reshape2)
library(anytime)

data1 <- read_csv("https://raw.githubusercontent.com/guozhaosengzs/ENVDS/master/activity7/2017_2020.csv")
data1$MapDate <- anydate(data1$MapDate)
data2 <- data1[, c(1, 3:8)]
data3 <- melt(data2, id.vars = "MapDate", measure.vars = c("None", "D0", "D1","D2","D3","D4"))
names(data3)[2] <- "Intensity"
names(data3)[3] <- "PercentArea"

stat.desc(data)

graph <- ggplot(data3, aes(fill=Intensity, y=PercentArea, x=MapDate)) + 
  geom_bar(position="stack", stat="identity") + 
  scale_fill_manual(values = c("black", "yellow", "tan1", "darkorange4", "red", "rosybrown"))

graph
