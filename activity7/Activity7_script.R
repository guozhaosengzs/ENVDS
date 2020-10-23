library(tidyverse)
library(reshape2)

data1 <- read_csv("https://raw.githubusercontent.com/guozhaosengzs/ENVDS/master/activity7/20181_2020.csv")
data2 <- data1[, c(1, 3:8)]
data3 <- melt(data2, id.vars = "MapDate", measure.vars = c("None", "D0", "D1","D2","D3","D4"))

stat.desc(data)

graph <- ggplot(data, aes(fill=variabe, y=value, x=MapDate)) + 
  geom_bar(position="stack", stat="identity")

graph


specie <- c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
condition <- rep(c("normal" , "stress" , "Nitrogen") , 4)
value <- abs(rnorm(12 , 0 , 15))
d <- data.frame(specie,condition,value)