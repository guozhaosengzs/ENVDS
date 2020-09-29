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
data_t <- na.omit(data.frame(NAME=data$NAME, year=data$year, TMAX=data$TMAX, TMIN=data$TMIN))
data_t <- data_t %>% mutate(TAVG = (TMAX + TMIN) / 2) 

avg_t <- aggregate(data_t$TAVG, by=list(data_t$NAME,data_t$year), FUN="mean", na.rm=TRUE)
avg_t$N <- aggregate(data_t$TAVG, by=list(data_t$NAME,data_t$year), FUN="length")$x
colnames(avg_t) <- c("SITE","YEAR","TAVG", "N")

av <- avg_t[avg_t$N >=350,]

ND_t_a <- av[av$SITE == nameS[3], ]
NY_t_a <- av[av$SITE == nameS[5], ]
q3_data <- rbind(ND_t_a, NY_t_a)

q3 <- ggplot(q3_data, aes(x = YEAR, y = TAVG, group = SITE)) +
        geom_line(aes(color=SITE), size = 1) +
        geom_point(aes(color=SITE), size = 2, shape = 18) +
        
        labs(title = "Mean Annual Temperature from 1930 - 2020 \n (excluding years with record counts < 350)", 
             x = 'Year',
             y = 'Temperature (°C)') +
        theme(plot.title = element_text(hjust = 0.5)) +
        
        scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
        scale_color_brewer(palette="Dark2")
q3

#Q4####
max_t <- aggregate(data_t$TMAX, by=list(data_t$NAME,data_t$year), FUN="mean", na.rm=TRUE)
max_t$N <- aggregate(data_t$TMAX, by=list(data_t$NAME,data_t$year), FUN="length")$x
colnames(max_t) <- c("SITE","YEAR","MAXAVG", "N")

max_av <- max_t[max_t$N >=350,]

ND_t_m <- max_av[max_av$SITE == nameS[3], ]
NY_t_m <- max_av[max_av$SITE == nameS[5], ]
q4_data <- rbind(ND_t_m, NY_t_m)

q4 <- ggplot(q4_data, aes(x = YEAR, y = MAXAVG, group = SITE)) +
        geom_line(aes(color=SITE), size = 1) +
        geom_point(aes(color=SITE), size = 2, shape = 18) +
        
        labs(title = "Mean Annual Maximum Temperature from 1930 - 2020 \n (excluding years with record counts < 350)", 
             x = 'Year',
             y = 'Temperature (°C)') +
        theme(plot.title = element_text(hjust = 0.5)) +
        
        scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
        scale_color_brewer(palette="Dark2")

q4


#Q5####
ggplot(data = pr, aes(x = year, y=totalP, color=NAME ) )+ #data for plot
        geom_point()+ #make points at data point
        geom_path()+ #use lines to connect data points
        labs(x="year", y="Annual Precipitation") #make axis labels
ggplot(data = pr, aes(x = year, y=totalP, color=NAME ) )+ #data for plot
        geom_point()+ #make points at data point
        geom_path()+ #use lines to connect data points
        labs(x="year", y="Annual Precipitation")+ #make axis labels
        theme_classic() #change plot theme
ggplot(data = pr, aes(x = year, y=totalP, color=NAME ) )+
        geom_point(alpha=1, size = 1.5)+
        geom_path(alpha=0.5, size = 1)+
        labs(x="year", y="Annual Precipitation")+
        theme_classic()+
        scale_color_manual(values = c("black","blue", "red", "green", "cyan"))

#Q6####
ggplot(data = data, aes(x=NAME, y=TMIN))+ #look at daily tmin
        geom_violin(fill=rgb(0.933,0.953,0.98))+ #add a violin plot with blue color
        geom_boxplot(width=0.2,size=0.25, fill="grey90")+ #add grey boxplots and make them about 20% smaller than normal with 25% thinner lines than normal
        theme_classic() #git rid of ugly gridlines


sub <- data[data$NAME == nameS[4] & data$ year == 1974,]

sub$DATE <- as.Date(sub$DATE,"%Y-%m-%d")

#7####
ggplot(data=sub, aes(x=DATE, y=TMAX))+
        geom_point(na.rm = TRUE)+
        geom_path()+
        theme_classic()+
        labs(x="year", y="Maximimum temperature (C)")
ggplot(data=sub, aes(x=DATE, y=PRCP))+
        geom_col(fill="royalblue3")+
        theme_classic()+
        labs(x="year", y="Daily precipitation (mm)")

#8####

sub1 <- data[data$NAME == nameS[2] & data$ year == 1974,]

sub1$DATE <- as.Date(sub1$DATE,"%Y-%m-%d")

ggplot(data=sub1, aes(x=DATE, y=TMAX))+
        geom_point(na.rm = TRUE)+
        geom_path()+
        theme_classic()+
        labs(x="year", y="Maximimum temperature (C)")
ggplot(data=sub1, aes(x=DATE, y=PRCP))+
        geom_col(fill="royalblue3")+
        theme_classic()+
        labs(x="year", y="Daily precipitation (mm)")

#9####
sub2 <- data[data$NAME == nameS[3] & data$year >= 2000,]
sub2$DATE <- as.Date(sub1$DATE,"%Y-%m-%d")

q9 <- ggplot(data=sub2, aes(x=DATE, y = TMIN))+
        geom_ribbon(aes(ymax = TMIN, ymin = -50), fill = "white")+
        labs(x="Year", y="Minimum temperature (°C)") +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
        theme(
                # get rid of panel grids
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                # Change plot and panel background
                plot.background=element_rect(fill = "gray"),
                panel.background = element_rect(fill = 'black'),
                # Change legend 
                legend.position = c(0.6, 0.07),
                legend.direction = "horizontal",
                legend.background = element_rect(fill = "black", color = NA),
                legend.key = element_rect(color = "gray", fill = "black"),
                legend.title = element_text(color = "white"),
                legend.text = element_text(color = "white")
        )
q9
        
