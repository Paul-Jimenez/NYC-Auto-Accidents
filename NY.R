library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggmap)


setwd("~/Desktop/Public Data Sets/NYC accidents")

motor <- read_csv("NYPD_Motor_Vehicle_Collisions.csv")
mdata <- motor

#summary(mdata)
#glimpse(mdata)

mdata <- motor %>% select(-`ZIP CODE`, -LOCATION, -`ON STREET NAME`, -`CROSS STREET NAME`) #unnecessary columns
mdata$DATE <- mdy(mdata$DATE) #using function from lubridate to convert DATE column to proper format
mdata$date.time <- paste(mdata$DATE, mdata$TIME, sep = " ") #making a date time column
mdata$date.time <- ymd_hm(mdata$date.time) #lubridate function to have access to both date and time
mdata$hour <- hour(mdata$date.time) #grabbing the hour for potential analysis
mdata$hour <- as.factor(mdata$hour)
mdata$year <- year(mdata$date.time) #grabbing the year for later facets
mdata$year <- as.factor(mdata$year)
mdata$BOROUGH <- as.factor(mdata$BOROUGH)
mdata$`CONTRIBUTING FACTOR VEHICLE 1` <- as.factor(mdata$`CONTRIBUTING FACTOR VEHICLE 1`) #the following have potential to explore
mdata$`CONTRIBUTING FACTOR VEHICLE 2` <- as.factor(mdata$`CONTRIBUTING FACTOR VEHICLE 2`)
mdata$`VEHICLE TYPE CODE 1` <- as.factor(mdata$`VEHICLE TYPE CODE 1`)
mdata$`VEHICLE TYPE CODE 2` <- as.factor(mdata$`VEHICLE TYPE CODE 2`)


by.date <- mdata %>% group_by(DATE) %>% summarise(count = n()) %>% mutate(YEAR = as.factor(year(DATE))) #how many accidents happen per date
death.year <- mdata %>% group_by(year) %>% summarise("Total Deaths" = sum(`NUMBER OF PERSONS KILLED`)) #how many people died per year from car accidents
max.acc <- by.date %>% group_by(YEAR,DATE) %>% summarise(high = max(count)) %>% filter(high == max(high)) #highest number of accidents on a given date per year
range.dates <- by.date %>% group_by(YEAR) %>% summarise(min = min(DATE), max =  max(DATE)) #the range of each year's dates. 2012 has only half a year of data
med.dates <- by.date %>% group_by(YEAR) %>% summarise(middle = median(DATE)) #finding the middle date for each year to use later for annotations


##############Exploring Accidents by Dates######################

#the below annotations use information from the data frames from above. Some are for nice placements

p <- by.date %>% ggplot(aes(x = DATE, y = count, color = YEAR)) + geom_line() + 
  annotate("text", x = med.dates$middle[1], y = max.acc$high[4] + 90, label = paste("Fatalities: ", death.year$`Total Deaths`[1], sep = ""), color = "#009933") + 
  annotate("text", x = med.dates$middle[2], y = max.acc$high[4] + 90, label = paste("Fatalities: ", death.year$`Total Deaths`[2], sep = ""), color = "#006666") +
  annotate("text", x = med.dates$middle[3], y = max.acc$high[4] + 90, label = paste("Fatalities: ", death.year$`Total Deaths`[3], sep = ""), color = "#0099CC") +
  annotate("text", x = med.dates$middle[4], y = max.acc$high[4] + 90, label = paste("Fatalities: ", death.year$`Total Deaths`[4], sep = ""), color = "#0000CC") +
  annotate("text", x = max.acc$DATE[1], y = max.acc$high[1] + 20, label = paste("High: ", max.acc$high[1], sep =""), color = "#009933") +
  annotate("text", x = max.acc$DATE[2], y = max.acc$high[2] + 20, label = paste("High: ", max.acc$high[2], sep = ""), color = "#006666") +
  annotate("text", x = max.acc$DATE[3], y = max.acc$high[3] + 20, label = paste("High: ", max.acc$high[3], sep = ""), color = "#0099CC") +
  annotate("text", x = max.acc$DATE[4], y = max.acc$high[4] + 20, label = paste("High: ", max.acc$high[4], sep = ""), color = "#0000CC")

p <- p + scale_color_manual(values = c("2012" = "#009933", "2013" = "#006666", "2014" = "#0099CC", "2015" = "#0000CC")) + 
  xlab("Date") + ylab("Number of\n Accidents") + labs(color = "Year") + ggtitle("Number of Accidents by Date")
p <- p + theme(axis.title.x = element_text(size = 14, face = "italic", color = "grey34"), 
               axis.title.y = element_text(size = 14, face = "italic", angle = 0, color = "grey34"),
               axis.text.x = element_text(color = "black"),
               axis.text.y = element_text(color = "black"),
               plot.title = element_text(size = 16, face = "bold.italic", color = "grey49"),
               legend.background = element_rect(color = "#000066", fill = "white"),
               legend.title = element_text(face = "bold", size = 12),
               panel.background = element_rect(fill = "grey91"),
               legend.key = element_rect(fill = "white"))
              
p
#################################################

###############Explore MLK day weekend 2014###########

ny2 <- get_map(location = "New York City", maptype = "roadmap", zoom = 11)

map2 <- mdata %>% filter(DATE == ymd("2014-01-21")) #202 missing out of 1161

MLK <- ggmap(ny2) + geom_point(aes(x = LONGITUDE, y = LATITUDE), data = map2) + xlab("") + ylab("") + 
  ggtitle("Accidents in New York City on January 21st 2014") + theme(plot.title = element_text(size = 16, face = "bold.italic"))

#######################################################





###############Exploring Accident Density using ggmap#################

mdata %>% filter(is.na(`LATITUDE`))  #89,878 NA values for LATITUDE

map.plot <- mdata
map.plot <- map.plot[-which(is.na(map.plot$LATITUDE)), ] #getting rid of missing latitudes
any(is.na(map.plot$LONGITUDE)) #FALSE. So all missing latitude longitude pairs have been removed

ny <- get_map("New York City")
dens.tot <- ggmap(ny) + stat_density2d(data = map.plot, aes(x = LONGITUDE, y = LATITUDE, fill = ..level..,
                                               alpha = 0.75), bins = 10, geom = 'polygon') + scale_fill_gradient(low = "black", high = "orange") + scale_alpha(guide = FALSE) + labs(fill = "Motor Accidents Density")
dens.tot <- dens.tot + xlab("") + ylab("") + ggtitle("Motor Accident Density by Year (July 2012 - April 2015)") + theme(plot.title = element_text(size = 16, face = "bold.italic"))

dens.fac <- dens.tot + facet_grid(~year) #facet by year

dens.tot
dens.fac

#*****************************************

################Zooming in on Latitude Longitude Pairs with highest amount of accidents####################

#explore latitude longitude pairs
zoom <- map.plot %>% group_by(LONGITUDE,LATITUDE) %>% summarise(count = n()) %>% ungroup() %>% arrange(desc(count)) #seeing highest amount of accidents by coordinates

close <- get_map(location = c(lon = zoom$LONGITUDE[1], lat = zoom$LATITUDE[1]), zoom = 15, maptype = "roadmap") #getting map for location of highest amount of accidents
first <- ggmap(close) + geom_point(data = zoom, aes(x = LONGITUDE, y = LATITUDE, size = count)) + xlab("") + ylab("") + ggtitle(paste("Accidents in New York at longitude ", zoom$LONGITUDE[1], " and latitude ",zoom$LATITUDE[1], sep = "")) #point size by how many accidents occurred there
#get a warning sign above because not all points can be plotted on the map because it is too small
#a lot of accidents at the end of that freeway and on that intersection
#lets see what the cause of the accidents were at the big intersection


big1 <- mdata %>% filter(LONGITUDE == zoom$LONGITUDE[1], LATITUDE == zoom$LATITUDE[1]) #filter the original data set to get only most occurred accident locations

cause <- big1 %>% group_by(`CONTRIBUTING FACTOR VEHICLE 1`) %>% summarise(count = n()) %>% arrange(desc(count)) #summarise by the contributing factor
cause$`CONTRIBUTING FACTOR VEHICLE 1` <- reorder(cause$`CONTRIBUTING FACTOR VEHICLE 1`,cause$count)

#begin plotting. Only looking at the top 10 factors

p1 <- ggplot(data = cause[1:10,], aes(x = `CONTRIBUTING FACTOR VEHICLE 1`, y = count, fill = `CONTRIBUTING FACTOR VEHICLE 1`, label = count)) + geom_bar(stat = "identity") + geom_text(vjust = -.2)
p1 <- p1 + xlab("") + ylab("Number of Occurances") + ggtitle(paste("Cause of Accidents Occuring at ", zoom$LONGITUDE[1], " and ",zoom$LATITUDE[1], sep = "")) + labs(fill = "Cause of Accident")
p1 <- p1 + theme(axis.text.x = element_blank(),
                 axis.ticks.x = element_blank(),
                 axis.title.y = element_text(size = 14, face = "italic"),
                 plot.title = element_text(size = 16, face = "bold.italic"),
                 panel.background = element_rect(fill = "white"),
                 panel.grid.major = element_line(color = "grey"))
p1 <- p1 + guides(fill = guide_legend(reverse = TRUE)) + scale_fill_brewer(palette = "Paired")
p1

##Explore area where next highest occurs
close2 <- get_map(location = c(lon = zoom$LONGITUDE[2], lat = zoom$LATITUDE[2]), zoom = 16, maptype = "roadmap") #getting 2nd highest location
second <- ggmap(close2) + geom_point(data = zoom, aes(x = LONGITUDE, y = LATITUDE, size = count))


big2 <- mdata %>% filter(LONGITUDE == zoom$LONGITUDE[2], LATITUDE == zoom$LATITUDE[2])
cause2 <- big2 %>% group_by(`CONTRIBUTING FACTOR VEHICLE 1`) %>% summarise(count = n()) %>% arrange(desc(count))
cause2$`CONTRIBUTING FACTOR VEHICLE 1` <- reorder(cause2$`CONTRIBUTING FACTOR VEHICLE 1`,cause2$count)

p2 <- ggplot(data = cause2[1:10,], aes(x = `CONTRIBUTING FACTOR VEHICLE 1`, y = count, fill = `CONTRIBUTING FACTOR VEHICLE 1`, label = count)) + geom_bar(stat = "identity") + geom_text(vjust = -.2)
p2 <- p2 + xlab("") + ylab("Number of Occurances") + ggtitle(paste("Cause of Accidents Occuring at ", zoom$LONGITUDE[2], " and ",zoom$LATITUDE[2], sep = "")) + labs(fill = "Cause of Accident")
p2 <- p2 + theme(axis.text.x = element_blank(),
                 axis.ticks.x = element_blank(),
                 axis.title.y = element_text(size = 14, face = "italic"),
                 plot.title = element_text(size = 16, face = "bold.italic"),
                 panel.background = element_rect(fill = "white"),
                 panel.grid.major = element_line(color = "grey"))
p2 <- p2 + guides(fill = guide_legend(reverse = TRUE)) + scale_fill_brewer(palette = "Paired")
p2

#**********************************************


#explore number of persons killed or injured highest amount. Problem is going to be with the missing data

glimpse(map.plot)
injured.killed <- map.plot %>% group_by(LONGITUDE,LATITUDE) %>% 
  summarise("Total Injured" = sum(`NUMBER OF PERSONS INJURED`), "Total Killed" = sum(`NUMBER OF PERSONS KILLED`)) %>%
  ungroup() %>% filter(`Total Injured` != 0) %>% arrange(desc(`Total Injured`)) 

injured.killed %>% arrange(desc(`Total Killed`))

hurt1 <- get_map(location = c(lon = injured.killed$LONGITUDE[1], lat = injured.killed$LATITUDE[1]), zoom = 15, maptype = "roadmap")
injured1 <- ggmap(hurt) + geom_point(data = injured.killed, aes(x = LONGITUDE, y = LATITUDE, size = `Total Injured`)) + xlab("") + ylab("")

hurt2 <- get_map(location = c(lon = injured.killed$LONGITUDE[2], lat = injured.killed$LATITUDE[2]), zoom = 15, maptype = "roadmap") 
injured2 <- ggmap(hurt2) + geom_point(data = injured.killed, aes(x = LONGITUDE, y = LATITUDE, size = `Total Injured`)) + xlab("") + ylab("") + ggtitle(paste("Injuries from Accidents at longitude ",injured.killed$LONGITUDE[2], " and latitude ",injured.killed$LATITUDE[2], sep = "" )) +
  theme(plot.title = element_text(size = 16, face = "bold.italic"))

injured2 #VERY IMPORTANT!!!!!!!!!!!!!!!!

xplore3 <- mdata %>% filter(LONGITUDE == injured.killed$LONGITUDE[2], LATITUDE == injured.killed$LATITUDE[2]) %>% group_by(`CONTRIBUTING FACTOR VEHICLE 1`) %>% summarise(count = n()) %>% arrange(desc(count))


save.image(file = "NewY.RDATA")




