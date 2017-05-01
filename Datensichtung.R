

library('ggmap')
library('leaflet')
library('ggplot2')
library('stringr')
library('scales')
library('zoo')
rm(list = ls())

dataFolder <- "data"

crimes2016 <- read.csv(file.path(dataFolder,"LAPD_Crime_and_Collision_Raw_Data_for_2016.csv"), 
                     encoding = "UTF-8",stringsAsFactors = FALSE)

crimes2016 <- read.csv(file.path(dataFolder,"LAPD_Crime_and_Collision_Raw_Data_for_2016_Classes.csv"), 
                       encoding = "UTF-8",stringsAsFactors = FALSE, sep = ';')

codes <- read.csv(file.path(dataFolder,"codes.csv"), 
                  encoding = "UTF-8",stringsAsFactors = FALSE)



str(crimes2016)


hist(crimes2016$AREA)
hist(crimes2016$Crm.Cd)
crimes2016$Location.1



test <- (gsub("\\(|\\)", "", crimes2016$Location.1))
crimes2016$lat <- as.double((str_split_fixed(test, ",", 2))[,1])
crimes2016$lng <- as.double((str_split_fixed(test, ",", 2))[,2])
?strsplit


summary(crimes2016$lat)
summary(crimes2016$lng)

summary(crimes2016$DATE.OCC)

hist(crimes2016$TIME.OCC, breaks = 24)
hist(crimes2016$Crm.Cd, breaks = 1000)
hist(crimes2016$lat)

crimes2016.small <- crimes2016[1:10000,]
crimes2016.small <- subset(crimes2016.small, lng != 0)
crimes2016$cat <- ceiling(crimes2016$Crm.Cd/100)



#Rasterkarte als Hintergrund
LA <- get_map(location = c(-118.3, 34.06), zoom = 10, source = 'google', color = 'bw')
map <- ggmap(LA)

#Vektorkarte als Hintergrund
USA.vec <- map_data("county")
LA.vec <- subset(USA.vec, subregion == 'los angeles')
map <- ggplot(data = LA.vec, aes(long, lat)) + geom_polygon(fill = 'light gray', color = 'black')
crimes2016.joined$lng
?map_data

map <- map + 
  stat_density2d(data = crimes2016.small, aes(lng, lat, fill = ..level..), geom = 'polygon', alpha = 1) + 
  scale_fill_gradient(low="lightpink", high="red") +
  geom_point(data = crimes2016.small, aes(lng, lat, colour=Crm.Cd), size = 1, alpha = 0.1) +
  scale_color_continuous(low = "blue", high = "yellow")

map

#Dichtekarte mit Flächen
map <- map + 
  stat_density2d(data = crimes2016.small, aes(lng, lat, fill = ..level..), alpha = 0.1, bins = 50, geom = 'polygon', contour = TRUE) +
  scale_fill_gradient(low = 'lightpink', high = 'red')

map
#Small Multiples nach Kategorie cat
map <- map + 
  stat_density2d(data = crimes2016, aes(lng, lat, fill = ..level..), alpha = 0.1, bins = 50, geom = 'polygon', contour = TRUE) +
  scale_fill_gradient(low = '#fff7fb', high = '#023858') + facet_wrap(~cat, nrow = 3, ncol = 3); map
ggsave("plot.png", width = 20, height = 20)

#Dichtekarte mit Iso-Lininen
map <- map + geom_density2d(data = crimes2016.small, aes(lng, lat, color = ..level..), bins = 30) +
  scale_color_gradient(low = 'pink', high = 'red'); map


?scale_fill_gradient2

map <- map + geom_point(data = crimes2016.small, aes(lng, lat, colour=Crm.Cd), size = 1, alpha = 1) 

map


map <- map + geom_point(data = crimes2016.small, aes(lng, lat, colour=Crm.Cd) , size = 3, alpha = 0.005, stroke = 0)



codes <- crimes2016[!duplicated(crimes2016$Crm.Cd),]
codes <- data.frame(codes$Crm.Cd, codes$Crm.Cd.Desc, codes$CatNr, codes$CatName)
codes$codes.CatNr <- replace(codes$codes.CatNr, codes$codes.CatNr == 16, 4)
write.csv(codes, "codes.csv")


crimes2016$date <- as.Date((substring(crimes2016$DATE.OCC, 1, 10)), format = "%m/%d/%Y")


p <- ggplot(crimes2016, aes(x = as.yearmon(date))) +
  geom_histogram(bins = 12, fill = 'blue', color = 'black', show.legend = FALSE) + coord_polar(start=0) + theme_minimal() +
  scale_x_yearmon(n = 24); p

p <- ggplot(crimes2016, aes(x = as.yearmon(date), y = ..count..)) +
  stat_bin(bins = 12, fill = 'lightblue', color = 'black', show.legend = FALSE) + coord_polar(start=0) + theme_minimal() +
  scale_x_yearmon(n = 15); p
crimes2016$date


crimes2016$hour <- floor(crimes2016$TIME.OCC/100)



bp <- ggplot(crimes2016, aes(group = hour, x = hour, fill = ..count.. / sapply(PANEL, FUN=function(x) sum(count[PANEL == x])))) + 
  geom_bar(aes(y = ..count../..count..), width = 1.05) + 
  coord_polar("x", start=25) + scale_fill_gradient(low = '#f7fbff', high = '#08306b', na.value = '#ffffff') +
  facet_wrap(~CatName, nrow=3, ncol=5) + theme_minimal(); bp

ggsave("clocks.png", width = 20, height = 20)

                                                              
bp <- ggplot(crimes2016, aes(x = hour, fill = cat)) + geom_bar(); bp
bp
?geom_ribbon

+ coord_polar(start=25)+ facet_wrap(~cat, nrow = 10, ncol = 4); bp
+coord_polar("x", start=0)
?scale_x_time

p <- ggplot(crimes2016, aes(x = date, fill = ..count..)) +
  geom_freqpoly(bins = 50, color = 'blue') + coord_polar(start=0) + theme_minimal() +
  scale_x_date(date_breaks = '1 month', date_labels = '%B %Y')

p <- ggplot(crimes2016, aes(x = date, y = ..count..)) +
  stat_bin(bins = 50, color = 'blue') + coord_polar(start=0) + theme_minimal() +
  scale_x_date(date_breaks = '1 month', date_labels = '%B %Y')

p



for (i in (1:(length(unique(codes$codes.CatNr))+1))){
crimes2016$cat <- replace(crimes2016$cat, crimes2016$Crm.Cd %in% (subset.data.frame(codes, codes$codes.CatNr == i))$codes.Crm.Cd, i)
}

crimes2016$catName <- crimes2016$cat
codes$codes.CatNr==1

crimes2016.joined <- merge(crimes2016, codes, by.x = "cat", by.y = "codes.CatNr", all.x = TRUE)
?ljoin

new

