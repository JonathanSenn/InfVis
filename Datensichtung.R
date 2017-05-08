install.packages("portfolio")

library('ggmap')
library('leaflet')
library('ggplot2')
library('stringr')
library('scales')
library('zoo')
library('portfolio')

rm(list = ls())

dataFolder <- "data"

crimes2016 <- read.csv(file.path(dataFolder,"LAPD_Crime_and_Collision_Raw_Data_for_2016.csv"), 
                     encoding = "UTF-8",stringsAsFactors = FALSE)

crimes2016 <- read.csv(file.path(dataFolder,"LAPD_Crime_and_Collision_Raw_Data_for_2016_Classes.csv"), 
                       encoding = "UTF-8",stringsAsFactors = FALSE, sep = ';')

codes <- read.csv(file.path(dataFolder,"codes.csv"), 
                  encoding = "UTF-8",stringsAsFactors = FALSE)

crimes <- read.csv(file.path(dataFolder,"Crime_Data_From_2010_to_Present.csv"), 
                       encoding = "UTF-8",stringsAsFactors = FALSE)


str(crimes2016)


hist(crimes2016$AREA)
hist(crimes2016$Crm.Cd)
crimes2016$Location.1



test <- (gsub("\\(|\\)", "", crimes$Location.1))
crimes$lat <- as.double((str_split_fixed(test, ",", 2))[,1])
crimes$lng <- as.double((str_split_fixed(test, ",", 2))[,2])
?strsplit


summary(crimes2016$lat)
summary(crimes2016$lng)

summary(crimes2016$DATE.OCC)

hist(crimes2016$TIME.OCC, breaks = 24)
hist(crimes2016$Crm.Cd, breaks = 1000)
hist(crimes2016$lat)

crimes.small <- crimes[1:10000,]
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
  stat_density2d(data = crimes.joined, aes(lng, lat, fill = ..level..), alpha = 0.1, bins = 50, geom = 'polygon', contour = TRUE) +
  scale_fill_gradient(low = '#9ecae1', high = '#08306b') + facet_wrap(~codes.CatName, nrow = 4, ncol = 4) + theme_minimal()

ggsave("mapsnew.pdf", width = 10, height = 10)



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


crimes$date <- as.Date((substring(crimes$DATE.OCC, 1, 10)), format = "%m/%d/%Y")


p <- ggplot(crimes2016, aes(x = as.yearmon(date))) +
  geom_histogram(bins = 12, fill = 'blue', color = 'black', show.legend = FALSE) + coord_polar(start=0) + theme_minimal() +
  scale_x_yearmon(n = 24); p

p <- ggplot(crimes2016, aes(x = as.yearmon(date), y = ..count..)) +
  stat_bin(bins = 12, fill = 'lightblue', color = 'black', show.legend = FALSE) + coord_polar(start=0) + theme_minimal() +
  scale_x_yearmon(n = 15); p
crimes2016$date


crimes$hour <- floor(crimes$Time.Occurred /100)

h <- ggplot(crimes.joined, aes(x = codes.CatName)) + geom_histogram(stat = "count", fill = '#08306b') + 
  scale_y_continuous(name = "Anzahl", labels = comma) + scale_x_discrete(name = "Kategorie")+
              theme_minimal(); h
ggsave("hist.pdf", width = 12.5, height = 5)

h <- ggplot(crimes.joined, aes(x = codes.CatName)) + geom_histogram(stat = "count", fill = '#08306b') + 
  scale_y_log10(name = "Anzahl", labels = comma) + scale_x_discrete(name = "Kategorie")+
  theme_minimal(); h
ggsave("hist_log.pdf", width = 12.5, height = 5)

?scale_color_discrete

bp <- ggplot(crimes.joined, aes(group = hour, x = hour, fill = ..count.. / sapply(PANEL, FUN=function(x) sum(count[PANEL == x])))) + 
  geom_bar(aes(y = ..count../..count..), width = 1.05) + 
  scale_fill_gradient(low = '#f7fbff', high = '#08306b', na.value = '#ffffff') +
  theme_minimal() + scale_x_continuous(breaks=seq(0, 23, 3)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  coord_polar("x", start=0) + facet_wrap(~codes.CatName, nrow=4, ncol=4); bp


ggsave("clocks.pdf", width = 12.5, height = 12.5)

?scale_x_continuous

?geom_bar
                                                              
bp <- ggplot(crimes2016, aes(x = hour, fill = cat)) + geom_bar(); bp


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

test <- ((subset(crimes.joined, crimes.joined$cat > 20)))

treemap <- map.market(id=crimes2016$AREA.NAME, area = crimes2016$cat, 
                      group = crimes2016$CatName, color = crimes2016$cat)


unique(crimes2016$Status.Desc)

crimes.joined$cat <- crimes.joined$Crm.Cd.1
for (i in (1:(length(unique(codes$codes.CatNr))+1))){
crimes.joined$cat <- replace(crimes.joined$cat, crimes.joined$Crm.Cd.1 %in% (subset.data.frame(codes, codes$codes.CatNr == i))$codes.Crm.Cd, i)
}


unique(subset.data.frame(codes, codes$codes.CatNr == 1)$codes.CatName)

crimes.small$catName <- crimes.small$cat
crimes.small$catName[codes$codes.CatNr]
?replace
codes.un <- aggregate(codes.CatNr ~ codes.CatName, codes, FUN = mean)
codes.un$codes.CatName <- replace(codes.un$codes.CatName, codes.un$codes.CatName == "public?nuisance", "Public nuisance")

gsub("?", "??", codes.un$codes.CatName)

replace(codes.un$codes.CatName, codes.un$codes.CatName == "public?nuisance", "Public nuisance")
gsub("?", "??", codes.un$codes.CatName)

apply(crimes[,30:30], 2, function(x) ifelse(x = NA, 999, x))


crimes$cat <- ifelse(crimes$cat > 25, 9, crimes$cat)


?apply

crimes.joined <- merge(crimes, codes.un, by.x = "cat", by.y = "codes.CatNr", all.x = TRUE)

crimes.joined$codes.CatName
crimes.joined$codes.CatName <- ifelse(is.na(crimes.joined$codes.CatName) = TRUE, "Other", crimes.joined$codes.CatName)

new

