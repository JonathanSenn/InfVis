install.packages('sqldf')
library('ggmap')
library('leaflet')
library('ggplot2')
library('stringr')
library('scales')
library('zoo')
library('portfolio')



rm(list = ls())

dataFolder <- "data"

crimes <- read.csv(file.path(dataFolder,"Crime_Data_From_2010_to_Present.csv"), 
                   encoding = "UTF-8",stringsAsFactors = FALSE)

codes <- read.csv(file.path(dataFolder,"codes.csv"), 
                  encoding = "UTF-8",stringsAsFactors = FALSE)

coords <- (gsub("\\(|\\)", "", crimes$Location.1))

crimes$lat <- as.double((str_split_fixed(coords, ",", 2))[,1])
crimes$lng <- as.double((str_split_fixed(coords, ",", 2))[,2])

crimes$hour <- floor(crimes$Time.Occurred /100)
crimes$date <- as.Date((substring(crimes$Date.Occurred, 1, 10)), format = "%m/%d/%Y")

crimes$timespan <- as.Date((substring(crimes$Date.Reported, 1, 10)), format = "%m/%d/%Y") - as.Date((substring(crimes$Date.Occurred, 1, 10)), format = "%m/%d/%Y")

crimes$cat <- crimes$Crm.Cd.1
for (i in (1:(length(unique(codes$codes.CatNr))+1))){
  crimes$cat <- replace(crimes$cat, crimes$Crm.Cd.1 %in% (subset.data.frame(codes, codes$codes.CatNr == i))$codes.Crm.Cd, i)
}


codes.unique <- aggregate(codes.CatNr ~ codes.CatName, codes, FUN = mean)
codes.unique$codes.CatName <- replace(codes.unique$codes.CatName, codes.unique$codes.CatName == "public?nuisance", "Public nuisance")


crimes$cat <- ifelse(crimes$cat > 25, 9, crimes$cat)
crimes$cat <- ifelse(is.na(crimes$cat) == TRUE, 9, crimes$cat)

crimes <- merge(crimes, codes.unique, by.x = "cat", by.y = "codes.CatNr", all.x = TRUE, sort = FALSE)



#Rasterkarte als Hintergrund
LA <- get_map(location = c(-118.3, 34.06), zoom = 10, source = 'google', color = 'bw')
map <- ggmap(LA)

#Small Multiples nach Kategorie cat
map <- map + 
  stat_density2d(data = crimes, aes(lng, lat, fill = ..level..), alpha = 0.1, bins = 50, geom = 'polygon', contour = TRUE) +
  scale_fill_gradient(low = '#9ecae1', high = '#08306b') + facet_wrap(~codes.CatName, nrow = 4, ncol = 4) + theme_minimal()

ggsave("mapsnew.pdf", width = 10, height = 10)


h <- ggplot(crimes, aes(x = codes.CatName)) + geom_histogram(stat = "count", fill = '#08306b') + 
  scale_y_continuous(name = "Anzahl", labels = comma) + scale_x_discrete(name = "Kategorie")+
  theme_minimal(); h

ggsave("hist.pdf", width = 12.5, height = 5)

crimes.reduced <- subset(crimes, (Time.Occurred == 1200 & timespan < 7) | (Time.Occurred == 1 & timespan < 7) | (Time.Occurred != 1200 & Time.Occurred != 1))


bp <- ggplot(crimes.reduced, aes(group = hour, x = hour, fill = ..count.. / sapply(PANEL, FUN=function(x) sum(count[PANEL == x])))) + 
  geom_bar(aes(y = ..count../..count..), width = 1.05) + 
  scale_fill_gradient(low = '#f7fbff', high = '#08306b', na.value = '#ffffff') +
  theme_minimal() + scale_x_continuous(breaks=seq(0, 23, 3)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  coord_polar("x", start=0) + facet_wrap(~codes.CatName, nrow=4, ncol=4); bp

ggsave("clocks.pdf", width = 12.5, height = 12.5)

tab <- table(as.yearmon(crimes$date))
plot(tab)
as.yearmon(crimes$date)
sp <- ggplot(crimes, aes(x = as.yearmon(date), group = codes.CatName, y = codes.CatName, color = codes.CatName)) + stat_summary(fun.y = "stdev") +
  geom_line() + scale_x_yearmon()
sp

