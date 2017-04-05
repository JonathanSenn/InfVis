

library('leaflet')
install.packages('ggplot2')
library('ggplot2')

dataFolder <- "data"

crimes2016 <- read.csv(file.path(dataFolder,"LAPD_Crime_and_Collision_Raw_Data_for_2016.csv"), 
                     encoding = "UTF-8",stringsAsFactors = FALSE)


str(crimes2016)


hist(crimes2016$AREA)
hist(crimes2016$Crm.Cd)
crimes2016$Location.1

leaf_map <- leaflet() %>% addTiles()
?leaflet
leaf_map


test <- (gsub("\\(|\\)", "", crimes2016$Location.1))
crimes2016$latlong <- strsplit(test, "")
leaf_map %>% addCircleMarkers(~crimes2016, ~latlong, radius = 5,
                              color = ~greens(value), fillOpacity = 0.5)

