library('leaflet')

dataFolder <- "data"

crimes2016 <- read.csv(file.path(dataFolder,"LAPD_Crime_and_Collision_Raw_Data_for_2016.csv"), 
                     encoding = "UTF-8",stringsAsFactors = FALSE)


str(crimes2016)


hist(crimes2016$AREA)
hist(crimes2016$Crm.Cd)
