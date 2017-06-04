library(jsonlite)
library(ggmap)
library(dplyr)

setwd("/home/mapastec/Documents/studia/KoloNaukowe/katowice")

# Load data

df <- fromJSON('przystanki.json')
all <- read.csv("lokale_48.csv", sep = ",", header = TRUE)
markety <- read.csv("../dane/remastered/source/markety1.csv", sep = ",", header = TRUE)
medic <- read.csv("../dane/remastered/source/medic.csv", sep = ",", header = TRUE)
szkoly <-read.csv("../dane/remastered/source/szkolykur.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE)

# przystanki

przystanki <- df$elements
przystanki$names <- przystanki$tags$name
przystanki <- subset(przystanki, select = c(names,lat,lon))

str(przystanki)
head(przystanki)

qmplot(lon, lat, data = przystanki, maptype = "toner-lite", geom = "density2d", color = I("red"))

qmplot(lon, lat, data = przystanki, geom = "blank", zoom = 12, maptype = "toner-background", darken = .7, legend = "topleft") +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .3, color = NA) +
  scale_fill_gradient2("Przystanki", low = "white", mid = "yellow", high = "red", midpoint = 20)

# szkoly
str(szkoly)

qmplot(lon, lat, data = szkoly, maptype = "toner-lite", geom = "density2d", color = I("red"))

qmplot(lon, lat, data = szkoly, geom = "blank", zoom = 12, maptype = "toner-background", darken = .7, legend = "topleft") +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .3, color = NA) +
  scale_fill_gradient2("Szkoly", low = "white", mid = "yellow", high = "red", midpoint = 20)

