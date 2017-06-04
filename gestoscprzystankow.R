library(jsonlite)
library(ggmap)
library(dplyr)

setwd("/home/mapastec/Documents/studia/KoloNaukowe/dane/remastered/source/katowiceVisualisations")

# Load data

df <- fromJSON('data/przystanki.json')
all <- read.csv("data/lokale_48.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
markety <- read.csv("data/markety1.csv", sep = ",", header = TRUE)
medic <- read.csv("data/medic.csv", sep = ",", header = TRUE)
szkoly <-read.csv("data/szkolykur.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE)

# prepare map

katowice_map_g_str <- get_map(location = "katowice", zoom = 12)

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

# Draw the heat map
ggmap(katowice_map_g_str, extent = "device") + geom_density2d(data = przystanki, aes(x = lon, y = lat), size = 0.3) + 
  stat_density2d(data = przystanki, 
                 aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), size = 0.01, 
                 bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)

# szkoly
str(szkoly)

qmplot(lon, lat, data = szkoly, maptype = "toner-lite", geom = "density2d", color = I("red"))

qmplot(lon, lat, data = szkoly, geom = "blank", zoom = 12, maptype = "toner-background", darken = .7, legend = "topleft") +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .3, color = NA) +
  scale_fill_gradient2("Szkoly", low = "white", mid = "yellow", high = "red", midpoint = 20)

# Draw the heat map
ggmap(katowice_map_g_str, extent = "device") + geom_density2d(data = szkoly, aes(x = lon, y = lat), size = 0.3) + 
  stat_density2d(data = szkoly, 
                 aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), size = 0.01, 
                 bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)

# przychodnie

str(medic)

qmplot(lon, lat, data = medic, maptype = "toner-lite", geom = "density2d", color = I("red"))

qmplot(lon, lat, data = medic, geom = "blank", zoom = 12, maptype = "toner-background", darken = .7, legend = "topleft") +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .3, color = NA) +
  scale_fill_gradient2("Przychodnie", low = "white", mid = "yellow", high = "red", midpoint = 20)

# Draw the heat map
ggmap(katowice_map_g_str, extent = "device") + geom_density2d(data = medic, aes(x = lon, y = lat), size = 0.3) + 
  stat_density2d(data = medic, 
                 aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), size = 0.01, 
                 bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)

# markety

str(markety)

qmplot(lon, lat, data = markety, maptype = "toner-lite", geom = "density2d", color = I("red"))

qmplot(lon, lat, data = markety, geom = "blank", zoom = 12, maptype = "toner-background", darken = .7, legend = "topleft") +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .3, color = NA) +
  scale_fill_gradient2("Markety", low = "white", mid = "yellow", high = "red", midpoint = 20)

# Draw the heat map
ggmap(katowice_map_g_str, extent = "device") + geom_density2d(data = markety, aes(x = lon, y = lat), size = 0.3) + 
  stat_density2d(data = markety, 
                 aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), size = 0.01, 
                 bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)

# Apartments price and scale. New dataframe will be created with important data only.

str(all)

apartments <- subset(all, select = c(Cena.jednostkowa.netto, Rodzaj.transakcji, 
                                     Kupujacy, Pow..lokalu.obliczeniowa, lat, lon))

str(apartments)

circle_scale_amt = 0.0001 # make the circles 1% of the price.

KatowiceMAP = ggmap(katowice_map_g_str)

KatowiceMAP +
  geom_point(aes(x=lon, y=lat), data=apartments, col="orange", alpha=0.4, 
             size=apartments$Cena.jednostkowa.netto*circle_scale_amt) + 
  scale_size_continuous(range=range(apartments$Cena.jednostkowa.netto))


max(apartments$Cena.jednostkowa.netto)

summary(apartments$Cena.jednostkowa.netto)

apartments[apartments$Cena.jednostkowa.netto > 22600,]
