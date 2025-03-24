library(maps)
library(ggplot2)


#gradient from "color" to "color"
colfunc <- colorRampPalette(c("orange", "white"))
colors<-colfunc(50)

#defined colors
colors<-c(rep(2, times=49), 3)

#map by state
map(database = "state", fill = TRUE, col = colors)

#map by county
map(database = "county", fill = TRUE, col = colors)






#with ggplot2

require("maps")

states <- map_data("state") #map data with coordinates with "region" names
arrests <- USArrests #measured data with capitalized "region" names 
names(arrests) <- tolower(names(arrests)) #lower case "region" for merging
arrests$region <- tolower(rownames(USArrests))
choro <- merge(states, arrests, sort = FALSE, by = "region") #merge (states) and (arrests) by "region"
choro <- choro[order(choro$order), ]

ggplot(choro, aes(long, lat)) + geom_polygon(aes(group = group, fill = assault)) + coord_map("albers",  at0 = 45.5, lat1 = 29.5)
  
ggplot(choro, aes(long, lat)) + geom_polygon(aes(group = group, fill = assault / murder)) + coord_map("albers",  at0 = 45.5, lat1 = 29.5)

ggplot(choro, aes(long, lat)) + geom_polygon(aes(group = group, fill = murder)) + coord_map("albers",  at0 = 45.5, lat1 = 29.5)



library(leaflet)
library(leaflet.providers)

#location records with GPS coordinates
wh<-c(-77.036530,38.897676, "White House")
sl<-c(-74.044500, 40.689249, "Statue of Liberty")
a51<-c(-115.80666344, 37.234332396, "Area 51")


#create a dataframe
locations<-rbind.data.frame(wh,sl,a51)
names(locations)[1] <- "lng"
names(locations)[2] <- "lat"
names(locations)[3] <- "label"

#the dataframe automatically makes the numbers into factors so have to convert back to munmebr
locations<-transform(locations, lng = as.numeric(as.character(lng)), lat = as.numeric(as.character(lat)))

#create map object and add annotation with an interactive pop-up label
m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=locations$lng, lat=locations$lat, popup=locations$label)

m





