#load in packages 
library(terra)
library(chronosphere)

#load in rast from maxent 
maast<-rast("maxent_output/mast/reef_mast_layers.asc")

#reconstruct past coastlines with chronosphere
coast<-chronosphere::fetch("paleomap", "paleocoastlines")

#get fossil reef points 
paleo_reefs <- read.csv("data/PARED_cleaned.csv")
maast_reefs <- paleo_reefs[paleo_reefs$interval_name=="Maastrichtian",]

#plot coastline reconstruction 
plot(coast["70", "coast"])
plot(maast, add=TRUE)

#find fossil reefs from Maastrichtian
paleo_reefs<-read.csv("PARED_cleaned.csv")
maast_reefs<-paleo_reefs[paleo_reefs$interval_name=="Maastrichtian",]

#plot them onto the map
points(maast_reefs$paleolng, maast_reefs$paleolat)

#plot coast lines and continental margins 
plot(coast["70", "margin"], col="#55000033")
plot(coast["70", "coast"], col="#55000033", add=TRUE)

#heat map
plot(maast, add=TRUE)

#fossil reefs 
points(maast_reefs$paleolng, maast_reefs$paleolat)

#plot onto reconstructed map 
points(maast_reefs$paleolng, maast_reefs$paleolat)

###------------------------###
###   Lutetian interval    ###
####-----------------------###

#Heat map from maxent 
lut<-rast("maxent_output/lutetian/reef_lutetian_layers.asc")

#Get fossil reef points 


#plot coastlines for Lutetian


#Plot heat map from maxent 



#Plot fossil reef points 



#Plot the same as above, but use the coastlines and the continental margins 
