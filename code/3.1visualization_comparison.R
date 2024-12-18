###------------------------###
###   Lutetian interval    ###
####-----------------------###

#heat map from maxent 
lut<-rast("maxent_output/lutetian/reef_lutetian_layers.asc")

#fossil reef points 
lut_reefs<-paleo_reefs[paleo_reefs$interval_name=="Lutetian",]

#coastlines 
plot(coast["45", "coast"])
plot(lut, add=TRUE)
points(lut_reefs$paleolng, lut_reefs$paleolat)

#coast and margins 
plot(coast["45", "margin"], col="#55000033")
plot(coast["45", "coast"], col="#55000033", add=TRUE)
plot(lut, add=TRUE)
points(lut_reefs$paleolng, lut_reefs$paleolat)
