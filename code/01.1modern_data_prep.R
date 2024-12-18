#change NA values to -9999

#make a container 
stk.na.fill.list<- vector(mode='list', length=length(names(stk)))

#replace NA with -9999 and save as a list 
for (i in 1:length(names(stk))){
  name<-names(stk)[i]
  stk.na.fill.list[[i]]<-classify(stk[name], cbind(NA, -9999))
}

#make into raster 
stk<-rast(stk.na.fill.list)

#save updated data 
for (i in 1:length(names(stk))){
  writeRaster(x = stk[[i]], filename = paste("data/layers/modern_layers/layer", names(stk)[i], ".asc", sep=""), overwrite=TRUE,
              NAflag=-9999)
}
