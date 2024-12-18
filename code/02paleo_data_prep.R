#Load libraries and analyses options
library(terra)
library(ncdf4)
library(spatialEco)

#set parameters 
res = c(1)
lat_bin_size = 20
reps = 1000

#generate empty raster for resampling
r <- rast(res = res)
#-------------------------------------------------
#set up syntax for reading files
months <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
irr_path <- c(paste("data/input_data/scotese/69_irid/texpo1a.pdcl", months, ".nc", sep = ""))
temps_path <- c(paste("data/input_data/scotese/69_temp/texpo1o.pfcl", months, ".nc", sep = ""))
#-------------------------------------------------

#temperature data 
  sol_list <- vector(mode='list', length=length(months))
  
#load in data 
  for(i in 1:length(months)){
    sol_dat<-rast(irr_path[i])
    irr_dat<-sol_dat[[3]]
    sol_list[[i]]<-irr_dat
  }
  
#raster 
  sol <- rast(sol_list)
  
  #resample data to desired resolution
  sol <- raster::resample(sol, r)
  
  #add layer names
  names(sol) <- paste("irr_", months, sep ="")
  
  #calculate cell by cell maximum
  max_irr <- app(sol[[1:12]], function(x){max(x, na.rm = FALSE)})
  
  #calculate cell by cell minimum
  min_irr <- app(sol[[1:12]], function(x){min(x, na.rm = FALSE)})
  
  #calculate range
  range_irr <- max_irr - min_irr
  
  #calculate mean
  mean_irr <- app(sol[[1:12]], function(x){mean(x, na.rm = FALSE)})
  
  #assign layer names
  names(max_irr) <- c("max_irr")
  names(min_irr) <- c("min_irr")
  names(range_irr) <- c("range_irr")
  names(mean_irr) <- c("mean_irr")
  
  #read sst data
  temp_list <- vector(mode='list', length=length(months))
  
  #load in data, put into a list 
  for(i in 1:length(months)){
    temp_dat <- rast(temps_path[i])
    t_dat <- temp_dat[[5]]
    temp_list[[i]] <- t_dat
  }
  
  #raster 
  sst<-rast(temp_list)
  
  #interpolate via nearest neighbors to replace NAs within shallow marine mask
  sst <- lapply(1:length(names(sst)), function(x){focal(x = sst[[x]], w = matrix(1,3,3), fun = mean, NAonly = TRUE, na.rm = TRUE)})
  sst <- rast(sst)
  
  #resample to desired resolution
  sst <- raster::resample(sst, r)
  
  #assign layer names
  names(sst) <- paste("sst_", months, sep = "")
  
  #calculate cell by cell maximum
  max_sst <- app(sst[[1:12]], function(x){max(x)})
  
  #calculate cell by cell minimum
  min_sst <- app(sst[[1:12]], function(x){min(x)})
  
  #calculate range
  range_sst <- max_sst - min_sst
  
  #caclulate mean
  mean_sst <- app(sst[[1:12]], function(x){mean(x, na.rm = FALSE)})
  
  #assign layer names
  names(max_sst) <- c("max_sst")
  names(min_sst) <- c("min_sst")
  names(range_sst) <- c("range_sst")
  names(mean_sst) <- c("mean_sst")
  
#get bathymetry data 
  
#data already masked to 200 m depth (so skip that step)
  
  DEM<-rast("data/input_data/paleo_depth/maastrichtian/tdihb_ocean_sst_shallow_200_ann_fsy.nc")
  DEM <- raster::resample(x = DEM, y = r)
  plot(DEM)
  names(DEM)<- "dem"
  
  #stack layers
  stk_list<-list(DEM, max_sst, min_sst,
               max_irr, min_irr)
  stk <- rast(stk_list)
  
  #mask stack by ocean layers and DEM
  stk <- mask(stk, mean_sst)
  stk <- mask(stk, DEM)
  
  #change NA to -9999
  stk.na.fill.list<- vector(mode='list', length=length(names(stk)))
  for (i in 1:length(names(stk))){
    name<-names(stk)[i]
    stk.na.fill.list[[i]]<-classify(stk[name], cbind(NA, -9999))
  }
  
  
  #save layers
  for (i in 1:length(names(stk))){
    writeRaster(x = stk[[i]], filename = paste("data/layers/mast_layers/layer", names(stk)[i], ".asc", sep=""), overwrite=TRUE, NAflag=-9999)
  }
  
#----------------------------------
### Eocene-Lutetian-45 mya  ###
#----------------------------------  
  
  #set up syntax for reading files
  stage <- 
  months <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
  irr_path <- c(paste("data/input_data/scotese/44.5_irid/texpj2a.pdcl", months, ".nc", sep = ""))
  temps_path <- c(paste("data/input_data/scotese/44.5_temp/texpj2o.pfcl", months, ".nc", sep = ""))
  #-------------------------------------------------
  
  #temperature data 
  sol_list <- vector(mode='list', length=length(months))
  
  #load in data 
  for(i in 1:length(months)){
    sol_dat<-rast(irr_path[i])
    irr_dat<-sol_dat[[3]]
    sol_list[[i]]<-irr_dat
  }
  
  #raster 
  sol <- rast(sol_list)
  
  #resample data to desired resolution
  sol <- raster::resample(sol, r)
  
  #add layer names
  names(sol) <- paste("irr_", months, sep ="")
  
  #calculate cell by cell maximum
  max_irr <- app(sol[[1:12]], function(x){max(x, na.rm = FALSE)})
  
  #calculate cell by cell minimum
  min_irr <- app(sol[[1:12]], function(x){min(x, na.rm = FALSE)})
  
  #calculate range
  range_irr <- max_irr - min_irr
  
  #calculate mean
  mean_irr <- app(sol[[1:12]], function(x){mean(x, na.rm = FALSE)})
  
  #assign layer names
  names(max_irr) <- c("max_irr")
  names(min_irr) <- c("min_irr")
  names(range_irr) <- c("range_irr")
  names(mean_irr) <- c("mean_irr")
  
  #read sst data
  temp_list <- vector(mode='list', length=length(months))
  
  #load in data, put into a list 
  for(i in 1:length(months)){
    temp_dat <- rast(temps_path[i])
    t_dat <- temp_dat[[5]]
    temp_list[[i]] <- t_dat
  }
  
  #raster 
  sst<-rast(temp_list)
  
  #interpolate via nearest neighbors to replace NAs within shallow marine mask
  sst <- lapply(1:length(names(sst)), function(x){focal(x = sst[[x]], w = matrix(1,3,3), fun = mean, NAonly = TRUE, na.rm = TRUE)})
  sst <- rast(sst)
  
  #resample to desired resolution
  sst <- raster::resample(sst, r)
  
  #assign layer names
  names(sst) <- paste("sst_", months, sep = "")
  
  #calculate cell by cell maximum
  max_sst <- app(sst[[1:12]], function(x){max(x)})
  
  #calculate cell by cell minimum
  min_sst <- app(sst[[1:12]], function(x){min(x)})
  
  #calculate range
  range_sst <- max_sst - min_sst
  
  #caclulate mean
  mean_sst <- app(sst[[1:12]], function(x){mean(x, na.rm = FALSE)})
  
  #assign layer names
  names(max_sst) <- c("max_sst")
  names(min_sst) <- c("min_sst")
  names(range_sst) <- c("range_sst")
  names(mean_sst) <- c("mean_sst")
  
  #get bathymetry data 
  
  #data already masked to 200 m depth (so skip that step)
  
  DEM<-rast("data/input_data/paleo_depth/lutetian/tdlur_ocean_sst_shallow_200_ann_fsy.nc")
  DEM <- raster::resample(x = DEM, y = r)
  plot(DEM)
  
  
  #stack layers
  stk_list<-list(DEM, max_sst, min_sst,
                 max_irr, min_irr)
  stk <- rast(stk_list)
  
  #mask stack by ocean layers and DEM
  stk <- mask(stk, mean_sst)
  stk <- mask(stk, DEM)
  
  #change NA to -9999
  stk.na.fill.list<- vector(mode='list', length=length(names(stk)))
  for (i in 1:length(names(stk))){
    name<-names(stk)[i]
    stk.na.fill.list[[i]]<-classify(stk[name], cbind(NA, -9999))
  }
  
  
  #save layers
  for (i in 1:length(names(stk))){
    writeRaster(x = stk[[i]], filename = paste("data/layers/",stage, "_layers/layer", names(stk)[i], ".asc", sep=""), overwrite=TRUE, NAflag=-9999)
  }
  



