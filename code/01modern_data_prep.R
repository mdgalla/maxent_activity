#Scripts edited from:
#-------------------------------------------------
# Project: Coral_Reef_Distribution
#
# Date: 2021-10-29
# Author: Lewis A. Jones
# Copyright (c) Lewis A. Jones, 2021
# Email: LewisA.Jones@outlook.com
#-------------------------------------------------

library(terra)

#set parameters
res = c(1)
#lat_bin_size = 20
#reps = 1000


#resolution for rasters
r <- rast(res = res)

#modern world map 
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

#set up 
#for monthly data 
months <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")

#irradiation (sunlight) file names 
irr <- c(paste("data/input_data/scotese/irid/texpa1a.pdcl", months, ".nc", sep = ""))

#temperature file names 
temps <- c(paste("data/input_data/scotese/temp/texpa1o.pfcl", months, ".nc", sep = ""))

#prepare bathymetry data
DEM <- rast("data/input_data/bio-oracle/terrain_characteristics_6d78_4f59_9e1f_U1731927994246.nc")
DEM <- mask(x = DEM, mask = world, inverse = TRUE)

#we only want 200 m and below 

#set absolute values
DEM[DEM > 0] <- NA
DEM <- abs(DEM)
DEM[DEM > 200] <- NA

#aggregate to desired resolution, retaining minimum cell value
DEM <- aggregate(DEM, fact = res/res(DEM)[1], fun = 'min')

#resample data to fit same extent as climate data
DEM <- resample(x = DEM, y = r)

#name data and plot
names(DEM) <- "dem"
plot(DEM)


####CLIMATE#####

#make a container to list the rasters we need
#terra has some issue where when you do c(list of rasters), it doesn't work (even though it should )
sol_list<-vector(mode='list', length=length(months))

#load data files into a list 
for(i in 1:length(months)){
sol_dat <- rast(irr[i])
irr_dat <- sol_dat[[3]]
sol_list[[i]] <- irr_dat
}

#make into a raster  
sol <- rast(sol_list)

#desired resolution
sol <- resample(sol, r)

#name the layers
names(sol) <- paste("irr_", months, sep = "")

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


####TEMPERATURE#####

#make a list 
temp_list<-vector(mode='list', length=length(months))

#load in the files and put them in a list 
for(i in 1:length(months)){
  temp_dat<-rast(temps[i])
  t_dat<-temp_dat[[5]]
  temp_list[[i]]<-t_dat
}

#make into a raster 
sst <- rast(temp_list)

#interpolate via nearest neighbors to replace NAs within shallow marine mask
sst <- lapply(1:length(names(sst)), function(x){focal(x = sst[[x]], w = matrix(1,3,3), fun = mean, NAonly = TRUE, na.rm = TRUE)})
sst<- rast(sst)  

#resample to desired resolution
sst <- terra::resample(sst, r)

#assign layer names
names(sst) <- paste("sst_", months, sep = "")

#calculate cell by cell maximum
max_sst <- app(sst[[1:12]], function(x){max(x)})

#calculate cell by cell minimum
min_sst <- app(sst[[1:12]], function(x){min(x)})

#calculate range
range_sst <- max_sst - min_sst

#calculate mean
mean_sst <- app(sst[[1:12]], function(x){mean(x, na.rm = FALSE)})

#name layers
names(max_sst) <- c("max_sst")
names(min_sst) <- c("min_sst")
names(range_sst) <- c("range_sst")
names(mean_sst) <- c("mean_sst")

#make a list 
stk<-list(sst, sol, DEM,
       max_sst, min_sst, range_sst, mean_sst, 
       max_irr, min_irr, range_irr, mean_irr)

#stack of all layers
stk <- rast(stk)

plot(stk)

#MASK---------------------------------------------
#mask environmental data by DEM and ocean layers
stk <- mask(stk, mask=DEM)
stk <- mask(stk, stk$mean_sst)

#check mask 
plot(stk)

##subset selected variables
stk<-list(DEM, stk$max_sst, stk$min_sst,
          stk$min_irr, stk$max_irr)
stk <- rast(stk)

#mask stack by ocean layers and DEM
stk<-mask(stk, max_sst)
stk<-mask(stk, DEM)


#plot data
plot(stk)


#WRITE---------------------------------------------

for (i in 1:length(names(stk))){
  writeRaster(x = stk[[i]], filename = paste("modern_layers/layer", names(stk)[i], ".asc", sep=""), overwrite=TRUE)
}






#-------------------------------------------------
#Prepare modern reef occurrence data 
#-------------------------------------------------
#load libraries
library(dplyr)
library(terra)
#-------------------------------------------------
#read data
data <- read.csv("data/ReefBase_24_08_2021.csv")

#remove non-reef coral community data
remove <- c("",
            "Non-reef coral community",
            "Non-Reef coral community",
            "Non-Reef Coral Community",
            "Non reef coral community",
            "Non-reef Coral community" ,
            "Fringing/Patch/Non-reef coral community",
            "Fringing/Patch/Non-reef",
            "Fringing/Non-reef coral community",
            "Coralline algae/Vermetid reef",
            "Patch Reef/Non-reef coral community",
            "Fringing/Patch/Non-reef",
            "Fringing/Parch Reef/Non-reef Coral community",
            "Fringing/Patch reefs/non-reef coral community",
            "Barrier/Fringing/Non-reef coral community",
            "Pseudo-atoll"
)

data <- data %>% filter(!REEF_TYPE %in% remove)

#reduce to coordinate data
pts <- data[,c("LON", "LAT")]
#remove data points without coordinates
pts <- na.omit(pts)
#plot data
plot(pts)
#create raster at desired resolution
r <- rast(res = res)
#rasterize points
ras <- rasterize(x = as.matrix(pts), y = r, field = 1) 
#plot raster
plot(ras)
#spatial subsampling
pts_ras <- as.points(ras)
pts_ras <- data.frame(crds(pts_ras))
pts_ras$species <- "reef"
pts_ras <- pts_ras[,c("species", "x", "y")]

colnames(pts) <- c("x", "y")
pts$species <- "reef"
pts <- pts[,c("species", "x", "y")]

#remove data outside mask
vars_list<-paste("data/layers/modern_layers/layer", c(names(stk)), ".asc", sep="")
vars <- rast(c(vars_list))
ext <- extract(x = vars, y = pts_ras[,c("x", "y")])
pts_ras$ext <- ext
pts_ras <- na.omit(pts_ras)
pts_ras <- pts_ras[,c("species", "x", "y")]

#plot presence points
plot(pts_ras[,2:3], 
     ylab = "Latitude", xlab = "Longitude",
     pch = 22, 
     col = "black", bg = "blue", 
     xlim = c(-180, 180), ylim = c(-90, 90))


#write data
write.csv(pts_ras, "ReefBase_pts_subsample.csv", row.names = FALSE)
writeRaster(ras, "ReefBase_raster.asc", overwrite = TRUE)

#file.remove("./data/enm/layers/Modern/dem.asc")
------------------------------------------------------------------------------------------

#Let's go to maxent! 
  
#Does it work? 



