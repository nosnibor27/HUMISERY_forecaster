#dependent packages

#looking up elevation
library(ggmap)
#for naming the array from the input string
library(stringr)
#for downloading data
library(RNetCDF)

#adding elevation data
elevation_grid <- open.nc("metdata_elevationdata.nc")
elevation_ref <- var.get.nc(elevation_grid,variable=2)

#Add site of interest here using "Town State"
sites <- "Orlando Florida"

#creating a data frame
locations <- data.frame(sites,stringsAsFactors = FALSE)

#number of sites in location file
#recycled from other respository when downloading multiple towns at once
N <- nrow(locations)

#adding column for town name (used for labelling arrays)
locations$town <- word(sites,1)

#adding columns for longitude and latitude
locations$lon <- rep(NA,N)
locations$lat <- rep(NA,N)

#looking up coordinates
for (n in 1:N){
  gps <- geocode(sites[n],source = "dsk")
  locations$lon[n] <- as.numeric(gps[1])
  locations$lat[n] <- as.numeric(gps[2])
}

#adding elevation column
locations$elevation <- rep(NA,N)

#adding elevation values
for (n in 1:N){
  x <- locations$lon[n]
  y <- locations$lat[n]
  lat <- var.get.nc(elevation_grid,"lat")
  lon <- var.get.nc(elevation_grid,"lon")
  flat = match(abs(lat - y) < 1/48, 1)
  latindex = which(flat %in% 1)
  flon = match(abs(lon - x) < 1/48, 1)
  lonindex = which(flon %in% 1)
  locations$elevation[n] <- 0.1*elevation_ref[lonindex,latindex]
}

#loading the 3-d array for Orlando

#for(n in 1:N){
#  assign(paste0(locations$town[n],"_hist"),
#         readRDS(paste0(locations$town[n],"_hist.rdata")))
#  print(paste(locations$town[n],"historical simulation loaded"))
#}
#for(n in 1:N){
#  assign(paste0(locations$town[n],"_45"),
#         readRDS(paste0(locations$town[n],"_45.rdata")))
#  print(paste(locations$town[n],"RCP 4.5 loaded"))
#}
#for(n in 1:N){
#  assign(paste0(locations$town[n],"_85"),
#         readRDS(paste0(locations$town[n],"_85.rdata")))
#  print(paste(locations$town[n],"RCP 8.5 loaded"))
#}

#base URL
url_1 <- "http://thredds.northwestknowledge.net:8080/thredds/dodsC/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/macav2livneh/"

#model list
model <- c("bcc-csm1-1", "bcc-csm1-1-m","BNU-ESM","CanESM2",
           "CSIRO-Mk3-6-0","GFDL-ESM2G","GFDL-ESM2M","HadGEM2-CC365",
           "HadGEM2-ES365","inmcm4","IPSL-CM5A-LR","IPSL-CM5A-MR",
           "IPSL-CM5B-LR","MIROC5","MIROC-ESM","MIROC-ESM-CHEM",
           "MRI-CGCM3","NorESM1-M","CNRM-CM5","CCSM4")

#model condition list becuase CCSM4 is different
condition <- c(rep("_r1i1p1_",19),"_r6i1p1_")

#timestep lists
timestep_historical <- c("historical_1950_1969_CONUS_daily.nc",
                         "historical_1970_1989_CONUS_daily.nc",
                         "historical_1990_2005_CONUS_daily.nc")
timestep_45 <- c("rcp45_2006_2025_CONUS_daily.nc",
                 "rcp45_2026_2045_CONUS_daily.nc",
                 "rcp45_2046_2065_CONUS_daily.nc",
                 "rcp45_2066_2085_CONUS_daily.nc",
                 "rcp45_2086_2099_CONUS_daily.nc")
timestep_85 <- c("rcp85_2006_2025_CONUS_daily.nc",
                 "rcp85_2026_2045_CONUS_daily.nc",
                 "rcp85_2046_2065_CONUS_daily.nc",
                 "rcp85_2066_2085_CONUS_daily.nc",
                 "rcp85_2086_2099_CONUS_daily.nc")
#variable list
variable_1 <- c("_tasmax_","_tasmin_","_huss_")

#reference longitude and latitude sequence
lon <- seq(235.40625,292.96875,0.0625)
lat <- seq(25.15625,52.84375,0.0625)

#building 3-d arrays [variable, model, time] for 10 towns for 1950-2005
for (n in 1:N){
  #assinging lon and lat from csv
  x <- locations$lon[n]
  y <- locations$lat[n]
  #changing into coordinates 
  coord <- c(360+x,y)
  #locating appropriate latitude index
  flat = match(abs(lat - coord[2]) < 1/32, 1)
  latindex = which(flat %in% 1)
  #locating appropriate longitude index
  flon = match(abs(lon - coord[1]) < 1/32, 1)
  lonindex = which(flon %in% 1)
  #creating a blank 3-d array
  db_hist <- array(dim=c(4,20,20454))
  #looping through variables and models
  for(i in 1:3){
    for(j in 1:20){
      nc_1 <- open.nc(paste0(url_1,model[j],"/macav2livneh",variable_1[i],
                             model[j],condition[j],timestep_historical[1]))
      var_1 <- as.numeric(var.get.nc(nc_1, variable=4,
                                     start=c(lonindex, latindex, 1),
                                     count=c(1,1,7305)))
      nc_2 <- open.nc(paste0(url_1,model[j],"/macav2livneh",variable_1[i],
                             model[j],condition[j],timestep_historical[2]))
      var_2 <- as.numeric(var.get.nc(nc_2, variable=4,
                                     start = c(lonindex, latindex, 1),
                                     count=c(1,1,7305)))
      nc_3 <- open.nc(paste0(url_1,model[j],"/macav2livneh",variable_1[i],
                             model[j],condition[j],timestep_historical[3]))
      var_3 <- as.numeric(var.get.nc(nc_3, variable=4,
                                     start=c(lonindex, latindex, 1),
                                     count=c(1,1,5844)))
      #combining results
      var <- c(var_1,var_2,var_3)
      #filling the 3-d array
      db_hist[i,j,] <- var
      #crudely printing the progress
      print(paste(locations$town[n],variable_1[i],model[j],"historic simulation completed"))
    }
  }
  #saving the 3-d array with the appropriate town name
  assign(paste0(locations$town[n],"_hist"),db_hist)
}

#building 3-d arrays [variable, model, time] for 10 towns for RCP 4.5
for (n in 1:N){
  #assinging lon and lat from csv
  x <- locations$lon[n]
  y <- locations$lat[n]
  #changing into coordinates 
  coord <- c(360+x,y)
  #locating appropriate latitude index
  flat = match(abs(lat - coord[2]) < 1/32, 1)
  latindex = which(flat %in% 1)
  #locating appropriate longitude index
  flon = match(abs(lon - coord[1]) < 1/32, 1)
  lonindex = which(flon %in% 1)
  #creating a blank 3-d array
  db_45 <- array(dim=c(4,20,34333))
  #looping through variables and models
  for(i in 1:3){
    for(j in 1:20){
      nc_1 <- open.nc(paste0(url_1,model[j],"/macav2livneh",variable_1[i],
                             model[j],condition[j],timestep_45[1]))
      var_1 <- as.numeric(var.get.nc(nc_1, variable=4,
                                     start=c(lonindex, latindex, 1),
                                     count=c(1,1,7305)))
      nc_2 <- open.nc(paste0(url_1,model[j],"/macav2livneh",variable_1[i],
                             model[j],condition[j],timestep_45[2]))
      var_2 <- as.numeric(var.get.nc(nc_2, variable=4,
                                     start=c(lonindex, latindex, 1),
                                     count=c(1,1,7305)))
      nc_3 <- open.nc(paste0(url_1,model[j],"/macav2livneh",variable_1[i],
                             model[j],condition[j],timestep_45[3]))
      var_3 <- as.numeric(var.get.nc(nc_3, variable=4,
                                     start=c(lonindex, latindex, 1),
                                     count=c(1,1,7305)))
      nc_4 <- open.nc(paste0(url_1,model[j],"/macav2livneh",variable_1[i],
                             model[j],condition[j],timestep_45[4]))
      var_4 <- as.numeric(var.get.nc(nc_4, variable=4,
                                     start=c(lonindex, latindex, 1),
                                     count=c(1,1,7305)))
      nc_5 <- open.nc(paste0(url_1,model[j],"/macav2livneh",variable_1[i],
                             model[j],condition[j],timestep_45[5]))
      var_5 <- as.numeric(var.get.nc(nc_5, variable=4,
                                     start=c(lonindex, latindex, 1),
                                     count=c(1,1,5113)))
      #combining results
      var <- c(var_1,var_2,var_3,var_4,var_5)
      #filling the 3-d array
      db_45[i,j,] <- var
      #crudely printing the progress
      print(paste(locations$town[n],variable_1[i],model[j],"RCP 4.5 completed"))
    }
  }
  #saving the 3-d array with the appropriate town name
  assign(paste0(locations$town[n],"_45"),db_45)
}

#building 3-d arrays [variable, model, time] for 10 towns for RCP 8.5
for (n in 1:N){
  #assinging lon and lat from csv
  x <- locations$lon[n]
  y <- locations$lat[n]
  #changing into coordinates 
  coord <- c(360+x,y)
  #locating appropriate latitude index
  flat = match(abs(lat - coord[2]) < 1/32, 1)
  latindex = which(flat %in% 1)
  #locating appropriate longitude index
  flon = match(abs(lon - coord[1]) < 1/32, 1)
  lonindex = which(flon %in% 1)
  #creating a blank 3-d array
  db_85 <- array(dim=c(4,20,34333))
  #looping through variables and models
  for(i in 1:3){
    for(j in 1:20){
      nc_1 <- open.nc(paste0(url_1,model[j],"/macav2livneh",variable_1[i],
                             model[j],condition[j],timestep_85[1]))
      var_1 <- as.numeric(var.get.nc(nc_1, variable=4,
                                     start=c(lonindex, latindex, 1),
                                     count=c(1,1,7305)))
      nc_2 <- open.nc(paste0(url_1,model[j],"/macav2livneh",variable_1[i],
                             model[j],condition[j],timestep_85[2]))
      var_2 <- as.numeric(var.get.nc(nc_2, variable=4,
                                     start=c(lonindex, latindex, 1),
                                     count=c(1,1,7305)))
      nc_3 <- open.nc(paste0(url_1,model[j],"/macav2livneh",variable_1[i],
                             model[j],condition[j],timestep_85[3]))
      var_3 <- as.numeric(var.get.nc(nc_3, variable=4,
                                     start=c(lonindex, latindex, 1),
                                     count=c(1,1,7305)))
      nc_4 <- open.nc(paste0(url_1,model[j],"/macav2livneh",variable_1[i],
                             model[j],condition[j],timestep_85[4]))
      var_4 <- as.numeric(var.get.nc(nc_4, variable=4,
                                     start=c(lonindex, latindex, 1),
                                     count=c(1,1,7305)))
      nc_5 <- open.nc(paste0(url_1,model[j],"/macav2livneh",variable_1[i],
                             model[j],condition[j],timestep_85[5]))
      var_5 <- as.numeric(var.get.nc(nc_5, variable=4,
                                     start=c(lonindex, latindex, 1),
                                     count=c(1,1,5113)))
      #combining results
      var <- c(var_1,var_2,var_3,var_4,var_5)
      #filling the 3-d array
      db_85[i,j,] <- var
      #crudely printing the progress
      print(paste(locations$town[n],variable_1[i],model[j],"RCP 8.5 completed"))
    }
  }
  #saving the 3-d array with the appropriate town name
  assign(paste0(locations$town[n],"_85"),db_85)
}

#reference date sequence
future_dates <- as.Date(seq(38716,73048,1),origin="1900-01-01")
historical_dates <- as.Date(seq(18262,38715,1),origin="1900-01-01")

#function to calculate humisery
HDX <- function(elevation,max_temp,min_temp,specific_humidity){
  #atmospheric pressure based on site elevation (kPa)
  atmo_pressure <- 101.3*((293-0.0065*elevation)/293)^5.26
  #mean air temperature (C)
  avg_temp <- ((max_temp-273.15)+(min_temp-273.15))/2
  #calculating dew temp
  dew_temp <- (((1/273.15)-(1.844*10^-4)*log((specific_humidity*atmo_pressure/0.622)/0.6113))^-1)-273.15
  #calculating humidex
  avg_temp + 0.5555*(6.11*exp(5417.7530*((1/273.16)-(1/(273.15+dew_temp))))-10)
}

#Plotting function from Richard McElreath's "rethinking" package
col.alpha <- function( acol , alpha=0.2 ) {
  acol <- col2rgb(acol)
  acol <- rgb(acol[1]/255,acol[2]/255,acol[3]/255,alpha)
  acol
}

#plotting function for RCP 4.5
PLOT_SIM_HDX_45 <- function(n,month,title){
  #collecting arrays
  A <- get(paste0(locations$town[n],"_hist"))
  B <- get(paste0(locations$town[n],"_45"))
  #calculating HDX
  for(j in 1:20){
    #calculating HDX by model
    A[4,j,] <- HDX(locations$elevation[n], A[1,j,], A[2,j,], A[3,j,])
    B[4,j,] <- HDX(locations$elevation[n], B[1,j,], B[2,j,], B[3,j,])
  }
  #creating plot frame
  plot(0,type="n",xlim=c(0,150),ylim=c(0,50),xlab="Year",
       ylab="Humisery (°C)",
       main=title,axes=FALSE)
  box()
  axis(1,at=seq(0,150,25),las=1,labels=seq(1950,2100,25))
  axis(2,at=seq(0,50,5),las=2)
  rect(-10,-10,160,20,density=NA,col=col.alpha("blue",0.25))
  rect(-10,20,160,30,density=NA,col=col.alpha("green",0.25))
  rect(-10,30,160,40,density=NA,col=col.alpha("yellow",0.25))
  rect(-10,40,160,45,density=NA,col=col.alpha("orange",0.25))
  rect(-10,45,160,60,density=NA,col=col.alpha("red",0.25))
  abline(v=56,col="black")
  #adding lines for each model
  for (j in 1:20){
    #starting data frame
    hist_frame <- as.data.frame(historical_dates)
    fut_frame <- as.data.frame(future_dates)
    #lubridating by month
    hist_frame$M <- lubridate::month(hist_frame$historical_dates)
    fut_frame$M <- lubridate::month(fut_frame$future_dates)
    #lubridating by year
    hist_frame$Y <- lubridate::year(hist_frame$historical_dates)
    fut_frame$Y <- lubridate::year(fut_frame$future_dates)
    hist_frame$HDX <- A[4,j,]
    fut_frame$HDX <- B[4,j,]
    #only keeping relevant month
    hist_frame <- hist_frame[hist_frame$M==month,]
    fut_frame <- fut_frame[fut_frame$M==month,]
    #summarizing results by year
    mean_hist <- sapply(split(hist_frame$HDX,hist_frame$Y),mean)
    mean_45 <- sapply(split(fut_frame$HDX,fut_frame$Y),mean)
    lines(y=mean_hist,x=seq(1:56),col=col.alpha("black",0.5),lwd=1)
    lines(y=mean_45,x=seq(57,150,1),col=col.alpha("black",0.5),lwd=1)
    #text(x=150,y=mean_45[93],labels=model[j],cex=0.5,pos=4)
  }
  legend("bottom", inset=.02, title="Misery level",
         c("No discomfort","Some discomfort","Great discomfort; avoid exertion","Dangerous; possible heat stroke"),
         fill=c("green","yellow","orange","red"))
}

#plotting function for RCP 8.5
PLOT_SIM_HDX_85 <- function(n,month,title){
  #collecting arrays
  A <- get(paste0(locations$town[n],"_hist"))
  B <- get(paste0(locations$town[n],"_85"))
  #calculating RH
  for(j in 1:20){
    #calculating RH by model
    A[4,j,] <- HDX(locations$elevation[n], A[1,j,], A[2,j,], A[3,j,])
    B[4,j,] <- HDX(locations$elevation[n], B[1,j,], B[2,j,], B[3,j,])
  }
  #creating plot frame
  plot(0,type="n",xlim=c(0,150),ylim=c(0,50),xlab="Year",
       ylab="Humisery (°C)",
       main=title,axes=FALSE)
  box()
  axis(1,at=seq(0,150,25),las=1,labels=seq(1950,2100,25))
  axis(2,at=seq(0,50,5),las=2)
  rect(-10,-10,160,20,density=NA,col=col.alpha("blue",0.25))
  rect(-10,20,160,30,density=NA,col=col.alpha("green",0.25))
  rect(-10,30,160,40,density=NA,col=col.alpha("yellow",0.25))
  rect(-10,40,160,45,density=NA,col=col.alpha("orange",0.25))
  rect(-10,45,160,60,density=NA,col=col.alpha("red",0.25))
  abline(v=56,col="black")
  #adding lines for each model
  for (j in 1:20){
    #starting data frame
    hist_frame <- as.data.frame(historical_dates)
    fut_frame <- as.data.frame(future_dates)
    #lubridating by month
    hist_frame$M <- lubridate::month(hist_frame$historical_dates)
    fut_frame$M <- lubridate::month(fut_frame$future_dates)
    #lubridating by year
    hist_frame$Y <- lubridate::year(hist_frame$historical_dates)
    fut_frame$Y <- lubridate::year(fut_frame$future_dates)
    hist_frame$HDX <- A[4,j,]
    fut_frame$HDX <- B[4,j,]
    #only keeping relevant month
    hist_frame <- hist_frame[hist_frame$M==month,]
    fut_frame <- fut_frame[fut_frame$M==month,]
    #summarizing results by year
    mean_hist <- sapply(split(hist_frame$HDX,hist_frame$Y),mean)
    mean_85 <- sapply(split(fut_frame$HDX,fut_frame$Y),mean)
    lines(y=mean_hist,x=seq(1:56),col=col.alpha("black",0.5),lwd=1)
    lines(y=mean_85,x=seq(57,150,1),col=col.alpha("black",0.5),lwd=1)
    #text(x=150,y=mean_85[93],labels=model[j],cex=0.5,pos=4)
  }
  legend("bottom", inset=.02, title="Misery level",
         c("No discomfort","Some discomfort","Great discomfort; avoid exertion","Dangerous; possible heat stroke"),
         fill=c("green","yellow","orange","red"))
}

#plotting example to display at the end
par(mfrow=c(1,2))
PLOT_SIM_HDX_45(1,7,"Orlando, FL; June Humisery; RCP 4.5")
PLOT_SIM_HDX_85(1,7,"Orlando, FL; June Humisery; RCP 8.5")
