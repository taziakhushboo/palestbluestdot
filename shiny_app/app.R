#How we created our visualization for the Pale Blue Dot competition:
# Install packages:
install.packages(c("ncdf4", "raster", "terra", "sf", "sp", "ggplot2", "leaflet", "dplyr", "geodata", "enmSdmX", "tidyverse", "htmlwidgets", "shiny"))

# Load the necessary R packages:
library(sp)
library(ncdf4)
library(raster)
library(dplyr)
library(geodata)
library(enmSdmX)
library(tidyverse)
library(leaflet)
library(shiny)
library(plotly)
library(ggplot2)

# Set directories. The data has been downloaded manually from AppEEARS and saved in the following directory:
input_dir <- "/Users/tazia.khushboo/Dropbox/Pale Blue Dot/data/"
setwd(input_dir)  
output_dir <- paste("/Users/tazia.khushboo/Dropbox/Pale Blue Dot/", 'output/', sep= '')
suppressWarnings(dir.create(output_dir))

# Get international boundary identifying the country of interest (Bangladesh here):
crs <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")
bgd <- gadm(country="BGD", level=0, path=tempdir())
bd <- spatVectorToSpatial(bgd)
ban <- spTransform(bd, crs)

#Find the land cover type data and keep only the coordinates and the land cover type variable:
file_list_lct <- list.files(pattern = '*LC_Type1')
lct_raster <- raster(x = file_list_lct[1])
lct_raster <- crop(lct_raster, bd)
lct_raster <- mask(lct_raster, bd)
lct_df <- raster::as.data.frame(lct_raster, xy=TRUE)
colnames(lct_df) <- c("x","y","c")

#Create the land cover type masks:
lct_water <- c(15, 16, 17, NA)
lct_forest <- c(1, 2, 3, 4, 5)
lct_savanna <- c(8, 9)
lct_grassland <- c(10)
lct_wetland <- c(11)
lct_cropland <- c(12)
lct_mosaic <- c(14)
lct_built <- c(13)

#Mask land cover type column data to only include specific land cover types:
lct_all <- lct_df %>% filter(!c %in% lct_water)
lct_forest <- lct_df %>% filter(c %in% lct_forest)
lct_savanna <- lct_df %>% filter(c %in% lct_savanna)
lct_grassland <- lct_df %>% filter(c %in% lct_grassland)
lct_wetland <- lct_df %>% filter(c %in% lct_wetland)
lct_cropland <- lct_df %>% filter(c %in% lct_cropland)
lct_mosaic <- lct_df %>% filter(c %in% lct_mosaic)
lct_built <- lct_df %>% filter(c %in% lct_built)

# Identify the correct file and produce empty vectors for the gross primary productivity and total evapotranspiration data by land cover type masks:
file_list_GPP <- list.files(pattern = '*Gpp')
len <- length(file_list_GPP)
GPP_raster <- vector(mode = "list", length = len)
GPP_df <- vector(mode = "list", length = len)

file_list_ET <- list.files(pattern = '*ET_500')
ET_raster <- vector(mode = "list", length = len)
ET_df <- vector(mode = "list", length = len)

df_list <- vector(mode = "list", length = len)

raster_list <- vector(mode = "list", length = len)

for (j in c("all", "forest", "savanna", "grassland", "wetland", "cropland", "mosaic", "built")) {
  v0 <- paste("df_", j, sep = "")
  v1 <- paste("raster_", j, sep = "")
  assign(v0, vector(mode = "list", length = len))
  assign(v1, vector(mode = "list", length = len))
}

#Do the above for ET quality control:
file_list_qc <- list.files(pattern = '*ET_QC')
length(file_list_qc)
qc <- c(0, 8, 16, 32, 40, 48)
qc_raster <- vector(mode = "list", length = len)
qc_df <- vector(mode = "list", length = len)

for (j in c("all", "forest", "savanna", "grassland", "wetland", "cropland", "mosaic", "built")) {
  v0 <- paste("df_", j, sep = "")
  v1 <- paste("stats_", j, sep = "")
  assign(v1, as.data.frame(matrix(ncol=5, nrow=46)))
}


#in the following loop, for each time period i and each variable:
  #"raster" the geoTiff files 
  #turn the raster data into a data frame
  #drop any missing values
  #make sure the data type of the gross primary productivity and the evapotranspiration is numeric
  #filter the data frames using the quality control masks for (evapotranspiration only)
  #inner join the GPP and ET data
  #divide the GPP data by the ET data
  #keep only the this new variable besides the coordinates
  #To get rid of extreme values, drop observations with the ratio greater than 7.5
  #inner join this dataframe with each of the land cover type masks to separate vectors 
  #Turn the dataframes into rasters
  #Use the country boundary to crop the rasters so that data points outside the boundary are not visible in the final leaflet map

for (i in 1:len) {
  qc_raster[[i]] <- raster(x = file_list_qc[i])
  qc_df[[i]] <- raster::as.data.frame(qc_raster[[i]], xy=TRUE)
  colnames(qc_df[[i]]) <- c("x","y","q")
  qc_df[[i]] <- qc_df[[i]] %>% filter(q %in% qc)
  
  GPP_raster[[i]] <- raster(x = file_list_GPP[i])
  GPP_df[[i]] <- raster::as.data.frame(GPP_raster[[i]], xy=TRUE)
  GPP_df[[i]] <- GPP_df[[i]] %>% drop_na()
  colnames(GPP_df[[i]]) <- c("x", "y", "z1")
  GPP_df[[i]]$z1 <- as.numeric(GPP_df[[i]]$z1)
  
  ET_raster[[i]] <- raster(x = file_list_ET[i])
  ET_df[[i]] <- raster::as.data.frame(ET_raster[[i]], xy=TRUE)
  ET_df[[i]] <- ET_df[[i]] %>% drop_na()
  colnames(ET_df[[i]]) <- c("x", "y", "z2")
  ET_df[[i]]$z2 <- as.numeric(ET_df[[i]]$z2)
  
  ET_df[[i]] <- ET_df[[i]] %>% inner_join(qc_df[[i]], 
                                          by=c('x'='x', 'y'='y'))
  
  df_list[[i]] <- GPP_df[[i]] %>% inner_join(ET_df[[i]], 
                                             by=c('x'='x', 'y'='y'))
  
  df_list[[i]]$z <- df_list[[i]]$z1/df_list[[i]]$z2
  colnames(df_list[[i]]) <- c("x", "y", "z1", "z2", "q", "z")
  df_list[[i]] <- df_list[[i]][c("x","y", "z")]
  df_list[[i]] <- df_list[[i]] %>% filter(!z > 7.5)
  
  df_all[[i]] <- df_list[[i]] %>% inner_join(lct_all, by=c('x'='x', 'y'='y'))
  df_all[[i]] <- df_all[[i]][c("x", "y", "z")]
  df_forest[[i]] <- df_list[[i]] %>% inner_join(lct_forest, by=c('x'='x', 'y'='y'))
  df_forest[[i]] <- df_forest[[i]][c("x", "y", "z")]
  df_savanna[[i]] <- df_list[[i]] %>% inner_join(lct_savanna, by=c('x'='x', 'y'='y'))
  df_savanna[[i]] <- df_savanna[[i]][c("x", "y", "z")]
  df_grassland[[i]] <- df_list[[i]] %>% inner_join(lct_grassland, by=c('x'='x', 'y'='y'))
  df_grassland[[i]] <- df_grassland[[i]][c("x", "y", "z")]
  df_wetland[[i]] <- df_list[[i]] %>% inner_join(lct_wetland, by=c('x'='x', 'y'='y'))
  df_wetland[[i]] <- df_wetland[[i]][c("x", "y", "z")]
  df_cropland[[i]] <- df_list[[i]] %>% inner_join(lct_cropland, by=c('x'='x', 'y'='y'))
  df_cropland[[i]] <- df_cropland[[i]][c("x", "y", "z")]
  df_mosaic[[i]] <- df_list[[i]] %>% inner_join(lct_mosaic, by=c('x'='x', 'y'='y'))
  df_mosaic[[i]] <- df_mosaic[[i]][c("x", "y", "z")]
  df_built[[i]] <- df_list[[i]] %>% inner_join(lct_built, by=c('x'='x', 'y'='y'))
  df_built[[i]] <- df_built[[i]][c("x", "y", "z")]
  
  raster_all[[i]] <- rasterFromXYZ(df_all[[i]], crs = crs)
  raster_forest[[i]] <- rasterFromXYZ(df_forest[[i]], crs = crs)
  raster_savanna[[i]] <- rasterFromXYZ(df_savanna[[i]], crs = crs)
  raster_grassland[[i]] <- rasterFromXYZ(df_grassland[[i]], crs = crs)
  raster_wetland[[i]] <- rasterFromXYZ(df_wetland[[i]], crs = crs)
  raster_cropland[[i]] <- rasterFromXYZ(df_cropland[[i]], crs = crs)
  raster_mosaic[[i]] <- rasterFromXYZ(df_mosaic[[i]], crs = crs)
  raster_built[[i]] <- rasterFromXYZ(df_built[[i]], crs = crs)
  
  raster_all[[i]] <- crop(raster_all[[i]], bd)
  raster_all[[i]] <- mask(raster_all[[i]], bd)
  raster_forest[[i]] <- crop(raster_forest[[i]], bd)
  raster_forest[[i]] <- mask(raster_forest[[i]], bd)
  raster_savanna[[i]] <- crop(raster_savanna[[i]], bd)
  raster_savanna[[i]] <- mask(raster_savanna[[i]], bd)
  raster_grassland[[i]] <- crop(raster_grassland[[i]], bd)
  raster_grassland[[i]] <- mask(raster_grassland[[i]], bd)
  raster_wetland[[i]] <- crop(raster_wetland[[i]], bd)
  raster_wetland[[i]] <- mask(raster_wetland[[i]], bd)
  raster_cropland[[i]] <- crop(raster_cropland[[i]], bd)
  raster_cropland[[i]] <- mask(raster_cropland[[i]], bd)
  raster_mosaic[[i]] <- crop(raster_mosaic[[i]], bd)
  raster_mosaic[[i]] <- mask(raster_mosaic[[i]], bd)
  raster_built[[i]] <- crop(raster_built[[i]], bd)
  raster_built[[i]] <- mask(raster_built[[i]], bd)
}

#Create a dummy vector for storing the stats for the time series plots:
colnames(stats_all) <- c("Mean", "Median", "Min", "Max", "N" )

#Name the columns of the dummy dataframes:
colnames(stats_all) <- c("Mean", "Median", "Min", "Max", "N")
colnames(stats_forest) <- c("Mean", "Median", "Min", "Max", "N")
colnames(stats_savanna) <- c("Mean", "Median", "Min", "Max", "N")
colnames(stats_grassland) <- c("Mean", "Median", "Min", "Max", "N")
colnames(stats_wetland) <- c("Mean", "Median", "Min", "Max", "N")
colnames(stats_cropland) <- c("Mean", "Median", "Min", "Max", "N")
colnames(stats_mosaic) <- c("Mean", "Median", "Min", "Max", "N")
colnames(stats_built) <- c("Mean", "Median", "Min", "Max", "N")

#Create the stats for each time period i and each dataframe by land cover type and store in the appropriate cell:
for (i in 1:len) {  
  stats_all[i, 1] <- mean(df_all[[i]]$z)
  stats_all[i, 1] <- round(stats_all[i, 1], 2)
  stats_all[i, 2] <- median(df_all[[i]]$z)
  stats_all[i, 2] <- round(stats_all[i, 2], 2)
  stats_all[i, 3] <- min(df_all[[i]]$z)
  stats_all[i, 3] <- round(stats_all[i, 3], 2)
  stats_all[i, 4] <- max(df_all[[i]]$z)
  stats_all[i, 4] <- round(stats_all[i, 4], 2)
  stats_all[i, 5] <- nrow(df_all[[i]])
  
  stats_forest[i, 1] <- mean(df_forest[[i]][,]$z)
  stats_forest[i, 1] <- round(stats_forest[i, 1], 2)
  stats_forest[i, 2] <- median(df_forest[[i]][,]$z)
  stats_forest[i, 2] <- round(stats_forest[i, 2], 2)
  stats_forest[i, 3] <- min(df_forest[[i]][,]$z)
  stats_forest[i, 3] <- round(stats_forest[i, 3], 2)
  stats_forest[i, 4] <- max(df_forest[[i]][,]$z)
  stats_forest[i, 4] <- round(stats_forest[i, 4], 2)
  stats_forest[i, 5] <- nrow(df_forest[[i]])
  
  stats_savanna[i, 1] <- mean(df_savanna[[i]][,]$z)
  stats_savanna[i, 1] <- round(stats_savanna[i, 1], 2)
  stats_savanna[i, 2] <- median(df_savanna[[i]][,]$z)
  stats_savanna[i, 2] <- round(stats_savanna[i, 2], 2)
  stats_savanna[i, 3] <- min(df_savanna[[i]][,]$z)
  stats_savanna[i, 3] <- round(stats_savanna[i, 3], 2)
  stats_savanna[i, 4] <- max(df_savanna[[i]][,]$z)
  stats_savanna[i, 4] <- round(stats_savanna[i, 4], 2)
  stats_savanna[i, 5] <- nrow(df_savanna[[i]])
  
  stats_grassland[i, 1] <- mean(df_grassland[[i]][,]$z)
  stats_grassland[i, 1] <- round(stats_grassland[i, 1], 2)
  stats_grassland[i, 2] <- median(df_grassland[[i]][,]$z)
  stats_grassland[i, 2] <- round(stats_grassland[i, 2], 2)
  stats_grassland[i, 3] <- min(df_grassland[[i]][,]$z)
  stats_grassland[i, 3] <- round(stats_grassland[i, 3], 2)
  stats_grassland[i, 4] <- max(df_grassland[[i]][,]$z)
  stats_grassland[i, 4] <- round(stats_grassland[i, 4], 2)
  stats_grassland[i, 5] <- nrow(df_grassland[[i]])
  
  stats_wetland[i, 1] <- mean(df_wetland[[i]][,]$z)
  stats_wetland[i, 1] <- round(stats_wetland[i, 1], 2)
  stats_wetland[i, 2] <- median(df_wetland[[i]][,]$z)
  stats_wetland[i, 2] <- round(stats_wetland[i, 2], 2)
  stats_wetland[i, 3] <- min(df_wetland[[i]][,]$z)
  stats_wetland[i, 3] <- round(stats_wetland[i, 3], 2)
  stats_wetland[i, 4] <- max(df_wetland[[i]][,]$z)
  stats_wetland[i, 4] <- round(stats_wetland[i, 4], 2)
  stats_wetland[i, 5] <- nrow(df_wetland[[i]])
  
  stats_cropland[i, 1] <- mean(df_cropland[[i]][,]$z)
  stats_cropland[i, 1] <- round(stats_cropland[i, 1], 2)
  stats_cropland[i, 2] <- median(df_cropland[[i]][,]$z)
  stats_cropland[i, 2] <- round(stats_cropland[i, 2], 2)
  stats_cropland[i, 3] <- min(df_cropland[[i]][,]$z)
  stats_cropland[i, 3] <- round(stats_cropland[i, 3], 2)
  stats_cropland[i, 4] <- max(df_cropland[[i]][,]$z)
  stats_cropland[i, 4] <- round(stats_cropland[i, 4], 2)
  stats_cropland[i, 5] <- nrow(df_cropland[[i]])
  
  stats_mosaic[i, 1] <- mean(df_mosaic[[i]][,]$z)
  stats_mosaic[i, 1] <- round(stats_mosaic[i, 1], 2)
  stats_mosaic[i, 2] <- median(df_mosaic[[i]][,]$z)
  stats_mosaic[i, 2] <- round(stats_mosaic[i, 2], 2)
  stats_mosaic[i, 3] <- min(df_mosaic[[i]][,]$z)
  stats_mosaic[i, 3] <- round(stats_mosaic[i, 3], 2)
  stats_mosaic[i, 4] <- max(df_mosaic[[i]][,]$z)
  stats_mosaic[i, 4] <- round(stats_mosaic[i, 4], 2)
  stats_mosaic[i, 5] <- nrow(df_mosaic[[i]])
  
  stats_built[i, 1] <- mean(df_built[[i]][,]$z)
  stats_built[i, 1] <- round(stats_built[i, 1], 2)
  stats_built[i, 2] <- median(df_built[[i]][,]$z)
  stats_built[i, 2] <- round(stats_built[i, 2], 2)
  stats_built[i, 3] <- min(df_built[[i]][,]$z)
  stats_built[i, 3] <- round(stats_built[i, 3], 2)
  stats_built[i, 4] <- max(df_built[[i]][,]$z)
  stats_built[i, 4] <- round(stats_built[i, 4], 2)
  stats_built[i, 5] <- nrow(df_built[[i]])
  
}

#To generate maps for each time period by land cover types and store in a list, first create the dummy lists:
maps_all <- vector(mode = "list", length = len)
maps_forest <- vector(mode = "list", length = len)
maps_savanna <- vector(mode = "list", length = len)
maps_grassland <- vector(mode = "list", length = len)
maps_wetland <- vector(mode = "list", length = len)
maps_cropland <- vector(mode = "list", length = len)
maps_mosaic <- vector(mode = "list", length = len)
maps_built <- vector(mode = "list", length = len)

#set the color palette:
pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), 0:7.5, na.color = "transparent")

#Generate the leaflet maps and populate the appropriate list element:
for (i in 1:len) {  
  maps_all[[i]] <- leaflet() %>% addTiles() %>%
    addProviderTiles("Esri.WorldGrayCanvas") %>%  
    addPolygons(data = ban, stroke = FALSE, fillColor = c("white"), fillOpacity = 0.5, smoothFactor = 0.5) %>%
    addRasterImage(raster_all[[i]], colors = pal, opacity = 1) %>%
    addLegend(pal = pal, values = values(raster_all[[i]]) ) %>%
    addMiniMap()
  
  maps_forest[[i]] <- leaflet() %>% addTiles() %>%
    addProviderTiles("Esri.WorldGrayCanvas") %>%  
    addPolygons(data = ban, stroke = FALSE, fillColor = c("white"), fillOpacity = 0.5, smoothFactor = 0.5) %>%
    addRasterImage(raster_forest[[i]], colors = pal, opacity = 1) %>%
    addLegend(pal = pal, values = values(raster_forest[[i]]) ) %>%
    addMiniMap()
  
  maps_savanna[[i]] <- leaflet() %>% addTiles() %>%
    addProviderTiles("Esri.WorldGrayCanvas") %>%  
    addPolygons(data = ban, stroke = FALSE, fillColor = c("white"), fillOpacity = 0.5, smoothFactor = 0.5) %>%
    addRasterImage(raster_savanna[[i]], colors = pal, opacity = 1) %>%
    addLegend(pal = pal, values = values(raster_savanna[[i]]) ) %>%
    addMiniMap()
  
  maps_grassland[[i]] <- leaflet() %>% addTiles() %>%
    addProviderTiles("Esri.WorldGrayCanvas") %>%  
    addPolygons(data = ban, stroke = FALSE, fillColor = c("white"), fillOpacity = 0.5, smoothFactor = 0.5) %>%
    addRasterImage(raster_grassland[[i]], colors = pal, opacity = 1) %>%
    addLegend(pal = pal, values = values(raster_grassland[[i]]) ) %>%
    addMiniMap()
  
  maps_wetland[[i]] <- leaflet() %>% addTiles() %>%
    addProviderTiles("Esri.WorldGrayCanvas") %>%  
    addPolygons(data = ban, stroke = FALSE, fillColor = c("white"), fillOpacity = 0.5, smoothFactor = 0.5) %>%
    addRasterImage(raster_wetland[[i]], colors = pal, opacity = 1) %>%
    addLegend(pal = pal, values = values(raster_wetland[[i]]) ) %>%
    addMiniMap()
  
  maps_cropland[[i]] <- leaflet() %>% addTiles() %>%
    addProviderTiles("Esri.WorldGrayCanvas") %>%  
    addPolygons(data = ban, stroke = FALSE, fillColor = c("white"), fillOpacity = 0.5, smoothFactor = 0.5) %>%
    addRasterImage(raster_cropland[[i]], colors = pal, opacity = 1) %>%
    addLegend(pal = pal, values = values(raster_cropland[[i]]) ) %>%
    addMiniMap()
  
  maps_mosaic[[i]] <- leaflet() %>% addTiles() %>%
    addProviderTiles("Esri.WorldGrayCanvas") %>%  
    addPolygons(data = ban, stroke = FALSE, fillColor = c("white"), fillOpacity = 0.5, smoothFactor = 0.5) %>%
    addRasterImage(raster_mosaic[[i]], colors = pal, opacity = 1) %>%
    addLegend(pal = pal, values = values(raster_mosaic[[i]]) ) %>%
    addMiniMap()
  
  maps_built[[i]] <- leaflet() %>% addTiles() %>%
    addProviderTiles("Esri.WorldGrayCanvas") %>%  
    addPolygons(data = ban, stroke = FALSE, fillColor = c("white"), fillOpacity = 0.5, smoothFactor = 0.5) %>%
    addRasterImage(raster_built[[i]], colors = pal, opacity = 1) %>%
    addLegend(pal = pal, values = values(raster_built[[i]]) ) %>%
    addMiniMap()
}

#create a dat vector:
date <- c("2023-01-01",
          "2023-01-09",
          "2023-01-17",
          "2023-01-25",
          "2023-02-02",
          "2023-02-10",
          "2023-02-18",
          "2023-02-26",
          "2023-03-06",
          "2023-03-14",
          "2023-03-22",
          "2023-03-30",
          "2023-04-07",
          "2023-04-15",
          "2023-04-23",
          "2023-05-01",
          "2023-05-09",
          "2023-05-17",
          "2023-05-25",
          "2023-06-02",
          "2023-06-10",
          "2023-06-18",
          "2023-06-26",
          "2023-07-04",
          "2023-07-12",
          "2023-07-20",
          "2023-07-28",
          "2023-08-05",
          "2023-08-13",
          "2023-08-21",
          "2023-08-29",
          "2023-09-06",
          "2023-09-14",
          "2023-09-22",
          "2023-09-30",
          "2023-10-08",
          "2023-10-16",
          "2023-10-24",
          "2023-11-01",
          "2023-11-09",
          "2023-11-17",
          "2023-11-25",
          "2023-12-03",
          "2023-12-11",
          "2023-12-19",
          "2023-12-27")
date <- as.Date(date)
#Add this date vector with each stats dataframe:
stats_all$date <- date
stats_built$date <- date
stats_cropland$date <- date
stats_wetland$date <- date
stats_grassland$date <- date
stats_savanna$date <- date
stats_forest$date <- date
stats_mosaic$date <- date

#Generate the interactive time series plots for the median value by land cover type:
graphs_all <- plot_ly(stats_all, x = ~stats_all$date, y = ~stats_all$Median, name = 'Median', type = 'scatter', mode = 'lines') %>%
  layout(title = 'Median WUE: Excluding Non-Vegetated Lands', plot_bgcolor = "#e6ecf2", xaxis = list(title = ''), 
         yaxis = list(title = 'Grams of carbon per kilogram of water'), legend = list(title=list(text='<b> Statistics Reported: </b>'))) 

graphs_forest <- plot_ly(stats_forest, x = ~stats_forest$date, y = ~stats_forest$Median, name = 'Median', type = 'scatter', mode = 'lines') %>%
  layout(title = 'Median WUE: Forests', plot_bgcolor = "#e6ecf2", xaxis = list(title = ''), 
         yaxis = list(title = 'Grams of carbon per kilogram of water'), legend = list(title=list(text='<b> Statistics Reported: </b>'))) 

graphs_savanna <- plot_ly(stats_savanna, x = ~stats_savanna$date, y = ~stats_savanna$Median, name = 'Median', type = 'scatter', mode = 'lines') %>%
  layout(title = 'Median WUE: Savannas', plot_bgcolor = "#e6ecf2", xaxis = list(title = ''), 
         yaxis = list(title = 'Grams of carbon per kilogram of water'), legend = list(title=list(text='<b> Statistics Reported: </b>'))) 

graphs_grassland <- plot_ly(stats_grassland, x = ~stats_grassland$date, y = ~stats_grassland$Median, name = 'Median', type = 'scatter', mode = 'lines') %>%
  layout(title = 'Median WUE: Grasslands', plot_bgcolor = "#e6ecf2", xaxis = list(title = ''), 
         yaxis = list(title = 'Grams of carbon per kilogram of water'), legend = list(title=list(text='<b> Statistics Reported: </b>'))) 

graphs_wetland <- plot_ly(stats_wetland, x = ~stats_wetland$date, y = ~stats_wetland$Median, name = 'Median', type = 'scatter', mode = 'lines') %>%
  layout(title = 'Median WUE: Wetlands', plot_bgcolor = "#e6ecf2", xaxis = list(title = ''), 
         yaxis = list(title = 'Grams of carbon per kilogram of water'), legend = list(title=list(text='<b> Statistics Reported: </b>'))) 

graphs_cropland <- plot_ly(stats_cropland, x = ~stats_cropland$date, y = ~stats_cropland$Median, name = 'Median', type = 'scatter', mode = 'lines') %>%
  layout(title = 'Median WUE: Croplands', plot_bgcolor = "#e6ecf2", xaxis = list(title = ''), 
         yaxis = list(title = 'Grams of carbon per kilogram of water'), legend = list(title=list(text='<b> Statistics Reported: </b>'))) 

graphs_mosaic <- plot_ly(stats_mosaic, x = ~stats_mosaic$date, y = ~stats_mosaic$Median, name = 'Median', type = 'scatter', mode = 'lines') %>%
  layout(title = 'Median WUE: Cropland/Natural Vegetation Mosaics', plot_bgcolor = "#e6ecf2", xaxis = list(title = ''),  
         yaxis = list(title = 'Grams of carbon per kilogram of water'), legend = list(title=list(text='<b> Statistics Reported: </b>'))) 

graphs_built <- plot_ly(stats_built, x = ~stats_built$date, y = ~stats_built$Median, name = 'Median', type = 'scatter', mode = 'lines') %>%
  layout(title = 'Median WUE: Excluding Non-Vegetated Lands', plot_bgcolor = "#e6ecf2", xaxis = list(title = ''), 
         yaxis = list(title = 'Grams of carbon per kilogram of water'), legend = list(title=list(text='<b> Statistics Reported: </b>'))) 

#remove some of the big files, they take up a lot of working memory:
rm(df_list)
rm(ET_df)
rm(GPP_df)
################################################################################
#Now for the App!

stats_all$count <- 1:46

#The user interface:
  #There will be a title panel, a main panel showing the output, and an input panel taking input from the user
  #In the main panel, the top output will be the leaflet map, the bottom output will be the time series
  #The input panel will include sliders for the date selected, the jumps in the sliders will be 8 consecutive days
  #The minimum and maximum date values will come from the date values in the stats dataframes (I'm using the stats dataframe for all land cover types here)
  #The time format will be 2023-MM-DD
  #The user will switch from different land cover types using radio buttons
#The backend work:
#The map:
#Using the input time period, get the row number based on the date selected and use that row number to identify which map to display from the list of maps
#The map plots will switch based on which radio button is selected
#The time series plot:
#Based on which radio button is selected, display the time series plot

ui <- fluidPage(
  titlePanel("Spatiotemporal Water Use Efficiency Metric: Bangladesh (2023)"),
  
  fluidRow(
    mainPanel(leafletOutput("map"), plotlyOutput("lineplot")),
    
    inputPanel(sliderInput("time", 
                           list(icon("calendar"), "Select date to view map:"),
                           min = min(stats_all$date),
                           max = max(stats_all$date),
                           step = 8,
                           value = as.Date("2023-06-26"),timeFormat="%Y-%m-%d"),
               
               radioButtons("landcover", 
                            list(icon("tree"), "Select a land cover type to view map and/or lineplot:"),
                            choices = c("All land, excluding non-vegetated lands",
                                        "Forests",
                                        "Savannas",
                                        "Grasslands",
                                        "Permanent Wetlands",
                                        "Croplands",
                                        "Cropland/Natural Vegetation Mosaics",
                                        "Urban and Built-up Lands"),
               )
               
    )
  )
)

server <- function(input, output){
  
  output$map <- renderLeaflet({
    u <- filter(stats_all, `date` == input$time)
    i <- u$count
    if (input$landcover == "All land, excluding non-vegetated lands") {
      j <- 1
    } else if (input$landcover == "Forests") {
      j <- 2
    } else if (input$landcover == "Savannas") {
      j <- 3
    } else if (input$landcover == "Grasslands") {
      j <- 4
    } else if (input$landcover == "Permanent Wetlands") {
      j <- 5
    } else if (input$landcover == "Croplands") {
      j <- 6
    } else if (input$landcover == "Cropland/Natural Vegetation Mosaics") {
      j <- 7
    } else if (input$landcover == "Urban and Built-up Lands") {
      j <- 8
    }
    plot <- switch(j, maps_all[[i]], maps_forest[[i]], maps_savanna[[i]], maps_grassland[[i]], maps_wetland[[i]], maps_cropland[[i]], maps_mosaic[[i]], maps_built[[i]])
    print(plot)
  })
  
  output$lineplot <- renderPlotly({
    if (input$landcover == "All land, excluding non-vegetated lands") {
      j <- 1
    } else if (input$landcover == "Forests") {
      j <- 2
    } else if (input$landcover == "Savannas") {
      j <- 3
    } else if (input$landcover == "Grasslands") {
      j <- 4
    } else if (input$landcover == "Permanent Wetlands") {
      j <- 5
    } else if (input$landcover == "Croplands") {
      j <- 6
    } else if (input$landcover == "Cropland/Natural Vegetation Mosaics") {
      j <- 7
    } else if (input$landcover == "Urban and Built-up Lands") {
      j <- 8
    }
    plot <- switch(j, graphs_all, graphs_forest, graphs_savanna, graphs_grassland, graphs_wetland, graphs_cropland, graphs_mosaic, graphs_built)
    print(plot)
  })
  
}

#Launch the Shiny App!
shinyApp(ui, server )
