# Spatiotemporal Water Use Efficiency Metric for Bangladesh (2023) - Entry for the Pale Blue Dot: Visualization Challenge

Install R, RStudio on your computer and get the interactive visualization up and running:

#Set up an input directory and download and save the data folder and the app.R file from this repository there. Then, run the following at the console:
input_dir <- "/YOUR DIRECTORY HERE/data/"\
setwd(input_dir)

#Open RStudio. At the console, type the following lines of code:
#Install packages:
install.packages(c("ncdf4", "raster", "terra", "sf", "sp", "ggplot2", "leaflet", "dplyr", "geodata", "enmSdmX", "tidyverse", "htmlwidgets", "shiny"))

#Load libraries:
library("shiny")\
library(sp)\
library(ncdf4)\
library(raster)\
library(dplyr)\
library(geodata)\
library(enmSdmX)\
library(tidyverse)\
library(leaflet)\
library(shiny)\
library(plotly)\
library(ggplot2)

#Open the app.R script and run it to see the interactive visualization
