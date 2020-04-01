library(dplyr)
library(highcharter)
library(lubridate)
library(pbapply)
library(gganimate)
library(ggplot2)
library(changepoint)

#---------Reading Block Polygon--------------------#
block = readOGR("D:/Hemanth/Bharatmaps/Block_Webgis.geojson")
block = read_sf("D:/Hemanth/Bharatmaps/Block_Webgis.geojson")# %>% as_Spatial()

files = list.files("./Winter Area",pattern = ".tif",full.names = T)

pboptions(type="txt")

#------------------Extracting Winter cropped area at the block level----------------------#
rabi = pblapply(files, function(z){
  x=raster(z)
  #vx = velox(x)
  vx = velox(x*area(x))
  
  df = vx$extract(sp = block, fun = function(y) sum(y, na.rm = TRUE))
  df = data.frame(block$objectid,df)
  df$year = gsub("India_cropped.area_1km_","",names(x))
  colnames(df)[2] = "Winter_Area"
  df
})


ras = mask(crop(x,block[20,]),block[20,])
