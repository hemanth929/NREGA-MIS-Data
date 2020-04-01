files = list.files("./SMST_monsoon_2001_2014",pattern=".nc",full.names =T,recursive = T)
options(scipen=999)

library(raster)
library(ncdf4)
library(sf)
library(rgdal)
library(velox)
library(dplyr)
library(highcharter)
library(lubridate)
library(pbapply)
library(gganimate)
library(ggplot2)
library(changepoint)


block = readOGR("D:/Hemanth/Bharatmaps/Block_Webgis.geojson").
block = read_sf("D:/Hemanth/Bharatmaps/Block_Webgis.geojson")# %>% as_Spatial()


pboptions(type="txt")

soil_m = pblapply(files, function(z){
x=brick(z,varname="SOIL_M",stopIfNotEqualSpaced=FALSE)
NAvalue(x) <- -999999994495727286400846660460848
vx = velox(x)

df = vx$extract(sp = block, fun = function(y) mean(y, na.rm = TRUE))
df = data.frame(block$objectid,df)
df$date = ymd(as.Date(gsub(".*SMST.\\s*|00.nc.*", "", z),format = "%Y%m%d"),tz="Asia/Calcutta")
df = reshape2::melt(df,id.vars=c("block.objectid","date"))
df$variable = as.numeric(gsub("X","",df$variable))
df$date =df$date + hours(3*(df$variable-1))
df
})

soil_m_df = bind_rows(soil_m)
soil_m = split(soil_m_df,soil_m_df$block.objectid)
names(soil_m) = unlist(lapply(soil_m, function(x) unique(x$FID)))
soil_m = lapply(soil_m, function(x){
  x %>% 
    left_join(.,block[,c(8,9)] %>% 
    st_drop_geometry(),by=c("block.objectid"="objectid")) 
  })
x= soil_m[[105]]

#-------------Data to match with NREGA------------#

nrega_soilm_dryspell = pblapply(soil_m, function(x){
  x %>% group_by(FID,Date= date(date)) %>% 
    summarise(soil_m = mean(value)) %>%
  mutate(year = lubridate::year(Date)) %>%
  left_join(.,dryspell_max,by=c("FID"="FID","year"="year")) %>%
    filter(Date >= date1 & Date <= date2) %>%
    group_by(FID,year,breakid,length_dryspell_d) %>%
    summarise(soil_m = mean(soil_m,na.rm=T))
})

nrega_soilm_dryspell = nrega_soilm_dryspell[lapply(nrega_soilm_dryspell,nrow)>0]
    
nrega_soilm_dryspell = lapply(nrega_soilm_dryspell, function(x){
  x %>% left_join(.,nrega_inv,by=c("FID"="block.FID","year"="year"))
})
x=nrega_soilm_dryspell[[148]]

highchart() %>% 
  hc_yAxis_multiples(
    list(lineWidth = 3),
    list(showLastLabel = FALSE, opposite = TRUE)
  ) %>%
  hc_add_series(x,"line",hcaes(x=year,y=soil_m)) %>%
  hc_add_series(x,"line",hcaes(x=year,y=cumm),yAxis=1)


#--------------gganimate-------------------#
ds = do.call(rbind,nrega_soilm_dryspell)
ggplot(ds,aes(y=cumm,x=soil_m))+
  geom_point()+
  transition_time(year)+
  ease_aes('linear', interval = 0.001)
