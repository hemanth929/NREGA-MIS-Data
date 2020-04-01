library(tibbletime)
library(raster)
library(velox)
library(sf)
library(reshape2)
library(lubridate)
library(ncdf4)
library(highcharter)
library(zoo)
library(dplyr)



#---------------- Reading Rainfall Data (CHIRPS processed in GEE)-------------#

rf_files = list.files(path="./Rainfall",full.names = T)
rf_files = lapply(rf_files, function(x){
        y = readr::read_csv(x)
        melt(y[,-c(1,ncol(y))],id.vars = "objectid",
                       variable.name="date",
                       value.name="rainfall")
})

rf = bind_rows(rf_files)
rf$date = as.Date(rf$date,format = "%Y%m%d")
rf$year = year(rf$date)

#--------Rainfall Visulization-----------------#

rf %>% filter(objectid==1828) %>%
  highchart(type="stock") %>% 
  hc_add_series(.,hcaes(x=date,y=rainfall))


#------------Extraction of agronomic onspells and Dry Spells --------------------#

rolling_mean_3 <- tibbletime::rollify(mean, window = 3)  # Function to caluclate rolling means
rolling_mean_5 <- tibbletime::rollify(mean, window = 5)


rf = rf %>%  group_by(objectid,year) %>% 
              mutate(rf_5=rolling_mean_5(rainfall),
                      rf_3=rolling_mean_3(rainfall))


onset = rf %>% group_by(objectid, year,occurence_of_10= rf_5>=10) %>% # Creating a dummy where 5-day mean rainfall is greater than 10mm
                arrange(objectid) %>%
                  mutate(count= row_number(), 
                                     onset_10 = occurence_of_10 & count==1) %>% 
                    filter(onset_10==TRUE)
colnames(onset)[2] = "onset_date"

as.vector(rle(rf$rainfall))                 


  #---------------- Extracting all dryspells(consecutive zero rainfall days)----------------#

dryspells = rf %>% arrange(objectid,year,date) %>%
  group_by(objectid,year) %>%
   left_join(.,onset[,c(1:2,4)]) %>% # joining with the onset date information
  filter(date >= onset_date) %>%
  mutate(breakid = data.table::rleid(rainfall)) %>% # calculate run length encoding for longest dry spell
  filter(rainfall==0) %>%
  group_by(objectid,year,breakid) %>% slice(c(1,n())) %>%
  arrange(objectid,year) %>%
  dplyr::select(-c(rainfall,onset_date))

dryspells = dryspells %>% # Getting start and end dates of the longest dry spell
  group_by(objectid,year,breakid) %>% 
  do( data.frame(.,variable = c("date1","date2")))
dryspells$date = as.character(dryspells$date)
  
dryspells = reshape2::dcast(dryspells,objectid+year+breakid~variable,value.var = "date")
dryspells$date1 =   as.Date(dryspells$date1,format="%Y-%m-%d")
dryspells$date2 =   as.Date(dryspells$date2,format="%Y-%m-%d")

dryspells = dryspells %>%
  mutate(length_dryspell_d = as.numeric(date2-date1)+1) %>% # Length of all dry dry spell
  filter(length_dryspell_d !=1)

dryspell_max = dryspells %>% group_by(objectid,year) %>%
  slice(which.max(length_dryspell_d)) %>%  # Keeping the rows of longest dry spell 
  left_join(.,block[,c(8,9)] %>% st_drop_geometry()) # Joining block ids
