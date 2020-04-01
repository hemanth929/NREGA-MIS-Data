#----------NREGA Data Preparation----------#
#---Hemanth Guthala 03/01/2020--------------#

library(dplyr)
library(readr)

files = list.files(path = "D://Hemanth/NREGA/Dynamic/",full.names = T)
file_index = startsWith(files,"4_")
files = files[file_index]

files = lapply(files, readr::read_csv)

df = lapply(files, function(x) { 
  x[-1,-1]  %>%
    mutate_at(c(1:11,16:20), as.character)
  })
df = bind_rows(df)
df$`Work Physically/ Completed Date` = gsub("\r\n                        01-01-1900","",df$`Work Physically/ Completed Date`)
df$`Work Physically/ Completed Date` = as.Date(df$`Work Physically/ Completed Date`,format = "%d-%m-%Y")
colnames(df)[c(12,13,18)] = c("sanction_amt_lacs","amount_inception_lacs","completion_date")

#-------------Matching codes with the polygons------------------#
block_code = readxl::read_excel("D:/Hemanth/Bharatmaps/Block_Webgis_lgd_match.xlsx")
colnames(block_code)[1] = "block.objectid"

df = left_join(df,block_code[,c(1,8:10)],by=c("State"="st_nrega",
                                              "District"="dist_nrega",
                                              "Block Name"="blk_nrega"))


#----------- Subsetting based on work type and category----------#
NRM_cats = c("Drought Proofing",
             "Land Development",
             "Micro Irrigation Works",
             "Renovation of traditional water bodies",
             "Water Conservation and Water Harvesting")

df %>% filter(`Work Category Name` %in% NRM_cats) %>%   #filter(block.objectid==2875) %>%
  group_by(completion_date) %>%
  filter(completion_date!="1900-01-01") %>%
  summarise(exp=sum(sanction_amt_lacs,na.rm = T)) %>%
  mutate(cumm=cumsum(exp)) %>%
  plot(cumm~completion_date, data = .)


nrega_inv = df %>% filter(`Work Category Name` %in% NRM_cats) %>%   #filter(block.objectid==2875) %>%
  filter(completion_date!="1900-01-01") %>%
  group_by(block.FID,year=year(completion_date)) %>%
  summarise(exp=sum(sanction_amt_lacs+amount_inception_lacs,na.rm = T)) %>%
  mutate(cumm=cumsum(exp))

nrega_inv = left_join(nrega_inv,nrega_soilm)

hchart(nrega_inv, "scatter", hcaes(x = exp, y = soilm))





