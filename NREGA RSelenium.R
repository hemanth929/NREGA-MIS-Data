####### Installing RSelenium ####
library(devtools)
install_version("binman", version = "0.1.0", repos = "https://cran.uni-muenster.de/")
install_version("wdman", version = "0.2.2", repos = "https://cran.uni-muenster.de/")
install_version("RSelenium", version = "1.7.1", repos = "https://cran.uni-muenster.de/")
################################


library(RSelenium) 
library(gtools)
library(rvest)
library(dplyr)
driver<- rsDriver(browser=c("chrome"))
remDr <- driver[["client"]]

remDr <- remoteDriver(remoteServerAddr = "192.168.99.100",browserName = "chrome",port = 4445L)
remDr$open()
remDr$navigate("http://mnregaweb4.nic.in/netnrega/MISreport4.aspx") #Running the website
remDr$screenshot(display = TRUE)

##Captcha##
captcha <- remDr$findElement(using = 'id', "ctl00_ContentPlaceHolder1_txtCaptcha")
captcha$sendKeysToElement(list("1"))


verifycode <- remDr$findElement(using = 'id', "ctl00_ContentPlaceHolder1_btnLogin")
verifycode$clickElement()
remDr$screenshot(display = TRUE)

select.year <- remDr$findElement(using = 'id', "ctl00_ContentPlaceHolder1_ddlfinyr")
year <- as.vector(unlist(strsplit(as.character(select.year$getElementText()),split = "\n")))
select.year$sendKeysToElement(list(year[5]))

##not the state observed
select.state <- remDr$findElement(using = 'id', "ctl00_ContentPlaceHolder1_ddl_States")
select.state$sendKeysToElement(list("ALL"))

remDr$screenshot(display = TRUE)
remDr

## Selects the 'Master Category Analysis' link
select.link <- remDr$findElement(using = "xpath", "//a[contains(@href,'all_lvl_details_dashboard_new.aspx?fin_year')]")
select.link$clickElement()
remDr$screenshot(display = TRUE)

#####
# Once the page is opened in the browser, 
# center the CAPTCHA and select year and 'All' in the state
#######


dropdown.state <- remDr$findElement(using = 'name', "ddl_state")
state <- as.vector(unlist(strsplit(as.character(dropdown.state$getElementText()),split = "\n ")))
state <- state[-1]  ## Creates the list of states


df_total <- data.frame()  ### Dataframe where entire data gets loaded

df <- data.frame()
skip <- data.frame()

rm(district,block)
for(i in 22:length(state)){
  dropdown.state <- remDr$findElement(using = 'id', "ddl_state")
  state <- as.vector(unlist(strsplit(as.character(dropdown.state$getElementText()),split = "\n ")))
  state <- state[-1]
  dropdown.state$sendKeysToElement(list(state[i]))
  remDr$refresh()
  
  dropdown.district <- remDr$findElement(using = 'id', "ddl_dist")
  district <- as.vector(unlist(strsplit(as.character(dropdown.district$getElementText()),split = "\n ")))
  district <- district[-1] ## Creates the list of districts for the given state
  
  for(j in 1:length(district)){
    
    remDr$screenshot(display = TRUE)
    
    dropdown.district <- remDr$findElement(using = 'id', "ddl_dist")
    dropdown.district$sendKeysToElement(list(district[j]))
    remDr$refresh()
    
    dropdown.block <- remDr$findElement(using = 'id', "ddl_blk")
    block <- as.vector(unlist(strsplit(as.character(dropdown.block$getElementText()),split = "\n ")))
    block <- block[-1] ## Creates the list of blocks for given district
    
    for(k in 1:length(block)){
      
      tryCatch({
        
        dropdown.block <- remDr$findElement(using = 'id', "ddl_blk")
        dropdown.block$sendKeysToElement(list(block[k])) 
        
        proceed <- remDr$findElement(using = 'id', "btproceed")
        proceed$clickElement()  ## View Detail button
        frames <- remDr$findElements("css", "iframe")
        
        
          frames <- remDr$findElements("css", "iframe")  ## Switching to iFrame to scrape
          remDr$switchToFrame(frames[[1]])
          
          data_html <- xml2::read_html(remDr$getPageSource()[[1]])
          df <- html_table(html_nodes(data_html, "table")[[2]],trim=T,fill = T, header = T)  ### Extracting the table
          
          colnames(df)[3] <- df[1,3] ## Cleaning the table
          colnames(df)[4:6] <- df[1,8:10]
          df <- df[-1,]
          df[,-c(2,8)] <- sapply(df[,-c(2,8)],as.numeric)
          df <- df[complete.cases(df), ]
          
          
          df <- mutate(df, Block = block[k])
          df <- mutate(df, District = district[j])
          df <- mutate(df, State = state[i])
          df <- mutate(df, id = paste(i,j,k,sep = "."))

          df_total <- gtools::smartbind(df,df_total) ##
          print(paste(i,j,k,sep = "."))
          
          remDr$goBack()

          
        
      },error=function(e){cat("ERROR :",conditionMessage(e),"\n")})
    }
      remDr$goBack()
 
    # remDr$switchToFrame(NA)
    
  }
  }


df_total <- df_total %>% filter( State != "ODISHA")

write.csv(df_total,"2014_15.csv")
############ 
### Proceed
dropdown.state <- remDr$findElement(using = 'name', "ddl_state")
state <- as.vector(unlist(strsplit(as.character(dropdown.state$getElementText()),split = "\n ")))
state <- state[-1]


bunch_total <- data.frame()

rm(i,j,k)
for(i in 1:length(state)){
  dropdown.state <- remDr$findElement(using = 'name', "ddl_state")
  state <- as.vector(unlist(strsplit(as.character(dropdown.state$getElementText()),split = "\n ")))
  state <- state[-1]
  dropdown.state$sendKeysToElement(list(state[i]))
  
  dropdown.district <- remDr$findElement(using = 'name', "ddl_dist")
  district <- as.vector(unlist(strsplit(as.character(dropdown.district$getElementText()),split = "\n ")))
  district <- district[-1]
  
  for(j in 1:length(district)){
    dropdown.district <- remDr$findElement(using = 'name', "ddl_dist")
    dropdown.district$sendKeysToElement(list(district[j]))
    
    dropdown.block <- remDr$findElement(using = 'name', "ddl_blk")
    block <- as.vector(unlist(strsplit(as.character(dropdown.block$getElementText()),split = "\n ")))
    block <- block[-1]
    
      for(k in 1:length(block)){
        
      dropdown.block <- remDr$findElement(using = 'name', "ddl_blk")
      dropdown.block$sendKeysToElement(list(block[k]))
    
      bunch <- data.frame()
      bunch <- c(state[i],district[j],block[k],paste(i,j,k.sep="."))
      #bunch <- mutate(bunch, Block = block[k])
      #bunch <- mutate(bunch, District = district[j])
      #bunch <- mutate(bunch, State = state[i])
      #bunch <- mutate(bunch, id = paste(i,j,k.sep="."))
      
      bunch_total <- rbind(bunch_total,bunch)
      print(paste(i,j,k,sep = "."))
      
      remDr$refresh()
      
      # remDr$switchToFrame(NA)
      
    }
  }
}
