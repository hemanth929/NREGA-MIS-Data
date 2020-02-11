####### Installing RSelenium ####
library(devtools)
install_version("binman", version = "0.1.0", repos = "https://cran.uni-muenster.de/")
install_version("wdman", version = "0.2.2", repos = "https://cran.uni-muenster.de/")
install_version("RSelenium", version = "1.7.1", repos = "https://cran.uni-muenster.de/")
################################

library(gtools)
library(rvest)
library(dplyr)
library(RSelenium)
# driver<- rsDriver(browser=c("chrome"))
# remDr <- driver[["client"]]

remDr <- remoteDriver(remoteServerAddr = "192.168.99.100",browserName = "chrome",port = 4445L)
remDr$open()
remDr$navigate("http://mnregaweb4.nic.in/netnrega/MISreport4.aspx") #Running the website
remDr$screenshot(display = TRUE)

captcha_text <- remDr$findElement(using = 'id', "ctl00_ContentPlaceHolder1_lblStopSpam")
string = captcha_text$getElementText()[[1]]
string = string %>% stringr::str_match_all("[0-9]+") %>% unlist %>% as.numeric
string = string[1]-string[2]
##Captcha##
captcha <- remDr$findElement(using = 'id', "ctl00_ContentPlaceHolder1_txtCaptcha")
captcha$sendKeysToElement(list((as.character(string))))


verifycode <- remDr$findElement(using = 'id', "ctl00_ContentPlaceHolder1_btnLogin")
verifycode$clickElement()
remDr$screenshot(display = TRUE)

select.year <- remDr$findElement(using = 'id', "ctl00_ContentPlaceHolder1_ddlfinyr")
year <- as.vector(unlist(strsplit(as.character(select.year$getElementText()),split = "\n")))
select.year$sendKeysToElement(list(year[1]))

##not the state observed
select.state <- remDr$findElement(using = 'id', "ctl00_ContentPlaceHolder1_ddl_States")
select.state$sendKeysToElement(list("ALL"))

remDr$screenshot(display = TRUE)



select.link <- remDr$findElement(using = "xpath", "//a[contains(@href,'dynamic_phy_fin_detail.aspx')]")
select.link$clickElement()
remDr$screenshot(display = TRUE)

dropdown.state <- remDr$findElement(using = 'name', "ctl00$ContentPlaceHolder1$ddl_state")
state <- as.vector(unlist(strsplit(as.character(dropdown.state$getElementText()),split = "\n   ")))
state <- state[-1] 



for(i in 1:length(state)){
  dropdown.state <- remDr$findElement(using = 'id', "ctl00_ContentPlaceHolder1_ddl_state")
  # state <- as.vector(unlist(strsplit(as.character(dropdown.state$getElementText()),split = "\n   ")))
  # state <- state[-1]
  dropdown.state$sendKeysToElement(list(state[i]))
  #remDr$refresh()
  
  dropdown.district <- remDr$findElement(using = 'id', "ctl00_ContentPlaceHolder1_ddl_dist")
  district <- as.vector(unlist(strsplit(as.character(dropdown.district$getElementText()),split = "\n   ")))
  district <- district[-1] ## Creates the list of districts for the given state
  
  for(j in 1:length(district)){
    
    remDr$screenshot(display = TRUE)
    
    dropdown.district <- remDr$findElement(using = 'id', "ctl00_ContentPlaceHolder1_ddl_dist")
    dropdown.district$sendKeysToElement(list(district[j]))
    #remDr$refresh()
    
    dropdown.block <- remDr$findElement(using = 'id', "ctl00_ContentPlaceHolder1_ddl_blk")
    block <- as.vector(unlist(strsplit(as.character(dropdown.block$getElementText()),split = "\n   ")))
    block <- block[-1] ## Creates the list of blocks for given district
    for(k in 1:length(block)){
      
      tryCatch({
        
        dropdown.block <- remDr$findElement(using = 'id', "ctl00_ContentPlaceHolder1_ddl_blk")
        dropdown.block$sendKeysToElement(list(block[k])) 
        #remDr$screenshot(display = TRUE)
        
        
        select.finance <- remDr$findElement(using = 'id', "ctl00_ContentPlaceHolder1_Rblstatus_1")
        select.finance$clickElement()
        
        proceed <- remDr$findElement(using = 'id', "ctl00_ContentPlaceHolder1_Btnsubmit")
        proceed$clickElement()  ## View Detail button
        remDr$screenshot(display = TRUE)
        #frames <- remDr$findElements("css", "iframe")
        
        data_html <- xml2::read_html(remDr$getPageSource()[[1]])
        df <- html_table(html_nodes(data_html, "table")[[6]],trim=T,fill = T, header = T)  ### Extracting the table
        df$State <- state[i]
        df$District <- district[j]
        
        write.csv(df,paste0("D:/NREGA_Structure/",i,"_",j,"_",k,".csv"),row.names = F)
      },error=function(e){cat("ERROR :",conditionMessage(e),"\n")})
      
      print(paste(i,j,k))
    }
  }
  remDr$refresh()
}
  
  