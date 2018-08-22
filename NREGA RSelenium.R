library(RSelenium) 
library(gtools)
library(rvest)
library(dplyr)

####### Running a chrome session in docker terminal#########
driver<- rsDriver(browser=c("chrome"))
remDr <- driver[["client"]]
remDr <- remoteDriver(remoteServerAddr = "192.168.99.100",browserName = "chrome",port = 4445L)
remDr$open()
remDr$navigate("http://mnregaweb4.nic.in/netnrega/MISreport4.aspx") #Running the website
remDr$screenshot(display = TRUE)

####### Enter the correct CAPTCHA required at <here> as you see in the screenshot #######
captcha <- remDr$findElement(using = 'id', "ctl00_ContentPlaceHolder1_txtCaptcha")
captcha$sendKeysToElement(list("<here>"))

verifycode <- remDr$findElement(using = 'id', "ctl00_ContentPlaceHolder1_btnLogin")
verifycode$clickElement()
remDr$screenshot(display = TRUE) ##Check if you have entered correct CAPTCHA, if not repeat 16

######## Selecting Financial Year
select.year <- remDr$findElement(using = 'id', "ctl00_ContentPlaceHolder1_ddlfinyr")
year <- as.vector(unlist(strsplit(as.character(select.year$getElementText()),split = "\n")))
year
select.year$sendKeysToElement(list(year[2]))  ###change year[i] accordingly

select.state <- remDr$findElement(using = 'id', "ctl00_ContentPlaceHolder1_ddl_States")
select.state$sendKeysToElement(list("ALL"))
remDr$screenshot(display = TRUE) 


########## Selects the 'Master Category Analysis' link

select.link <- remDr$findElement(using = "xpath", "//a[contains(@href,'all_lvl_details_dashboard_new.aspx?fin_year')]")
select.link$clickElement()
remDr$screenshot(display = TRUE)


dropdown.state <- remDr$findElement(using = 'name', "ddl_state")
state <- as.vector(unlist(strsplit(as.character(dropdown.state$getElementText()),split = "\n ")))
state <- state[-1]  ## Creates the list of states


df_total <- data.frame()  ## Dataframe where entire data gets loaded
df <- data.frame()

for(i in 1:length(state)){
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

        df_total <- gtools::smartbind(df,df_total) 
        print(paste(i,j,k,sep = ".")) ## Prints progress
         
        remDr$goBack()
        
      },error=function(e){cat("ERROR :",conditionMessage(e),"\n")})
    }
    remDr$goBack()   
  }
}


