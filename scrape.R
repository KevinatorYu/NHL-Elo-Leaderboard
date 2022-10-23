library(RSelenium)
library(rvest)
library(plyr)
library(dplyr)
library(xml2)

# 2022 Season URL:
#url = "https://www.nhl.com/stats/teams?aggregate=0&reportType=game&seasonFrom=20212022&seasonTo=20212022&dateFromSeason&gameType=2&homeRoad=H&filter=gamesPlayed,gte,1&sort=a_gameDate&page=0&pageSize=50/#footer"

# 2023 Season URL:
url = "https://www.nhl.com/stats/teams?aggregate=0&reportType=game&seasonFrom=20222023&seasonTo=20222023&dateFromSeason&gameType=2&homeRoad=H&filter=gamesPlayed,gte,1&sort=a_gameDate&page=0&pageSize=50"

rD = rsDriver(port=4444L, browser="chrome", chromever="106.0.5249.21") #specify whichever version of chrome you are using
remDr = rD[['client']]
remDr$navigate(url) #this will open a chrome window (that means it is working)

Sys.sleep(3)

src = remDr$getPageSource()[[1]] #we are just selecting everything for now

## read in the info from the first table
df = read_html(src) %>% 
  html_elements(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "rt-td", " " ))]') %>%
  xml_text() %>%
  matrix(.,ncol=24, byrow = T) %>%
  data.frame(.,stringsAsFactors = F)

for (i in 2:30) {
  pages = remDr$findElement(using = "css selector",".next-button") #we are selecting the next button
  pages$clickElement()  
  
  ## wait 1 second to load
  Sys.sleep(1)
  
  src = remDr$getPageSource()[[1]]
  temp = read_html(src) %>% 
    html_elements(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "rt-td", " " ))]') %>%
    xml_text() %>%
    matrix(., ncol=24, byrow = T) %>%
    data.frame(., stringsAsFactors = F)
  
  ## bind new data
  df = df %>% bind_rows(temp) %>% distinct()

}


## add column names
colnames(df) = c("index", "team", "game", "gp", 
                         "w", "l", "t", "ot",
                         "p", "p_pct", "rw", "row",
                         "so_win", "gf", "ga", "gf_gp", "ga_gp",
                         "pp_pct", "pk_pct", "net_pp_pct", "net_pk_pct",
                         "shots_gp", "sa_gp", "fow_pct")

## save file
save(df, file = "C:\\Users\\kevin\\OneDrive\\R Documents\\Hockey\\1204_nhl_home_sat_stats_2018-2019.rsav")

## close
remDr$close()
rD$server$stop() 
gc()

rm(list = c("temp", "i", "pages", "rD", "remDr", "src", "url"))
## adapted from: https://github.com/Christianlee19/hockey-stats/blob/main/bin/article_scripts/1204_scrape_sat.R