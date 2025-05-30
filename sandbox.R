library(XML)
library(dplyr)
library(reshape2)
options(stringsAsFactors = FALSE)

list_URL <- read.csv("TransferMarkt_Links.csv")

DF_raw <- 
  do.call("rbind",
          
          lapply(1:dim(list_URL)[1], function(x){
            
            TM_URL <- list_URL[x,]$URL
            
            matches_html  <- readLines(TM_URL)
            matches_parse <- htmlTreeParse(matches_html,useInternal=TRUE)
            
            html_parsed <- 
              xpathSApply(matches_parse, 
                          "//td"
                          , xmlValue) 
            
            
            matrix(html_parsed,ncol = 3,byrow = TRUE) %>% as.data.frame() %>% select(-V2) %>% 
              rename(year = V1) %>% rename(Trophy = V3) %>% mutate(Team = list_URL[x,]$Team)
          }
          )
  )


DF_raw %>% group_by(Trophy) %>% summarise(tot = n()) %>% ungroup() %>% View()
DF_raw %>%  group_by(Trophy, Team) %>% summarise(num = n()) %>% 
  dcast(Trophy ~ Team, value.var = "num") %>% 
  View()

DF <- 
  DF_raw %>% mutate(year = if_else(nchar(year) == 5,
                                   if_else(substr(year, 0, 2) %>% as.integer() <= 25, 
                                           paste("20", year, sep = ""),
                                           paste("19", year, sep = "")),
                                   if_else(nchar(year) == 4,
                                           paste(substr(year, 1, 2), substr(year, 3, 4), "/",
                                                 substr(year, 3, 4) %>% as.integer()+ 1, sep = ""), year)))


# DF %>% dcast(year ~ Trophy, value.var = "Team", fun.aggregate = max) %>% View()

DF %>% 
  filter(grepl("winner", Trophy) | grepl("Winner", Trophy) | grepl("English Champion", Trophy)) %>%
  filter(!grepl("Cup Winners Cup Runner Up", Trophy)) %>% 
  filter(!grepl("Football League Trophy Winner", Trophy)) %>% 
  group_by(Team) %>% 
  filter(year == max(year)) %>% 
  select(Team, year, Trophy) %>% 
  arrange(desc(year)) %>% 
  View()
  


DF %>% 
  filter(grepl("winner", Trophy) | grepl("Winner", Trophy) | grepl("English Champion", Trophy)) %>%
  filter(!grepl("Cup Winners Cup Runner Up", Trophy)) %>% 
  filter(!grepl("Football League Trophy Winner", Trophy)) %>% 
  filter(year > "2000/01") %>% 
  dcast(year ~ Trophy, value.var = "Team", fun.aggregate = max) %>% View()
  

