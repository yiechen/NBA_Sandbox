################################################################################
#### SCRIPT INFORMATION #### BEGIN ################
################################################################################
# Created by: ychen
# Objective: How to Webscrape in R, the RVest and pipeR Way
# Version: 0.1
# Last Update: 2017.01.24

#### Description: ####
# Reproducing http://asbcllc.com/blog/2014/november/creating_bref_scraper/
# Testing data-mining and analysis with R. Extraction of NBA statistics 
# from www.basketball-reference.com and computing advanced stats.

################################################################################
#### SETUP #### BEGIN ################
################################################################################
# set working directory:
switch(Sys.info()[['sysname']],
       Windows= {setwd( "D:/Users/YCHEN/Desktop/RWorkspace/NBA_Sandbox/" )},
       Linux  = {print( "I'm a penguin." ) & stop('Erro! Script configurado para outros sistemas operacionais')},
       Darwin = {setwd( "~/Desktop/RWorkspace/NBA Sandbox/" )})

# Set of packages, versions and scripts used:
c('rvest','plyr','pipeR', 'knitr', 'XML', 'tidyverse', 'dplyr', 'stringr',
  'ggthemes', 'httr', 'tictoc', 'RCurl', 'curl', 'htmltab') -> packages    # not sure i'm using every package listed
lapply(packages, require, character.only = T) #loops through the packages and loads them
rm(packages)

# setting curl opts to bypass firewall
switch(Sys.info()[['sysname']],
       Windows= {
               username <- Sys.getenv("USERNAME")
               pass <- scan(paste("D:/Users/",username,"/pass.txt",sep=""),what="character",quiet=TRUE)
               opts <- list(
                       proxy           = "proxy.inf.bndes.net",
                       proxyport       = 8080,
                       proxyusername   = paste(Sys.getenv("USERDOMAIN"), "\\",username, sep=""),
                       proxypassword   = pass
               )
               set_config(use_proxy(url="proxy.inf.bndes.net",8080,username,pass))
               rm(pass)
               
               curl <- getCurlHandle()
               curlSetOpt(.opts = opts, curl = curl)
       },
       Linux  = {print( "I'm a penguin." ) & stop('Erro! Script configurado para outros sistemas operacionais')},
       Darwin = {print( "I'm an apple." )})

# setting parallelization by addressing backend
switch(Sys.info()[['sysname']],
       Windows= {parallel = FALSE},
       Linux  = {print( "I'm a penguin." ) & stop('Erro! Script configurado para outros sistemas operacionais')},
       Darwin = {parallel = FALSE})

# cleaning tictoc log
tic.clearlog()

# creating get_url function
switch(Sys.info()[['sysname']],
       Windows= {
               get_url <- function(target_url) {
                       username <-Sys.getenv("USERNAME")
                       pass <- scan(paste("D:/Users/",username,"/pass.txt",sep=""), what="character", quiet=TRUE)
                       opts <- list( 
                               proxy         = "proxy.inf.bndes.net",
                               proxyport     = 8080,
                               proxyusername = paste(Sys.getenv("USERDOMAIN"),"\\", username, sep=""),
                               proxypassword = pass,
                               ssl.verifypeer = FALSE
                       )
                       rm(pass)
                       getURL(target_url, .opts = opts)
               }},
       Linux  = {print( "I'm a penguin." ) & stop('Erro! Script configurado para outros sistemas operacionais')},
       Darwin = {
               get_url <- function(target_url) {
                       getURL(target_url)
               }
       })

################################################################################
#### SETUP #### END ################
################################################################################

################################################################################
#### REPRODUCTION OF SCRIPT #### BEGIN  ####                                
################################################################################

'http://www.basketball-reference.com/leagues/NBA_2014.html' -> url
'#team' -> css_page
url %>>%
        get_url %>>%
        read_html %>>%
        html_nodes(css_page) %>>%
        html_table(header = F) %>>%
        data.frame() %>>%
        tbl_df() -> total_table
total_table %>>%
        filter(X.1 == 'Rk') %>>% as.character -> names
'Rk' %>>% grep(x = total_table$X.1) -> row_of_header #find where rank is
names %>>% tolower -> names(total_table)
names(total_table) %>>% (gsub('\\%|/','\\.',.)) -> names(total_table)
(row_of_header + 1) %>>% (total_table[.:nrow(total_table),]) -> total_table #skip that row and go to the end row and go to the end
total_table %>>% head

#### read_html(get_url(url))

total_table %>>% write.csv('Desktop/2014_team_data.csv', row.names = F) 

getBREFTeamStatTable <- function(season_end = 2015, table_name = 'team', date = T){
        c('rvest','dplyr','pipeR') -> packages
        lapply(packages, library, character.only = T)
        'http://www.basketball-reference.com/leagues/' -> base
        'NBA' -> league
        '#' %>>% paste0(table_name) -> css_page
        css_page %>>% paste0(" , ", css_page,' a') -> css_id
        table_name %>>% tolower -> table_name
        table_name %>>% paste('stats', sep = "_") -> table
        base %>>% paste0(league,'_',season_end,".html") -> url
        url %>>% ## get table
                html %>>%
                html_nodes(css_page) %>>%
                html_table(header = F) %>>% data.frame() %>>% tbl_df() -> df
        
        if(df$X.1[1] == 'Rk'){
                df %>>%
                        filter(X.1 == "Rk") %>>% as.character -> names
                'Rk' %>>% grep(x = df$X.1) -> row_of_header #find where rank is
                (row_of_header + 1) %>>% (df[.:nrow(df),]) -> df #skip that row and go to the end
                names %>>% tolower-> names(df)} else{
                        df %>>%
                                filter(X.1 == "Rk") %>>% as.character -> names
                        'Rk' %>>% grep(x = df$X.1) -> row_of_header #find where rank is
                        (row_of_header + 1) %>>% (df[.:nrow(df),]) -> df #skip that row and go to the end
                        names %>>% tolower-> names(df)
                }
        names(df) %>>% (gsub('\\%|/','\\.',.)) -> names(df)
        NULL -> df$rk
        c('team','arena') -> table_name_character
        df[,!(df %>>% names) %in% table_name_character] %>>%
                apply(2, function(x) gsub('\\,','',x) %>>% as.numeric(x))  ->
                df[,!(df %>>% names) %in% table_name_character] #get rid of commas and make numeric
        df$team %>>% grepl(pattern = '\\*') -> df$playoff_team
        df$team %>>% (gsub('\\*','',.)) -> df$team
        df %>>% nrow() -1  -> rows
        df[1:rows,] -> df
        (season_end-1) %>>% paste0("-",season_end) -> season
        ##Grab Team Ids
        url %>>% ## get table
                html %>>%
                html_nodes(css_id) %>>%
                html_attrs() %>>% unlist %>>% as.character -> stems
        stems[3:length(stems)] -> stems #skip the 1st 2 rows since they are labels
        stems %>>% (gsub('\\/|.html|teams','',.)) %>>% #strip out the text
                (gsub(season_end,'',.)) -> bref_team_id #strip out the year to get the team id
        data.frame(season,table_name = table, bref_team_id, df) -> df  #combine into 1 df
        if(date == T){
                Sys.time() -> df$scrape_time #add scrape time if you want it
        }
        return(df)
}

getBREFTeamStatTable() -> team2015
team2015 %>>% kable('html', table.attr='id="team2015"')

getBREFTeamStatTable(season_end = 1984,table_name = 'misc',date = F) -> misc1984
misc1984 %>>% kable('html', table.attr='id="misc1984"')

################################################################################
#### REPRODUCTION OF SCRIPT #### END ####                                
################################################################################
