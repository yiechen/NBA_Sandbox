################################################################################
#### SCRIPT INFORMATION #### BEGIN ################
################################################################################
# Created by: ychen
# Objective: Scraping data from www.basketball-reference.com
# Version: 0.7
# Last Update: 2017.01.24

#### Description: ####
# Testing data-mining and analysis with R. Extraction of NBA statistics 
# from www.basketball-reference.com and computing advanced stats.
# Further, it's expected to run prediction models for regular season and/or
# prediction for single games.

################################################################################
#### SETUP #### BEGIN ################
################################################################################
# set working directory:
switch(Sys.info()[['sysname']],
       Windows= {setwd( "D:/Users/YCHEN/Desktop/RWorkspace/NBA_Sandbox/" )},
       Linux  = {print( "I'm a penguin." ) & stop('Erro! Script configurado para outros sistemas operacionais')},
       Darwin = {setwd( "~/Desktop/RWorkspace/NBA Sandbox/" )})

# clean environment:
rm(list = ls())

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

# creating get_data function
switch(Sys.info()[['sysname']],
       Windows= {
         get_data <- function(target_url) {
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
           readHTMLTable(getURL(target_url, .opts = opts), stringsAsFactors = FALSE)
       }},
       Linux  = {print( "I'm a penguin." ) & stop('Erro! Script configurado para outros sistemas operacionais')},
       Darwin = {
         get_data <- function(target_url) {
           readHTMLTable(getURL(target_url), stringsAsFactors = FALSE)
         }
         })
################################################################################
#### SETUP #### END ################
################################################################################

tic()
####################################################################################################
#### LOADING PLAYER DATABASE #### BEGIN #### AVG RUNTIME: 50 sec ############
####################################################################################################

fetch_playerdb <- function(initials) {
        url <- paste0("http://www.basketball-reference.com/players/", initials, "/")
        data <- get_data(url)
        data <- data$players
        data
}
# creating and loading a data base containing all player data from A to Z
player_db <- ldply(letters, fetch_playerdb, .progress="text", .parallel = parallel)

colnames(player_db)<-c("Name","From","To","Position","Height","Weight","BirthDate","College")
# removing symbols and converting player names to lower case
make_character <- function(x) {
        x <- tolower(x)
        x <- str_replace_all(x, "'", "") # remove all 'commas'
        x <- str_replace_all(x, "\\.", "") # remove all dots
        x <- str_replace_all(x, "\\*", "") # remove all asteriscs
}
player_db$Player <- make_character(player_db$Name)

# formating data as date or as numeric
#player_db$BirthDate<-as.Date(player_db$BirthDate,format="%B %d, %Y")
player_db$From<-as.numeric(as.character(player_db$From))
player_db$To<-as.numeric(as.character(player_db$To))

# correcting overlapping Positions played labels
player_db$Position[player_db$Position=="PG"]<-"G"               # fixing missing value for one player
player_db$Position[player_db$Name=="George Karl"]<-"G"          # fixing missing value for George Karl
player_db$Position[player_db$Position=="F-C"]<-"C-F"
player_db$Position[player_db$Position=="F-G"]<-"G-F"
player_db$Position<-factor(player_db$Position,levels=c("C","G","F","C-F","G-F"))

###### Creating Players ID #### adding first and last names ###################
player_db <- separate(player_db, col = Player, into = c("first_name", "family_name"), sep = " ", extra = "merge", fill = "right")
remove.spaces <- function(x) {x <- str_replace_all(x, " ", "")} # remove all spaces
player_db$family_name <- remove.spaces(player_db$family_name)
player_db$player_id <- paste0(substring(player_db$family_name,1,5),substring(player_db$first_name,1,2), "01")

# finding duplicated player ids and creating vector of tags to isolate and fix
db2 <- (player_db$player_id[duplicated(player_db$player_id)])
id_to_fix <- as.data.frame(unique(db2))
# creating rectifying function
rectify_id <- function(id) {
  df <- subset(player_db, player_id %in% id)
  ndf<-order(df$From, decreasing=F)
  df = df[ndf,]
  df$order <- order(df$From, decreasing=F)
  df$player_id <- paste0(substring(df$family_name,1,5),substring(df$first_name,1,2), "0", df$order)
  df$order<-NULL
  df
}
# generating rectified ids
df <- rectify_id(id_to_fix[1,1])
len <- length(id_to_fix[,1])
for (i in 2:len) 
  {df<-rbind(df,rectify_id(id_to_fix[i,1]))}
player_db_id_fixed <- df
# excluding original incorrect data and adding fixed data
df <- player_db[player_db$player_id!=as.character(id_to_fix[1,1]),]
for (i in 2:len) 
{df<-df[df$player_id!=as.character(id_to_fix[i,1]),]}
df <- rbind(df, player_db_id_fixed)
player_db <- df

# dropping intermediate data
rm(df)
rm(player_db_id_fixed)
rm(id_to_fix)
rm(db2)
rm(rectify_id)
rm(username)
rm(i)
rm(len)
################################################################################
#### FIX 47 IDS: METTA WORLD PEACE, B FREE, AND MORE ####
################################################################################
# 1-10
player_db$player_id[player_db$player_id=="worldme01"]<-"artesro01"      # ron artest aka metta world peace
player_db$player_id[player_db$player_id=="bfreewo01"]<-"freewo01"       # world b free
player_db$player_id[player_db$player_id=="capelcl01"]<-"capelca01"      # clint capela capela
player_db$player_id[player_db$player_id=="jolliho01"]<-"jolifho01"      # Howie Jolliff
player_db$player_id[player_db$player_id=="senemo01"]<-"senesa01"        # Mouhamed Sene
player_db$player_id[player_db$player_id=="tormoge01"]<-"tormobu01"      # Gene Tormohlen
player_db$player_id[player_db$player_id=="leewied01"]<-"wilkied01"      # Eddie Lee Wilkins
player_db$player_id[player_db$player_id=="rodwiho01"]<-"williho01"      # Hot Rod Williams
player_db$player_id[player_db$player_id=="hoguepa01"]<-"hoguedu01"      # Paul Hogue
player_db$player_id[player_db$player_id=="ilungdi01"]<-"mbengdj01"      # Didier Ilunga-Mbenga
# 11-20
player_db$player_id[player_db$player_id=="johnrpe01"]<-"ramospe01"      # Peter John Ramos
player_db$player_id[player_db$player_id=="ayresje01"]<-"pendeje02"      # Jeff Ayres            # pendeje01 exists and is correct
player_db$player_id[player_db$player_id=="dilladw01"]<-"dilladu01"      # Dwaine Dillard
player_db$player_id[player_db$player_id=="joneswa01"]<-"joneswa02"      #### Wali Jones #### prepare for next fix ####
player_db$player_id[player_db$player_id=="wahjowa01"]<-"joneswa01"      ## Wah Wah Jones
player_db$player_id[player_db$player_id=="kidd-mi01"]<-"kiddgmi01"      # Michael Kidd-Gilchrist
player_db$player_id[player_db$player_id=="laceyed01"]<-"lacyed01"       # Edgar Lacey
player_db$player_id[player_db$player_id=="michaja01"]<-"mcadoja01"      # James Michael McAdoo
player_db$player_id[player_db$player_id=="rensbbo01"]<-"rensbro01"      # Bob Rensberger
player_db$player_id[player_db$player_id=="vinicma01"]<-"vincima01"      # Marcus Vinicius
# 21-30
player_db$player_id[player_db$player_id=="walkehe01"]<-"walkebi01"      # Henry Walker
player_db$player_id[player_db$player_id=="walleta01"]<-"princta02"      # Taurean Waller-Prince # princta01 exists and is correct
player_db$player_id[player_db$player_id=="bareajj01"]<-"bareajo01"      # J.J. Barea
player_db$player_id[player_db$player_id=="raybabi01"]<-"batesbi01"      # Billy Ray Bates
player_db$player_id[player_db$player_id=="eichhri01"]<-"eichhdi01"      # Rich Eichhorst
player_db$player_id[player_db$player_id=="el-amkh01"]<-"elamikh01"      # Khalid El-Amin
player_db$player_id[player_db$player_id=="joengjo01"]<-"englijo01"      # Jo Jo English
player_db$player_id[player_db$player_id=="fosteji01"]<-"fosteja01"      # Jimmy Foster
player_db$player_id[player_db$player_id=="abdulma01"]<-"abdulma02"      # Mahmoud Abdul-Rauf    # abdulma01 does not exist
player_db$player_id[player_db$player_id=="hazzawa01"]<-"abdulma01"      # Walt Hazzard
# 31-40
player_db$player_id[player_db$player_id=="rodhuho01"]<-"hundlho01"      # Hot Rod Hundley
player_db$player_id[player_db$player_id=="munfoxa01"]<-"munfoxa02"      # Xavier Munford        # munfoxa01 does not exist
player_db$player_id[player_db$player_id=="carloju01"]<-"navarju01"      # Juan Carlos Navarro
player_db$player_id[player_db$player_id=="putmado01"]<-"putnado01"      # Don Putman
player_db$player_id[player_db$player_id=="smithto01"]<-"smithto02"      # Tony Smith            # smithto01 does not exist
player_db$player_id[player_db$player_id=="jowhijo01"]<-"whitejo01"      # Jo Jo White*
player_db$player_id[player_db$player_id=="barryjo01"]<-"carrojo01"      #### Joe Barry Carroll #### prepare for next fix ####
player_db$player_id[player_db$player_id=="barryjo02"]<-"barryjo01"      ## Jon Barry
player_db$player_id[player_db$player_id=="willimo03"]<-"willima04"      #### Marcus Williams #### prepare for next fix ####
player_db$player_id[player_db$player_id=="willimo02"]<-"willima03"      #### Marcus Williams #### prepare for next fix ####
# 41-50
player_db$player_id[player_db$player_id=="willimo01"]<-"willima02"      #### Marvin Williams #### prepare for next fix ####
player_db$player_id[player_db$player_id=="willimo02"]<-"willima01"      ## Mo Williams
player_db$player_id[player_db$player_id=="bogdabo01"]<-"bogdabo02"      # Bojan Bogdanovic      # bogdabo01 does not exist
player_db$player_id[player_db$player_id=="hairspj01"]<-"hairspj02"      # P.J. Hairston         # hairspj01 does not exist
player_db$player_id[player_db$player_id=="hugheal01"]<-"hugueri01"      # Alfredrick Hughes
player_db$player_id[player_db$player_id=="pavlosa01"]<-"pavloal01"      # Sasha Pavlovic
player_db$player_id[player_db$player_id=="rayrimi01"]<-"richami01"      # Micheal Ray Richardson
# 48
################################################################################
#### FIX SPECIAL ONES: METTA WORLD PEACE, B FREE, AND MORE? ####
################################################################################

# writing database to disk
#player_db <- subset(player_db, To>=2017)
write.csv(player_db, file="player_db.csv")

# reload data 
player_db <- read.csv(file="player_db.csv", stringsAsFactors = FALSE)

####################################################################################################
#### LOADING PLAYER DATABASE #### END ################
####################################################################################################
toc(log = T)

tic()
####################################################################################################
#### LOADING PLAYER DATABASE - ALTERNATIVE LEAN SCRIPT #### BEGIN #### AVG RUNTIME: 50sec #######
####################################################################################################
#from https://analystatlarge.wordpress.com/2014/06/03/basketball-data-part-ii-length-of-career-by-position/
###### URLs
url<-paste0("http://www.basketball-reference.com/players/",letters[-24],"/")
len<-length(url)
###### Reading data
tbl<-get_data(url[1])[[1]]
for (i in 2:len)
{tbl<-rbind(tbl,get_data(url[i])[[1]])}
###### Formatting data
colnames(tbl)<-c("Name","StartYear","EndYear","Position","Height","Weight","BirthDate","College")
####################################################################################################
#### LOADING PLAYER DATABASE - ALTERNATIVE LEAN SCRIPT #### END ################
####################################################################################################
toc(log = T)

tic()
####################################################################################################
#### LOADING CAREER DATA BY SEASON #### BEGIN #### AVG RUNTIME: 4H 17MIN ############
####################################################################################################
# creating function to fetch player career data
fetch_playerdata0 <- function(player_id) {
  url <- paste0("http://www.basketball-reference.com/players/", substring(player_id,1,1), "/", player_id, ".html")
  data <- get_data(url)
  data$player_id <- player_id
  #data <- melt(data)
  data <- as.data.frame(data)
  data
}
# creating and loading a data base containing all player data
# creating subsets by position to soften the burden from data mining process
player_db <- read.csv(file="player_db.csv", stringsAsFactors = FALSE)

# by center
player_db_C <- subset(player_db, Position=="C")
players_compiled_data_C <- ldply(player_db_C$player_id, fetch_playerdata0, .progress="text")
write.csv(players_compiled_data_C, file="players_compiled_data_C.csv")
rm(players_compiled_data_C)

# by guard
player_db_G <- subset(player_db, Position=="G")
players_compiled_data_G <- ldply(player_db_G$player_id, fetch_playerdata0, .progress="text")
write.csv(players_compiled_data_G, file="players_compiled_data_G.csv")
rm(players_compiled_data_G)

# by forward
player_db_F <- subset(player_db, Position=="F")
players_compiled_data_F <- ldply(player_db_F$player_id, fetch_playerdata0, .progress="text")
write.csv(players_compiled_data_F, file="players_compiled_data_F.csv")
rm(players_compiled_data_F)

# by guard-forward
player_db_GF <- subset(player_db, Position=="G-F")
players_compiled_data_GF <- ldply(player_db_GF$player_id, fetch_playerdata0, .progress="text")
write.csv(players_compiled_data_GF, file="players_compiled_data_GF.csv")
rm(players_compiled_data_GF)

# by center-forward
player_db_CF <- subset(player_db, Position=="C-F")
players_compiled_data_CF <- ldply(player_db_CF$player_id, fetch_playerdata0, .progress="text")
write.csv(players_compiled_data_CF, file="players_compiled_data_CF.csv")
rm(players_compiled_data_CF)

# reading the databases from file
players_compiled_data_G <- read.csv(file="players_compiled_data_G.csv", stringsAsFactors = FALSE)
players_compiled_data_F <- read.csv(file="players_compiled_data_F.csv", stringsAsFactors = FALSE)
players_compiled_data_C <- read.csv(file="players_compiled_data_C.csv", stringsAsFactors = FALSE)
players_compiled_data_GF <- read.csv(file="players_compiled_data_GF.csv", stringsAsFactors = FALSE)
players_compiled_data_CF <- read.csv(file="players_compiled_data_CF.csv", stringsAsFactors = FALSE)

# joining the databases into one single dataframe
players_compiled_data <- join(players_compiled_data_CF,players_compiled_data_C, type = "full")
players_compiled_data <- join(players_compiled_data,players_compiled_data_F, type = "full")
players_compiled_data <- join(players_compiled_data,players_compiled_data_G, type = "full")
players_compiled_data <- join(players_compiled_data,players_compiled_data_GF, type = "full")

rm(players_compiled_data_C)
rm(players_compiled_data_G)
rm(players_compiled_data_F)
rm(players_compiled_data_GF)
rm(players_compiled_data_CF)

# writing data to disk
write.csv(players_compiled_data, file="players_compiled_data.csv")

# reload data 
players_compiled_data <- read.csv(file="players_compiled_data.csv", stringsAsFactors = FALSE)
rm(players_compiled_data)

####################################################################################################
#### LOADING CAREER DATA BY SEASON #### END ################
####################################################################################################
toc(log = T)
tic()
####################################################################################################
#### LOADING CAREER DATA BY GAMELOG #### BEGIN #### AVG RUNTIME: 8+HS ????? ############
####################################################################################################
# creating function to fetch player gamelog data
fetch_playerdata <- function(player_id, year) {
        url <- paste0("http://www.basketball-reference.com/players/", substring(player_id,1,1), "/", player_id, "/gamelog/", year, "/")
        data <- get_data(url)
        data <- data$pgl_basic
        data$player_id <- player_id
        data$year <- year
        data <- as.data.frame(data)
        data
}
fetch_careerdata <- function(player_id) {
        player_list <- c(player_id)
        from <- as.numeric(player_db$From[player_db$player_id==player_id])
        to <- as.numeric(player_db$To[player_db$player_id==player_id])
        data <- mdply(expand.grid(player_id = player_list, year = from:to), fetch_playerdata)
        data
}

# creating and loading a data base containing all player data
# creating subsets by position to soften the burden from data mining process
player_db <- read.csv(file="player_db.csv", stringsAsFactors = FALSE)

# by center
player_db_C <- subset(player_db, Position=="C")
players_gamelog_data_C <- ldply(player_db_C$player_id, fetch_careerdata, .progress="text")
write.csv(players_gamelog_data_C, file="players_gamelog_data_C.csv")
rm(players_gamelog_data_C)

# by guard
player_db_G <- subset(player_db, Position=="G")
players_gamelog_data_G <- ldply(player_db_G$player_id, fetch_careerdata, .progress="text")
write.csv(players_gamelog_data_G, file="players_gamelog_data_G.csv")
rm(players_gamelog_data_G)

# by forward
player_db_F <- subset(player_db, Position=="F")
players_gamelog_data_F <- ldply(player_db_F$player_id, fetch_careerdata, .progress="text")
write.csv(players_gamelog_data_F, file="players_gamelog_data_F.csv")
rm(players_gamelog_data_F)

# by guard-forward
player_db_GF <- subset(player_db, Position=="G-F")
players_gamelog_data_GF <- ldply(player_db_GF$player_id, fetch_careerdata, .progress="text")
write.csv(players_gamelog_data_GF, file="players_gamelog_data_GF.csv")
rm(players_gamelog_data_GF)

# by center-forward
player_db_CF <- subset(player_db, Position=="C-F")
players_gamelog_data_CF <- ldply(player_db_CF$player_id, fetch_careerdata, .progress="text")
write.csv(players_gamelog_data_CF, file="players_gamelog_data_CF.csv")
rm(players_gamelog_data_CF)

# reading the databases from file
players_gamelog_data_G <- read.csv(file="players_gamelog_data_G.csv", stringsAsFactors = FALSE)
players_gamelog_data_F <- read.csv(file="players_gamelog_data_F.csv", stringsAsFactors = FALSE)
players_gamelog_data_C <- read.csv(file="players_gamelog_data_C.csv", stringsAsFactors = FALSE)
players_gamelog_data_GF <- read.csv(file="players_gamelog_data_GF.csv", stringsAsFactors = FALSE)
players_gamelog_data_CF <- read.csv(file="players_gamelog_data_CF.csv", stringsAsFactors = FALSE)

# joining the databases into one single dataframe
players_gamelog_data <- join(players_gamelog_data_CF,players_gamelog_data_C, type = "full")
players_gamelog_data <- join(players_gamelog_data,players_gamelog_data_F, type = "full")
players_gamelog_data <- join(players_gamelog_data,players_gamelog_data_G, type = "full")
players_gamelog_data <- join(players_gamelog_data,players_gamelog_data_GF, type = "full")

rm(players_gamelog_data_C)
rm(players_gamelog_data_G)
rm(players_gamelog_data_F)
rm(players_gamelog_data_GF)
rm(players_gamelog_data_CF)

# writing data to disk
write.csv(players_gamelog_data, file="players_gamelog_data.csv")

# reload data 
players_gamelog_data <- read.csv(file="players_gamelog_data.csv", stringsAsFactors = FALSE)
rm(players_gamelog_data)

####################################################################################################
#### LOADING CAREER DATA BY GAMELOG #### END ################
####################################################################################################
toc(log = T)
tic()
####################################################################################################
#### LOADING SPLITS CAREER DATA #### BEGIN #### AVG RUNTIME: 4H 17MIN ############
####################################################################################################
# creating function to fetch player career data
fetch_playersplits <- function(player_id) {
        url <- paste0("http://www.basketball-reference.com/players/", substring(player_id,1,1), "/", player_id, "/splits/")
        data <- htmltab(doc = get_url(url), header = 1:2)
        data$player_id <- player_id
        #data <- melt(data)
        data <- as.data.frame(data)
        data
}
# creating and loading a data base containing all player data
# creating subsets by position to soften the burden from data mining process
player_db <- read.csv(file="player_db.csv", stringsAsFactors = FALSE)

# by center
player_db_C <- subset(player_db, Position=="C")
players_splits_data_C <- ldply(player_db_C$player_id, fetch_playersplits, .progress="text")
write.csv(players_splits_data_C, file="players_splits_data_C.csv")
rm(players_splits_data_C)

# by guard
player_db_G <- subset(player_db, Position=="G")
players_splits_data_G <- ldply(player_db_G$player_id, fetch_playersplits, .progress="text")
write.csv(players_splits_data_G, file="players_splits_data_G.csv")
rm(players_splits_data_G)

# by forward
player_db_F <- subset(player_db, Position=="F")
players_splits_data_F <- ldply(player_db_F$player_id, fetch_playersplits, .progress="text")
write.csv(players_splits_data_F, file="players_splits_data_F.csv")
rm(players_splits_data_F)

# by guard-forward
player_db_GF <- subset(player_db, Position=="G-F")
players_splits_data_GF <- ldply(player_db_GF$player_id, fetch_playersplits, .progress="text")
write.csv(players_splits_data_GF, file="players_splits_data_GF.csv")
rm(players_splits_data_GF)

# by center-forward
player_db_CF <- subset(player_db, Position=="C-F")
players_splits_data_CF <- ldply(player_db_CF$player_id, fetch_playersplits, .progress="text")
write.csv(players_splits_data_CF, file="players_splits_data_CF.csv")
rm(players_splits_data_CF)

# reading the databases from file
players_splits_data_G <- read.csv(file="players_splits_data_G.csv", stringsAsFactors = FALSE)
players_splits_data_F <- read.csv(file="players_splits_data_F.csv", stringsAsFactors = FALSE)
players_splits_data_C <- read.csv(file="players_splits_data_C.csv", stringsAsFactors = FALSE)
players_splits_data_GF <- read.csv(file="players_splits_data_GF.csv", stringsAsFactors = FALSE)
players_splits_data_CF <- read.csv(file="players_splits_data_CF.csv", stringsAsFactors = FALSE)

# joining the databases into one single dataframe
players_splits_data <- join(players_splits_data_CF,players_splits_data_C, type = "full")
players_splits_data <- join(players_splits_data,players_splits_data_F, type = "full")
players_splits_data <- join(players_splits_data,players_splits_data_G, type = "full")
players_splits_data <- join(players_splits_data,players_splits_data_GF, type = "full")

rm(players_splits_data_C)
rm(players_splits_data_G)
rm(players_splits_data_F)
rm(players_splits_data_GF)
rm(players_splits_data_CF)

# writing data to disk
write.csv(players_splits_data, file="players_splits_data.csv")

# reload data 
players_splits_data <- read.csv(file="players_splits_data.csv", stringsAsFactors = FALSE)
rm(players_splits_data)

####################################################################################################
#### LOADING SPLIT CAREER DATA #### END ################
####################################################################################################
toc(log = T)
tic()
####################################################################################################
#### LOADING PLAYER DATA #### ALTERNATIVE SCRIPT #### BEGIN ################
####################################################################################################
# from: http://stackoverflow.com/questions/27831970/scraping-basketball-reference-com-in-r-xml-package-not-fully-working
# URL="http://www.basketball-reference.com/players/j/jamesle01/splits/"
# tablefromURL = readHTMLTable(URL)
# table = tablefromURL[[1]]
# 
# dd <- html_session("http://www.basketball-reference.com/players/j/jamesle01/splits/") %>%
#   html_node("table#stats") %>%
#   html_table()  #### does not work: Error in UseMethod("html_table") : no applicable method for 'html_table' applied to an object of class "xml_missing"
# 
# appURL <- "http://www.basketball-reference.com/players/j/jamesle01/splits/"
# doc <- htmlParse(appURL)
# appTables <- doc['//table/tbody']
# myHeaders <- unlist(doc["//thead/tr[2]/th", fun = xmlValue])
# myTables <- lapply(appTables, readHTMLTable, header = myHeaders)
# bigTable <- do.call(rbind, myTables)
# head(bigTable)

############################# LOOK #################################################
# ### need to investigate following package with more attention, it's promising ####
# devtools::install_github("crubba/htmltab")
# library(htmltab)
# dd<-htmltab(doc = get_url("http://www.basketball-reference.com/players/j/jamesle01/splits/"), header = 1:2)
# # dd<-htmltab(doc = "http://www.basketball-reference.com/players/j/jamesle01/gamelog/2017/", which = 2, header = 1:2)
# # df<-htmltab(doc = "http://www.basketball-reference.com/players/j/jamesle01/", header = 1:2)

################ LOOK FOR TIPS ON LINK BELOW ###############################
#### from: http://asbcllc.com/blog/2014/november/creating_bref_scraper/ ####


####################################################################################################
#### LOADING PLAYER DATA #### ALTERNATIVE SCRIPT #### END ################
####################################################################################################
toc(log = T)


####################################################################################################
#### ALTERNATIVE TAKE ON MUNGING DATA FROM BREF ####
####################################################################################################
# # from 10960-2016-poster.pdf
# url.list = sprintf("http://www.basketball-reference.com/leagues/NBA_%s.html", c(1980:1998,2000:2011,2013:2014)) #create a list of links
# columnclasses = c("character","character", rep("integer", 4), "numeric",
#                   rep("integer",2), "numeric", rep("integer",2), 
#                   "numeric", rep("integer",2), "numeric", rep("integer",9), "numeric") # vector for setting column names
# for (i in 1:33) {       #read in the tables from the urls
#         nam = paste0("x", i)
#         assign(nam, readHTMLTable(getURL(url.list[[i]], .opts = opts), colClasses = columnclasses, stringsAsFactors = FALSE))
# }
# #ddf <- as.data.frame(x1$divs_standings_W)
# # use https://rpubs.com/dvorakt/235621 
# # to see if rvest package can manage to help reading those tables readHTMLtable cant
####################################################################################################
####################################################################################################

tic()
################################################################################
#### DATA ANALYSIS  #####
################################################################################

####################################################################################################
#### build a graphics template based on fivethirtyeight color palette ##############################
#### such as in: https://fivethirtyeight.com/features/chris-paul-is-a-point-god/ ###################
####################################################################################################

# # re-uploading the dataset
# df <- read.csv(file="mlb_standings_and_payroll.csv", stringsAsFactors = FALSE)
# # sorting variables and limiting to observations from 1985 to present
# df <- df[,c("tm", "year", "w", "wins_losses", "est_payroll")]
# df <- df[df$year >= 1985,]
# head(df)    #sample of data to check for errors
# 
# # loading metadata for each team (made by yhat's Greg Lamp)
# # recovers and reclassify the teams with their hystorical counterparts
# # (franchise is kept albeit the city changes)
# team.lookups <- read.csv("./team-lookups.csv")
# df <- merge(df, team.lookups, by.x="tm", by.y="historic_team", stringsAsFactors=FALSE)
# head(df)
# 
# # loading metadata for team colors (made by Greg Lamp, data from http://jim-nielsen.com/teamcolors/)
# team.colors <- read.csv("./team-colors.csv", stringsAsFactors=FALSE)
# df <- merge(df, team.colors, by.x="modern_team", by.y="tm")
# head(df)
# 
# ggplot(team.colors, aes(x=team_name, fill=team_color)) +
#         geom_bar() +
#         scale_fill_identity() +
#         coord_flip() + xlab("") + ylab("")      # show team colors
# 
# # calculating the mean and standard deviation for the estimated payrolls by year
# yearly_payroll <- ddply(df, .(year), function(x) {
#   data.frame(
#     mean_payroll=mean(x$est_payroll, na.rm=TRUE),
#     std_payroll=sd(x$est_payroll, na.rm=TRUE)
#   )
# })
# 
# # adding the new info into the dataset
# df <- merge(df, yearly_payroll, by="year")
# 
# # calculate number of standard deviations from mean and adding to dataset
# df$standardized_payroll <- (df$est_payroll - df$mean_payroll) / df$std_payroll

################################################################################
# DATA ANALYSIS and VIZ                                                        #
################################################################################
df <- sessionInfo()
print(df)

toc(log = T)
##########################
# End of script
##########################