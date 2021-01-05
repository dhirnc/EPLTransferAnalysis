library(ggplot2)
library(dplyr)
library(tidyverse)

# Read data
trans <- read.csv("top250-00-19.csv", fileEncoding = "UTF-8", strip.white = T)

plres <- read.csv("result.csv", strip.white = T)


##### Data checking and Wrangling #######

# Looking at TRANSFERS dataset

str(trans)
head(trans)

# Filtering transfers between 2000/2001 season and 2016/2017 season
trans$Season %>% as.character() -> trans$Season
trans %>% filter(Season <= "2016-2017") -> trans
trans$Season %>% as.factor() -> trans$Season

#  Keeping only the columns required for analysis
trans %>% select(Name,Position,Age,Team_from,League_from,Team_to,League_to,Season,Transfer_fee) -> trans

# Filtering data for Premier League only.
trans %>% 
  filter(League_from=='Premier League' | League_to=='Premier League' | League_from==" England" | League_to==" England") -> trans

# Checking for missing values in each column
colSums(is.na(trans))
## No missing values found

length(unique(trans))



# Looking at Premier League results dataset

str(plres)
head(plres)

# Filtering data from 2000/2001 season till 2016/2017 season for PL results
plres$year %>% as.character() -> plres$year
plres %>% filter(year >= "2000/2001", year <= "2016/2017") -> plres
plres$year %>% as.factor() -> plres$year

#  Keeping only the columns required for analysis
plres %>% select(year,Team,Pos,Pts, W, D, L, F, A, GD) -> plres

# Checking missing values
colSums(is.na(plres))

# Checking if there is data for all seasons
levels(plres$year)
## Looks like we donot have data from 2015/2016 season. 
# Reading data fro 2015/2016 season
plres_1516 <- read.csv("epl20152016.csv")
# Refomatting before joining
# Changing column names
plres_1516 %>% rename(Pos = X., Pts = P) -> plres_1516
# Creating new columns for year and Goal Difference
plres_1516 %>% mutate(year = "2015/2016") -> plres_1516
plres_1516 %>% mutate(GD = F-A) -> plres_1516
# Selecting columns to join with main results data frame.
plres_1516 %>% select(year, Team, Pos, Pts, W, D, L, F, A, GD) -> plres_1516
# Joining data from 2015/2016 season to original results dataset 
plres <- rbind(plres, plres_1516)

# Ordering original dataset as per year of season
plres$year %>% as.character() -> plres$year
plres %>% arrange(desc(year)) -> plres
plres %>% mutate(year = factor(year, unique(year))) -> plres

# Renaming season year values to make it consistent
levels(plres$year)
levels(plres$year)[levels(plres$year)=="2005/6"] <- "2005/2006"
levels(plres$year)[levels(plres$year)=="2004/5"] <- "2004/2005"
levels(plres$year)[levels(plres$year)=="2003/4"] <- "2003/2004"
levels(plres$year)[levels(plres$year)=="2002/3"] <- "2002/2003"
levels(plres$year)

# Changing format for how year is displayed to make it consistent with other dataset
plres$year <- as.factor(gsub("/","-", as.character(plres$year))) 

# Writing files for analysis in Tableau
write.csv(plres,file="plresults_2000-2017_clean.csv", row.names = F)
write.csv(trans,file="pltransfers_2000-2017_clean.csv", row.names = F)

###### Data Exploration #####

# Exploring Transfers data set
trans %>%
  filter(Team_to=="Chelsea") %>%
  group_by(Season)%>%
  top_n(2) -> che





# Exploring results
plres %>%
  filter(Team=="Manchester City", as.character(year) >= "2010-2011") %>%
  group_by(year) %>%
  arrange(year) %>%
  top_n(4) -> plmc

# Exploring results
plres %>%
  filter(Team=="Manchester United", as.character(year) >= "2010-2011") %>%
  group_by(year) %>%
  arrange(year) %>%
  top_n(4) -> plmu

# Exploring results
plres %>%
  filter(Team=="Manchester United", as.character(year) >= "2010-2011") %>%
  group_by(year) %>%
  arrange(year) %>%
  top_n(4) -> plche

filteam <- 'Transfer_to'
trans %>% filter(League_to=="Premier League") %>% group_by(Team_to) %>% summarise(avg = mean(as.name(filteam)))
