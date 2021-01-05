## Name : Dhir Nilesh Chandan
## Student ID: 29853583
## File: server file for shiny Application

# load the required packages
library(shiny)
library(plyr)
library(dplyr)
library(plotly)

trans <- read.csv("top250-00-19.csv", fileEncoding = "UTF-8", strip.white = T)

plres <- read.csv("result.csv", strip.white = T)

# Filtering transfers between 2000/2001 season and 2016/2017 season
trans$Season %>% as.character() -> trans$Season
trans %>% filter(Season <= "2016-2017") -> trans
trans$Season %>% as.factor() -> trans$Season

#  Keeping only the columns required for analysis
trans %>% select(Name,Position,Age,Team_from,League_from,Team_to,League_to,Season,Transfer_fee) -> trans

# Filtering data for Premier League only.
trans %>% 
  filter(League_from=='Premier League' | League_to=='Premier League' | League_from==" England" | League_to==" England") -> trans

# Filtering data from 2000/2001 season till 2016/2017 season for PL results
plres$year %>% as.character() -> plres$year
plres %>% filter(year >= "2000/2001", year <= "2016/2017") -> plres
plres$year %>% as.factor() -> plres$year

#  Keeping only the columns required for analysis
plres %>% select(year,Team,Pos,Pts, W, D, L, F, A, GD) -> plres

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

# # Writing files for analysis in Tableau
# write.csv(plres,file="plresults_2000-2017_clean.csv", row.names = F)
# write.csv(trans,file="pltransfers_2000-2017_clean.csv", row.names = F)

trans %>% 
  filter(League_to=="Premier League") -> pltrans
pltrans$Team_to<- pltrans$Team_to %>% as.character() %>% as.factor()

pltrans$Team_to <- revalue(pltrans$Team_to,
                           c("Leeds"="Leeds United","Spurs"="Tottenham Hotspurs","Man Utd"="Manchester United","Newcastle"="Newcastle United","Ipswich"="Ipswich Town","Charlton"="Charlton Athletic","Derby"="Derby County","Man City"="Manchester City","Leicester"="Leicester City","West Ham"="West Ham United","Blackburn"="Blackburn Rovers","West Brom"="West Bromwich Albion","Birmingham"="Birmingham City","Wolves"="Wolverhampton Wanderers","Norwich"="Norwich City","Bolton"="Bolton Wanderers","Sheffield Utd."="Sheffield United","QPR"="Queens Park Rangers")
)


pltrans %>% 
  group_by(Team_to) %>% 
  summarise(Total=sum(Transfer_fee)) %>% 
  arrange(desc(Total)) %>% 
  top_n(10) %>% as.data.frame() -> top10teams

# merging transfers and results and aggregating
pltrans %>% 
  group_by(Team_to, Season) %>%
  summarise(Transfer_fee=sum(Transfer_fee)) %>% as.data.frame() %>% 
  merge.data.frame(plres[,c("year","Team","Pos","Pts","GD")], by.x=c("Team_to", "Season"), by.y=c("Team", "year"), all.x = TRUE) %>% filter(!is.na(Pos)) -> plrestrans


teamnames <- as.character(unique(trans[(trans$League_to == "Premier League"),"Team_to"]))
seasons <- as.character(unique(trans[,"Season"]))



server <- shinyServer(function(input,output){
  
  output$bar_plot <- renderPlotly({
    plot_ly(data = top10teams, x=~as.character(top10teams$Team_to), y=~Total, 
            type='bar') %>% 
      layout(xaxis=list(title="Team Names"),
             yaxis=list(title="Total Spending"))
  })
  
  output$bar_motion_plot <- renderPlotly({
    # bar motion chart all teams
    pltrans %>%
      group_by(Team_to, Season) %>% 
      summarise(TransferTotal = sum(Transfer_fee)) %>% as.data.frame() %>% 
      plot_ly(
        x = ~as.character(Team_to),
        y = ~TransferTotal,
        frame = ~Season,
        type = 'bar',
        showlegend = T
      ) %>% 
      layout(xaxis = list(title="",ticks = "outside"),
             yaxis = list(title="Total Transfer during Season"))
    
  })
  output$top2caption <- renderText({
    paste("Top 2 spending seasons for ",input$TeamName)
  })
  
  filterteamdf <- reactive({
    team_filter <-input$TeamName
    pltrans %>%
      group_by(Team_to, Season) %>% 
      summarise(TransferTotal = sum(Transfer_fee)) %>% as.data.frame() %>% 
      filter(Team_to==team_filter) -> tempdf
    tempdf
  })  
  
  output$overseasons <- renderPlotly({
    filterteamdf() %>% plot_ly(x = ~Season, y = ~TransferTotal, type = 'scatter', mode='line') %>% 
      layout(xaxis=list(title="Season"),
             yaxis=list(title="Transfer Spending"))  
  })
  
  topseasonsdf <- reactive({
    filterteamdf() %>% arrange(desc(TransferTotal)) %>% 
      top_n(2) -> tempdf
    tempdf
  })
  
  tops1players <- reactive({
    team_filter <-input$TeamName
    s1<-as.character(topseasonsdf()$Season[1])
      pltrans %>% 
        filter(Team_to==team_filter, Season==s1) %>% 
        arrange(desc(Transfer_fee)) %>% 
        top_n(5) %>% 
        select(Name,Position,Transfer_fee) -> tempdf
      tempdf
  })
  
  tops2players <- reactive({
    team_filter <-input$TeamName
    s2<-as.character(topseasonsdf()$Season[2])
      pltrans %>% 
        filter(Team_to==team_filter, Season==s2) %>% 
        arrange(desc(Transfer_fee)) %>% 
        top_n(5) %>% 
        select(Name,Position,Transfer_fee) -> tempdf
      tempdf
  })
  
  output$top1season <- renderPlotly({
    tops1players() %>% 
    plot_ly(x=~as.character(Name),y=~Transfer_fee, type="bar") %>% 
      layout(title=paste("Season: ",as.character(topseasonsdf()$Season[1])),
             xaxis=list(title="Player Names"),
             yaxis=list(title="Transfer Fees"))
  })
  
  output$top1position <- renderPlotly({
    tops1players() %>% 
    plot_ly(labels = ~Position, values = ~Transfer_fee, type = 'pie') %>%
      layout(title = '',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$top2season <- renderPlotly({
    tops2players() %>% 
    plot_ly(x=~as.character(Name),y=~Transfer_fee, type="bar") %>% 
      layout(title=paste("Season: ",as.character(topseasonsdf()$Season[2])),
             xaxis=list(title="Player Names"),
             yaxis=list(title="Total Spending"))
  })
  
  output$top2position <- renderPlotly({
    tops2players() %>% 
    plot_ly(labels = ~Position, values = ~Transfer_fee, type = 'pie') %>%
      layout(title = '',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
    

   output$top1seasoncaption <- renderText({
    paste(" Highest spending season by ",input$TeamName,"")
  })
  output$top1positioncaption <- renderText({
    paste("Positions spent on the most in this season")
  })
  output$top2seasoncaption <- renderText({
    paste(" 2nd Highest spending season by ",input$TeamName,"")
  })
  
   output$resultplot1<- renderPlotly({
    if(input$TeamNameResult == "All Teams"){
      fit_data <- plrestrans
    }
    else{
      team_filter = input$TeamNameResult
      plrestrans %>% filter(Team_to==team_filter) -> fit_data
    }
    # Transfer vs Pts fit
    fit <- lm(Transfer_fee ~ Pts, data = fit_data)
    summary(fit)
    
    fit_data %>% 
      plot_ly(x = ~Pts) %>% 
      add_markers(y = ~Transfer_fee) %>% 
      add_lines(x = ~Pts, y = fitted(fit)) %>% 
      layout(showlegend = F,xaxis=list(title="Points"),
             yaxis=list(title="Transfer Fees"))
  })
  output$resultplot2<- renderPlotly({
    if(input$TeamNameResult == "All Teams"){
      fit_data <- plrestrans
    }
    else{
      team_filter = input$TeamNameResult
      plrestrans %>% filter(Team_to==team_filter) -> fit_data
    }
    # Transfer vs GD fit
    fit <- lm(Transfer_fee ~ GD, data = fit_data)
    summary(fit)
    
    fit_data %>% 
      plot_ly(x = ~GD) %>% 
      add_markers(y = ~Transfer_fee) %>% 
      add_lines(x = ~GD, y = fitted(fit)) %>% 
      layout(showlegend = F,xaxis=list(title="Goal Difference"),
           yaxis=list(title="Transfer Fees"))
  })
  
  output$resultplot3<- renderPlotly({
    if(input$TeamNameResult == "All Teams"){
      plrestrans %>% 
        plot_ly(x = ~Transfer_fee, color = ~as.factor(Pos), type='box' ) %>% 
        layout(xaxis=list(title="Transfer Fees"),
               yaxis=list(title="Position"))
    }
  })
  
})
