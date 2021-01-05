## Name : Dhir Nilesh Chandan
## Student ID: 29853583
## File: ui file for shiny Application


library(shiny)
library(plotly)
library(plyr)
library(dplyr)


trans <- read.csv("top250-00-19.csv", fileEncoding = "UTF-8", strip.white = T)
trans %>% select(Name,Position,Age,Team_from,League_from,Team_to,League_to,Season,Transfer_fee) -> trans
trans %>% 
  filter(League_to=="Premier League") -> pltrans
pltrans$Team_to<- pltrans$Team_to %>% as.character() %>% as.factor()
pltrans$Team_to <- revalue(pltrans$Team_to,
                           c("Leeds"="Leeds United","Spurs"="Tottenham Hotspurs","Man Utd"="Manchester United","Newcastle"="Newcastle United","Ipswich"="Ipswich Town","Charlton"="Charlton Athletic","Derby"="Derby County","Man City"="Manchester City","Leicester"="Leicester City","West Ham"="West Ham United","Blackburn"="Blackburn Rovers","West Brom"="West Bromwich Albion","Birmingham"="Birmingham City","Wolves"="Wolverhampton Wanderers","Norwich"="Norwich City","Bolton"="Bolton Wanderers","Sheffield Utd."="Sheffield United","QPR"="Queens Park Rangers")
                           )


teamnames <- as.character(unique(pltrans[,"Team_to"]))
teamnames_all<-c("All Teams",teamnames)



ui <- shinyUI(fluidPage(
  headerPanel("English Premier League Transfers Analysis"),
  tabsetPanel(
    tabPanel("Transfers", fluid = TRUE,
             fluidRow(
               column(10,
                      h2("Transfer Market Analysis over the seasons 2000-2017 for English Premier League")
               )
             ),
             fluidRow(
               column(4,
                      h3("Top 10 teams by Transfer Spending"),
                      plotlyOutput("bar_plot")
               ),
               column(8, 
                      h3("Transfer Spending over the seasons 2000-2017"),
                      plotlyOutput("bar_motion_plot")
               )
             ),
             fluidRow(
               column(10,
                      h3("Team Specific transfer analysis")
               )
             ),
             fluidRow(
               column(4,
                      selectInput("TeamName", label = h5("Select Team:"),
                                  choices = teamnames, selected = "Chelsea")
               ),
               column(4,
                      h4(textOutput("top2caption"))
               )
               # ,column(2,
               #        selectInput("season", label = h5("Select Season:"),
               #                    choices = seasons, selected = FALSE)
               #        )
             ),
             fluidRow(
               column(4,
                      h5("Spending over the seasons 2000-2017"),
                      plotlyOutput("overseasons")
               ),
               column(4,
                      h5(textOutput("top1seasoncaption")),
                      plotlyOutput("top1season")
               ),
               column(4,
                      h5("Positions spent on the most in this season"),
                      plotlyOutput("top1position")
               )
             ),
             fluidRow(
               column(4, offset = 4,
                      h5(textOutput("top2seasoncaption")),
                      plotlyOutput("top2season")
               ),
               column(4,
                      h5("Positions spent on the most in this season"),
                      plotlyOutput("top2position")
               )
             )
    ),
    #Tab 2
    tabPanel("Results", fluid = TRUE,
             fluidRow(
               column(6,
                      h3("Analysing Effect of Transfer spending on Results by model fitting")
               )
             ),
             fluidRow(
               column(3,
                      selectInput("TeamNameResult", "Choose Team: ",
                                  choices = teamnames_all, selected = "All Teams")
               )
             ),
             fluidRow(
               column(4,
                      plotlyOutput("resultplot1")
               ),
               column(4,
                      plotlyOutput("resultplot2")
               ),
               column(4,
                      plotlyOutput("resultplot3")
               )
             )
    )
  )))
