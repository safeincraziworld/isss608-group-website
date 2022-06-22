########################## Packages in use ########################## 

packages=c('ggiraph', 'plotly', 'rmarkdown','psych','sf','tmap',
           'DT', 'patchwork','gglorenz','hrbrthemes','shinydashboard',
           'gganimate', 'tidyverse','ggthemes','reactable',
           'readxl', 'gifski', 'gapminder','quantmod','shinythemes',
           'treemap', 'treemapify','ggridges','zoo','reactablefmtr',
           'rPackedBar','lubridate','remotes')
for (p in packages){
  if(!require(p,character.only=T)){
    install.packages(p)
  }
  library(p,character.only = T)
}

library(shiny)

########################## Reading the files ########################## 

FinancialJournal<-read_csv("data/FinancialJournal.csv",show_col_types = FALSE)
Participants<-read_csv("data/Participants.csv",show_col_types = FALSE)
ParticipantsApartmentLocation<-read_csv("data/ParticipantsApartmentLocation.csv",show_col_types = FALSE)
buildings<-read_sf("data/buildings.csv", 
                   options = "GEOM_POSSIBLE_NAMES=location")



########################## Q1 ########################## 

########################## Q2 ########################## 

######## Data Cleaning

PartMonthYear=FinancialJournal%>%
  mutate(Year=as.numeric(year(timestamp)),
         Month=as.character(timestamp,"%b %y"),
         MonthNumeric=as.numeric(month(timestamp)))%>%
  group_by(participantId,Year,Month,MonthNumeric,category)%>%
  summarise(TotalAmount=sum(amount))

######## Getting the location of all participants

#ParticipantLog<-readRDS('data/logs_fread.rds')
#ParticipantsApartmentLocation<-ParticipantLog%>%
#  filter(currentMode=="AtHome")%>%
#  distinct(participantId,currentLocation)
#write_csv(ParticipantsApartmentLocation,"data/ParticipantsApartmentLocation.csv")


######## Data for Time Series
PartDailyExpense<-FinancialJournal%>%
  mutate(date=date(timestamp))%>%
  group_by(participantId,date,category)%>%
  summarise(TotalAmount=sum(amount))

PartDetailsDailyExpense<-left_join(x=PartDailyExpense,
                                   y=Participants,
                                   by=c("participantId"="participantId")
)




ParticipantsFinancialJournal <- inner_join(x= PartMonthYear,
                                           y= Participants, 
                                           by= 'participantId')


ParticipantsFinancialJournalExpense=ParticipantsFinancialJournal%>%
  filter(category!='Wage')%>%
  group_by(participantId,Year,Month)%>%
  summarise(Expense=sum(TotalAmount)*-1)

ParticipantsFinancialJournalEarnings=ParticipantsFinancialJournal%>%
  filter(category=='Wage')%>%
  group_by(participantId,Year,Month)%>%
  summarise(Earn=sum(TotalAmount))

ParticipantsEarningsVsExpense <- left_join(
  x= ParticipantsFinancialJournalExpense, 
  y= ParticipantsFinancialJournalEarnings, 
  by= c('participantId'='participantId',
        'Year'='Year',
        'Month'='Month'))


FinHealth=ParticipantsFinancialJournal%>%
  group_by(Year,Month,category)%>%
  summarise(TotalAmount=sum(TotalAmount))



Expenditure=FinHealth%>%
  filter(category!='Wage' & category!='RentAdjustment')%>%
  group_by(Year,Month)%>%
  summarise(Expense=sum(TotalAmount)*-1)

Earnings=FinHealth%>%
  filter(category=='Wage')%>%
  group_by(Year,Month)%>%
  summarise(Earn=sum(TotalAmount))


EarningsVsExpense <- inner_join(
  x= Expenditure, 
  y= Earnings, 
  by= c('Year'='Year','Month'='Month'))


ParticipantMonthlySavings<-left_join(
  x=ParticipantsEarningsVsExpense,
  y=Participants,
  by='participantId')%>%
  mutate(Savings=Earn-Expense)


ParticipantSavings<-
  left_join(x=ParticipantMonthlySavings%>%
              group_by(participantId)%>%
              summarise(TotalSavings=sum(Savings),
                        TotalEarning=sum(Earn),
                        TotalExpense=sum(Expense)),
            y=Participants,
            by='participantId')%>%
  left_join(.,ParticipantsApartmentLocation,
            by='participantId')

# Data for Heat Map

#StatusLogDetails<-PartDetailsDailyExpense%>%
#  mutate(Weekday=weekdays(date),Month=zoo::as.yearmon(date,"%Y %m"))%>%
#  filter(category=='Food' | category=='Recreation')

#Data for candlestick
#DailyCurrentModeTime<-PartDetailsDailyExpense%>%
#  mutate(Weekday=weekdays(date),Month=zoo::as.yearmon(date,"%Y %m"))


# Open=DailyCurrentModeTime%>%
#   group_by(Month,category)%>%
#   filter(day(date)==max(day(date)))%>%
#   group_by(Month,category)%>%
#   summarise(OpenTimeSpent=mean(TotalAmount))
# 
# Close=DailyCurrentModeTime%>%
#   group_by(Month,category)%>%
#   filter(day(date)==min(day(date)))%>%
#   group_by(Month,category)%>%
#   summarise(CloseTimeSpent=mean(TotalAmount))
# 
# High=DailyCurrentModeTime%>%
#   group_by(Month,category)%>%
#   summarise(HighTimespent=max(TotalAmount))
# 
# Low=DailyCurrentModeTime%>%
#   group_by(Month,category)%>%
#   summarise(LowTimespent=min(TotalAmount))
# 
# 
# CandlestickData=left_join(High, Low, by= c('Month'='Month',
#                                            'category'='category')) %>%
#   left_join(., Open, by=c(
#     'Month'='Month',
#     'category'='category'))%>% 
#   left_join(., Close, by=c(
#     'Month'='Month',
#     'category'='category'))

### Data for Sparklines

ParticipantMonthlyExpenseSpark<-ParticipantMonthlySavings%>%
  group_by(participantId)%>%
  summarise(Expense=list(Expense))

ParticipantMonthlyEarningSpark<-ParticipantMonthlySavings%>%
  group_by(participantId)%>%
  summarise(Earning=list(Earn))



######## Plots 


### Time series ###

PartDailyExpense<-FinancialJournal%>%
  mutate(date=date(timestamp))%>%
  group_by(participantId,date,category)%>%
  summarise(TotalAmount=sum(amount))%>%
  filter(category!="Wage")%>%
  group_by(participantId,date)%>%
  summarise(Expense=sum(TotalAmount))

PartDetailsDailyExpense<-left_join(x=PartDailyExpense,
                                   y=Participants,
                                   by=c("participantId"="participantId"))


S<-PartDetailsDailyExpense%>%
  group_by(date,interestGroup)%>%
  summarise(Expense=sum(Expense))

a<-ggplot(PartDetailsDailyExpense%>%
            group_by(date,interestGroup)%>%
            summarise(Expense=sum(Expense)))+
  geom_line(aes(x=date,y=log(Expense*-1),color=interestGroup))

### Coordinated Plot ###

PShighlighted <- highlight_key(ParticipantSavings%>%select(-TotalSavings))
Er <- ggplot(data=PShighlighted, 
             aes(x = TotalEarning,
                 y = joviality,
                 color=as.character(householdSize),
                 text=paste("Earning: ",round(TotalEarning,2),
                            "<br>Joviality: ",round(joviality,2),
                            "<br>Household Size: ",householdSize))) +
  geom_point(size=1)+
  xlab("Earning")+
  ylab("Joviality")

Ex <- ggplot(data=PShighlighted, 
             aes(x = TotalExpense,
                 y = joviality,
                 color=as.character(householdSize),
                 text=paste("Expense: ",round(TotalExpense,2),
                            "<br>Joviality: ",round(joviality,2),
                            "<br>Household Size: ",householdSize))) +
  geom_point(size=1)+
  ggtitle("Can money buy happiness?")+
  theme(legend.position="none")

#FB<-highlight(subplot(ggplotly(Er,tooltip = c("text")),ggplotly(Ex,tooltip = c("text"))),"plotly_selected")
#crosstalk::bscols(FB,DT::datatable(z,options = list(
#  columnDefs = list(list(className = 'dt-center', targets = 5)),
#  pageLength = 10,
#  autoWidth = TRUE,
#  scrollX = T,
#  lengthMenu = c(5, 10, 15, 20))),
#  widths = c(12,12))




########################## Q3 ########################## 


########################## UI ########################## 
ui <- navbarPage(
  title = "Financial Health of the city",
  fluid = TRUE,
  theme=shinytheme("flatly"),
  id = "navbarID",
  tabPanel("Introduction"),
  navbarMenu("Q1"),
  navbarMenu("Q2",
             
             tabPanel("Q2.1",
                      
                      fluidRow(
                        column(4,valueBoxOutput("value1")),
                        column(4,valueBoxOutput("value2")),
                        column(4,valueBoxOutput("value3"))
                      ),
                      
                      fluidRow(
                        column(6,reactableOutput("EarningReactableDashboard", 
                                                 width = "auto", 
                                                 height = "auto", 
                                                 inline = FALSE)),
                        column(6,reactableOutput("ExpenseReactableDashboard", 
                                                 width = "auto", 
                                                 height = "auto", 
                                                 inline = FALSE))
                      ),
                      fluidRow(
                        column(6,checkboxGroupInput("category", "Variables to show:",
                                                    c("Education" = "Education",
                                                      "Food" = "Food",
                                                      "Recreation" = "Recreation",
                                                      "Shelter" = "Shelter"),
                                                    selected = "Education")),
                        column(6,plotlyOutput("ExpensesTrellis"))
                        
                      ),
                      
                      fluidRow(
                        column(6,checkboxGroupInput("Months", "Variables to show:",
                                                    c("Nov 22" = "Nov 22",
                                                      "Dec 22" = "Dec 22",
                                                      "Jan 23" = "Jan 23",
                                                      "Feb 23" = "Feb 23"),
                                                    selected = "Nov 22")),
                        column(6,plotOutput("ExpensesEachMonth"))
                      ),
                      
                      
                      fluidRow(
                        column(6,tmapOutput("FinLocation")),
                        column(6,plotlyOutput("InterestGroups"))
                        
                        
                      ),
                      fluidRow(
                        column(6,plotOutput("HeatMap")),
                        column(6,plotlyOutput("CandleStickGraph"))
                        
                        
                      ),
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      #mainPanel(
                      #  uiOutput("CoordinatedPlot"),
                      #  width = "100%", height = "400px"
                      #),
                      
                      
                      
                      
             )
  ),
  navbarMenu("Q3",
             tabPanel("Principal Component Analysis"),
             tabPanel("Hierarchical Custering"),
             tabPanel("kmeans Clustering"),
             tabPanel("Multiple Linear Regression"))
)

#========================#
###### Shiny Server ######
#========================#

server <- function(input, output){
  
  NumberOfParicipants<-Participants%>%
    tally()
  #### tmap #### 
  output$FinLocation<-renderTmap({
    z<-ParticipantSavings%>%
      st_as_sf(wkt="currentLocation")
    
    tmap_mode("plot")
    tm_shape(buildings)+
      tm_polygons(col = "white",
                  border.col = "grey",
                  border.lwd = 1) +
      
      tm_shape(z)+
      
      tm_dots(col="blue",
              alpha=0.5,
              border.col="black",
              border.lwd=1,
              border.alpha=1,
              size="TotalEarning",
              palette="Set1")+
      tm_layout(main.title = "Where is the work?*",
                
                main.title.size = 2,
                legend.height = 0.3,
                legend.width = 0.3,
                legend.outside = FALSE,
                legend.position = c("right", "top"),
                frame = FALSE)+
      tm_compass()+
      tm_credits("*Observed from Mon-Fri at 8 am-8 pm",
                 position=c("left", "bottom"))
  })
  
  
  #### valueBox #### 
  output$value1 <- renderValueBox({
    valueBox(
      formatC(NumberOfParicipants$n, format="d", big.mark=',')
      ,'Total'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")  
  })
  output$value2 <- renderValueBox({ 
    valueBox(
      formatC("Mar 2022 - May 2023", format="d", big.mark=',')
      ,'Period'
      ,icon = icon("gbp",lib='glyphicon')
      ,color = "green")  
  })
  output$value3 <- renderValueBox({
    valueBox(
      formatC("8", format="d", big.mark=',')
      ,'Interest Groups'
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "yellow")   
  })
  
  
  
  waits <- reactiveValues()
  waits$resetindicator<-0   # used to change button labels
  #waits$data <- rep(0,52)   # initial waitlist profile
  #waits$count<-0            # week number
  #waits$vol<-0              # current week volume
  #waits$trackvol<-rep(0,20) # volume for last 20 weeks
  #waits$xdates<-rep(NA,20)  # x axis for volume graph
  #waits$past52<-0           # number of expired
  #waits$slices<-c(0,0,0)    # pie chart value
  
  
  output$resetbutton<-renderUI({
    if(waits$resetindicator==0){
      lbl<-"Stop"
    }else{
      lbl<-"Stop"
    }
    actionButton("reset",label=lbl)
  })
  
  # dynamic start button label
  output$startbutton<-renderUI({
    
    lbl<-"Start"
    actionButton("nextweek",label=lbl)
  })
  
  observeEvent(input$nextweek,{
    forward()
  })
  
  
  
  ### Trellis Display ###
  output$ExpensesTrellis <- renderPlotly({
    
    
    ggplotly((ggplot(FinHealth%>%
                       filter(category==input$category)%>%
                       group_by(Year,Month)%>%
                       mutate(percent=round(TotalAmount*100/sum(TotalAmount),2))%>%
                       ungroup(),
                     aes(x=factor(Month,
                                  levels=c("Mar 22","Apr 22","May 22","Jun 22","Jul 22","Aug 22","Sep 22",
                                           "Oct 22","Nov 22","Dec 22","Jan 23","Feb 23","Mar 23","Apr 23",
                                           "May 23")),
                         y=TotalAmount*-1,
                         fill=category,
                         shape=category,
                         text=paste("Total Amount: ", round(TotalAmount*-1,2),"<br>Category: ",category)))+
                geom_point()+
                scale_fill_brewer(palette="Set2")+
                xlab("Month")+
                ylab("Expenditure")+
                ggtitle("How have been monthly expenses been?")+
                coord_flip()+
                theme_bw()),
             tooltip = c("text"))
    
  })
  
  
  ### Time series ###
  output$ExpensesEachMonth <- renderPlot({
    
    
    ggplot(ParticipantMonthlySavings%>%filter(Month %in% input$Months)) +
      geom_density_ridges_gradient(aes(y = haveKids, 
                                       x = Expense,
                                       fill=stat(x)),
                                   scale = 1, 
                                   rel_min_height = 0.01,
                                   bandwidth=80)+
      scale_fill_viridis_c(name = "Amount", option = "C")+
      xlab("Amount")+
      ylab("Kids")+
      facet_grid(~Month)+
      ggtitle("Expenses during the On vs Off season")+
      labs(caption="Source: https://r-graph-gallery.com/ridgeline-plot.html")+
      theme(axis.title.y=element_text(angle=0))
    
  })
  
  
  output$EarningReactableDashboard <- renderReactable({
    
    
    
    reactable(
      ParticipantMonthlyEarningSpark,
      columns = list(
        participantId = colDef(maxWidth = 200),
        `Earning` = colDef(
          cell = react_sparkline(ParticipantMonthlyEarningSpark,
                                 highlight_points = highlight_points(
                                   min = "red", max = "blue"),
                                 labels = c("first", "last"))
        )
      )
    )
    
    
    
    
    
  })
  
  output$ExpenseReactableDashboard <- renderReactable({
    
    reactable(
      ParticipantMonthlyExpenseSpark,
      columns = list(
        participantId = colDef(maxWidth = 200),
        `Expense` = colDef(
          cell = react_sparkline(ParticipantMonthlyExpenseSpark,
                                 highlight_points = highlight_points(
                                   min = "red", max = "blue"),
                                 labels = c("first", "last"))
        )
      )
    )
    
  })
  
  
  
  #output$CoordinatedPlot <- renderUI({
  
  #crosstalk::bscols(FB,DT::datatable(z,options = list(
  #columnDefs = list(list(className = 'dt-center', targets = 5)),
  #pageLength = 10,
  #autoWidth = TRUE,
  #scrollX = T,
  #lengthMenu = c(5, 10, 15, 20))),
  #widths = c(12,12))
  #})
  
  output$CandleStickGraph<- renderPlotly({
    
    # candlestickfood<-CandlestickData%>%
    #   mutate(MonthUpdated=as.factor(Month)) %>%
    #   filter(category=="Food")%>% 
    #   plot_ly(x = ~MonthUpdated, type="candlestick",
    #           open=~OpenTimeSpent,close=~CloseTimeSpent,
    #           high=~HighTimespent,low=~LowTimespent) 
    # 
    # candlestickrecreation<-CandlestickData%>%
    #   mutate(MonthUpdated=as.factor(Month)) %>%
    #   filter(category=="Recreation")%>% 
    #   plot_ly(x = ~MonthUpdated, type="candlestick",
    #           open=~OpenTimeSpent,close=~CloseTimeSpent,
    #           high=~HighTimespent,low=~LowTimespent) 
    # 
    # 
    # fig <- subplot(candlestickfood, candlestickrecreation,nrows=2,shareX=TRUE) %>% 
    #   layout(title = 'Time spent at home',annotations = list( 
    #     list( 
    #       x = 0.2,  
    #       y = 1.0,  
    #       text = "Participant Id: 320",  
    #       xref = "paper",  
    #       yref = "paper",  
    #       xanchor = "center",  
    #       yanchor = "bottom",  
    #       showarrow = FALSE 
    #     ),  
    #     list( 
    #       x = 0.2,  
    #       y = 0.5,  
    #       text = "Participant Id: 113",  
    #       xref = "paper",  
    #       yref = "paper",  
    #       xanchor = "center",  
    #       yanchor = "bottom",  
    #       showarrow = FALSE 
    #     )))
    
  })
  
  output$InterestGroups<- renderPlotly({
    
    ggplotly(a)
    
    
    
    
    
  })
  
  
  
  output$HeatMap<- renderPlot({
    
    #ggplot(StatusLogDetails%>%group_by(participantId,Weekday,category)%>%
    #         summarise(Expense=sum(TotalAmount)), 
    #       aes(x=factor(Weekday,levels=c("Monday","Tuesday",
    #                                     "Wednesday","Thursday",
    #                                     "Friday","Saturday","Sunday")), 
    #           category, 
    #           fill = Expense)) + 
    #  geom_tile(aes(text=paste("Total Time: ",Expense)),color = "white", 
    #            size = 0.1,lwd = 1.5,linetype = 1) + 
    #  coord_equal() +
    #  scale_fill_gradient2(low = "#075AFF",
    #                       mid = "#FFFFCC",
    #                       high = "#FF0000")+
    #  labs(x = NULL, 
    #       y = NULL, 
    #       title = "Is it all work and no play?")+
    #  theme_ipsum()+
    #  guides(fill = guide_colourbar(barwidth = 0.5,
    #                                barheight = 5))+
    #  theme(axis.ticks = element_blank(),
    #        axis.text.x = element_text(size = 7,angle=90),
    #        axis.text.y = element_text(size = 7),
    #        plot.title = element_text(hjust = 0.5),
    #        legend.title = element_text(size = 8),
    #        legend.text = element_text(size = 6))
    
    
  })
  
}

shinyApp(ui = ui, server = server)
