########################## Packages in use ########################## 

packages=c('ggiraph', 'plotly', 'rmarkdown','psych','sf','tmap',
           'DT', 'patchwork','gglorenz','hrbrthemes','shinydashboard',
           'gganimate', 'tidyverse','ggthemes','reactable',
           'readxl', 'gifski', 'gapminder','quantmod','shinythemes',
           'treemap', 'treemapify','ggridges','zoo','reactablefmtr','crosstalk',
           'rPackedBar','lubridate','remotes','ggplot2','dplyr','ggstatsplot',
           'lubridate','shiny','tools','writexl')
for (p in packages){
  if(!require(p,character.only=T)){
    install.packages(p)
  }
  library(p,character.only = T)
}

library(shiny)

########################## Reading the files ########################## 

# FinancialJournal<-read_csv("data/FinancialJournal.csv",show_col_types = FALSE)
Participants<-read_csv("data/Participants.csv",show_col_types = FALSE)
ParticipantsApartmentLocation<-read_csv("data/ParticipantsApartmentLocation.csv",show_col_types = FALSE)
buildings<-read_sf("data/buildings.csv", 
                   options = "GEOM_POSSIBLE_NAMES=location")

################################# Reading rds files ##########################################

#### Q2 ####

ParticipantSavings<-readRDS("data/Q2/ParticipantSavings.rds")
FinHealth<-readRDS("data/Q2/FinHealth.rds")
ParticipantMonthlySavings<-readRDS("data/Q2/ParticipantMonthlySavings.rds")
ParticipantMonthlySpark<-readRDS("data/Q2/ParticipantMonthlySpark.rds")

InterestGroupGraph<-readRDS("data/Q2/InterestGroupGraph.rds")
StatusLogDetails<-readRDS("data/Q2/StatusLogDetails.rds")


#### Q3 ####
employers <- readRDS('data/Q3/employers.rds')
empWorkinMultiplePlaces <- readRDS('data/Q3/empWorkinMultiplePlaces.rds')
empWorkinMultiplePlaces_latest <- readRDS('data/Q3/empWorkinMultiplePlaces_latest.rds')
empWorkinMultiplePlaces_latest_groupby <- readRDS('data/Q3/empWorkinMultiplePlaces_latest_groupby.rds')
empWorkinMultiplePlaces_previous <- readRDS('data/Q3/empWorkinMultiplePlaces_previous.rds')
empWorkinMultiplePlaces_previous_groupby <- readRDS('data/Q3/empWorkinMultiplePlaces_previous_groupby.rds')
jobs <- readRDS('data/Q3/jobs.rds')
participants <- readRDS('data/Q3/participants.rds')
pay_hires <- readRDS('data/Q3/pay_hires.rds')
prevEmp_sf <- readRDS('data/Q3/prevEmp_sf.rds')
switchEmployeesAllDetails <- readRDS('data/Q3/switchEmployeesAllDetails.rds')
transitionEmpDetails <- readRDS('data/Q3/transitionEmpDetails.rds')
transitionTable <- readRDS('data/Q3/transitionTable.rds')
work <- readRDS('data/Q3/work.rds')
work_home <- readRDS('data/Q3/work_home.rds')
work_home_filt <- readRDS('data/Q3/work_home_filt.rds')
workinmoreplaces <- readRDS('data/Q3/workinmoreplaces.rds')
logs_selected<-readRDS('data/logs_selected.rds')

#################################### Other Data Wrangling ######################################


########################## Q1 ########################## 

########################## Q2 ########################## 

# ######## Data Cleaning
# 
# PartMonthYear=FinancialJournal%>%
#   mutate(Year=as.numeric(year(timestamp)),
#          Month=as.character(timestamp,"%b %y"),
#          MonthNumeric=as.numeric(month(timestamp)))%>%
#   group_by(participantId,Year,Month,MonthNumeric,category)%>%
#   summarise(TotalAmount=sum(amount))
# 
# ######## Getting the location of all participants
# 
# #ParticipantLog<-readRDS('data/logs_fread.rds')
# #ParticipantsApartmentLocation<-ParticipantLog%>%
# #  filter(currentMode=="AtHome")%>%
# #  distinct(participantId,currentLocation)
# #write_csv(ParticipantsApartmentLocation,"data/ParticipantsApartmentLocation.csv")
# 
# 
# ######## Data for Time Series
# PartDailyExpense<-FinancialJournal%>%
#   mutate(date=date(timestamp))%>%
#   group_by(participantId,date,category)%>%
#   summarise(TotalAmount=sum(amount))
# 
# PartDetailsDailyExpense<-left_join(x=PartDailyExpense,
#                                    y=Participants,
#                                    by=c("participantId"="participantId")
# )
# 
# 
# 
# 
# ParticipantsFinancialJournal <- inner_join(x= PartMonthYear,
#                                            y= Participants, 
#                                            by= 'participantId')
# 
# 
# ParticipantsFinancialJournalExpense=ParticipantsFinancialJournal%>%
#   filter(category!='Wage')%>%
#   group_by(participantId,Year,Month)%>%
#   summarise(Expense=sum(TotalAmount)*-1)
# 
# ParticipantsFinancialJournalEarnings=ParticipantsFinancialJournal%>%
#   filter(category=='Wage')%>%
#   group_by(participantId,Year,Month)%>%
#   summarise(Earn=sum(TotalAmount))
# 
# ParticipantsEarningsVsExpense <- left_join(
#   x= ParticipantsFinancialJournalExpense, 
#   y= ParticipantsFinancialJournalEarnings, 
#   by= c('participantId'='participantId',
#         'Year'='Year',
#         'Month'='Month'))
# 
# 
# FinHealth=ParticipantsFinancialJournal%>%
#   group_by(Year,Month,category)%>%
#   summarise(TotalAmount=sum(TotalAmount))
# 
# 
# 
# Expenditure=FinHealth%>%
#   filter(category!='Wage' & category!='RentAdjustment')%>%
#   group_by(Year,Month)%>%
#   summarise(Expense=sum(TotalAmount)*-1)
# 
# Earnings=FinHealth%>%
#   filter(category=='Wage')%>%
#   group_by(Year,Month)%>%
#   summarise(Earn=sum(TotalAmount))
# 
# 
# EarningsVsExpense <- inner_join(
#   x= Expenditure, 
#   y= Earnings, 
#   by= c('Year'='Year','Month'='Month'))
# 
# 
# ParticipantMonthlySavings<-left_join(
#   x=ParticipantsEarningsVsExpense,
#   y=Participants,
#   by='participantId')%>%
#   mutate(Savings=Earn-Expense)
# 
# 
# ParticipantSavings<-
#   left_join(x=ParticipantMonthlySavings%>%
#               group_by(participantId)%>%
#               summarise(TotalSavings=sum(Savings),
#                         TotalEarning=sum(Earn),
#                         TotalExpense=sum(Expense)),
#             y=Participants,
#             by='participantId')%>%
#   left_join(.,ParticipantsApartmentLocation,
#             by='participantId')
# 
# # Data for Heat Map
# 
# #StatusLogDetails<-PartDetailsDailyExpense%>%
# #  mutate(Weekday=weekdays(date),Month=zoo::as.yearmon(date,"%Y %m"))%>%
# #  filter(category=='Food' | category=='Recreation')
# 
# #Data for candlestick
# #DailyCurrentModeTime<-PartDetailsDailyExpense%>%
# #  mutate(Weekday=weekdays(date),Month=zoo::as.yearmon(date,"%Y %m"))
# 
# 
# # Open=DailyCurrentModeTime%>%
# #   group_by(Month,category)%>%
# #   filter(day(date)==max(day(date)))%>%
# #   group_by(Month,category)%>%
# #   summarise(OpenTimeSpent=mean(TotalAmount))
# # 
# # Close=DailyCurrentModeTime%>%
# #   group_by(Month,category)%>%
# #   filter(day(date)==min(day(date)))%>%
# #   group_by(Month,category)%>%
# #   summarise(CloseTimeSpent=mean(TotalAmount))
# # 
# # High=DailyCurrentModeTime%>%
# #   group_by(Month,category)%>%
# #   summarise(HighTimespent=max(TotalAmount))
# # 
# # Low=DailyCurrentModeTime%>%
# #   group_by(Month,category)%>%
# #   summarise(LowTimespent=min(TotalAmount))
# # 
# # 
# # CandlestickData=left_join(High, Low, by= c('Month'='Month',
# #                                            'category'='category')) %>%
# #   left_join(., Open, by=c(
# #     'Month'='Month',
# #     'category'='category'))%>% 
# #   left_join(., Close, by=c(
# #     'Month'='Month',
# #     'category'='category'))
# 
# ### Data for Sparklines
# 
# ParticipantMonthlySpark<-ParticipantMonthlySavings%>%
#   group_by(participantId)%>%
#   summarise(Expense=list(Expense),
#             Earning=list(Earn))%>%
#   left_join(.,Participants,
#             by=c("participantId"="participantId"))
# 
# #ParticipantMonthlyEarningSpark<-ParticipantMonthlySavings%>%
# #  group_by(participantId)%>%
# #  summarise(Earning=list(Earn))
# 
# 
# 
# ######## Plots 
# 
# 
# ### Time series ###
# 
# PartDailyExpense<-FinancialJournal%>%
#   mutate(date=date(timestamp))%>%
#   group_by(participantId,date,category)%>%
#   summarise(TotalAmount=sum(amount))%>%
#   filter(category!="Wage")%>%
#   group_by(participantId,date)%>%
#   summarise(Expense=sum(TotalAmount))
# 
# PartDetailsDailyExpense<-left_join(x=PartDailyExpense,
#                                    y=Participants,
#                                    by=c("participantId"="participantId"))
# 
# 
# S<-PartDetailsDailyExpense%>%
#   group_by(date,interestGroup)%>%
#   summarise(Expense=sum(Expense))
# 
# 
# InterestGroupGraph<-PartDetailsDailyExpense%>%
#   group_by(date,interestGroup)%>%
#   summarise(Expense=sum(Expense))


### Coordinated Plot ###

# PShighlighted <- highlight_key(ParticipantSavings%>%select(-TotalSavings))
# Er <- ggplot(data=PShighlighted, 
#              aes(x = TotalEarning,
#                  y = joviality,
#                  color=as.character(householdSize),
#                  text=paste("Earning: ",round(TotalEarning,2),
#                             "<br>Joviality: ",round(joviality,2),
#                             "<br>Household Size: ",householdSize))) +
#   geom_point(size=1)+
#   xlab("Earning")+
#   ylab("Joviality")
# 
# Ex <- ggplot(data=PShighlighted, 
#              aes(x = TotalExpense,
#                  y = joviality,
#                  color=as.character(householdSize),
#                  text=paste("Expense: ",round(TotalExpense,2),
#                             "<br>Joviality: ",round(joviality,2),
#                             "<br>Household Size: ",householdSize))) +
#   geom_point(size=1)+
#   ggtitle("Can money buy happiness?")+
#   theme(legend.position="none")

#FB<-highlight(subplot(ggplotly(Er,tooltip = c("text")),ggplotly(Ex,tooltip = c("text"))),"plotly_selected")
#crosstalk::bscols(FB,DT::datatable(z,options = list(
#  columnDefs = list(list(className = 'dt-center', targets = 5)),
#  pageLength = 10,
#  autoWidth = TRUE,
#  scrollX = T,
#  lengthMenu = c(5, 10, 15, 20))),
#  widths = c(12,12))




########################## Q3 ########################## 

########## before and after route map ################

hex <- st_make_grid(buildings, 
                    cellsize=100, 
                    square=FALSE) %>%
  st_sf() %>%
  rowid_to_column('hex_id')
points_in_hex <- sf::st_join(logs_selected, 
                         hex, 
                         join=st_within)

points_in_hex <- sf::st_join(logs_selected, 
                         hex, 
                         join=st_within) %>%
  st_set_geometry(NULL) %>%
  dplyr::count(name='pointCount', hex_id)

hex_combined <- hex %>%
  left_join(points_in_hex, 
            by = 'hex_id') %>%
  replace(is.na(.), 0)

p <- tm_shape(hex_combined %>%
                filter(pointCount > 0))+
  tm_fill("pointCount",
          n = 8,
          style = "quantile") +
  tm_borders(alpha = 0.1)

logs_path <- logs_selected %>%
  group_by(participantId, day) %>%
  dplyr::summarize(m = mean(Timestamp), 
                   do_union=FALSE) %>%
  mutate(date = as_date(m)) %>%
  st_cast("LINESTRING")
logs_path_PrevJob <-logs_path %>%
  filter(participantId %in% 
           empWorkinMultiplePlaces_previous$participantId &
           date %in% 
           empWorkinMultiplePlaces_previous$StartDate) %>%
  slice(which.min(date)) %>%
  dplyr::select(participantId,date,currentLocation)

partid = c(logs_path_PrevJob$participantId)

logs_path_RecJob <-logs_path %>%
  filter(participantId %in% 
           empWorkinMultiplePlaces_latest$participantId &
           date %in% 
           empWorkinMultiplePlaces_latest$StartDate) %>%
  slice(which.max(date)) %>%
  dplyr::select(participantId,date,currentLocation)

################## treemap ##############

no.ofjobs <- jobs %>% 
  group_by(employerId) %>%
  summarise(no.ofjobs = n(),
            totalWage = sum(hourlyRate),
            avgWage = mean(hourlyRate)) %>%
  dplyr::rename('Average Wage' = 'avgWage') %>%
  mutate(label = paste(no.ofjobs, 'Employees'))

no.ofjobs_table <- jobs %>% 
  group_by(employerId) %>%
  summarise(no.ofjobs = n(),
            totalWage = sum(hourlyRate),
            avgWage = mean(hourlyRate),
            eduLevel = educationRequirement) %>%
  dplyr::rename('Average Wage' = 'avgWage') %>%
  mutate(label = paste(no.ofjobs, 'Employees')) %>%
  dplyr::select(employerId, no.ofjobs, `Average Wage`, eduLevel)


################### difference in wage bar plot ###################

### Switch employee all details

transitionTableWithPrevPay <- left_join(x=transitionTable, y= pay_hires, by= c("previous_employer"="employerId")) %>%
  dplyr::select(participantId, previous_employer, employeepay) %>%
  dplyr::rename("prevPay" = "employeepay")

transitionTableWithRecentPay <- left_join(x=transitionTable, y= pay_hires, by= c("recent_employer"="employerId")) %>%
  dplyr::select(participantId, recent_employer, employeepay) %>%
  dplyr::rename("recentPay" = "employeepay")

transitionTablewithPay <- inner_join(x=transitionTableWithPrevPay,
                                     y=transitionTableWithRecentPay,
                                     by = "participantId")

switchEmployeesAllDetails <- switchEmployeesAllDetails %>%
  inner_join(x=transitionTablewithPay,
             y=transitionEmpDetails,
             by="participantId") %>%
  mutate(payDiff = recentPay - prevPay)

switchEmployeesAllDetails <- switchEmployeesAllDetails %>%
  mutate(payStatus = case_when(payDiff > 0 ~ "Pay Increase",
                               payDiff < 0 ~ "Pay Decrease",
                               TRUE ~ "Same Pay"))


########################## UI ########################## 
ui <- navbarPage(
  title = "Financial Health of the city",
  fluid = TRUE,
  theme=shinytheme("united"),
  id = "navbarID",
  tabPanel("Introduction"),
  navbarMenu("Q1"),
  navbarMenu("Q2",
             
             tabPanel("Q2.1",
                      
                      fluidRow(
                        column(3,div(valueBoxOutput("value1"),style="color:white"),style="background-color:navy;width=100px"),
                        
                        column(6,div(valueBoxOutput("value2"),style="color:white"),style="background-color:purple;"),
                        
                        column(3,div(valueBoxOutput("value3"),style="color:white"),style="background-color:green;")
                      ),
                      
                      fluidRow(
                        column(3,selectInput(inputId = "HaveKidsDashboard", 
                                             label =   "Have Kids",
                                             choices =  c("All" = "All",
                                                          "Yes" = "TRUE",
                                                          "No" = "FALSE"),
                                             selected = "All"
                        )),
                        
                        column(3,selectInput(inputId = "HouseHoldSizeDashboard", 
                                             label =   "HouseHold Size",
                                             choices =  c("1" = "1",
                                                          "2" = "2",
                                                          "3"="3"),
                                             multiple = TRUE,
                                             selected = c("1",
                                                          "2",
                                                          "3")
                        )),
                        column(4,selectInput(inputId = "EducationDashboard", 
                                             label =   "Education Qualification",
                                             choices =  c("Low"="Low",
                                               "High School or College"="HighSchoolOrCollege",
                                                          "Bachelors"="Bachelors",
                                               "Graduate"="Graduate"),
                                             multiple = TRUE,
                                             selected = c("Low",
                                                          "HighSchoolOrCollege",
                                                          "Bachelors",
                                                          "Graduate")
                        ))
                        
                               
                               
                        
                        
                      ),
                      
                      fluidRow(
                        column(3,sliderInput("age", "Age:",
                                           min = min(participants$age), max = max(participants$age),
                                           value = c(min(participants$age),max(participants$age))))),
                      
                      fluidRow(
                        column(12,reactableOutput("EarningReactableDashboard", 
                                                 width = "auto", 
                                                 height = "auto", 
                                                 inline = FALSE))
                      ),
                      fluidRow(
                        column(3,checkboxGroupInput("category", "Variables to show:",
                                                    c("Education" = "Education",
                                                      "Food" = "Food",
                                                      "Recreation" = "Recreation",
                                                      "Shelter" = "Shelter"),
                                                    selected = "Education")),
                        column(9,plotlyOutput("ExpensesTrellis"))
                        
                      ),
                      
                      fluidRow(
                        column(3,checkboxGroupInput("Months", "Variables to show:",
                                                    c("Nov 22" = "Nov 22",
                                                      "Dec 22" = "Dec 22",
                                                      "Jan 23" = "Jan 23",
                                                      "Feb 23" = "Feb 23"),
                                                    selected = "Nov 22")),
                        column(9,plotOutput("ExpensesEachMonth"))
                      ),
                      
                      
                      fluidRow(
                        column(12,plotOutput("FinLocation")),
                        
                        
                        
                      ),
                      fluidRow(
                        column(6,plotOutput("HeatMap")),
                        column(6,plotlyOutput("CandleStickGraph"))
                        
                        
                      ),

                      #mainPanel(
                      #  uiOutput("CoordinatedPlot"),
                      #  width = "100%", height = "400px"
                      #),
                      
                      
                      
                      
             ),
             
             tabPanel("Q2.3",
                      
                      fluidRow(
                        column(3,
                               checkboxGroupInput("InterestGroup", "Interest Group",
                                                    c("A" = "A",
                                                      "B" = "B",
                                                      "C" = "C",
                                                      "D" = "D"),
                                                    selected = "A")),
                        
                        
                        column(9,plotlyOutput("InterestGroups")))
                      
                      
             )
             
  ),
             
  navbarMenu("Employment & Turnover",
             tabPanel("Turnover Analysis",
                      fluidPage(
                      titlePanel("What is the impact of job switch among participants ?"),
                      fluidRow(
                        column(
                          width = 12,
                          height = 100,
                          tabsetPanel(
                            tabPanel("One Sample Test ",
                                     box(
                                       width = 4,
                                       height = 60,
                                       selectInput(inputId = "variable_selection", 
                                                   label =   "Type of test:",
                                                   choices =  c("Parametric" = "parametric",
                                                                "Non Parametric" = "nonparametric",
                                                                "Robust" = "robust"),
                                                   selected = "Parametric"
                                       )),
                                     box(plotOutput("testPlot")),
                                     box(
                                       width = 8,
                                       height = 120
                                       
                                       
                                     )
                            ),
                            tabPanel("Job Route",
                                     box(
                                       width = 20,
                                       height = 100,
                                       selectInput(inputId = "participants",
                                                   label = "Select Participant Id",
                                                   choices = partid,
                                                   selected = c(partid[5]))
                                       
                                     ),
                                     fluidRow(
                                       box("Commute route from home to work before job change",
                                           plotOutput(outputId = "befRoute",
                                                      width = 500,
                                                      height = 500),
                                       ),
                                       box("Commute route from home to work after job change",
                                           plotOutput(outputId  = "aftRoute",
                                                      width = 500,
                                                      height = 500)
                                       )
                                     )
                                     
                                     
                            ),
                            tabPanel("Change of Wage",
                                     box(
                                       width = 4,
                                       height = 60,
                                       checkboxGroupInput(inputId = "groupbyCategory", 
                                                   label =   "Choose Category :",
                                                   choices =  c("Education Level" = "educationLevel",
                                                                "Household Size" = "householdSize",
                                                                "Having Kids" = "haveKids",
                                                                "Interest Group" = "interestGroup"),
                                                   selected = "educationLevel"
                                       )),
                                     plotlyOutput("eduPayPlot"),
                                     plotlyOutput("partPayPlot"),
                                     verbatimTextOutput("drildownlinfo")
                                         
                                     
                            ),
                            
                            
                          )
                        )
                      )
                    )
                 ),
             tabPanel("Employment Pattern",
                      fluidPage(
                        titlePanel("What is the pattern found in the employment ?"),
                        fluidRow(
                          column(
                            width = 12,
                            height = 100,
                            tabsetPanel(
                              tabPanel("Education vs Pay",
                                       box(
                                         width = 4,
                                         height = 320,
                                         checkboxGroupInput(inputId = "edu", 
                                                            label =   "Education Requirement:",
                                                            choices =  c("Low" = "Low",
                                                                         "High School or College" = "HighSchoolOrCollege",
                                                                         "Bachelors" = "Bachelors",
                                                                         "Graduate" = "Graduate"),
                                                            selected = c("Low","HighSchoolOrCollege")
                                         ),
                                         textInput(
                                           inputId = "plot_title",
                                           label = "Plot title",
                                           placeholder = "Enter text to be used as plot title"),
                                         actionButton("goButton", "Go!"),
                                         
                                         checkboxInput(inputId = "showData",
                                                       label = "Show data table",
                                                       value = TRUE)
                                       ),
                                       box(
                                         height = 400,
                                         plotlyOutput("rainPlot")
                                       ),
                                       DT::dataTableOutput(outputId = "rainPlotTable")
                                       
                              ),
                              
                              
                            )
                          )
                        )
                        
                      )),
             tabPanel("Employer Health",
                      fluidPage(
                        titlePanel("Which employers are financially healthy ?"),
                        fluidRow(
                          column(
                            width = 12,
                            height = 100,
                            tabsetPanel(
                              tabPanel("Wage By Employers",
                                       box(
                                         width = 4,
                                         height = 150,
                                         selectInput(inputId = "color",
                                                     label = "Choose Color type:",
                                                     choices = c("Red-Blue Diverging" = "RdYlBu",
                                                                 "Orange-Red Diverging" = "OrRd",
                                                                 "Light -Dark Blue" = "Blues"),
                                                     selected = "RdYlBu"),
                                         sliderInput(inputId = "no.ofemp",
                                                     label = "Choose min. and max. no. of employees",
                                                     min = 2,
                                                     max = 9,
                                                     value= c(3,7))
                                       ),
                                       box(plotOutput("treemapPlot")),
                                       DT::dataTableOutput(outputId = "treemapTable")
                                                           
                                       
                                       
                              ),
                              tabPanel("Emp location",
                                       box(
                                         width = 4,
                                         height = 150,
                                         selectInput(inputId = "emp",
                                                     label = "Employees left ",
                                                     choices = c("Left" = "left",
                                                                 "Joined" = "joined"),
                                                     selected = "Left"),
                                         checkboxInput(inputId = "showData",
                                                       label = "Show data table",
                                                       value = TRUE)
                                       ),
                                       box(plotOutput("mapPlot")),
                                       DT::dataTableOutput(outputId = "aTable")
                                       
                              ),
                              
                            )
                          )
                        )
                        
                      )))
)

#========================#
###### Shiny Server ######
#========================#

server <- function(input, output){
  
  ########################## Q1 ########################## 
  
  
  ########################## Q2 ########################## 
  NumberOfParicipants<-Participants%>%
    tally()
  #### tmap #### 
  output$FinLocation<-renderPlot({
    z<-ParticipantSavings%>%
      st_as_sf(wkt="currentLocation")
    
    
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
      value=div(NumberOfParicipants$n,style="font-size:16px;")
      #paste(NumberOfParicipants$n, format="d")
      ,subtitle = HTML('<b style = "padding-left:0px;font-size:10px">Participants</b>')
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")  
  })
  output$value2 <- renderValueBox({ 
    valueBox(
      value=div(paste("Mar 22-May 23"),style="font-size:16px;")
      ,subtitle = HTML('<b style = "padding-left:0px;font-size:10px">Period</b>')
      ,icon = icon("calendar")
      )  
  })
  output$value3 <- renderValueBox({
    valueBox(
      value=div(paste("8"),style="font-size:16px;")
      ,subtitle = HTML('<b style = "padding-left:0px;font-size:10px">Groups</b>')
      ,icon = icon("user",lib='glyphicon')
      ,color = "yellow")   
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
    
    
    ggplot(ParticipantMonthlySavings%>%
             filter(Month %in% input$Months)) +
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
    
    if(input$HaveKidsDashboard!="All"){
      ParticipantMonthlySparkData<-ParticipantMonthlySpark%>%
        filter(householdSize %in% input$HouseHoldSizeDashboard)%>%
        filter(haveKids==input$HaveKidsDashboard)%>%
        filter(age>= input$age[1] & age<=input$age[2])%>%
        filter(educationLevel %in% input$EducationDashboard)%>%
        select(participantId,Earning,Expense,joviality)
    }
    else{
      ParticipantMonthlySparkData<-ParticipantMonthlySpark%>%
        filter(householdSize %in% input$HouseHoldSizeDashboard)%>%
        filter(age>= input$age[1] & age<=input$age[2])%>%
        filter(educationLevel %in% input$EducationDashboard)%>%
        select(participantId,Earning,Expense,joviality)
    }

    

    
    #ParticipantMonthlySparkShared<-SharedData$new(ParticipantMonthlySparkData)

  
  
    reactable(
      ParticipantMonthlySparkData,
      columns = list(
        participantId = colDef(maxWidth = 120),
        `Earning` = colDef(
          cell = react_sparkline(ParticipantMonthlySparkData,
                                 highlight_points = highlight_points(
                                   min = "red", max = "blue"),
                                 labels = c("first", "last"))
        ),
        `Expense` = colDef(
          cell = react_sparkline(ParticipantMonthlySparkData,
                                 highlight_points = highlight_points(
                                   min = "red", max = "blue"),
                                 labels = c("first", "last"))
        ),
        `joviality` =  colDef(
          cell = data_bars(
            data =ParticipantMonthlySparkData,
            fill_color = viridis::mako(5),
            background = '#F1F1F1',
            fill_opacity = 0.8,
            round_edges = TRUE,
            text_position = 'outside-end',
            number_fmt = scales::comma_format(accuracy = 0.001)
          )
        )
      )
    )%>% 
      add_title(
        title = 'Are we financially fit?', 
  
        align = 'center',
        font_color = '#000000'
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
  
  
  
  output$InterestGroups<- renderPlotly({
    
    
    ggplot(InterestGroupGraph%>%
             filter(interestGroup %in% input$InterestGroup))+
      geom_line(aes(x=date,y=log(Expense*-1),color=interestGroup))
    
    
    
    
  })
  
  
  
  output$HeatMap<- renderPlot({
    
    ggplot(StatusLogDetails%>%
             group_by(participantId,Weekday,category)%>%
            summarise(Expense=sum(TotalAmount)),
          aes(x=factor(Weekday,levels=c("Monday","Tuesday",
                                        "Wednesday","Thursday",
                                        "Friday","Saturday","Sunday")),
              category,
              fill = Expense)) +
     geom_tile(aes(text=paste("Total Time: ",Expense)),color = "white",
               size = 0.1,lwd = 1.5,linetype = 1) +
     coord_equal() +
     scale_fill_gradient2(low = "#075AFF",
                          mid = "#FFFFCC",
                          high = "#FF0000")+
     labs(x = NULL,
          y = NULL,
          title = "Is it all work and no play?")+
     theme_ipsum()+
     guides(fill = guide_colourbar(barwidth = 0.5,
                                   barheight = 5))+
     theme(axis.ticks = element_blank(),
           axis.text.x = element_text(size = 7,angle=90),
           axis.text.y = element_text(size = 7),
           plot.title = element_text(hjust = 0.5),
           legend.title = element_text(size = 8),
           legend.text = element_text(size = 6))
    
    
  })
  
  ########################## Q3 ########################## 
  
  output$testPlot <- renderPlot({
    work_home <- travel %>%
      filter(purpose == "Work/Home Commute") %>%
      group_by(participantId,travelEndLocationId) %>%
      tally() %>%
      dplyr::select('participantId','travelEndLocationId') 
    work <- inner_join(x = work_home, y = emp, by= c("travelEndLocationId"="employerId" )) %>%
      dplyr::select('participantId','travelEndLocationId') %>%
      group_by(participantId) %>%
      tally() %>%
      dplyr::rename('numberofplacesworked'='n')
    workinmoreplaces = work %>%
      filter(numberofplacesworked > 1) %>%
      arrange(desc(numberofplacesworked))
    
    gg <- gghistostats(
      data = work, 
      x = numberofplacesworked, 
      xlab = "numbers of places worked", 
      type = input$variable_selection,
      title = "Distribution of turnover rate", 
      test.value = 1
    )
    
    return(gg)
    
  })
  
  output$err_op <- renderPlot({
    weeklypay_education <- jobs %>%
      group_by(educationRequirement) %>%
      summarise(
        n=n(),
        mean=mean(weeklypay),
        sd=sd(weeklypay))%>%
      mutate(se=sd/sqrt(n-1))
    
    errplt <- ggplot(weeklypay_education) +
      geom_errorbar(
        aes(x=educationRequirement, 
            ymin=mean-se, 
            ymax=mean+se), 
        width=0.2, 
        colour="black", 
        alpha=0.9, 
        size=0.5) +
      geom_point(aes
                 (x=educationRequirement, 
                   y=mean), 
                 stat="identity", 
                 color="red",
                 size = 1.5,
                 alpha=1) +
      ggtitle("Weekly pay vs educational requirement")+
      theme(plot.title = element_text(hjust = 0.5))
    
    return(errplt)
    
  })
  
  output$rainPlot <- renderPlotly({
    input$goButton
    
    p <- ggplot(jobs %>% filter(educationRequirement == input$edu),
                aes(x = educationRequirement, y = hourlyRate, fill=educationRequirement)) + 
      ggdist::stat_halfeye(
        adjust = .5, 
        width = .6, 
        .width = 0, 
        justification = -.3, 
        point_colour = NA) + 
      geom_boxplot(
        width = .25, 
        outlier.shape = NA
      ) +
      geom_point(
        size = 1.3,
        alpha = .3,
        position = position_jitter(
          seed = 1, width = .1
        )
      ) + 
      coord_cartesian(xlim = c(1.2, NA), clip = "off")+
      #ggtitle(label = "Wage Distribution for Different Education Level",
      #subtitle = "High Wages For Higher Educated")+
      theme_minimal()+
      theme(plot.title = element_text(size=14, face="bold",hjust = 0.5),
            plot.subtitle = element_text(size=12,hjust = 0.5,color='mediumvioletred'))+
      theme(axis.title.y= element_text(angle=0), axis.ticks.x= element_blank(),
            panel.background= element_blank(), axis.line= element_line(color= 'grey')) +
      labs(title = isolate({
        toTitleCase(input$plot_title)
      }))
    
    ggplotly(p)
  })
  
  output$rainPlotTable <- DT::renderDataTable({
    if(input$showData){
    DT::datatable(jobs %>% filter(educationRequirement == input$edu) %>%
                    select(jobId, employerId, hourlyRate, educationRequirement),
                  options= list(pageLength = 10),
                  rownames = FALSE)
    }
    
  })  
  
  
  output$mapPlot <- renderPlot({
    
    
    if(input$emp == "left") {
      tmap_mode("plot")
      tm_shape(buildings)+
        tm_polygons(col = "grey60",
                    size = 1,
                    border.col = "black",
                    border.lwd = 1)+
        tm_shape(prevEmp_sf) +
        tm_compass()+
        tm_bubbles(col = "red",
                   n=3,
                   size = "no.ofempLeft") 
      
    } 
    else {
      tmap_mode("plot")
      tm_shape(buildings)+
        tm_polygons(col = "grey60",
                    size = 1,
                    border.col = "black",
                    border.lwd = 1)+
        tm_shape(recntEmp_sf) +
        tm_compass()+
        tm_bubbles(col = "green",
                   size = "no.ofempShifted") 
    }
    
  })
  
  output$aTable <- DT::renderDataTable({
    if(input$showData & input$emp == "left"){
      DT::datatable(prevEmp_sf,
                    options= list(pageLength = 10),
                    rownames = FALSE)
    }
    else if(input$showData & input$emp == "joined"){
      DT::datatable(recntEmp_sf,
                    options= list(pageLength = 10),
                    rownames = FALSE)
    }
  })  
  
  ############# Turnover analysis before and after route map ###################
  
  
  output$befRoute <- renderPlot({
    
    
    logs_path_PrevJob <- logs_path_PrevJob %>%
      filter(participantId == input$participants)
    
    
    tmap_mode("plot")
    tm_shape(buildings)+
      tm_polygons(col = "grey60",
                  size = 1,
                  border.col = "grey",
                  border.lwd = 1) +
      tm_shape(logs_path_PrevJob) +
      tm_lines(col = "red") +
      tm_compass()+
      tm_layout(main.title = "Previous Job Route",
                main.title.position = "center",
                main.title.size = 1,
                legend.show = FALSE)
  })
  
  output$aftRoute <- renderPlot({
    
    logs_path_RecJob <- logs_path_RecJob %>%
      filter(participantId == input$participants)
    
    
    tmap_mode("plot")
    tm_shape(buildings)+
      tm_polygons(col = "grey60",
                  size = 1,
                  border.col = "grey",
                  border.lwd = 1) +
      tm_shape(logs_path_RecJob) +
      tm_lines(col = "red") +
      tm_compass()+
      tm_layout(main.title = "Latest Job Route",
                main.title.position = "center",
                main.title.size = 1,
                legend.show = TRUE)
    
    
  })
  
  
  output$treemapPlot <- renderPlot ({
                  treemap(no.ofjobs %>% 
                            filter(no.ofjobs >= input$no.ofemp[1] &
                                     no.ofjobs <= input$no.ofemp[2]),
                         index = c('label', 'employerId'),
                         vSize = 'totalWage',
                         vColor = 'Average Wage',
                         palette = input$color,
                         type = 'value',
                         title = 'Wage by Employer')
    
                    rootname = 'Employee Hourly Wage by Workplace'
                  })
  output$treemapTable <- DT::renderDataTable({
    if(input$showData){
      DT::datatable(no.ofjobs_table %>% 
                      filter(no.ofjobs >= input$no.ofemp[1] &
                               no.ofjobs <= input$no.ofemp[2]),
                    caption = "Is the high wage due to educational qualification ?",
                    options= list(pageLength = 10),
                    rownames = FALSE)
    }
    
  })  
  
    
  
  output$eduPayPlot <- renderPlotly({
    
    switchEmployeesAllDetails$participantId <- as.character(switchEmployeesAllDetails$participantId)
    p1<- ggplot(switchEmployeesAllDetails,
                aes(x=educationLevel, y=payDiff))+
      geom_bar(stat="identity", aes(fill = payStatus))+
      scale_fill_manual(values=c(`Pay Decrease` ="firebrick1", `Pay Increase` ="steelblue")) +
      labs(y= 'Pay\n Difference',title="Change in Wage by Education Level", x='Education Level') +
      theme(axis.title.y=element_text(angle=0), axis.ticks.x=element_blank(),panel.background = element_blank(),
            axis.line = element_line(color='grey'), plot.title = element_text(hjust = 0.5),
            axis.title.y.left = element_text(vjust = 0.5), axis.text = element_text(face="bold")
      )
   ggplotly(p1)
  
  })
  
  output$partPayPlot <- renderPlotly({
    d <- event_data("plotly_click")
    if (is.null(d)) return(NULL)
    p2 <- switchEmployeesAllDetails %>% 
      filter(educationLevel %in% d$x) %>%
      ggplot(aes(x=haveKids, y=payDiff))+
      geom_bar(stat="identity", aes(fill = payStatus))+
      scale_fill_manual(values=c(`Pay Decrease` ="firebrick1", `Pay Increase` ="steelblue")) +
      labs(y= 'Pay\n Difference',title="Change in Wage by Kids", x='Having Kids') +
      theme(axis.title.y=element_text(angle=0), axis.ticks.x=element_blank(),panel.background = element_blank(),
            axis.line = element_line(color='grey'), plot.title = element_text(hjust = 0.5),
            axis.title.y.left = element_text(vjust = 0.5), axis.text = element_text(face="bold")
      ) 
    ggplotly(p2) %>%
      layout(xaxis = list(title = d$x))
  })
  output$drildownlinfo <- renderPrint({
    event_data("plotly_click")
  }) 
}

shinyApp(ui = ui, server = server)


