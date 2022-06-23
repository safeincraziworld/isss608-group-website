########################## Packages in use ########################## 

packages=c('ggiraph', 'plotly', 'rmarkdown','psych','sf','tmap',
           'DT', 'patchwork','gglorenz','hrbrthemes','shinydashboard',
           'gganimate', 'tidyverse','ggthemes','reactable',
           'readxl', 'gifski', 'gapminder','quantmod','shinythemes',
           'treemap', 'treemapify','ggridges','zoo','reactablefmtr',
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

FinancialJournal<-read_csv("data/FinancialJournal.csv",show_col_types = FALSE)
Participants<-read_csv("data/Participants.csv",show_col_types = FALSE)
ParticipantsApartmentLocation<-read_csv("data/ParticipantsApartmentLocation.csv",show_col_types = FALSE)
buildings<-read_sf("data/buildings.csv", 
                   options = "GEOM_POSSIBLE_NAMES=location")

jobs <- read_csv("data/Jobs.csv")
emp <- read_csv("data/Employers.csv")
travel <- read_csv("data/TravelJournal.csv")
apartments <- read_csv("data/wkt/Apartments.csv")
participants <- read_csv("data/Participants.csv")
buildings <- read_sf("data/wkt/Buildings.csv", 
                     options = "GEOM_POSSIBLE_NAMES=location")
employers <- read_sf("data/wkt/Employers.csv", 
                     options = "GEOM_POSSIBLE_NAMES=location")
logs_selected <- read_rds("data/logs_selected.rds")



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

jobs<-jobs %>%
  mutate(workinghours=difftime(jobs$endTime,jobs$startTime,units='hours')*5)
jobs<-jobs %>%
  mutate(weeklypay=hourlyRate*workinghours)
jobs$weeklypay=as.numeric(jobs$weeklypay)
jobs <-jobs %>%
  mutate(educationRequirement = factor(jobs$educationRequirement, level = c('Low', 'HighSchoolOrCollege','Bachelors','Graduate')))

participants<- participants %>%
  mutate(jovialityGroup= cut(joviality, breaks =c(-Inf,0.2,0.5,0.8,1),labels=c("Not too Happy","Fairly Happy","Happy","Very Happy"))) %>%
  mutate(ageGroup = cut(age,breaks=c(18,35,55,Inf),labels=c("Young Adult","Middle Age","Older Adult"),
                        include.lowest = TRUE))

hires <- jobs %>%
  group_by(employerId) %>% tally() %>%
  arrange(desc(n)) %>%
  dplyr::rename("No. of employees" = "n")

employerpay <- jobs %>%
  group_by(employerId) %>%
  dplyr::summarise(emppay = sum(weeklypay))

pay_hires <- merge(x = hires, y = employerpay, by = "employerId", all = TRUE) %>%
  mutate(employeepay = emppay / `No. of employees`) %>%
  arrange(desc(employeepay)) %>%
  dplyr::select(employerId,`No. of employees`, employeepay) %>%
  arrange(employerId)
pay_hires


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

workinmoreplaces <- work %>%
  filter(numberofplacesworked > 1) %>%
  arrange(participantId)

work_home_filt <- travel %>%
  filter(purpose == "Work/Home Commute") %>%
  group_by(participantId,travelEndLocationId) %>%
  tally() %>%
  dplyr::select('participantId','travelEndLocationId') %>%
  filter(travelEndLocationId
         %in% emp$employerId & participantId %in% workinmoreplaces$participantId)

empWorkinMultiplePlaces <- travel %>%
  mutate(StartDate = as_date(travelStartTime)) %>%
  filter (participantId %in% work_home_filt$participantId &
            purpose == "Work/Home Commute" &
            travelEndLocationId %in% work_home_filt$travelEndLocationId) %>%
  dplyr::select(participantId,StartDate,travelEndLocationId) %>%
  arrange(participantId)

empWorkinMultiplePlaces <- empWorkinMultiplePlaces %>%
  group_by(participantId) %>%
  filter(StartDate == min(StartDate) | StartDate == max(StartDate)) %>%
  ungroup

empWorkinMultiplePlaces_latest <- empWorkinMultiplePlaces %>%
  group_by(participantId) %>%
  slice(which.max(StartDate)) %>%
  dplyr::rename ("recent_employer" = "travelEndLocationId")

empWorkinMultiplePlaces_previous <- empWorkinMultiplePlaces %>%
  group_by(participantId) %>%
  slice(which.min(StartDate)) %>%
  dplyr::rename ("previous_employer" = "travelEndLocationId")

empWorkinMultiplePlaces_latest_groupby <- empWorkinMultiplePlaces_latest %>%
  group_by(recent_employer) %>%
  tally() %>%
  dplyr::rename("no.ofempShifted" = "n") %>%
  arrange(desc(`no.ofempShifted`))

empWorkinMultiplePlaces_previous_groupby <- empWorkinMultiplePlaces_previous %>%
  group_by(previous_employer) %>%
  tally() %>%
  dplyr::rename("no.ofempLeft" = "n") %>%
  arrange(desc(`no.ofempLeft`))

transitionTable <- inner_join(x=empWorkinMultiplePlaces_previous ,
                              y=empWorkinMultiplePlaces_latest,
                              by = "participantId") %>%
  dplyr::select(participantId,previous_employer,recent_employer)
transitionEmpDetails <- participants %>%
  filter(participantId %in% transitionTable$participantId)

employers <- employers %>% 
  mutate(across(employerId, as.integer))

prevEmp_sf <- employers %>%
  filter(employerId %in% transitionTable$previous_employer ) %>%
  mutate(empWorkinMultiplePlaces_previous_groupby$no.ofempLeft) %>%
  dplyr::rename("no.ofempLeft" = "empWorkinMultiplePlaces_previous_groupby$no.ofempLeft")

recntEmp_sf <- employers %>%
  filter(employerId %in% transitionTable$recent_employer )%>%
  mutate(empWorkinMultiplePlaces_latest_groupby$no.ofempShifted) %>%
  dplyr::rename("no.ofempShifted" = "empWorkinMultiplePlaces_latest_groupby$no.ofempShifted")


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
                                       height = 120,
                                       h4('Insights:')
                                       
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
                                     box(plotOutput("paychangePlot"))
                                         
                                     
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
                                                     label = "No. of Employees",
                                                     min = 2,
                                                     max = 9,
                                                     value= 4)
                                       ),
                                       box(plotOutput("treemapPlot"))
                                       
                                       
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
    input$showData
    DT::datatable(jobs %>% filter(educationRequirement == input$edu) %>%
                    select(jobId, employerId, hourlyRate, educationRequirement),
                  options= list(pageLength = 10),
                  rownames = FALSE)
    
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
      tm_layout(main.title = "Latest Job Route",
                main.title.position = "center",
                main.title.size = 1,
                legend.show = TRUE)
    
    
  })
  
  output$treemapPlot <- renderPlot({
    treemap(no.ofjobs %>% filter(no.ofjobs == input$no.ofemp),
            index = c('label', 'employerId'),
            vSize = 'totalWage',
            vColor = 'Average Wage',
            palette = input$color,
            type = 'value',
            title = 'Wage by Employer')
    
  })
  
  output$paychangePlot <- renderPlot({
    
    switchEmployeesAllDetails$participantId <- as.character(switchEmployeesAllDetails$participantId)
    p1<- ggplot(switchEmployeesAllDetails,
                aes(x=educationLevel, y=payDiff))+
      geom_bar(stat="identity", aes(fill = payStatus))+
      scale_fill_manual(values=c(`Pay Decrease` ="firebrick1", `Pay Increase` ="steelblue")) +
      labs(y= 'Pay\n Difference',title="Employee Wage Difference Between Previous and Recent Workplace", x='Participant Id') +
      theme(axis.title.y=element_text(angle=0), axis.ticks.x=element_blank(),panel.background = element_blank(),
            axis.line = element_line(color='grey'), plot.title = element_text(hjust = 0.5),
            axis.title.y.left = element_text(vjust = 0.5), axis.text = element_text(face="bold")
      )
   p1
    
  })
  
}

shinyApp(ui = ui, server = server)


