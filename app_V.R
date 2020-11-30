#app for STW
library(shinydashboard)
library(dashboardthemes)
library(dplyr)
library(readr)
library(leaflet)
library(sf)
library(plotly)
library(shinyjs)
library(rintrojs)
library(shinyBS)
library(shinyWidgets)
library(DT)
library(RColorBrewer)
library(stringr)
library(shinyalert)
library(shiny)
library(jrvFinance)
library(FinCal)
library(scales)
library(ggplot2)

#additional
#estimate the whole matrix
#add inputs

rm(list=ls())

#--------------------------------------------------------------------------------------------------------------
#DATA
#--------------------------------------------------------------------------------------------------------------
#expenditures by county in VA
expend_raw <- read_csv('~/Documents/STW/data/expenditures_county_va.csv')
#cost of community colleges in VA: Nursing
cost_cc <- read_csv('~/Documents/STW/data/cost_cc_va.csv')
#earnings neede from MIT Living wage, 2 counties
earnings_raw <- read_csv('~/Documents/STW/data/wage_county_va.csv')
#earnings by county and education level
earnings_acs <- read_csv('~/Documents/STW/data/earnings.csv')
#eanings according to different community college and careers from onet
earnings_cc <- read_csv("~/Documents/STW/data/earnings_cc_VA_onet.csv")


#--------------------------------------------------------------------------------------------------------------
#UI
#--------------------------------------------------------------------------------------------------------------

ui <- dashboardPage(title = "Skill Technical Workforce - Affordability",
                    
                    dashboardHeader(
                      titleWidth='100%',
                      title = span(
                        #tags$img(src = "header.jpg", width = '100%'), 
                        column(12, class = "title-box" 
                        )
                      )
                    ),
                    
                    dashboardSidebar(

                      sidebarMenu(
                        selectInput("ccollege", "Choose a Community College:",
                                    list(`Arlington` = list("Northern Virginia", "CommCollege2", "CommCollege3"),
                                         `Norfolk` = list("Tidewater", "CommCollege2"),
                                         `Albermarle` = list("Piedmont", "CommCollege2", "CommCollege3"))
                        )
                      ),
                      
                      sidebarMenu(
                        selectInput(inputId = 'career', "Choose a Career Path:",
                                    choices = c(
                                      'Accounting and Related Services, Other' ,
                                      'Airframe Mechanics and Aircraft Maintenance Technology/Technician' ,
                                      'Allied Health Diagnostic, Intervention, and Treatment Professions, Other' ,
                                      'Architectural Technology/Technician' ,
                                      'Automobile/Automotive Mechanics Technology/Technician' ,
                                      'Business Administration, Management and Operations, Other', 
                                      'Business Operations Support and Secretarial Services, Other' ,
                                      'CAD/CADD Drafting and/or Design Technology/Technician' ,
                                      'Career Services Certificate' ,
                                      'Child Care Provider/Assistant' ,
                                      'Clinical/Medical Laboratory Technician' ,
                                      'Computer and Information Sciences, General' ,
                                      'Construction Trades, General' ,
                                      'Cooking and Related Culinary Arts, General' ,
                                      'Criminal Justice/Law Enforcement Administration' ,
                                      'Dental Hygiene/Hygienist' ,
                                      'Design and Visual Communications, General' ,
                                      'Diagnostic Medical Sonography/Sonographer and Ultrasound Technician' ,
                                      'Electrical, Electronic and Communications Engineering Technology/Technician' ,
                                      'Electrician' ,
                                      'Emergency Medical Technology/Technician (EMT Paramedic)' ,
                                      'Engineering Technology, General' ,
                                      'Fire Science/Fire-fighting' ,
                                      'Funeral Service and Mortuary Science, General' ,
                                      'Health Information/Medical Records Technology/Technician' ,
                                      'Heating, Air Conditioning, Ventilation and Refrigeration Maintenance Technology/Technician' ,
                                      'Hospitality Administration/Management, General' ,
                                      'Industrial Electronics Technology/Technician' ,
                                      'Industrial Production Technologies/Technicians, Other' ,
                                      'Industrial Technology/Technician' ,
                                      'Interior Design' ,
                                      'Legal Assistant/Paralegal' ,
                                      'Mechanical Engineering Related Technologies/Technicians, Other' ,
                                      'Natural Resources/Conservation, General' ,
                                      'Occupational Therapist Assistant' ,
                                      'Photography' ,
                                      'Physical Therapy Technician/Assistant' ,
                                      'Radiologic Technology/Science - Radiographer',
                                      'Registered Nursing/Registered Nurse',
                                      'Respiratory Care Therapy/Therapist',
                                      'Sign Language Interpretation and Translation' ,
                                      'Veterinary/Animal Health Technology/Technician and Veterinary Assistant' ,
                                      'Visual and Performing Arts, General' ,
                                      'Visual and Performing Arts, Other'
                                    ),
                                    selected = 'Registered Nursing/Registered Nurse',
                                    multiple = FALSE
                        )
                      )
                      
                    ),
                    dashboardBody(
                      fluidRow(
                        box(title = "Choose your career path",
                            width = 12,
                            "Select your career preferences and the details about your preferences.  The bottom figure presents a time line forecast of 
                            EXPENDITURES and EARNINGS according to the selected career."
                        )
                        ),
                      fluidRow(height = 300,
                        box(title = "Plan your career for the next years",
                            width = 12,
                           
                            column(
                              
                              width = 4,
                              "Select your current status",
                              selectInput(inputId = "status",
                              #varSelectInput(inputId = "status",
                                          label = "Current status",
                                          #expend_t[ , 3:14]  #for varSelectInput
                                          #multiple = TRUE
                                          
                                          choices = c("1 adult 0 children" = 'expend1.0',
                                                      "1 adult 1 children" = 'expend1.1',
                                                      "1 adult 2 children" = 'expend1.2',
                                                      "1 adult 3 children" = 'expend1.3',
                                          
                                                      "2 adults 1 working 0 children" = 'expend2.0',
                                                      "2 adults 1 working 1 children" = 'expend2.1',
                                                      "2 adults 1 working 2 children" = 'expend2.2',
                                                      "2 adults 1 working 3 children" = 'expend2.3',
                                                      "2 adults 2 working 0 children" = 'expend3.0',
                                                      "2 adults 2 working 1 children" = 'expend3.1',
                                                      "2 adults 2 working 2 children" = 'expend3.2',
                                                      "2 adults 2 working 3 children" = 'expend3.3' ),
                                          selected = 'expend1.0',
                              multiple = FALSE
                                
                              )
                              
                              ) ,
                            
                            #column 2
                            column(width = 4,
                              checkboxInput(inputId = "ccpayment",
                                            label = strong("Add college payments per year"),
                                            value = FALSE), 
                              
                              # Display this only if payment is chosen 
                              conditionalPanel(condition = "input.ccpayment == true",
                                               textOutput("text")
                              ) 
                            ),
                            
                            #column 3
                            column(width = 4,
                              checkboxInput(inputId = "debt",
                                            label = strong("Add debt payments over 10 years"),
                                            value = FALSE),
                              
                              # Display this only if debt is chosen 
                              conditionalPanel(condition = "input.debt == true",
                                               sliderInput(inputId = "debtpercentage",
                                                           label = "Percentage of debt:",
                                                           min = 0, max = 1, value = 0.9, step = 0.1) 
                              )  
                            )


                        )
                      )
                        ,
                        
                      fluidRow(
                        
                        box(width = 12,
                            title = "Plan your career for the these years",
                            # width = 12,
                          plotlyOutput("financialplan" , width = "auto" ,  height = "500")
                        )
                        
                      )
                    )
                    
)

#--------------------------------------------------------------------------------------------------------------
#SERVER
#--------------------------------------------------------------------------------------------------------------

server <- function(input, output) {

  #--------------------------------------------------------------------------------------------------------------
  #GENERAL INFORMATION
  #--------------------------------------------------------------------------------------------------------------
  #set for a period of 42 years 
  years <- 42
  startyear <- 18
  #establish years and ages
  dat <- data.frame(years=seq(0,years), age=seq(startyear, c(startyear+years)) )
  
  
  #--------------------------------------------------------------------------------------------------------------
  #EXPENDITURES
  #--------------------------------------------------------------------------------------------------------------
  #growing expenditures by inflation
  inflation<- 0.013
  #year of community college
  year_cc <- 2
  #--------
  #expenditures for all categories
  
  expend_1adult <-  expend_raw %>% filter(county=="Arlington", category=="expenditure", 
                                          item=="Required annual income before taxes", adults=="1 adult")
  expend_2adult1 <- expend_raw %>% filter(county=="Arlington", category=="expenditure", 
                                          item=="Required annual income before taxes", adults=="2 adult 1 work")
  expend_2adult2 <- expend_raw %>% filter(county=="Arlington", category=="expenditure", 
                                          item=="Required annual income before taxes", adults=="2 adult 2 work")
  #join 
  expend_t0 <- cbind(expend_1adult[1,5:8], expend_2adult1[1,5:8], expend_2adult2[1,5:8] )
  
  cat <- c("expend1.0", "expend1.1", "expend1.2", "expend1.3", 
           "expend2.0", "expend2.1", "expend2.2", "expend2.3",
           "expend3.0", "expend3.1", "expend3.2", "expend3.3")
  
  names(expend_t0)[1:12] <-  cat
  
  expend_t <- cbind(dat, expend_t0)
  
  
  for(i in (3:14)) {
    expend_t[ , i] <- cumprod( c(expend_t[1,i], rep(c(1+inflation), years ))) 
  }
  
  #matrix of all calculations ready

  
  #---------
  
  #expenditures over time
  #dat$exp_t <- cumprod(c(exp[1,1], rep(c(1+inflation), years )))
  tuition_cc <- cost_cc %>% filter(Institution=="Northern Virginia", Program=="AAS") %>%    select("Tuition/costs credits")
  other_costs <- 3500  #other costs considering uniforms,   
  total_cc <- tuition_cc+other_costs
  tot_cost_cc_year <- total_cc/year_cc
  
  #community college expenditures: includes tuition and other costs
  expend_t$cc_expend <- c( tot_cost_cc_year,tot_cost_cc_year, rep(0, nrow(expend_t) -year_cc)) 
  expend_t$cc_expend <- as.numeric(expend_t$cc_expend)
  
  
  #--------------------------------------------------------------------------------------------------------------
  #DEBT
  #--------------------------------------------------------------------------------------------------------------
  
  #debt expenditures
  
  # if (input$debt==TRUE) {
  #   debt_pc <- input$debtpercentage
  # }
  

  
  
  debt_pc <- 0.9
    #input$debtpercentage   #50% of tuition becomes debt  THIS SHOULD BE SLIDER
  debt <- total_cc*debt_pc
  interest_rate <- 0.0466  #annual interest rate
  years_debt <- 10
  debt_pmt <- pmt(r = interest_rate, n = years_debt, pv = debt, fv = 0)
  ##result of payment pmt comes negative so, use negative to adjust the sign
  #dat$debt_pmt <- c( rep(0,year_cc) , rep(-debt_pmt, years_debt), rep(0, years-year_cc -years_debt +1) )
  expend_t$debt_pmt <- c( rep(0,year_cc) , rep(-debt_pmt, years_debt), rep(0, years-year_cc -years_debt +1) )
  expend_t$debt_pmt <- as.numeric(expend_t$debt_pmt)


  
  
  #--------------------------------------------------------------------------------------------------------------
  #EARNINGS
  #--------------------------------------------------------------------------------------------------------------
  
  #EARNINGS min wage------------------

  
  minwage <- data.frame(
    earnings_raw %>% filter(county=="Arlington", category=="wage", 
                       item=="Minimum Wage", adults=="1 adult") %>% select(value="0child")
  )
  
  indexwages<- 0
  hoursweek <- 43.34
  weeks <- 4
  months <-  12
  #initialize the matrix
  earnings <- dat
  earnings$earn_minwage <- cumprod(c(minwage[1,1]*hoursweek*weeks*months, rep(c(1+indexwages), years )))
  
  
  #EARNINGS HS------------------
  
  earnings_dollar_hs <- data.frame(earnings_acs %>% filter(NAME =="Arlington County, Virginia") %>% select(earnings_hs)) 
  earngr_hs <- 0.018   #growht of earnings for hs 2012-2018
  earnings$earnings_hs <- cumprod(c(earnings_dollar_hs[1,1] , rep(c(1+earngr_hs), years )))   #revise the growth PENDIENTE
  
  
  #EARNINGS ASSOCIATE------------------
  #general earnings - median for every county in VA

  earnings_cc_career <- earnings_cc %>% filter(college== 'Northern Virginia Community College') %>% 
      filter(years==1.5) %>% filter( program == 'input$career' ) %>%  select(wagemedian)
    #filter( program == "Registered Nursing/Registered Nurse" ) %>% filter(years==1.5) %>% select(wagemedian)



  
  earngr_assoc <- 0.01   #growht of earnings for hs 2012-2018
  
  earnings$earnings_assoc <- cumprod(c(earnings_cc_career[1,1] , rep(c(1+earngr_assoc), years )))

  #earnings$earnings_assoc <- cumprod(c(earnings_dollar_assoc[1,1] , rep(c(1+earngr_assoc), years )))
  earnings$earnings_assoc[1] <- 0
  earnings$earnings_assoc[2] <- 0  
  

    
  #--------------------------------------------------------------------------------------------------------------
  #PLOTS
  #--------------------------------------------------------------------------------------------------------------
  #plot income vs expenditures
  
  colors <- c("Expenditures"="darkred", 
              "Expenditures Comm-Colleges"= "red",
              "Earnings Minimum Wage"="orange", 
              "Earnings HighSchool"="blue",
              "Earnings Associate"="dodgerblue2"
  )
  
   
  
  output$financialplan <- renderPlotly({ 
    
    plot_dat <- cbind(expend_t %>% select(years, age, Expense=input$status, cc_expend, debt_pmt) , earnings[ ,3:5])  
    
    plot1 <- ggplot(plot_dat , aes(x=years)) + 
      
      geom_line(aes(y = Expense  , color = "Expenditures" ), size=0.6, alpha=1 )  +  #for varSelectInput
    
      geom_line(aes(y = earn_minwage  , color = "Earnings Minimum Wage" ), size=0.6, alpha=1 ) +
      geom_line(aes(y = earnings_hs  , color = "Earnings HighSchool" ), size=0.6, alpha=1 ) +
      geom_line(aes(y = earnings_assoc  , color = "Earnings Associate" ), size=0.6, alpha=1 ) +
    
      labs(x = "Years",
           y = "Dollars",
           color = "Legend") +
      scale_color_manual(values = colors)+
      ggtitle("Personal Expenses and Earnings for 40 years")+
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_x_continuous(breaks= seq(0, 43, by = 5))+
      #ylim( min(plot_dat[,3:7]), 130000)+
      scale_y_continuous( seq( min(plot_dat[,3:7]), max(plot_dat[,3:7])*1.1, by = 10000 ) , labels = comma) 
           #max(plot_dat[,3:7])*1.1 )   
    
if(input$ccpayment == TRUE){
      plot1 <- plot1 + geom_line(aes(y = Expense+cc_expend  , color = "Expenditures Comm-Colleges" ), size=0.6, alpha=1 )
    }
  
  plot1 
  
  if(input$debt == TRUE){
    plot1 <- plot1 + geom_line(aes(y = Expense+ debt_pmt , color = "Expenditures Comm-Colleges" ), size=0.6, alpha=1 )
  }
  
  plot1 
  

  })
  
 
#add text
  output$text <- renderText({
    paste("First year:",
         tot_cost_cc_year,
         "Second year: $", 
          tot_cost_cc_year)
    
    })
  


  
  
  #------#------#------#------#------#------
  
#test lab-----------------  
  set.seed(100)
  x <- rnorm(n = 200, mean = 100 , sd = 15) 
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    #x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  #test lab end------------  
  
}

shinyApp(ui, server)