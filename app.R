library(shiny)
library(deSolve)
library(ggplot2) 
library(shinydashboard)
library(truncnorm) # R package for Truncated normal distribution
library(EnvStats) # Package for Environmental Statistics, Including US EPA Guidance
## Sidebar content
sidebar <- dashboardSidebar(
  sidebarMenu(id = 'abc',
              menuItem("Plot Options", tabName = 'plot', icon = icon("microchip"), selected = TRUE),
              menuItem("Table", tabName = 'table', icon = icon("table")),
              menuItem("Model Structure", tabName = 'modelstructure', icon = icon("microchip")),
              menuItem('Model Parameters', tabName = 'modelparameter', icon = icon('line-chart')),
              menuItem("Code", tabName = 'code', icon = icon("code")),
              menuItem("About", tabName = 'about', icon = icon("question-circle"))
  )
)
body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = 'modelparameter',
      sidebarLayout(
        sidebarPanel(width = 2,
                     tabsetPanel(id = 'tabs', type = 'pills', # "tabs" = "tabs" in conditionalPanel
                                 tabPanel(title = 'Physiological parameters'),
                                 tabPanel(title = 'Chemical parameters')),
                     fluidRow(
                       br(),
                       conditionalPanel(condition = "input.tabs == 'Physiological parameters'",
                                        splitLayout(
                                          numericInput(inputId = 'BW.mean', label = 'BW mean', value = 299.96, 
                                                       step = 0.01, width = 100
                                          ),
                                          
                                          numericInput(inputId = 'BW.sd', label = 'BW SD', value = 46.19, 
                                                       step = 0.01, width = 100
                                          )
                                        ),
                                        splitLayout(
                                          numericInput(inputId = 'QCC.mean', label = 'QCC mean', value = 5.97,
                                                       step = 0.01, width = 100
                                          ),
                                          
                                          numericInput(inputId = 'QCC.sd', label = 'QCC SD', value = 1.99,
                                                       step = 0.01, width = 100
                                          )
                                        ),
                                        splitLayout(
                                          numericInput(inputId = 'QLC.mean', label = 'QLC mean', value = 0.405,
                                                       step = 0.01, width = 100
                                          ),
                                          
                                          numericInput(inputId = 'QLC.sd', label = 'QLC SD', value = 0.1942,
                                                       step = 0.01, width = 100
                                          )
                                        ),
                                        splitLayout(
                                          numericInput(inputId = 'QKC.mean', label = 'QKC mean', value = 0.090,
                                                       step = 0.01, width = 100
                                          ),
                                          
                                          numericInput(inputId = 'QKC.sd', label = 'QKC SD', value = 0.027,
                                                       step = 0.01, width = 100
                                          )
                                        ),
                                        splitLayout(
                                          numericInput(inputId = 'QMC.mean', label = 'QMC mean', value = 0.180,
                                                       step = 0.01, width = 100
                                          ),
                                          
                                          numericInput(inputId = 'QMC.sd', label = 'QMC SD', value = 0.054,
                                                       step = 0.01, width = 100
                                          )
                                        ),
                                        splitLayout(
                                          numericInput(inputId = 'QFC.mean', label = 'QFC mean', value = 0.080,
                                                       step = 0.01, width = 100
                                          ),
                                          
                                          numericInput(inputId = 'QFC.sd', label = 'QFC SD', value = 0.024,
                                                       step = 0.010, width = 100
                                          )
                                        ),
                                        splitLayout(
                                          numericInput(inputId = 'QrestC.mean', label = 'QrestC mean', value = 0.245,
                                                       step = 0.01, width = 100
                                          ),
                                          
                                          numericInput(inputId = 'QrestC.sd', label = 'QrestC SD', value = 0.0736,
                                                       step = 0.001, width = 100
                                          )
                                        ),
                                        splitLayout(
                                          numericInput(inputId = 'VLC.mean', label = 'VLC mean', value = 0.014,
                                                       step = 0.001, width = 100
                                          ),
                                          
                                          numericInput(inputId = 'VLC.sd', label = 'VLC SD', value = 0.00163,
                                                       step = 0.0001, width = 100
                                          )
                                        ),
                                        
                                        splitLayout(
                                          numericInput(inputId = 'VKC.mean', label = 'VKC mean', value = 0.002,
                                                       step = 0.01, width = 100
                                          ),
                                          
                                          numericInput(inputId = 'VKC.sd', label = 'VKC SD', value = 4.321e-4,
                                                       step = 0.00001, width = 100
                                          )
                                        ),
                                        
                                        splitLayout(
                                          numericInput(inputId = 'VFC.mean', label = 'VFC mean', value = 0.150,
                                                       step = 0.01, width = 100
                                          ),
                                          
                                          numericInput(inputId = 'VFC.sd', label = 'VFC SD', value = 4.5e-2,
                                                       step = 0.001, width = 100
                                          )
                                        ),
                                        
                                        splitLayout(
                                          numericInput(inputId = 'VMC.mean', label = 'VMC mean', value = 0.270,
                                                       step = 0.01, width = 100
                                          ),
                                          
                                          numericInput(inputId = 'VMC.sd', label = 'VMC SD', value = 8.1e-2,
                                                       step = 0.001, width = 100
                                          )
                                        ),
                                        
                                        splitLayout(
                                          numericInput(inputId = 'VLuC.mean', label = 'VLuC mean', value = 0.008,
                                                       step = 0.0001, width = 100
                                          ),
                                          
                                          numericInput(inputId = 'VLuC.sd', label = 'VLuC SD', value = 1.696e-3,
                                                       step = 0.0001, width = 100
                                          )
                                        ),
                                        
                                        splitLayout(
                                          numericInput(inputId = 'VvenC.mean', label = 'VvenC mean', value = 0.030,
                                                       step = 0.001, width = 100
                                          ),
                                          
                                          numericInput(inputId = 'VvenC.sd', label = 'VvenC SD', value = 8.88e-3,
                                                       step = 0.0001, width = 100
                                          )
                                        ),
                                        
                                        splitLayout(
                                          numericInput(inputId = 'VartC.mean', label = 'VartC mean', value = 0.01,
                                                       step = 0.001, width = 100
                                          ),
                                          numericInput(inputId = 'VartC.sd', label = 'VartC SD', value = 3.12e-3,
                                                       step = 0.0001, width = 100
                                          )
                                        ),
                                        splitLayout(
                                          numericInput(inputId = 'VrestC.mean', label = 'VrestC mean', value = 0.516,
                                                       step = 0.01, width = 100
                                          ),
                                          
                                          numericInput(inputId = 'VrestC.sd', label = 'VrestC SD', value = 0.1548,
                                                       step = 0.01, width = 100
                                          )
                                        )
                                        
                       ), 
                       
                       conditionalPanel(condition = "input.tabs == 'Chemical parameters'",
                                        
                                        splitLayout(
                                          numericInput(inputId = 'PL.mean', label = 'PL mean', value = 3,
                                                       step = 0.01, width = 100
                                          ),
                                          
                                          numericInput(inputId = 'PL.sd', label = 'PL SD', value = 0.6,
                                                       step = 0.01, width = 100
                                          )
                                        ),
                                        
                                        splitLayout(
                                          numericInput(inputId = 'PK.mean', label = 'PK mean', value = 2.500,
                                                       step = 0.01, width = 100
                                          ),
                                          
                                          numericInput(inputId = 'PK.sd', label = 'PK SD', value = 0.5,
                                                       step = 0.01, width = 100
                                          )
                                        ),
                                        
                                        splitLayout(
                                          numericInput(inputId = 'PM.mean', label = 'PM mean', value = 0.3,
                                                       step = 0.01, width = 100
                                          ),
                                          
                                          numericInput(inputId = 'PM.sd', label = 'PM SD', value = 0.06,
                                                       step = 0.001, width = 100
                                          )
                                        ),
                                        
                                        splitLayout(
                                          numericInput(inputId = 'PF.mean', label = 'PF mean', value = 0.04,
                                                       step = 0.0001, width = 100
                                          ),
                                          
                                          numericInput(inputId = 'PF.sd', label = 'PF SD', value = 0.008,
                                                       step = 0.0001, width = 100
                                          )
                                        ),
                                        
                                        splitLayout(
                                          numericInput(inputId = 'PLu.mean', label = 'PLu mean', value = 0.180,
                                                       step = 0.01, width = 100
                                          ),
                                          
                                          numericInput(inputId = 'PLu.sd', label = 'PLu SD', value = 0.036,
                                                       step = 0.001, width = 100
                                          )
                                        ),
                                        
                                        splitLayout(
                                          numericInput(inputId = 'Prest.mean', label = 'Prest mean', value = 0.479,
                                                       step = 0.01, width = 100
                                          ),
                                          
                                          numericInput(inputId = 'Prest.sd', label = 'Prest SD', value = 0.0958,
                                                       step = 0.001, width = 100
                                          )
                                        ),
                                        
                                        splitLayout(
                                          numericInput(inputId = 'Kim.mean', label = 'Kim mean', value = 0.070,
                                                       step = 0.001, width = 100
                                          ),
                                          
                                          numericInput(inputId = 'Kim.sd', label = 'Kim SD', value = 0.021,
                                                       step = 0.0001, width = 100
                                          )
                                        ),
                                        
                                        splitLayout(
                                          numericInput(inputId = 'Frac.mean', label = 'Frac mean', value = 0.600,
                                                       step = 0.01, width = 100
                                          ),
                                          
                                          numericInput(inputId = 'Frac.sd', label = 'Frac SD', value = 0.0600,
                                                       step = 0.001, width = 100
                                          )
                                        ),
                                        
                                        splitLayout(
                                          numericInput(inputId = 'Kdiss.mean', label = 'Kdiss mean', value = 1e-5,
                                                       step = 0.01, width = 100
                                          ),
                                          
                                          numericInput(inputId = 'Kdiss.sd', label = 'Kdiss SD', value = 3e-6,
                                                       step = 0.0000001, width = 100
                                          )
                                        ),
                                        
                                        splitLayout(
                                          numericInput(inputId = 'Ksc.mean', label = 'Ksc mean', value = 0.02,
                                                       step = 0.001, width = 100
                                          ),
                                          
                                          numericInput(inputId = 'Ksc.sd', label = 'Ksc SD', value = 0.006,
                                                       step = 0.0001, width = 100
                                          )
                                        ),
                                        
                                        splitLayout(
                                          numericInput(inputId = 'Fracsc.mean', label = 'Fracsc mean', value = 0.7,
                                                       step = 0.01, width = 100
                                          ),
                                          
                                          numericInput(inputId = 'Fracsc.sd', label = 'Fracsc SD', value = 0.07,
                                                       step = 0.001, width = 100
                                          )
                                        ),
                                        
                                        splitLayout(
                                          numericInput(inputId = 'Kdisssc.mean', label = 'Kdisssc mean', value = 1e-4,
                                                       step = 1e-5, width = 100
                                          ),
                                          
                                          numericInput(inputId = 'Kdisssc.sd', label = 'Kdisssc SD', value = 3e-5,
                                                       step = 1e-6, width = 100
                                          )
                                        ),
                                        
                                        splitLayout(
                                          numericInput(inputId = 'PB.mean', label = 'PB mean', value = 0.483,
                                                       step = 0.01, width = 100
                                          ),
                                          
                                          numericInput(inputId = 'PB.sd', label = 'PB SD', value = 0.1449,
                                                       step = 0.001, width = 100
                                          )
                                        ),
                                        
                                        splitLayout(
                                          numericInput(inputId = 'KmC.mean', label = 'KmC mean', value = 0.0025,
                                                       step = 1e-5, width = 100
                                          ),
                                          
                                          numericInput(inputId = 'KmC.sd', label = 'KmC SD', value = 7.5e-4,
                                                       step = 1e-5, width = 100
                                          )
                                        ),
                                        
                                        splitLayout(
                                          numericInput(inputId = 'KurineC.mean', label = 'KurineC mean', value = 0.45,
                                                       step = 0.001, width = 100
                                          ),
                                          
                                          numericInput(inputId = 'KurineC.sd', label = 'KurineC SD', value = 0.135,
                                                       step = 0.0001, width = 100
                                          )
                                        )
                                        
                       )
                     )
        ),
        
        mainPanel(
          tabItem(tabName = 'modelparameter',
                  fluidRow(
                    box(width = 6,downloadButton('downl','Download'),
                        title = 'Concentrations in liver', status = "primary", solidHeader = TRUE,
                        plotOutput(outputId = 'liver', height = 350)
                    ),
                    box(width = 6,downloadButton('downk','Download'),
                        title = 'Concentrations in kidney',status = "primary", solidHeader = TRUE,
                        plotOutput(outputId = 'kidney', height = 350)
                    ),
                    box(width = 6,downloadButton('downm','Download'),
                        title = 'Concentrations in muscle',status = "primary", solidHeader = TRUE,
                        plotOutput(outputId = 'muscle', height = 350)
                    ),
                    box(width = 6,downloadButton('downf','Download'),
                        title = 'Concentrations in fat',status = "primary", solidHeader = TRUE,
                        plotOutput(outputId = 'fat', height = 350)
                    )
                    
                  )
          )
        )
      )
    ),
    tabItem(
      tabName = 'plot',
      sidebarLayout(
        sidebarPanel(width = 2,
                     fluidRow(
                       radioButtons(inputId = 'animal', label = 'Species', choices = c('Cattle','Swine')
                       ),
                       radioButtons(inputId = 'drug', label = 'Drug', choices = c('Penicillin G')
                       ),
                       radioButtons(inputId = 'target', label = 'Target tissue', choices = c('Plasma','Liver','Kidney','Muscle','Fat'),
                                    selected = 'Liver'
                       ),
                       radioButtons(inputId = 'route', label = 'Administration route', choices = c('iv bolus','im','sc', 'oral'),
                                    selected = 'im'             
                       ),
                       numericInput(inputId = 'doselevel', label = 'Dose level (mg/kg)', value = 6.5, 
                                    min = 0,  step = 0.01, width = 150
                       ),
                       numericInput(inputId = 'tinterval', label = 'Dose interval (h)', value = 24, 
                                    min = 1, max = 24*10, step = 1, width = 150
                       ),
                       numericInput(inputId = 'numdose', label = 'Number of administrations', value = 5, 
                                    min = 1, max = 5*10, step = 1, width = 150
                       ),
                       numericInput(inputId = 'N', label = 'Number of animials', value = 5, min = 1, step = 1, width = 150),
                       tags$p("Labelled WDT (day): 5"),
                       tags$p("Labelled dose (mg/kg): 6.5"),
                       tags$p("Tolerance (ppm or ug/g): 0.05"),
                       actionButton(inputId = 'action',label = 'Apply Changes')
                     )
                     
                     
        ),
        mainPanel(
          tabName = 'plot',
          fluidRow(
            box(
              title = 'Extralabel Withdrawal Interval Plot',
              plotOutput(outputId = 'wdtplot'),height = 800, width = 2000,
              br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
              
              downloadButton('downloadwdt','Download')
            )
          )
        )
      )
    ),
    tabItem(
      tabName = 'table',
      box(width = 8, status = "primary", solidHeader = TRUE, title="Concentrations in tissues", 
          downloadButton('downloadtable','Download(each animal)'),
          tableOutput("table1")
          # fluidRow(
          #   tableOutput(outputId = 'table1')
      )
    ),
    tabItem(
      tabName = 'modelstructure',
      # box(width = 8, title='Model Structure', solidHeader = TRUE, status = "primary",
      #     #downloadButton('downloadp1','Download'),
      #     imageOutput(outputId = 'modelplot'))
      fluidRow(
        box(width = 3,status = "primary",title= "Model Structure", 
            solidHeader = TRUE, tags$img(src = 'https://i.imgur.com/5NbETNa.jpg'),
            tags$p("
                   Fig. 1. A schematic diagram of the physiologically based pharmacokinetic (PBPK)
                   model for penicillin G in swine and cattle. Two different administration routes
                   including intramuscular (IM) and subcutaneous (SC) injections are presented in the
                   model. A mechanistic two - compartment dissolution model is used to describe the IM
                   and SC injections of procaine penicillin G (PPG). Admin Site represents administration
                   site. (
                   For interpretation of the references to colour in this figure legend,
                   the reader is
                   referred to the web version of this article.
                   )
                   ")
        )
        
      )
    ),
    tabItem(
      tabName = 'code',
      box( width = NULL, status = "primary", solidHeader = TRUE, title="Penicillin G in Cattle.R",
           downloadButton('downloadcode', 'Download'),
           br(),br(),
           pre(includeText("Penicillin_Cattle.R"))
      )
    )
  )
) 

ui <- dashboardPage(
  skin = 'purple',
  dashboardHeader(
    title = 'Extralabel Withdrawal Interval Simulator', titleWidth = 400
  ),
  sidebar,
  body
)
# { 
#   Monte Carlo Analysis based on Penicillin PBPK model for Cattle (flow-limited model, linear metabolism equation, plasma protein binding)
#   The PBPK model code is based on the Oxytetracycline.mmd from Zhoumeng Lin
# }
server <- function(input, output) {
  r1 <- reactive({
    library(deSolve) # R package for numerical treatment of systems of differential equations. 
    library(truncnorm) # R package for Truncated normal distribution
    library(EnvStats) # Package for Environmental Statistics, Including US EPA Guidance
    library(ggplot2) 
    N = input$N # 1000, Number of iterations in the Monte Carlo simulation
    starttime <- 0  # start time(h)
    stoptime <- 24*15 # 15  simulation stop time that is depending on the withdrawal time
    dtout = 0.01 # 0.01
    Time <- seq(starttime, stoptime, dtout)
    # Dosing, repeated doses
    input$action
    tinterval = isolate(input$tinterval)			# Varied dependent on the exposure paradigm (h)
    Tdose = isolate(input$numdose) 			# times for multiple oral gavage
    #{Parameters for Various Exposure Scenarios}
    
    PDOSEsc = 0			# (mg/kg)
    PDOSEim = isolate(input$doselevel) 	# (mg/kg)
    
    CL =  CLu =  CK =  CM =  CF = CV  = matrix(NA,(stoptime / dtout + 1), N)
    #plot.new()
    
    for (i in 1:N) {
      set.seed(1000+i) # set random seed so that the simulation result is reproducible, because randomly generated data is same if you set same random seed.
      ##{Physiological Parameters}
      # Blood Flow Rates
      QCC = rtruncnorm(
        n = 1,
        a = qnorm(0.025, mean = input$QCC.mean, sd = input$QCC.sd),
        b = qnorm(0.975, mean = input$QCC.mean, sd = input$QCC.sd),
        mean = input$QCC.mean,
        sd = input$QCC.sd
      )  # Cardiac output index (L/h/kg)
      # Fracion of blood flow to organs (unitless) 
      QLC = rtruncnorm(
        n = 1,
        a = qnorm(0.025, mean = input$QLC.mean, sd = input$QLC.sd),
        b = qnorm(0.975, mean = input$QLC.mean, sd = input$QLC.sd),
        mean = input$QLC.mean,
        sd = input$QLC.sd
      )
      # Fraction of blood flow to the kidneys (2016 Lin)
      QKC = rtruncnorm(
        n = 1,
        a = qnorm(0.025,
                  mean = input$QKC.mean,
                  sd =  input$QKC.sd),
        b = qnorm(0.975,
                  mean = input$QKC.mean,
                  sd =  input$QKC.sd),
        mean = input$QKC.mean,
        sd =  input$QKC.sd
      )
      #QMC = 0.180			# Fraction of blood flow to the muscle (2016 Lin)
      QMC = rtruncnorm(
        n = 1,
        a = qnorm(0.025,
                  mean = input$QMC.mean,
                  sd =  input$QMC.sd),
        b = qnorm(0.975, mean = input$QMC.mean, sd =  input$QMC.sd),
        mean = input$QMC.mean,
        sd =  input$QMC.sd
      )
      # Fraction of blood flow to the fat (2016 Lin)
      QFC = rtruncnorm(
        n = 1,
        a = qnorm(0.025, mean = input$QFC.mean, sd =  input$QFC.sd),
        b = qnorm(0.975, mean = input$QFC.mean, sd =  input$QFC.sd),
        mean = input$QFC.mean,
        sd =  input$QFC.sd
      )
      QLuC = 1			# Fraction of blood flow to the lung considered to be 1
      # Fraction of blood flow to the rest of body (total sum equals to 1)
      QrestC = rtruncnorm(
        n = 1,
        a = qnorm(0.025, mean = input$QrestC.mean, sd = input$QrestC.sd),
        b = qnorm(0.975, mean = input$QrestC.mean, sd = input$QrestC.sd),
        mean = input$QrestC.mean,
        sd = input$QrestC.sd
      )
      sum.Q <- QLC + QKC + QMC + QFC + QrestC # sum up cardiac output fraction
      QLC <- QLC / sum.Q # adjusted blood flow rate fraction to liver
      QKC <- QKC / sum.Q # adjusted blood flow rate fraction to kidney
      QMC <- QMC / sum.Q # adjusted blood flow rate fraction to muscle
      QFC <- QFC / sum.Q # adjusted blood flow rate fraction to fat
      QrestC <-
        QrestC / sum.Q # adjusted blood flow rate fraction to rest of body
      # Tissue Volumes
      #BW = 299.96			# Body Weight (kg) (2011 Mirzaei)
      
      BW = rtruncnorm(n = 1, 
                      a = qnorm(0.025, mean = input$BW.mean, sd = input$BW.sd), 
                      b = qnorm(0.975, mean = input$BW.mean, sd = input$BW.sd), 
                      mean = input$BW.mean, sd = input$BW.sd) 
      # Fractional organ tissue volumes (unitless)
      # Fractional liver tissue (1933 Swett)
      VLC = rtruncnorm(
        n = 1,
        a = qnorm(0.025, mean = input$VLC.mean, sd =  input$VLC.sd),
        b = qnorm(0.975, mean = input$VLC.mean, sd =  input$VLC.sd) ,
        mean = input$VLC.mean,
        sd =  input$VLC.sd
      )
      # Fractional kidney tissue (1933 Swett)
      VKC = rtruncnorm(
        n = 1,
        a = qnorm(0.025, mean = input$VKC.mean, sd =  input$VKC.sd),
        b = qnorm(0.975, mean = input$VKC.mean, sd =  input$VKC.sd),
        mean = input$VKC.mean,
        sd =  input$VKC.sd
      )
      
      # Fractional fat tissue (2016 Lin, 2014 Leavens)
      VFC = rtruncnorm(
        n = 1,
        a = qnorm(0.025, mean = input$VFC.mean, sd =  input$VFC.sd),
        b = qnorm(0.975, mean = input$VFC.mean, sd =  input$VFC.sd),
        mean = input$VFC.mean,
        sd =  input$VFC.sd
      )
      # Fractional muscle tissue (2016 Lin, 2014 Leavens)
      VMC = rtruncnorm(
        n = 1,
        a = qnorm(0.025, mean = input$VMC.mean, sd =  input$VMC.sd),
        b = qnorm(0.975, mean = input$VMC.mean, sd =  input$VMC.sd),
        mean = input$VMC.mean,
        sd =  input$VMC.sd
      )
      # Fractional lung tissue (2016 Lin, 2014 Leavens)
      VLuC = rtruncnorm(
        n = 1,
        a = qnorm(0.025, mean = input$VLuC.mean, sd =  input$VLuC.sd),
        b = qnorm(0.975, mean = input$VLuC.mean, sd =  input$VLuC.sd),
        mean = input$VLuC.mean,
        sd =  input$VLuC.sd
      )
      # Venous blood volume, fraction of blood volume (2016 Lin# 2008 Leavens)
      VvenC = rtruncnorm(
        n = 1,
        a = qnorm(0.025, mean = input$VvenC.mean, sd =  input$VvenC.sd),
        b = qnorm(0.975, mean = input$VvenC.mean, sd =  input$VvenC.sd),
        mean = input$VvenC.mean,
        sd =  input$VvenC.sd
      )
      # Arterial blood volume, fraction of blood volume (2016 Lin# 2008 Leavens)
      VartC = rtruncnorm(
        n = 1,
        a = qnorm(0.025, mean = input$VartC.mean, sd =  input$VartC.sd),
        b = qnorm(0.975, mean = input$VartC.mean, sd =  input$VartC.sd),
        mean = input$VartC.mean,
        sd =  input$VartC.sd
      )
      # Fractional rest of body (total sum equals to 1)
      VrestC = rtruncnorm(
        n = 1,
        a = qnorm(0.025, mean = input$VrestC.mean, sd =  input$VrestC.sd),
        b = qnorm(0.975, mean = input$VrestC.mean, sd =  input$VrestC.sd),
        mean = input$VrestC.mean,
        sd =  input$VrestC.sd
      )
      sum.V <- VLC + VKC + VMC + VFC + VLuC + VvenC + VartC + VrestC# sum up the tissue volumes
      VLC <- VLC / sum.V # adjusted fraction of tissue volume of liver
      VKC <- VKC / sum.V # adjusted fraction of tissue volume of kidney
      VMC <- VMC / sum.V # adjusted fraction of tissue volume of muscle
      VFC <- VFC / sum.V # adjusted fraction of tissue volume of fat
      VLuC <- VLuC / sum.V # adjusted fraction of tissue volume of lung
      VartC <- VartC / sum.V
      VvenC <- VvenC / sum.V
      VrestC <- VrestC / sum.V # adjusted fraction of tissue volume of rest of body
      #{Mass Transfer Parameters (Chemical-Specific Parameters)}
      # Partition Coefficients (PC, tissue:plasma)
      # Liver:plasma PC (0.157, Tsuji et al., 1983, Table 4, in rats)
      
      PL.mean <- input$PL.mean # mean value
      PL.sd <- input$PL.sd # standard deviation
      sd.log.PL <-
        sqrt(log(1 + PL.sd ^ 2 / PL.mean ^ 2)) # standard deviation of lognormal distribution
      m.log.PL <-
        log(PL.mean) - 0.5 * sd.log.PL ^ 2 # mean of lognormal distribution
      PL = rlnormTrunc(
        1,
        meanlog = m.log.PL,
        sdlog = sd.log.PL,
        min = qlnorm(0.025, meanlog = m.log.PL, sdlog = sd.log.PL),
        max = qlnorm(0.975, meanlog = m.log.PL, sdlog = sd.log.PL)
      )
      
      PK.mean <- input$PK.mean # mean value
      PK.sd <- input$PK.sd # standard deviation
      sd.log.PK <-
        sqrt(log(1 + PK.sd ^ 2 / PK.mean ^ 2)) # standard deviation of lognormal distribution
      m.log.PK <-
        log(PK.mean) - 0.5 * sd.log.PK ^ 2 # mean of lognormal distribution
      PK = rlnormTrunc(
        1,
        meanlog = m.log.PK,
        sdlog = sd.log.PK,
        min = qlnorm(0.025, meanlog = m.log.PK, sdlog = sd.log.PK),
        max = qlnorm(0.975, meanlog = m.log.PK, sdlog = sd.log.PK)
      )
      PM.mean <- input$PM.mean # mean value
      PM.sd <- input$PM.sd # standard deviation
      sd.log.PM <-
        sqrt(log(1 + PM.sd ^ 2 / PM.mean ^ 2)) # standard deviation of lognormal distribution
      m.log.PM <-
        log(PM.mean) - 0.5 * sd.log.PM ^ 2 # mean of lognormal distribution
      PM = rlnormTrunc(
        1,
        meanlog = m.log.PM,
        sdlog = sd.log.PM,
        min = qlnorm(0.025, meanlog = m.log.PM, sdlog = sd.log.PM),
        max = qlnorm(0.975, meanlog = m.log.PM, sdlog = sd.log.PM)
      )
      
      PF.mean <- input$PF.mean # mean value
      PF.sd <- input$PF.sd # standard deviation
      sd.log.PF <-
        sqrt(log(1 + PF.sd ^ 2 / PF.mean ^ 2)) # standard deviation of lognormal distribution
      m.log.PF <-
        log(PF.mean) - 0.5 * sd.log.PF ^ 2 # mean of lognormal distribution
      PF = rlnormTrunc(
        1,
        meanlog = m.log.PF,
        sdlog = sd.log.PF,
        min = qlnorm(0.025, meanlog = m.log.PF, sdlog = sd.log.PF),
        max = qlnorm(0.975, meanlog = m.log.PF, sdlog = sd.log.PF)
      )
      
      PLu.mean <- input$PLu.mean # mean value
      PLu.sd <- input$PLu.sd # standard deviation
      sd.log.PLu <-
        sqrt(log(1 + PLu.sd ^ 2 / PLu.mean ^ 2)) # standard deviation of lognormal distribution
      m.log.PLu <-
        log(PLu.mean) - 0.5 * sd.log.PLu ^ 2 # mean of lognormal distribution
      PLu = rlnormTrunc(
        1,
        meanlog = m.log.PLu,
        sdlog = sd.log.PLu,
        min = qlnorm(0.025, meanlog = m.log.PLu, sdlog = sd.log.PLu),
        max = qlnorm(0.975, meanlog = m.log.PLu, sdlog = sd.log.PLu)
      )
      
      Prest.mean <- input$Prest.mean # mean value
      Prest.sd <- input$Prest.sd # standard deviation
      sd.log.Prest <-
        sqrt(log(1 + Prest.sd ^ 2 / Prest.mean ^ 2)) # standard deviation of lognormal distribution
      m.log.Prest <-
        log(Prest.mean) - 0.5 * sd.log.Prest ^ 2 # mean of lognormal distribution
      Prest = rlnormTrunc(
        1,
        meanlog = m.log.Prest,
        sdlog = sd.log.Prest,
        min = qlnorm(0.025, meanlog = m.log.Prest, sdlog = sd.log.Prest),
        max = qlnorm(0.975, meanlog = m.log.Prest, sdlog = sd.log.Prest)
      )
      #{Kinetic Constants}
      # IM Absorption Rate Constants
      # /h, IM absorption rate constant
      
      Kim.mean <- input$Kim.mean # mean value
      Kim.sd <- input$Kim.sd # standard deviation
      sd.log.Kim <-
        sqrt(log(1 + Kim.sd ^ 2 / Kim.mean ^ 2)) # standard deviation of lognormal distribution
      m.log.Kim <-
        log(Kim.mean) - 0.5 * sd.log.Kim ^ 2 # mean of lognormal distribution
      Kim = rlnormTrunc(
        1,
        meanlog = m.log.Kim,
        sdlog = sd.log.Kim,
        min = qlnorm(0.025, meanlog = m.log.Kim, sdlog = sd.log.Kim),
        max = qlnorm(0.975, meanlog = m.log.Kim, sdlog = sd.log.Kim)
      )
      
      Frac.mean <- input$Frac.mean # mean value
      Frac.sd <- input$Frac.sd # standard deviation
      sd.log.Frac <-
        sqrt(log(1 + Frac.sd ^ 2 / Frac.mean ^ 2)) # standard deviation of lognormal distribution
      m.log.Frac <-
        log(Frac.mean) - 0.5 * sd.log.Frac ^ 2 # mean of lognormal distribution
      Frac = rlnormTrunc(
        1,
        meanlog = m.log.Frac,
        sdlog = sd.log.Frac,
        min = qlnorm(0.025, meanlog = m.log.Frac, sdlog = sd.log.Frac),
        max = qlnorm(0.975, meanlog = m.log.Frac, sdlog = sd.log.Frac)
      )
      
      Kdiss.mean <- input$Kdiss.mean # mean value
      Kdiss.sd <- input$Kdiss.sd # standard deviation
      sd.log.Kdiss <-
        sqrt(log(1 + Kdiss.sd ^ 2 / Kdiss.mean ^ 2)) # standard deviation of lognormal distribution
      m.log.Kdiss <-
        log(Kdiss.mean) - 0.5 * sd.log.Kdiss ^ 2 # mean of lognormal distribution
      Kdiss = rlnormTrunc(
        1,
        meanlog = m.log.Kdiss,
        sdlog = sd.log.Kdiss,
        min = qlnorm(0.025, meanlog = m.log.Kdiss, sdlog = sd.log.Kdiss),
        max = qlnorm(0.975, meanlog = m.log.Kdiss, sdlog = sd.log.Kdiss)
      )
      # SC Absorption Rate Constants
      # /h, SC absorption rate constant
      Ksc.mean <- input$Ksc.mean # mean value
      Ksc.sd <- input$Ksc.sd # standard deviation
      sd.log.Ksc <-
        sqrt(log(1 + Ksc.sd ^ 2 / Ksc.mean ^ 2)) # standard deviation of lognormal distribution
      m.log.Ksc <-
        log(Ksc.mean) - 0.5 * sd.log.Ksc ^ 2 # mean of lognormal distribution
      Ksc = rlnormTrunc(
        1,
        meanlog = m.log.Ksc,
        sdlog = sd.log.Ksc,
        min = qlnorm(0.025, meanlog = m.log.Ksc, sdlog = sd.log.Ksc),
        max = qlnorm(0.975, meanlog = m.log.Ksc, sdlog = sd.log.Ksc)
      )
      
      
      Fracsc.mean <- input$Fracsc.mean # mean value
      Fracsc.sd <- input$Fracsc.sd # standard deviation
      sd.log.Fracsc <-
        sqrt(log(1 + Fracsc.sd ^ 2 / Fracsc.mean ^ 2)) # standard deviation of lognormal distribution
      m.log.Fracsc <-
        log(Fracsc.mean) - 0.5 * sd.log.Fracsc ^ 2 # mean of lognormal distribution
      Fracsc = rlnormTrunc(
        1,
        meanlog = m.log.Fracsc,
        sdlog = sd.log.Fracsc,
        min = qlnorm(0.025, meanlog = m.log.Fracsc, sdlog = sd.log.Fracsc),
        max = qlnorm(0.975, meanlog = m.log.Fracsc, sdlog = sd.log.Fracsc)
      )
      
      
      Kdisssc.mean <- input$Kdisssc.mean # mean value
      Kdisssc.sd <- input$Kdisssc.sd # standard deviation
      sd.log.Kdisssc <-
        sqrt(log(1 + Kdisssc.sd ^ 2 / Kdisssc.mean ^ 2)) # standard deviation of lognormal distribution
      m.log.Kdisssc <-
        log(Kdisssc.mean) - 0.5 * sd.log.Kdisssc ^ 2 # mean of lognormal distribution
      Kdisssc = rlnormTrunc(
        1,
        meanlog = m.log.Kdisssc,
        sdlog = sd.log.Kdisssc,
        min = qlnorm(0.025, meanlog = m.log.Kdisssc, sdlog = sd.log.Kdisssc),
        max = qlnorm(0.975, meanlog = m.log.Kdisssc, sdlog = sd.log.Kdisssc)
      )
      # Percentage Plasma Protein Binding unitless
      
      PB.mean <- input$PB.mean # mean value
      PB.sd <- input$PB.sd # standard deviation
      sd.log.PB <-
        sqrt(log(1 + PB.sd ^ 2 / PB.mean ^ 2)) # standard deviation of lognormal distribution
      m.log.PB <-
        log(PB.mean) - 0.5 * sd.log.PB ^ 2 # mean of lognormal distribution
      PB = rlnormTrunc(
        1,
        meanlog = m.log.PB,
        sdlog = sd.log.PB,
        min = qlnorm(0.025, meanlog = m.log.PB, sdlog = sd.log.PB),
        max = qlnorm(0.975, meanlog = m.log.PB, sdlog = sd.log.PB)
      )
      Free = 1 - PB  			# Percentage of drug not bound to plasma protein
      
      #{Metabolic Rate Constant}
      
      KmC.mean <- input$KmC.mean # mean value
      KmC.sd <- input$KmC.sd # standard deviation
      sd.log.KmC <-
        sqrt(log(1 + KmC.sd ^ 2 / KmC.mean ^ 2)) # standard deviation of lognormal distribution
      m.log.KmC <-
        log(KmC.mean) - 0.5 * sd.log.KmC ^ 2 # mean of lognormal distribution
      KmC = rlnormTrunc(
        1,
        meanlog = m.log.KmC,
        sdlog = sd.log.KmC,
        min = qlnorm(0.025, meanlog = m.log.KmC, sdlog = sd.log.KmC),
        max = qlnorm(0.975, meanlog = m.log.KmC, sdlog = sd.log.KmC)
      )
      # Urinary Elimination Rate Constants
      
      KurineC.mean <- input$KurineC.mean # mean value
      KurineC.sd <- input$KurineC.sd # standard deviation
      sd.log.KurineC <-
        sqrt(log(1 + KurineC.sd ^ 2 / KurineC.mean ^ 2)) # standard deviation of lognormal distribution
      m.log.KurineC <-
        log(KurineC.mean) - 0.5 * sd.log.KurineC ^ 2 # mean of lognormal distribution
      KurineC = rlnormTrunc(
        1,
        meanlog = m.log.KurineC,
        sdlog = sd.log.KurineC,
        min = qlnorm(0.025, meanlog = m.log.KurineC, sdlog = sd.log.KurineC),
        max = qlnorm(0.975, meanlog = m.log.KurineC, sdlog = sd.log.KurineC)
      )
      
      
      #{Cardiac output and blood flow to tissues (L/h)}
      QC = QCC*BW 			# Cardiac output
      QL = QLC*QC 			# Liver
      QK = QKC*QC 			# Kidney
      QF = QFC*QC 			# Fat
      QM = QMC*QC 			# Muscle
      QLu = QLuC*QC 			# Lung
      QR = QrestC*QC 			# Rest of body
      
      #{Tissue volues (L)}
      VL = VLC*BW 			# Liver
      VK = VKC*BW 			# Kidney
      VF = VFC*BW 			# Fat
      VM = VMC*BW 			# Muscle
      VLu = VLuC*BW 			# Lung
      VR = VrestC*BW 			# Rest of body
      Vven = VvenC*BW 			# Venous Blood
      Vart = VartC*BW 			# Arterial Blood
      
      # Metabolism Rate Constants
      Kmet = KmC*BW
      
      # Urinary Elimination Rate Constants
      Kurine = KurineC*BW
      
      #{Dosing}
      # Dosing caculation based on BW
      DOSEsc = PDOSEsc*BW 	# (mg)
      DOSEim = PDOSEim*BW 	# (mg)
      tlen = dtout
      
      pbpkmodel <- function(Time, State, Parameters) {
        with(as.list(c(State, Paras)), {
          dosingperiod <- Time <= Tdose * tinterval-dtout
          Rinputim  <- DOSEim/tlen * (Time %% tinterval < tlen) * dosingperiod
          Rpenim = Rinputim*(1-Frac)#
          Rppgim =  Rinputim*Frac#
          Rim = Kim*Amtsiteim
          dAbsorbim = Rim
          dAmtsiteim = Rpenim- Rim + Kdiss* DOSEppgim
          dDOSEppgim = Rppgim-Kdiss* DOSEppgim
          # Dosing, SC, subcutaneous
          Rinputsc = DOSEsc*(Time %% tinterval < dtout)*dosingperiod
          Rpensc = Rinputsc*(1-Fracsc)#
          Rppgsc =  Rinputsc*Fracsc
          Rsc = Ksc*Amtsitesc
          dAbsorbsc = Rsc
          dAmtsitesc = Rpensc- Rsc + Kdisssc* DOSEppgsc
          dDOSEppgsc = Rppgsc-Kdisssc* DOSEppgsc
          
          ## Concentrations in the tissues and in the capillary blood of the tissues
          
          CV = AV/Vven			# CV drug concentration in the venous blood (mg/L)
          CA = AA/Vart			# CAfree concentration of unbound drug in the arterial blood (mg/L)
          
          CL = AL/VL      # Concentration of parent drug in the tissue of liver
          CVL = CL/PL     # Concentration of parent drug in the capillary blood of liver
          
          CLu = ALu/VLu   # Concentration of parent drug in the tissue of lung
          CVLu = CLu/PLu  # Concentration of parent drug in the capillary blood of lung
          
          CK = AK/VK      # Concentration of parent drug in the tissue of kidney
          CVK = CK/PK     # Concentration of parent drug in the capillary blood of kidney 
          
          CM = AM/VM      # Concentration of parent drug in the tissue of muscle
          CVM = CM/PM     # Concentration of parent drug in the capillary blood of muscle 
          
          CF = AF/VF      # Concentration of parent drug in the tissue of fat
          CVF = CF/PF     # Concentration of parent drug in the capillary blood of  fat
          
          CR = AR/VR			# Crest drug concentration in the rest of the body (mg/L)
          CVR = AR/(VR*Prest) # Concentration of parent drug in the capillary blood of the rest of body
          
          #{Penicillin distribution in each compartment}
          # Penicillin in venous blood compartment
          RV = (QL*CVL+QK*CVK+QF*CVF+QM*CVM+QR*CVR+Rsc+Rim)-QC*CV# RV the changing rate in the venous blood (mg/h)
          dAV = RV			# AV the amount of the drug in the venous blood (mg)
          CAfree = CA*Free
          RA = QC*(CVLu-CAfree)		# RA the changing rate in the arterial blood (mg/h)
          dAA = RA
          
          dAUCCV = CV		# AUCCV AUC of drug concentration in the venous blood (mg*h/L)
          ABlood = AA+AV
          # Penicillin in liver compartment, flow-limited model
          Rmet = Kmet*CL*VL		# Rmet the metabolic rate in liver (mg/h)
          dAmet = Rmet		# Amet the amount of drug metabolized in liver (mg)
          RL = QL*(CAfree-CVL)-Rmet 	# RL the changing rate of the amount of drug in liver (mg/h)
          dAL = RL 			# AL amount of drug in liver (mg) 
          dAUCCL = CL		# AUCCL area under the curve of drug concentration in liver (mg*h/L)
          # Metabolism of Penicillin in liver compartment
          
          # Penicillin in kidney compartment, flow-limited model
          Rurine = Kurine*CVK
          dAurine = Rurine
          RK = QK*(CAfree-CVK)-Rurine	# RK the changing rate of the amount of drug in kidney (mg/h)
          dAK = RK			# AK amount of drug in kidney (mg)
          dAUCCK = CK		# AUCCK AUC of drug concentration in kidney (mg*h/L)
          # Penicillin urinary excretion
          
          # Penicillin in muscle compartment, flow-limited model
          RM = QM*(CAfree-CVM) 	# RM the changing rate of the amount of drug in muscle (mg/h) 
          dAM = RM			# AM amount of the drug in muscle (mg)
          dAUCCM = CM
          # Penicillin in fat compartment, flow-limited model
          RF = QF*(CAfree-CVF) 		# RF the changing rate of the amount of drug in fat (mg/h)
          dAF = RF			# AF amount of the drug in fat (mg)
          dAUCCF = CF		# AUCCF AUC of drug concentration in fat (mg*h/L)
          # Penicillin in the compartment of rest of body, flow-limited model
          RR = QR*(CAfree-CVR) 		# Rrest the changing rate of the amount of drug in the rest of the body (mg/h)
          dAR = RR			# Arest amount of the drug in the rest of the body (mg)
          dAUCCR = CR		# AUCCrest AUC of drug concentration in the rest of the body (mg*h/L)
          # Penicillin in lung compartment, flow-limited model
          RLu = QLu*(CV-CVLu)		# RLu the changing rate of the amount of drug in the lung (mg/h)
          dALu = RLu			# ALu amount of the drug in the lung (mg)
          dAUCCLu = CLu		# AUCCLu AUC of drug concentration in the lung (mg*h/L)
          #{Mass balance equations}
          Qbal = QC-QM-QR-QF-QK-QL
          Tmass = ABlood+AM+ALu+AR+AF+AK+AL+Aurine+Amet
          Input = Absorbim+Absorbsc
          Bal = Input-Tmass
          list(c(dAbsorbim, dAmtsiteim, dDOSEppgim,  dAbsorbsc, dAmtsitesc, dDOSEppgsc,
                 dAV, dAA, dAUCCV, dAL, dAUCCL, dAmet, dAK, dAUCCK, dAurine, dAM, dAUCCM, 
                 dAF, dAUCCF, dAR, dAUCCR, dALu, dAUCCLu))
        })
      }
      
      State <- c(Absorbim = 0, Amtsiteim = 0, DOSEppgim = 0, Absorbsc = 0, Amtsitesc = 0, DOSEppgsc = 0,
                 AV = 0, AA = 0, AUCCV = 0, AL = 0, AUCCL = 0, Amet = 0, AK = 0, AUCCK = 0, Aurine = 0, 
                 AM = 0, AUCCM = 0, AF = 0, AUCCF = 0, AR = 0, AUCCR = 0, ALu = 0, AUCCLu = 0)
      
      Paras <- c(QC, QL, QK, QM, QF, QR)  # parameter constants
      # solution of ODE systems
      out <- ode(y = State,
                 times = Time, 
                 func = pbpkmodel, 
                 parms = Paras,
                 method = 'lsoda') 
      pbpkout <- as.data.frame(out)
      ## Check balance for parent drug
      Tmass = pbpkout$AA + pbpkout$AV + pbpkout$AL + pbpkout$AK + pbpkout$Aurine + 
        pbpkout$AM + pbpkout$AF + pbpkout$AR + pbpkout$ALu + pbpkout$Amet
      Bal = pbpkout$Absorbim + pbpkout$Absorbsc - Tmass
      # Check balance for the major metabolite ciprofloxacin
      
      
      #################################################################################
      ## calculate the concentration of the parent drug and the major metabolite in vein compartment
      CV[,i] = pbpkout$AV/Vven # concentratin of parent drug in vein
      ## Liver
      CL[,i] = pbpkout$AL/VL # concentratin of parent drug in the tissue of liver
      ## Kidney
      CK[,i] = pbpkout$AK/VK # concentratin of parent drug in the tissue of kidney
      ## Muslce
      CM[,i] = pbpkout$AM/VM # concentratin of parent drug in the tissue of muscle
      ## Fat
      CF[,i] = pbpkout$AF/VF # concentratin of parent drug in the tissue of fat
      
      
    }
    filename='Penicillin-Cattle'
    timejump=seq(from = 1, to = length(Time), by = 0.1/dtout) 
    CV = as.data.frame(CV[timejump,]) # concentration in vein of 0.1 hour time interval
    names(CV) <- paste0('cv',1:N) 
    CL = as.data.frame(CL[timejump,]) # concentration in liver of 0.1 hour time interval
    names(CL) <- paste0('cl',1:N)
    CK = as.data.frame(CK[timejump,]) # concentration in kidney of 0.1 hour time interval
    names(CK) <- paste0('ck',1:N)
    CM = as.data.frame(CM[timejump,]) # concentration in muscle of 0.1 hour time interval
    names(CM) <- paste0('cm',1:N)
    CF = as.data.frame(CF[timejump,]) # concentration in fat of 0.1 hour time interval
    names(CF) <- paste0('cf',1:N)
    Time1=Time[timejump]/24 - Tdose + 1 # change time to daily scale and start from last injection
    data_each <- data.frame(CV,CL,CK,CM,CF)
    # Kidney
    CK.01 <- as.data.frame(apply(CK,1,function(x) quantile(x,prob = c(0.01))))
    CK.m <- as.data.frame(apply(CK, 1, median))
    CK.99 <- as.data.frame(apply(CK,1,function(x) quantile(x,prob = c(0.99))))
    CK.data <- cbind(CK.01, CK.m, CK.99)
    names(CK.data) = c('CK1', 'CKM','CK99')
    
    
    # Liver
    CL.01 <- as.data.frame(apply(CL,1,function(x) quantile(x,prob = c(0.01))))
    CL.m <- as.data.frame(apply(CL, 1, median))
    CL.99 <- as.data.frame(apply(CL,1,function(x) quantile(x,prob = c(0.99))))
    CL.data <- cbind(CL.01, CL.m, CL.99)
    names(CL.data) = c('CL1', 'CLM','CL99' )
    
    # Muscle
    CM.01 <- as.data.frame(apply(CM,1,function(x) quantile(x,prob = c(0.01))))
    CM.m <- as.data.frame(apply(CM, 1, median))
    CM.99 <- as.data.frame(apply(CM,1,function(x) quantile(x,prob = c(0.99))))
    CM.data <- cbind(CM.01, CM.m, CM.99)
    names(CM.data) = c('CM1', 'CMM','CM99' )
    
    # Vein
    CV.01 <- as.data.frame(apply(CV,1,function(x) quantile(x,prob = c(0.01))))
    CV.m <- as.data.frame(apply(CV, 1, median))
    CV.99 <- as.data.frame(apply(CV,1,function(x) quantile(x,prob = c(0.99))))
    CV.data <- cbind(CV.01, CV.m, CV.99)
    names(CV.data) = c('CV1', 'CVM','CV99' )
    
    # Fat
    CF.01 <- as.data.frame(apply(CF,1,function(x) quantile(x,prob = c(0.01))))
    CF.m <- as.data.frame(apply(CF, 1, median))
    CF.99 <- as.data.frame(apply(CF,1,function(x) quantile(x,prob = c(0.99))))
    CF.data <- cbind(CF.01, CF.m, CF.99)
    names(CF.data) = c('CF1', 'CFM','CF99' )
    
    TOL=0.05
    data_tissue <- data.frame(Time1, CV.data,CL.data,CK.data,CM.data,CF.data)
    
    return(data_tissue)
    
  })
  plot_liver <- reactive({
    TOL=0.05
    # ggplot2; draw the concentration curve.
    a=ggplot(r1(), aes(Time1)) + 
      geom_ribbon(aes(ymin = CL1,
                      ymax = CL99
      ), fill = 'red',
      show.legend = T, 
      size = 0.2,
      alpha = 0.3) +  # alpha is transparency parameter. 
      geom_line(aes(y = CLM,
                    color = 'Median'), 
                size = 1, 
                show.legend = T) + # draw the mean value to the chart.
      geom_line(aes(y = CL99,
                    color = '99 Percentile'), 
                size = 1, 
                show.legend = T) +  
      geom_line(aes(y = CL1,
                    color = '1 Percentile'), 
                size = 1, 
                show.legend = T) + 
      scale_x_continuous(name = c('Time (Day)'),breaks = c((-4):15)) + 
      ylab(expression(paste('Concentration (',mu,'g/mL)'))) + 
      geom_line(aes(y = TOL),color = 'black',size = 0.5, linetype = 'twodash', show.legend = F) + # add tolerance line
      scale_y_log10() + theme_bw() + theme(axis.text=element_text(size=16))
    return(a)
  })
  plot_muscle <- reactive({
    TOL=0.05
    # ggplot2; draw the concentration curve.
    ggplot(r1(), aes(Time1)) + 
      geom_ribbon(aes(ymin = CM1,
                      ymax = CM99
      ), fill = 'red',
      show.legend = T, 
      size = 0.2,
      alpha = 0.3) +  # alpha is transparency parameter. 
      geom_line(aes(y = CMM,
                    color = 'Median'), 
                size = 1, 
                show.legend = T) + # draw the mean value to the chart.
      geom_line(aes(y = CM99,
                    color = '99 Percentile'), 
                size = 1, 
                show.legend = T) +  
      geom_line(aes(y = CM1,
                    color = '1 Percentile'), 
                size = 1, 
                show.legend = T) + 
      scale_x_continuous(name = c('Time (Day)'),breaks = c((-4):15)) + 
      ylab(expression(paste('Concentration (',mu,'g/mL)'))) + 
      geom_line(aes(y = TOL),color = 'black',size = 0.5, linetype = 'twodash', show.legend = F) + # add tolerance line
      scale_y_log10() + theme_bw() + theme(axis.text=element_text(size=16))
  })
  plot_kidney <- reactive({
    TOL=0.05
    # ggplot2; draw the concentration curve.
    ggplot(r1(), aes(Time1)) + 
      geom_ribbon(aes(ymin = CK1,
                      ymax = CK99
      ), fill = 'red',
      show.legend = T, 
      size = 0.2,
      alpha = 0.3) +  # alpha is transparency parameter. 
      geom_line(aes(y = CKM,
                    color = 'Median'), 
                size = 1, 
                show.legend = T) + # draw the mean value to the chart.
      geom_line(aes(y = CK99,
                    color = '99 Percentile'), 
                size = 1, 
                show.legend = T) +  
      geom_line(aes(y = CK1,
                    color = '1 Percentile'), 
                size = 1, 
                show.legend = T) + 
      scale_x_continuous(name = c('Time (Day)'),breaks = c((-4):15)) + 
      ylab(expression(paste('Concentration (',mu,'g/mL)'))) + 
      geom_line(aes(y = TOL),color = 'black',size = 0.5, linetype = 'twodash', show.legend = F) + # add tolerance line
      scale_y_log10() + theme_bw() + theme(axis.text=element_text(size=16))
  })
  plot_fat <- reactive({
    TOL=0.05
    # ggplot2; draw the concentration curve.
    ggplot(r1(), aes(Time1)) + 
      geom_ribbon(aes(ymin = CF1,
                      ymax = CF99
      ), fill = 'red',
      show.legend = T, 
      size = 0.2,
      alpha = 0.3) +  # alpha is transparency parameter. 
      geom_line(aes(y = CFM,
                    color = 'Median'), 
                size = 1, 
                show.legend = T) + # draw the mean value to the chart.
      geom_line(aes(y = CF99,
                    color = '99 Percentile'), 
                size = 1, 
                show.legend = T) +  
      geom_line(aes(y = CF1,
                    color = '1 Percentile'), 
                size = 1, 
                show.legend = T) + 
      scale_x_continuous(name = c('Time (Day)'),breaks = c((-4):15)) + 
      ylab(expression(paste('Concentration (',mu,'g/mL)'))) + 
      geom_line(aes(y = TOL),color = 'black',size = 0.5, linetype = 'twodash', show.legend = F) + # add tolerance line
      scale_y_log10() + theme_bw() + theme(axis.text=element_text(size=16))
  })
  plot_plasma <- reactive({
    TOL=0.05
    # ggplot2; draw the concentration curve.
    ggplot(r1(), aes(Time1)) + 
      geom_ribbon(aes(ymin = CV1,
                      ymax = CV99
      ), fill = 'red',
      show.legend = T, 
      size = 0.2,
      alpha = 0.3) +  # alpha is transparency parameter. 
      geom_line(aes(y = CVM,
                    color = 'Median'), 
                size = 1, 
                show.legend = T) + # draw the mean value to the chart.
      geom_line(aes(y = CV99,
                    color = '99 Percentile'), 
                size = 1, 
                show.legend = T) +  
      geom_line(aes(y = CV1,
                    color = '1 Percentile'), 
                size = 1, 
                show.legend = T) + 
      scale_x_continuous(name = c('Time (Day)'),breaks = c((-4):15)) + 
      ylab(expression(paste('Concentration (',mu,'g/mL)'))) + 
      geom_line(aes(y = TOL),color = 'black',size = 0.5, linetype = 'twodash', show.legend = F) + # add tolerance line
      scale_y_log10() + theme_bw() + theme(axis.text=element_text(size=16))
  })
  
  output$fat <- renderPlot({
    print(plot_fat())
  })
  output$liver <- renderPlot({
    plot_liver() 
  })
  output$muscle <- renderPlot({
    print(plot_muscle())
  })
  output$kidney <- renderPlot({
    print(plot_kidney())
  })
  r2 <- reactive({
    p <- switch(input$target,
                Liver = plot_liver(),
                Plasma = plot_plasma(),
                Kidney = plot_kidney(),
                Muscle = plot_muscle(),
                Fat = plot_fat())
    p
  })
  output$table1 <- renderTable ({
    N <- (dim(r1())[2]-16)/5
    datatable <- r1()
    names(datatable)[5*N+1] <- c('Time (Day)')
    print(datatable[1:30,(-1):(-N)])
    
  })
  
  output$downloadtable <- downloadHandler(
    filename = 'table.csv',
    content = function(file) {
      write.csv(r1(),file)
    }
  )
  
  output$wdtplot <- renderPlot({
    input$action
    a=isolate(r2())
    a
  })
  
  output$downl <- downloadHandler(
    filename = "Concentrations in liver.jpg",
    content = function(file) {
      ggsave(file, plot_liver())
    },
    contentType = 'image/jpg'
  )
  
  output$downk <- downloadHandler(
    filename = "Concentrations in kidney.jpg",
    content = function(file) {
      ggsave(file, plot_kidney())
    },
    contentType = 'image/jpg'
  )
  
  output$downm <- downloadHandler(
    filename =  "Concentrations in muscle.jpg",
    content = function(file) {
      ggsave(file, plot_muscle())
    },
    contentType = 'image/jpg'
  )
  
  output$downf <- downloadHandler(
    filename = "Concentrations in fat.jpg",
    content = function(file) {
      ggsave(file, plot_fat())
    },
    contentType = 'image/jpg'
  )
  
  output$downloadwdt <- downloadHandler(
    filename = 'concentration.jpg',
    content = function(file) {
      ggsave(file, r2())
    },
    contentType = 'image/jpg'
  )
  
  output$downloadcode <- downloadHandler(
    filename = "Penicillin_Cattle.R",
    content = function(file) {
      con        = file("Penicillin_Cattle.R", open = "r")
      lines      = readLines(con)
      close(con)
      write(lines, file)
    }
  )
}


shinyApp(ui = ui, server = server)