#how to deploy
#rsconnect::deployApp('C:/Users/Adil/Documents/Rfiles/Shiny/bcom4933')

######-------------------global--------------###
#adapted from Ananda Mahto at https://gist.github.com/mrdwab/984707
#Global Functions
sample.size.table = function(c.lev, margin=50, c.interval=5, population) {
  z      = qnorm(.5+c.lev/200)
  m      = margin/100
  int    = c.interval/100
  ss     = (z^2 * m * (1-m))/(int^2)
  p.ss   = ss/(1 + ((ss-1)/population))
  CL     = c(paste(c.lev, "%", sep = ""))
  CI     = c(paste(c.interval, "%", sep = ""))
  M      = c(paste(margin, "%", sep = ""))
  METHOD = c("Sample size calculations")
  RES    = data.frame(population, CL, CI, M, round(p.ss, digits = 0))
  pre    = structure(list(POP = "Population", CL = "Confidence level",
                          CI = "Confidence interval (+/-)", MOE = "Margin of error",
                          SS = "Sample size", method = METHOD), class = "power.htest")
  names(RES) = c("POP","CL", "CI", "MOE", "SS")
  print(pre)
  print(RES)
}

sample.size = function(c.lev, margin=.5, c.interval=.05, population) {
  z.val  = qnorm(.5+c.lev/200)
  ss     = (z.val^2 * margin * (1-margin))/c.interval^2
  p.ss   = round((ss/(1 + ((ss-1)/population))), digits=0)
  METHOD = paste("Recommended sample size for a population of ",  population,
                 " at a ", c.lev, "% confidence level", sep = "")
  moe    = paste((c.interval*100), "%", sep="")
  resp.dist = paste((margin*100),"%", sep="")
  structure(list(Population = population, "Confidence level" = c.lev, 
                 "Margin of error" = moe, "Response distribution" = resp.dist,
                 "Recommended sample size" = p.ss, method = METHOD), class = "power.htest")
}

confidence.interval = function(c.lev, margin=.5, p.ss, population) {
  z.val      = qnorm(.5+c.lev/200)
  ss         = ((population*p.ss-p.ss)/(population-p.ss))
  c.interval = sqrt((z.val^2 * margin * (1-margin))/ss)
  r.cint     = round(c.interval*100, digits = 2)
  METHOD     = paste("The confidence interval is ", r.cint, 
                     "% with the following assumptions:", sep = "")
  resp.dist  = paste((margin*100),"%", sep="")
  structure(list(Population = population, "Sample size" = p.ss,
                 "Confidence level" = c.lev, "Response distribution" = resp.dist, 
                 method = METHOD), class = "power.htest")
}


####

numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 
######-------------------closing global functions--------------###
library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggthemes)
library(pwr)
library(semTools)
library(stringr)


header <-  dashboardHeader(title = "Data Analysis")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Power Analysis", tabName = "intro",icon = icon("home", lib="glyphicon"), 
             badgeLabel = "new", badgeColor = "green"),
    
    #menuItem("Power Analysis", icon = icon("plus", lib="glyphicon"), 
    menuItem("Sample Size Calculator", tabName = "population",icon = icon("minus", lib="glyphicon")),    
    
    menuItem("ANOVA", tabName = "anova",icon = icon("minus", lib="glyphicon")), 
    
    
    menuItem("t-Test", tabName = "ttest",icon = icon("minus", lib="glyphicon")), 
    
    
    menuItem("Correlation", tabName = "cor",icon = icon("minus", lib="glyphicon")), 
    
    
    menuItem("Regression", tabName = "reg",icon = icon("minus", lib="glyphicon")), 
    
    
    menuItem("CFA df", tabName = "sem",icon = icon("minus", lib="glyphicon")), 
    menuItem("SEM df", tabName = "sem2",icon = icon("minus", lib="glyphicon")), 
    menuItem("SEM Sample Size", tabName = "sem3",icon = icon("minus", lib="glyphicon")),
    menuItem("SEM Power Analysis", tabName = "sem4",icon = icon("minus", lib="glyphicon")),
    menuItem("Plotting SEM Power", tabName = "sem5",icon = icon("minus", lib="glyphicon")),
    #menuItem("SNA", tabName = "sna",icon = icon("asterisk", lib="glyphicon")), 
    
    
    menuItem("About Me", tabName = "about", icon = icon("pencil", lib="glyphicon"))
  )
)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
body <- dashboardBody(
  #~~~~~~~~~~~~~~~~~~~~~~~~~~  
  tabItems( #opening tabItems()
    
    ################################## 
    #-----Tab0 Introduction------#
    ######################################
    
    tabItem(tabName = "intro",
            h1("Online Calculators to Estimate Power and Sample Size"),
            h2("Use This page to illustrate to students the impact of sample size on distribution"),
            h3("On the left side you will find online tools to estimate power and sample size"),
            fluidRow(
              column(4,
                     box(title = "Inputs", status = "warning", solidHeader = TRUE, width = 12, height = NULL,
                         
                         numericInput(inputId="num",
                                      label = "Change Sample Size Here",
                                      value = 100),
                         textInput(inputId = "title", 
                                   label = "Write a title",
                                   value = "Type the histogram title here"),
                         numericInput(inputId = "val",
                                      label = "Insert breaks",
                                      value = 10),
                         actionButton(inputId = "go", 
                                      label = "Update"))),
              column(5,
                     box(title = "Outputs", status = "warning", solidHeader = TRUE, width = 12, height = NULL,
                         plotOutput("hist"),
                         verbatimTextOutput("sum"))
              )
              
            ), #closing fluidrow1
            fluidRow(
              box(title = "Statistical Power Analysis", status = "primary", solidHeader = TRUE, width = 6, height = NULL,
                  
                  tags$img(height = 406,
                           width = 500,
                           src = "statPower.png")
              ))
    ),
    
    ################################## 
    #-----Tab0 Population Sample Size Calculator------#
    ###################################### 
    tabItem(tabName = "population",
            box(width = 12, height = NULL, background = "purple",
                h1("Sample Size Calculator")),
            
            
            
            fluidRow(
              
              
              tabBox(height = "850px", width = 12,
                     tabPanel(h3("Compute Margin of Error"), 
                              column(4,
                                     
                                     
                                     #tabBox(width = 12, height = NULL,  
                                     # The id lets us use input$resuls on the server to find the current tab
                                     #id = "results", height = "600px", width = 12,
                                     box(title = "Inputs", status = "warning", solidHeader = TRUE, width = 12, height = NULL,
                                         
                                         numericInput(inputId = "population_a",
                                                      label = "Enter Population Size",
                                                      value = 2000),
                                         
                                         numericInput(inputId = "p.ss_a",
                                                      label = "Enter Sample Size",
                                                      value = 300), 
                                         
                                         sliderInput(inputId = "c.lev_a", 
                                                     label = "Enter Confidence Level", 
                                                     value = 95, min = 10, max = 100),
                                         
                                         sliderInput(inputId = "margin_a", 
                                                     label = "Enter Expected Response", 
                                                     value = 0.5, min = 0.0, max = 1.0)
                                         
                                         #actionButton(inputId = "computeAnova", 
                                         #             label = "Compute Power")
                                         
                                         
                                     ) #closing box()
                                     
                                     
                              ), #closing column1
                              column(6,
                                     box(title = "Outputs", status = "warning", solidHeader = TRUE, width = 12, height = NULL,
                                         plotOutput("population_a")
                                     )
                              ),
                              
                              box(width = 12, height = NULL,
                                  fluidRow(
                                    infoBoxOutput("margin_error_a_info"),
                                    
                                    infoBoxOutput("p.ss_a_info")
                                    
                                  ),
                                  fluidRow(
                                    infoBoxOutput("c.lev_a_info"),
                                    infoBoxOutput("margin_a_info"),
                                    infoBoxOutput("population_a_info")
                                    
                                  ))
                     ), #closing tabPanel()
                     
                     #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                     #~~~~~~~~~~~~~~~compute Sample Size
                     
                     tabPanel(h3("Compute Sample Size"), 
                              column(4,
                                     
                                     
                                     #tabBox(width = 12, height = NULL,  
                                     # The id lets us use input$resuls on the server to find the current tab
                                     #id = "results", height = "600px", width = 12,
                                     box(title = "Inputs", status = "warning", solidHeader = TRUE, width = 12, height = NULL,
                                         
                                         numericInput(inputId = "population_b",
                                                      label = "Enter Population Size",
                                                      value = 2000),
                                         
                                         sliderInput(inputId = "c.interval_b", 
                                                     label = "Margin of Error", 
                                                     value = 0.05, min = 0.0, max = .1),
                                         
                                         sliderInput(inputId = "c.lev_b", 
                                                     label = "Enter Confidence Level", 
                                                     value = 95, min = 10, max = 100),
                                         
                                         sliderInput(inputId = "margin_b", 
                                                     label = "Enter Expected Response", 
                                                     value = 0.5, min = 0.0, max = 1.0)
                                         
                                         #actionButton(inputId = "computeAnova", 
                                         #             label = "Compute Power")
                                         
                                         
                                     ) #closing box()
                                     
                                     
                              ), #closing column1
                              column(6,
                                     box(title = "Outputs", status = "warning", solidHeader = TRUE, width = 12, height = NULL,
                                         plotOutput("population_b")
                                     )
                              ),
                              box(width = 12, height = NULL,
                                  fluidRow(
                                    infoBoxOutput("recom_Sample_b_info"),
                                    #infoBoxOutput("population_b_info"),
                                    infoBoxOutput("c.interval_b_info")
                                    
                                  ),
                                  fluidRow(
                                    infoBoxOutput("c.lev_b_info"),
                                    infoBoxOutput("margin_b_info"),
                                    infoBoxOutput("population_b_info")
                                    
                                  ))
                     ) #closing tabPanel() 2
                     
                     #~~~~~~~~~~~~~~~~~~~~~~~
              ) #closing tabBox()
            )#closing fluidrow()
            
            
            
            
    ), #closing tabItem()     
    ################################## 
    #-----Tab1 Anova------#
    ######################################
    
    tabItem(tabName = "anova",
            box(width = 12, height = NULL, background = "purple",
                h1("Computing Power and Sample Size of one-way ANOVA")),
            
            
            
            fluidRow(
              
              
              tabBox(height = "850px", width = 12,
                     tabPanel(h3("Compute Power"), 
                              column(4,
                                     
                                     
                                     #tabBox(width = 12, height = NULL,  
                                     # The id lets us use input$resuls on the server to find the current tab
                                     #id = "results", height = "600px", width = 12,
                                     box(title = "Inputs", status = "warning", solidHeader = TRUE, width = 12, height = NULL,
                                         numericInput(inputId = "grpsAnova",
                                                      label = "Enter Number of Groups",
                                                      value = 3),
                                         
                                         numericInput(inputId = "sampleAnova",
                                                      label = "Enter Sample Size",
                                                      value = 30), 
                                         
                                         sliderInput(inputId = "effectAnova", 
                                                     label = "Enter Effect Size", 
                                                     value = 0.3, min = 0.0, max = 1.0),
                                         
                                         sliderInput(inputId = "sigAnova", 
                                                     label = "Choose Signifiance Level", 
                                                     value = 0.05, min = 0.0, max = 0.1)
                                         
                                         #actionButton(inputId = "computeAnova", 
                                         #             label = "Compute Power")
                                         
                                         
                                     ) #closing box()
                                     
                                     
                              ), #closing column1
                              column(6,
                                     box(title = "Outputs", status = "warning", solidHeader = TRUE, width = 12, height = NULL,
                                         plotOutput("anova")
                                     )
                              ),
                              
                              box(width = 12, height = NULL,
                                  fluidRow(
                                    infoBoxOutput("power"),
                                    infoBoxOutput("nSize")
                                    
                                  ),
                                  fluidRow(
                                    infoBoxOutput("sigLevel"),
                                    infoBoxOutput("numGrps")
                                    #infoBoxOutput("eSize")
                                    
                                  ))
                     ), #closing tabPanel()
                     
                     #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                     #~~~~~~~~~~~~~~~compute Sample Size
                     
                     tabPanel(h3("Compute Sample Size"), 
                              column(4,
                                     
                                     
                                     #tabBox(width = 12, height = NULL,  
                                     # The id lets us use input$resuls on the server to find the current tab
                                     #id = "results", height = "600px", width = 12,
                                     box(title = "Inputs", status = "warning", solidHeader = TRUE, width = 12, height = NULL,
                                         
                                         sliderInput(inputId = "pwrAnova2", 
                                                     label = "Enter Desired Power", 
                                                     value = 0.8, min = 0.0, max = 1.0),
                                         
                                         sliderInput(inputId = "effectAnova2", 
                                                     label = "Enter Effect Size", 
                                                     value = 0.3, min = 0.0, max = 1.0),
                                         
                                         
                                         sliderInput(inputId = "sigAnova2", 
                                                     label = "Choose Signifiance Level", 
                                                     value = 0.05, min = 0.0, max = 0.1),
                                         
                                         numericInput(inputId = "grpsAnova2",
                                                      label = "Enter Number of Groups",
                                                      value = 3)
                                         
                                         
                                         #actionButton(inputId = "computeAnova", 
                                         #             label = "Compute Power")
                                         
                                         
                                     ) #closing box()
                                     
                                     
                              ), #closing column1
                              column(6,
                                     box(title = "Outputs", status = "warning", solidHeader = TRUE, width = 12, height = NULL,
                                         plotOutput("anova2")
                                     )
                              ),
                              box(width = 12, height = NULL,
                                  fluidRow(
                                    infoBoxOutput("power2"),
                                    infoBoxOutput("nSize2")
                                    
                                  ),
                                  fluidRow(
                                    infoBoxOutput("sigLevel2"),
                                    infoBoxOutput("numGrps2")
                                    #infoBoxOutput("eSize")
                                    
                                  ))
                     ) #closing tabPanel() 2
                     
                     #~~~~~~~~~~~~~~~~~~~~~~~
              ) #closing tabBox()
            )#closing fluidrow()
            
            
            
            
    ), #closing tabItem()
    
    #############################################   
    #-----Tab2 t-Test------#
    #############################################
    
    
    tabItem(tabName = "ttest",
            box(width = 12, height = NULL, background = "purple",
                h1("Computing Power and Sample Size for t-Test")),
            
            
            
            fluidRow(
              
              
              tabBox(height = "850px", width = 12,
                     tabPanel(h3("Compute Power"), 
                              column(4,
                                     
                                     
                                     #tabBox(width = 12, height = NULL,  
                                     # The id lets us use input$resuls on the server to find the current tab
                                     #id = "results", height = "600px", width = 12,
                                     box(title = "Inputs", status = "warning", solidHeader = TRUE, width = 12, height = NULL,
                                         
                                         selectInput("type1", "Choose Type of Test:", 
                                                     choices = c("1-Sample t"="one.sample", "2-Sample t"="two.sample", "Paired t"="paired")),
                                         
                                         selectInput("alternate1", "Choose an alternative hypothesis:", 
                                                     choices = c("Two-Sided"="two.sided", "Greater"="greater", "Less"="less")),
                                         
                                         numericInput(inputId = "sampleTtest1",
                                                      label = "Enter Sample Size",
                                                      value = 30),
                                         
                                         
                                         sliderInput(inputId = "effectTtest1", 
                                                     label = "Enter Effect Size", 
                                                     value = 0.3, min = 0.0, max = 1.0),
                                         
                                         sliderInput(inputId = "sigTtest1", 
                                                     label = "Choose Signifiance Level", 
                                                     value = 0.05, min = 0.0, max = 0.1)
                                         
                                         #actionButton(inputId = "computeAnova", 
                                         #             label = "Compute Power")
                                         
                                         
                                     ) #closing box()
                                     
                                     
                              ), #closing column1
                              column(6,
                                     box(title = "Outputs", status = "warning", solidHeader = TRUE, width = 12, height = NULL,
                                         plotOutput("tTest1")
                                     )
                              ),
                              
                              box(width = 12, height = NULL,
                                  fluidRow(
                                    infoBoxOutput("powerTtest1"),
                                    infoBoxOutput("nSizeTtest1")
                                    
                                  ),
                                  fluidRow(
                                    infoBoxOutput("sigLevelTtest1"),
                                    infoBoxOutput("eSizeTtest1")
                                    
                                  ))
                     ), #closing tabPanel()
                     
                     #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                     #~~~~~~~~~~~~~~~compute Sample Size
                     
                     tabPanel(h3("Compute Sample Size"), 
                              column(4,
                                     
                                     
                                     #tabBox(width = 12, height = NULL,  
                                     # The id lets us use input$resuls on the server to find the current tab
                                     #id = "results", height = "600px", width = 12,
                                     box(title = "Inputs", status = "warning", solidHeader = TRUE, width = 12, height = NULL,
                                         
                                         selectInput("type2", "Choose Type of Test:", 
                                                     choices = c("1-Sample t"="one.sample", "2-Sample t"="two.sample", "Paired t"="paired")),
                                         
                                         selectInput("alternate2", "Choose an alternative hypothesis:", 
                                                     choices = c("Two-Sided"="two.sided", "Greater"="greater", "Less"="less")),
                                         
                                         sliderInput(inputId = "pwrTtest2", 
                                                     label = "Enter Desired Power", 
                                                     value = 0.8, min = 0.0, max = 1.0),
                                         
                                         
                                         sliderInput(inputId = "effectTtest2", 
                                                     label = "Enter Effect Size", 
                                                     value = 0.3, min = 0.0, max = 1.0),
                                         
                                         sliderInput(inputId = "sigTtest2", 
                                                     label = "Choose Signifiance Level", 
                                                     value = 0.05, min = 0.0, max = 0.1)
                                         
                                         #actionButton(inputId = "computeAnova", 
                                         #             label = "Compute Power")
                                         
                                         
                                     ) #closing box()
                                     
                                     
                              ), #closing column1
                              column(6,
                                     box(title = "Outputs", status = "warning", solidHeader = TRUE, width = 12, height = NULL,
                                         plotOutput("tTest2")
                                     )
                              ),
                              
                              box(width = 12, height = NULL,
                                  fluidRow(
                                    infoBoxOutput("powerTtest2"),
                                    infoBoxOutput("nSizeTtest2")
                                    
                                  ),
                                  fluidRow(
                                    infoBoxOutput("sigLevelTtest2"),
                                    infoBoxOutput("eSizeTtest2")
                                    
                                  ))
                     ) #closing tabPanel()
                     
                     #~~~~~~~~~~~~~~~~~~~~~~~
              ) #closing tabBox()
            )#closing fluidrow()
            
            
            
            
    ), #closing tabItem()
    
    
    #############################################   
    #-----Tab3 Correlation------#
    #############################################
    
    
    tabItem(tabName = "cor",
            box(width = 12, height = NULL, background = "purple",
                h1("Correlation Power Calculation with arctanh Transformation")),
            
            
            
            fluidRow(
              
              
              tabBox(height = "850px", width = 12,
                     tabPanel(h3("Compute Power"), 
                              column(4,
                                     
                                     box(title = "Inputs", status = "warning", solidHeader = TRUE, width = 12, height = NULL,
                                         
                                         numericInput(inputId = "sampleCor1",
                                                      label = "Enter Sample Size",
                                                      value = 50),
                                         
                                         sliderInput(inputId = "cor1", 
                                                     label = "Choose correlation coefficient", 
                                                     value = 0.4, min = 0.0, max = 1.0),
                                         
                                         sliderInput(inputId = "sigCor1", 
                                                     label = "Choose Significance Level", 
                                                     value = 0.05, min = 0.0, max = 0.1),
                                         
                                         selectInput("corAlternate1", "Choose an alternative hypothesis:", 
                                                     choices = c("Two-Sided"="two.sided", "Greater"="greater", "Less"="less"))
                                         
                                         
                                         
                                         
                                         
                                     ) #closing box()
                                     
                                     
                              ), #closing column1
                              column(6,
                                     box(title = "Outputs", status = "warning", solidHeader = TRUE, width = 12, height = NULL,
                                         plotOutput("cor1")
                                     )
                              ),
                              
                              box(width = 12, height = NULL,
                                  fluidRow(
                                    infoBoxOutput("powerCor1"),
                                    infoBoxOutput("nSizeCor1")
                                    
                                  ),
                                  fluidRow(
                                    infoBoxOutput("sigLevelCor1"),
                                    infoBoxOutput("corCoefficient1")
                                    
                                  ))
                     ), #closing tabPanel()
                     
                     #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                     #~~~~~~~~~~~~~~~compute Sample Size
                     
                     tabPanel(h3("Compute Sample Size"), 
                              column(4,
                                     
                                     box(title = "Inputs", status = "warning", solidHeader = TRUE, width = 12, height = NULL,
                                         
                                         sliderInput(inputId = "corPower2", 
                                                     label = "Choose Desired Power", 
                                                     value = 0.8, min = 0.0, max = 1.0),
                                         
                                         sliderInput(inputId = "cor2", 
                                                     label = "Choose correlation coefficient", 
                                                     value = 0.4, min = 0.0, max = 1.0),
                                         
                                         sliderInput(inputId = "sigCor2", 
                                                     label = "Choose Significance Level", 
                                                     value = 0.05, min = 0.0, max = 0.1),
                                         
                                         selectInput("corAlternate2", "Choose an alternative hypothesis:", 
                                                     choices = c("Two-Sided"="two.sided", "Greater"="greater", "Less"="less"))
                                         
                                         
                                         
                                         
                                         
                                     ) #closing box()
                                     
                                     
                              ), #closing column1
                              column(6,
                                     box(title = "Outputs", status = "warning", solidHeader = TRUE, width = 12, height = NULL,
                                         plotOutput("cor2")
                                     )
                              ),
                              
                              box(width = 12, height = NULL,
                                  fluidRow(
                                    infoBoxOutput("powerCor2"),
                                    infoBoxOutput("nSizeCor2")
                                    
                                  ),
                                  fluidRow(
                                    infoBoxOutput("sigLevelCor2"),
                                    infoBoxOutput("corCoefficient2")
                                    
                                  ))
                     ) #closing tabPanel()
                     
                     #~~~~~~~~~~~~~~~~~~~~~~~
              ) #closing tabBox()
            )#closing fluidrow()
            
            
            
            
    ), #closing tabItem()
    #########################################    
    #-----Tab4 Regression------#
    #########################################
    
    #-----Tab5 SEM------#
    
    tabItem(tabName = "sem",
            h1("Computing Degree of Freedom for CFA"),
            #h2("Use This page to illustrate to students the impact of sample size on distribution"),
            #h3("On the left side you will find online tools to estimate power and sample size"),
            fluidRow(
              column(4,
                     box(title = "Inputs", status = "warning", solidHeader = TRUE, width = 12, height = NULL,
                         
                         numericInput(inputId="manifest",
                                      label = "Enter Number of Manifest Variables (Items)",
                                      value = 6),
                         
                         numericInput(inputId = "latent",
                                      label = "Enter Number of Latent Variables",
                                      value = 2),
                         actionButton(inputId = "goCFA", 
                                      label = "Calculate df"))),
              column(5,
                     box(title = "Computed Degree of Freedom (df) CFA", status = "warning", solidHeader = TRUE, width = 12, height = NULL,
                         #plotOutput("hist"),
                         verbatimTextOutput("dfCFA"))
              )
              
            ), #closing fluidrow
            fluidRow(
              box(title = "Formula for Computing CFA degree of freedom", status = "primary", solidHeader = TRUE, width = 12, height = NULL,
                  
                  tags$img(height = 406,
                           width = 600,
                           src = "cfaFormula.png")
              ))
    ),
    
    #-----Tab6 SEM2------#
    
    tabItem(tabName = "sem2",
            h1("Computing Degree of Freedom for SEM"),
            #h2("Use This page to illustrate to students the impact of sample size on distribution"),
            #h3("On the left side you will find online tools to estimate power and sample size"),
            fluidRow(
              column(4,
                     box(title = "Inputs", status = "warning", solidHeader = TRUE, width = 12, height = NULL,
                         
                         numericInput(inputId="manifest2",
                                      label = "Enter Number of Manifest Variables (Items)",
                                      value = 6),
                         
                         numericInput(inputId = "latent2",
                                      label = "Enter Number of Exogenous Variables",
                                      value = 2),
                         numericInput(inputId = "gFree",
                                      label = "Enter free paths of 'g'",
                                      value = 2),
                         numericInput(inputId = "bFree",
                                      label = "Enterfree paths of 'b'",
                                      value = 2),
                         actionButton(inputId = "goSEM", 
                                      label = "Calculate df"))),
              column(5,
                     box(title = "Computed Degree of Freedom (df) SEM", status = "warning", solidHeader = TRUE, width = 10, height = NULL,
                         #plotOutput("hist"),
                         verbatimTextOutput("dfSEM"))
              )
              
            ), #closing fluidrow
            fluidRow(
              box(title = "Formula for Computing SEM degree of freedom", status = "primary", solidHeader = TRUE, width = 10, height = NULL,
                  
                  tags$img(height = 603,
                           width = 705,
                           src = "semFormula.png")
              ))
    ),
    
    #-----Tab6 SEM3 - power for sem------#
    
    tabItem(tabName = "sem3",
            h1("Computing Minimum Sample Size for SEM using semTools package"),
            #h2("Use This page to illustrate to students the impact of sample size on distribution"),
            #h3("On the left side you will find online tools to estimate power and sample size"),
            fluidRow(
              column(4,
                     box(title = "Inputs", status = "warning", solidHeader = TRUE, width = 12, height = NULL,
                         
                         numericInput(inputId="rmsea0",
                                      label = "Null RMSEA",
                                      value = .05),
                         
                         numericInput(inputId = "rmseaA",
                                      label = "Alternative RMSEA",
                                      value = .08 ),
                         numericInput(inputId = "df",
                                      label = "Model degrees of freedom",
                                      value = NULL),
                         numericInput(inputId = "power",
                                      label = "Desired statistical power",
                                      value = .8),
                         numericInput(inputId = "alpha",
                                      label = "Alpha level used in power calculations",
                                      value = .05),
                         numericInput(inputId = "group",
                                      label = "Number of Groups",
                                      value = 1),                         
                         actionButton(inputId = "goPower", 
                                      label = "Calculate Sample Size"))),
              column(5,
                     box(title = "Computed Degree of Freedom (df) SEM", status = "warning", solidHeader = TRUE, width = 10, height = NULL,
                         #plotOutput("hist"),
                         verbatimTextOutput("powerSEM"))
              )
              
            ), #closing fluidrow
            fluidRow(
              box(title = "Information", status = "primary", solidHeader = TRUE, width = 10, height = NULL,
                  tags$h3("Analysis was computed using R package 'semTools;
                          Jorgensen, T. D., Pornprasertmanit, S., Schoemann, A. M., & Rosseel, Y. (2018). semTools: Useful
                          tools for structural equation modeling. R package version 0.5-1. Retrieved from
                          https://CRAN.R-project.org/package=semTools"),
                  
                  tags$h3("Fromula used from 
                          MacCallum, R. C., Browne, M. W., & Sugawara, H. M. (1996). Power analysis and determination of sample size for covariance structure modeling. Psychological Methods, 1(2), 130-149.
                          doi:10.1037/1082-989X.1.2.130")
              ))
    ),  
    
    
    ####
    #-----Tab6 SEM4 - power for sem------#
    
    tabItem(tabName = "sem4",
            h1("Statistical Power Analysis Based on Population RMSEA using semTools package"),
            #h2("Use This page to illustrate to students the impact of sample size on distribution"),
            #h3("On the left side you will find online tools to estimate power and sample size"),
            fluidRow(
              column(4,
                     box(title = "Inputs", status = "warning", solidHeader = TRUE, width = 12, height = NULL,
                         
                         numericInput(inputId="rmsea0_b",
                                      label = "Null RMSEA",
                                      value = .05),
                         
                         numericInput(inputId = "rmseaA_b",
                                      label = "Alternative RMSEA",
                                      value = .08 ),
                         numericInput(inputId = "df_b",
                                      label = "Model degrees of freedom",
                                      value = NULL),
                         numericInput(inputId = "n_b",
                                      label = "Sample Size of a dataset",
                                      value = NULL),
                         numericInput(inputId = "alpha_b",
                                      label = "Alpha level used in power calculations",
                                      value = .05),
                         numericInput(inputId = "group_b",
                                      label = "Number of Groups",
                                      value = 1),                         
                         actionButton(inputId = "goPower_b", 
                                      label = "Calculate Power"))),
              column(5,
                     box(title = "Computed Degree of Freedom (df) SEM", status = "warning", solidHeader = TRUE, width = 10, height = NULL,
                         #plotOutput("hist"),
                         verbatimTextOutput("powerSEM_b"))
              )
              
            ), #closing fluidrow
            fluidRow(
              box(title = "Information", status = "primary", solidHeader = TRUE, width = 10, height = NULL,
                  tags$h3("Analysis was computed using R package 'semTools;
                          Jorgensen, T. D., Pornprasertmanit, S., Schoemann, A. M., & Rosseel, Y. (2018). semTools: Useful
                          tools for structural equation modeling. R package version 0.5-1. Retrieved from
                          https://CRAN.R-project.org/package=semTools"),
                  
                  tags$h3("Fromula used from 
                          MacCallum, R. C., Browne, M. W., & Sugawara, H. M. (1996). Power analysis and determination of sample size for covariance structure modeling. Psychological Methods, 1(2), 130-149.
                          doi:10.1037/1082-989X.1.2.130")
              ))
    ),  
    ################
    #########################################    
    #-----Tab4 Regression------#
    #########################################
    
    tabItem(tabName = "reg",
            h1("F Tests - Computing Sample Size for Regression"),
            #h2("Use This page to illustrate to students the impact of sample size on distribution"),
            #h3("On the left side you will find online tools to estimate power and sample size"),
            fluidRow(
              column(4,
                     box(title = "Inputs", status = "warning", solidHeader = TRUE, width = 12, height = NULL,
                         
                         numericInput(inputId="predictors",
                                      label = "Enter Number of Independent Variables",
                                      value = 2),
                         sliderInput(inputId = "effectReg", 
                                     label = "Enter Effet Size", 
                                     value = 0.15, min = 0.0, max = 1.0),
                         
                         sliderInput(inputId = "sigReg", 
                                     label = "Choose Significance Level", 
                                     value = 0.05, min = 0.0, max = 0.1),
                         sliderInput(inputId = "powerReg", 
                                     label = "Enter Desired Power", 
                                     value = 0.8, min = 0.0, max = 1.0),
                         actionButton(inputId = "goReg", 
                                      label = "Estimate Sample"))),
              column(5,
                     box(title = "Minimum Recommended Sample Size", status = "warning", solidHeader = TRUE, width = 12, height = NULL,
                         #plotOutput("hist"),
                         verbatimTextOutput("regOutput"))
              )
              
            ), #closing fluidrow
            fluidRow(
              box(title = "General Info for Estimating Sample in Regression", status = "primary", solidHeader = TRUE, width = 12, height = NULL,
                  
                  tags$img(height = 303,
                           width = 600,
                           src = "regFormula.png")
              ))
            
    ),
    
    ############
    ################
    #-----Tab6 SEM4 - power for sem------#
    
    tabItem(tabName = "sem5",
            h1("Plotting Power Curves for RMSEA using semTools package"),
            #h2("Use This page to illustrate to students the impact of sample size on distribution"),
            #h3("On the left side you will find online tools to estimate power and sample size"),
            fluidRow(
              column(4,
                     box(title = "Inputs", status = "warning", solidHeader = TRUE, width = 12, height = NULL,
                         
                         numericInput(inputId="rmsea0_c",
                                      label = "Null RMSEA",
                                      value = .025),                      
                         numericInput(inputId = "rmseaA_c",
                                      label = "Alternative RMSEA",
                                      value = .075 ),
                         numericInput(inputId = "df_c",
                                      label = "Model degrees of freedom",
                                      value = NULL),
                         numericInput(inputId = "nlow",
                                      label = "Lower Sample Size",
                                      value = 100),
                         numericInput(inputId = "nhigh",
                                      label = "Upper Sample Size",
                                      value = 500),
                         numericInput(inputId = "steps",
                                      label = "Sample Size Increment",
                                      value = 10),									  
                         numericInput(inputId = "alpha_c",
                                      label = "Alpha level used in power calculations",
                                      value = .05),
                         numericInput(inputId = "group_c",
                                      label = "Number of Groups",
                                      value = 1),                         
                         actionButton(inputId = "goPower_c", 
                                      label = "Plot Power"))),
              column(5,
                     box(title = "Plotted Power Curves for RMSEA", status = "warning", solidHeader = TRUE, width = 18, height = NULL,
                         plotOutput("powerSEM_c"))
                     #verbatimTextOutput("powerSEM_c"))
              )
              
            ), #closing fluidrow
            fluidRow(
              box(title = "Information", status = "primary", solidHeader = TRUE, width = 10, height = NULL,
                  tags$h3("Analysis was computed using R package 'semTools;
                          Jorgensen, T. D., Pornprasertmanit, S., Schoemann, A. M., & Rosseel, Y. (2018). semTools: Useful
                          tools for structural equation modeling. R package version 0.5-1. Retrieved from
                          https://CRAN.R-project.org/package=semTools"),
                  
                  tags$h3("Fromula used from 
                          MacCallum, R. C., Browne, M. W., & Sugawara, H. M. (1996). Power analysis and determination of sample size for covariance structure modeling. Psychological Methods, 1(2), 130-149.
                          doi:10.1037/1082-989X.1.2.130")
              ))
    ),     
    #-----Tab7 About------#
    
    tabItem(tabName = "about",
            tags$h2("Adil Al-Busaidi"),
            tags$h3("Assistant Professor at Sultan Qaboos University"),
            tags$h4("aalbusaidi@gmail.com"),
            tags$br(),
            
            tags$p("I used the following packages to develop this app..."),
            strong(tags$h3(tags$code("pwr,semTools,ggthemes,ggplot2,shinydashboard")))
    )
    #~~~~~~
  ) #Closing tabItems()
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  
) #closing dashboardBody(

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
ui <- dashboardPage(skin = "purple",
                    header, sidebar, body)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
server <- function(input, output) {
  #########################################
  ##########Anova##########################
  ano1 <- reactive({
    p1 <- pwr.anova.test(f=input$effectAnova,
                         k=input$grpsAnova,
                         n=input$sampleAnova,
                         sig.level=input$sigAnova)
  })
  
  #infobox results~~~~~
  output$power <- renderInfoBox({
    infoBox(
      #count reTweets
      "Computed Power", paste("Estimated:",round(ano1()$power,3), " "),
      icon = icon("flash", lib = "glyphicon"),fill = FALSE, color = "purple")
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$nSize <- renderInfoBox({
    infoBox(
      #count reTweets
      "Sample Size", paste("N:",ano1()$n, " "),
      icon = icon("equalizer", lib = "glyphicon"),fill = FALSE, color = "yellow")
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$sigLevel <- renderInfoBox({
    infoBox(
      #count reTweets
      "Significance Level", paste("alpha:",ano1()$sig.level, " "),
      icon = icon("thumbs-up", lib = "glyphicon"),fill = TRUE, color = "purple")
    
  })
  #~~~~~~~~~~~~~~~
  #output$eSize <- renderInfoBox({
  #infoBox(
  #count reTweets
  #"Effect Size", paste("Size:",ano1()$f, " "),
  #  icon = icon("glyphicon glyphicon-save", lib = "glyphicon"),fill = TRUE, color = "light-blue")
  #  
  #})
  #~~~~~~~~~~~~~~~
  output$numGrps <- renderInfoBox({
    infoBox(
      #count reTweets
      "Number of Groups", paste("Groups:",ano1()$k, " "),
      icon = icon("user", lib = "glyphicon"),fill = TRUE, color = "yellow")
    
  })
  #~~~~~~~~~~~~~~~``
  #~~~~~~~~~ for anova ~~~~~~~#
  
  output$anova <- renderPlot({
    
    p2 <- ano1()
    p3 <- plot(p2)
    
    p3 + theme_solarized() +
      scale_colour_solarized("green")
    
    
  })
  
  #~~~~~~~Sample Size~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ano2 <- reactive({
    p1 <- pwr.anova.test(f=input$effectAnova2,
                         k=input$grpsAnova2,
                         power=input$pwrAnova2,
                         sig.level=input$sigAnova2)
  })
  
  #~~~~~~~~~ for anova ~~~~~~~#
  
  output$anova2 <- renderPlot({
    
    p2 <- ano2()
    p3 <- plot(p2)
    
    p3 + theme_solarized() +
      scale_colour_solarized("green")
    
    
  })
  
  #infobox results~~~~~
  output$nSize2 <- renderInfoBox({
    infoBox(
      #count reTweets
      "Estimated Sample Size", paste("N:",round(ano2()$n,2), " "),
      icon = icon("equalizer", lib = "glyphicon"),fill = FALSE, color = "purple")
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$power2 <- renderInfoBox({
    infoBox(
      #count reTweets
      "Specified Power", paste("1-B:",round(ano2()$power,2), " "),
      icon = icon("flash", lib = "glyphicon"),fill = FALSE, color = "yellow")
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$sigLevel2 <- renderInfoBox({
    infoBox(
      #count reTweets
      "Significance Level", paste("alpha:",ano2()$sig.level, " "),
      icon = icon("thumbs-up", lib = "glyphicon"),fill = TRUE, color = "purple")
    
  })
  #~~~~~~~~~~~~~~~
  #output$eSize <- renderInfoBox({
  #infoBox(
  #count reTweets
  #"Effect Size", paste("Size:",ano1()$f, " "),
  #  icon = icon("glyphicon glyphicon-save", lib = "glyphicon"),fill = TRUE, color = "light-blue")
  #  
  #})
  #~~~~~~~~~~~~~~~
  output$numGrps2 <- renderInfoBox({
    infoBox(
      #count reTweets
      "Number of Groups", paste("Groups:",ano2()$k, " "),
      icon = icon("user", lib = "glyphicon"),fill = TRUE, color = "yellow")
    
  })
  #~~~~~~~~~~~~~~~``
  #########################################
  ##########tTest##########################
  
  tTest1 <- reactive({
    p1 <- pwr.t.test(n=input$sampleTtest1,
                     d=input$effectTtest1,
                     sig.level=input$sigTtest1,
                     type=input$type1,
                     alternative=input$alternate1)
  })
  
  #infobox results~~~~~
  output$powerTtest1 <- renderInfoBox({
    infoBox(
      #count reTweets
      "Computed Power", paste("Estimated:",round(tTest1()$power,3), " "),
      icon = icon("flash", lib = "glyphicon"),fill = FALSE, color = "purple")
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$nSizeTtest1 <- renderInfoBox({
    infoBox(
      #count reTweets
      "Sample Size", paste("N:",tTest1()$n, " "),
      icon = icon("equalizer", lib = "glyphicon"),fill = FALSE, color = "yellow")
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$sigLevelTtest1 <- renderInfoBox({
    infoBox(
      #count reTweets
      "Significance Level", paste("alpha:",tTest1()$sig.level, " "),
      icon = icon("thumbs-up", lib = "glyphicon"),fill = TRUE, color = "purple")
    
  })
  #~~~~~~~~~~~~~~~
  output$eSizeTtest1 <- renderInfoBox({
    infoBox(
      "Effect Size", paste("Size:",tTest1()$d, " "),
      icon = icon("glyphicon glyphicon-save", lib = "glyphicon"),fill = TRUE, color = "light-blue")
    
  })
  #~~~~~~~~~~~~~~~
  
  #~~~~~~~~~~~~~~~``
  #~~~~~~~~~ for tTest ~~~~~~~#
  
  output$tTest1 <- renderPlot({
    
    p2 <- tTest1()
    p3 <- plot(p2)
    
    p3 + theme_solarized() +
      scale_colour_solarized("green")
    
    
  })
  ##############################################
  #~~~~~~~Sample Size~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tTest2 <- reactive({
    p1 <- pwr.t.test(power=input$pwrTtest2,
                     d=input$effectTtest2,
                     sig.level=input$sigTtest2,
                     type=input$type2,
                     alternative=input$alternate2)
  })
  
  #~~~~~~~~~ for tTest ~~~~~~~#
  
  output$tTest2 <- renderPlot({
    
    p2 <- tTest2()
    p3 <- plot(p2)
    
    p3 + theme_solarized() +
      scale_colour_solarized("green")
    
    
  })
  ################
  #infobox results~~~~~
  output$powerTtest2 <- renderInfoBox({
    infoBox(
      #count reTweets
      "Desired Power", paste("Power:",round(tTest2()$power,3), " "),
      icon = icon("flash", lib = "glyphicon"),fill = FALSE, color = "purple")
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$nSizeTtest2 <- renderInfoBox({
    infoBox(
      #count reTweets
      "Estimated Sample Size", paste("N:",round(tTest2()$n,2), " "),
      icon = icon("equalizer", lib = "glyphicon"),fill = FALSE, color = "yellow")
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$sigLevelTtest2 <- renderInfoBox({
    infoBox(
      #count reTweets
      "Significance Level", paste("alpha:",tTest1()$sig.level, " "),
      icon = icon("thumbs-up", lib = "glyphicon"),fill = TRUE, color = "purple")
    
  })
  #~~~~~~~~~~~~~~~
  output$eSizeTtest2 <- renderInfoBox({
    infoBox(
      "Effect Size", paste("Size:",tTest1()$d, " "),
      icon = icon("glyphicon glyphicon-save", lib = "glyphicon"),fill = TRUE, color = "light-blue")
    
  })
  ##############
  #########################################
  ##########Correlation##########################
  corTest1 <- reactive({
    p1 <- pwr.r.test(n=input$sampleCor1,
                     r=input$cor1,
                     sig.level=input$sigCor1,
                     alternative=input$corAlternate1)
  })
  
  #infobox results~~~~~
  output$powerCor1 <- renderInfoBox({
    infoBox(
      #count reTweets
      "Estimated Power", paste("Estimated:",round(corTest1()$power,3), " "),
      icon = icon("flash", lib = "glyphicon"),fill = FALSE, color = "purple")
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$nSizeCor1 <- renderInfoBox({
    infoBox(
      #count reTweets
      "Sample Size", paste("N:",round(corTest1()$n,2), " "),
      icon = icon("equalizer", lib = "glyphicon"),fill = FALSE, color = "yellow")
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$sigLevelCor1 <- renderInfoBox({
    infoBox(
      #count reTweets
      "Significance Level", paste("alpha:",corTest1()$sig.level, " "),
      icon = icon("thumbs-up", lib = "glyphicon"),fill = TRUE, color = "purple")
    
  })
  #~~~~~~~~~~~~~~~
  output$corCoefficient1 <- renderInfoBox({
    infoBox(
      "Correlation Coefficient", paste("r:",corTest1()$r, " "),
      icon = icon("glyphicon glyphicon-save", lib = "glyphicon"),fill = TRUE, color = "light-blue")
    
  })
  #~~~~~~~~~~~~~~~
  
  #~~~~~~~~~~~~~~~``
  #~~~~~~~~~ for correlation ~~~~~~~#
  
  output$cor1 <- renderPlot({
    
    p2 <- corTest1()
    p3 <- plot(p2)
    
    p3 + theme_solarized() +
      scale_colour_solarized("green")
    
    
  })
  ##############################################
  #~~~~~~~Sample Size~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  corTest2 <- reactive({
    p1 <- pwr.r.test(power=input$corPower2,
                     r=input$cor1,
                     sig.level=input$sigCor1,
                     alternative=input$corAlternate1)
  })
  
  #infobox results~~~~~
  output$powerCor2 <- renderInfoBox({
    infoBox(
      #count reTweets
      "Estimated Power", paste("Estimated:",round(corTest2()$power,3), " "),
      icon = icon("flash", lib = "glyphicon"),fill = FALSE, color = "purple")
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$nSizeCor2 <- renderInfoBox({
    infoBox(
      #count reTweets
      "Sample Size", paste("N:",round(corTest2()$n,2), " "),
      icon = icon("equalizer", lib = "glyphicon"),fill = FALSE, color = "yellow")
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$sigLevelCor2 <- renderInfoBox({
    infoBox(
      #count reTweets
      "Significance Level", paste("alpha:",corTest2()$sig.level, " "),
      icon = icon("thumbs-up", lib = "glyphicon"),fill = TRUE, color = "purple")
    
  })
  #~~~~~~~~~~~~~~~
  output$corCoefficient2 <- renderInfoBox({
    infoBox(
      "Correlation Coefficient", paste("r:",corTest2()$r, " "),
      icon = icon("glyphicon glyphicon-save", lib = "glyphicon"),fill = TRUE, color = "light-blue")
    
  })
  #~~~~~~~~~~~~~~~
  
  #~~~~~~~~~~~~~~~``
  #~~~~~~~~~ for correlation ~~~~~~~#
  
  output$cor2 <- renderPlot({
    
    p2 <- corTest2()
    p3 <- plot(p2)
    
    p3 + theme_solarized() +
      scale_colour_solarized("green")
    
    
  })
  
  ############################
  ###Introduction####
  ##############################################
  #~~~~~~~Sample Size~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  data <- isolate({eventReactive(input$go, {
    rnorm(input$num) #rondom dist
  })})
  
  output$hist <- renderPlot({
    hist(data(), breaks = isolate({input$val}),
         col="orange",border = "blue",
         freq = FALSE,
         main = isolate({input$title}))
    lines(density(data()), col = "black", lwd = 2)
  })
  
  output$sum <- renderPrint({
    summary(data())
  })  
  ##########
  #~~~~~~~df cfa~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dataCFA <- isolate({eventReactive(input$goCFA, {
    #rnorm(input$num) #rondom dist
    df = (input$manifest*(input$manifest+1)/2)-(2*input$manifest)- (input$latent*(input$latent-1)/2)
  })})
  
  
  output$dfCFA <- renderPrint({
    #print(dataCFA())
    paste("df =",dataCFA())
    
  })  
  
  #~~~~~~~df sem~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dataSEM <- isolate({eventReactive(input$goSEM, {
    #rnorm(input$num) #rondom dist
    df2 = (input$manifest2*(input$manifest2+1)/2)-(2*input$manifest2)- (input$latent2*(input$latent2-1)/2)- (input$gFree) - (input$bFree)
    
  })})
  
  
  output$dfSEM <- renderPrint({
    #print(dataCFA())
    paste("df =",dataSEM())
    
  })  
  
  #######sample SEM
  #~~~~~~~df sem~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dataPower <- isolate({eventReactive(input$goPower, {
    #rnorm(input$num) #rondom dist
    df3 <- findRMSEAsamplesize(rmsea0 = input$rmsea0, rmseaA = input$rmseaA, df = input$df, power = input$power,
                               alpha=input$alpha, group = input$group)    
  })})
  
  
  output$powerSEM <- renderPrint({
    #print(dataCFA())
    paste(" Minimum Sample Size =",dataPower())
    
  })  
  
  #######powerSEM power not sample
  #~~~~~~~df sem~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dataPower_b <- isolate({eventReactive(input$goPower_b, {
    #rnorm(input$num) #rondom dist
    df3_b <- findRMSEApower(rmsea0 = input$rmsea0_b, rmseaA = input$rmseaA_b, df = input$df_b, n = input$n_b,
                            alpha=input$alpha_b, group = input$group_b)    
  })})
  
  
  output$powerSEM_b <- renderPrint({
    #print(dataCFA())
    paste("Estimated Power =",round(dataPower_b(),3))
    
  })  
  #######Plot powerSEM
  #~~~~~~~plotRMSEApower~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dataPower_c <- isolate({eventReactive(input$goPower_c, {
    df3_c <- plotRMSEApower(rmsea0 = input$rmsea0_c, rmseaA = input$rmseaA_c, df = input$df_c,
                            nlow = input$nlow, nhigh = input$nhigh, steps = input$steps, alpha=input$alpha_c,
                            group=input$group_c)  })})
  
  output$powerSEM_c <- renderPlot({
    dataPower_c() 
    
  })
  ##################
  ##########margin of Error##########################
  MoE <- reactive({
    p_me1 <- confidence.interval(population=input$population_a,
                                 p.ss=input$p.ss_a,
                                 c.lev=input$c.lev_a,
                                 margin=input$margin_a)
  })
  
  #infobox results~~~~~
  output$population_a_info <- renderInfoBox({
    infoBox(
      #count reTweets
      "Population", paste("Population Size =",MoE()$"Population", " "),
      icon = icon("flash", lib = "glyphicon"),fill = FALSE, color = "purple")
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$p.ss_a_info <- renderInfoBox({
    infoBox(
      #count reTweets
      "Sample Size", paste("Sample =",MoE()$"Sample size", " "),
      icon = icon("equalizer", lib = "glyphicon"),fill = FALSE, color = "yellow")
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$c.lev_a_info <- renderInfoBox({
    infoBox(
      #count reTweets
      "Significance Level", paste("Confidence =",MoE()$"Confidence level", " "),
      icon = icon("thumbs-up", lib = "glyphicon"),fill = TRUE, color = "purple")
    
  })
  #~~~~~~~~~~~~~~~
  output$margin_a_info <- renderInfoBox({
    infoBox(
      #count reTweets
      "Expected Response Rate", paste("Size =",MoE()$"Response distribution", " "),
      icon = icon("glyphicon glyphicon-save", lib = "glyphicon"),fill = TRUE, color = "light-blue")
    
  })
  #~~~~~~~~~~~~~~~
  output$margin_error_a_info <- renderInfoBox({
    infoBox(
      #count reTweets
      "Margin of Error", paste("MoE =",as.numeric(numextract(MoE()$"method")), "%"),
      icon = icon("user", lib = "glyphicon"),fill = TRUE, color = "yellow")
    
  })
  #~~~~~~~~~~~~~~~``
  #  #~~~~~~~~~ for anova ~~~~~~~#
  #  
  #  output$anova <- renderPlot({
  #    
  #    p_me2 <- MoE()
  #    p_me3 <- plot(p_me2)
  #    
  #    p_me3 + theme_solarized() +
  #      scale_colour_solarized("green")
  #    
  #    
  #  })
  
  #~~~~~~~Sample Size~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  sampleSize <- reactive({
    p_me1_b <- sample.size(population=input$population_b,
                           c.interval=input$c.interval_b,
                           c.lev=input$c.lev_b,
                           margin=input$margin_b)
  })
  
  #infobox results~~~~~
  output$population_b_info <- renderInfoBox({
    infoBox(
      #count reTweets
      "Population", paste("Population Size =",sampleSize()$"Population", " "),
      icon = icon("flash", lib = "glyphicon"),fill = FALSE, color = "purple")
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$c.interval_b_info <- renderInfoBox({
    infoBox(
      #count reTweets
      "Margin of Error", paste("MoE =",sampleSize()$"Margin of error", " "),
      icon = icon("equalizer", lib = "glyphicon"),fill = FALSE, color = "yellow")
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$c.lev_b_info <- renderInfoBox({
    infoBox(
      #count reTweets
      "Significance Level", paste("Confidence =",sampleSize()$"Confidence level", " "),
      icon = icon("thumbs-up", lib = "glyphicon"),fill = TRUE, color = "purple")
    
  })
  #~~~~~~~~~~~~~~~
  output$margin_b_info <- renderInfoBox({
    infoBox(
      #count reTweets
      "Expected Response Rate", paste("Size =",sampleSize()$"Response distribution", " "),
      icon = icon("glyphicon glyphicon-save", lib = "glyphicon"),fill = TRUE, color = "light-blue")
    
  })
  #~~~~~~~~~~~~~~~
  output$recom_Sample_b_info <- renderInfoBox({
    infoBox(
      #count reTweets
      "Recommended Sample", paste("Size =",sampleSize()$"Recommended sample size", " "),
      icon = icon("user", lib = "glyphicon"),fill = TRUE, color = "yellow")
    
  })
  ############
  #######Sample fo Regression
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dataReg <- isolate({eventReactive(input$goReg, {
    #rnorm(input$num) #rondom dist
    pwr.f2.test(u = input$predictors, f2 = input$effectReg, sig.level = input$sigReg, power = input$powerReg)    
  })})
  
  
  output$regOutput <- renderPrint({
    #print(dataCFA())
    paste("Minimum Recommended Sample =",round(dataReg()$v,3))
    
  })   
  ##################################  
} #to close server

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  

shinyApp(ui, server)