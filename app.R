# linear regression shiny application
# made with wiredjs and xkcd
# 
# 
# 
# # to implement: 
# 
# display bivariate regression model with ggplot
# regression output with sjplot / sjstat
# select regression types? 
# 
# 
# 
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinydashboardPlus)
library(wired)
library(xkcd)
# library(extrafont)
library(DT)
library(psych)
library(magrittr)
# loadfonts()

ui <- fluidPage(
    titlePanel("Regression by Hand"),
    sidebarLayout(sidebarPanel(
        fileInput("FileInput", "Input Your Data Set (Must be .csv)", 
                 placeholder = "No file selected",
                 accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
                 ), #fileinput

        tags$br(),
        tags$p(tags$b("Build your Model:")), 
        wired_radio(
            inputId = "rgrssn", label = "Regression:",
            choices = c("linear" = "linear",
                        "logistic" = "logistic")
        ), # radio
        wired_toggle(inputId = "rbst", label = "Robust Standard Errors"),
        tags$p("Select your variables for analysis:"),
        # checkbox input
        tags$p("Your DV / Response Variable:"),
        wired_select(inputId = "responsevar",
                     label = "Dependent Varibale:", 
                     choices = textOutput(outputId = "variable_names")
                     ),
        # checkbox input
        tags$p("Your IV / Predictor Variable(s):"),
        # wired_select(inputId = "predictor",
        #              label = "Independent Variable(s):", 
        #              choices = textOutput(outputId = "variable_names"), 
        #              multiple = TRUE),
        # selectizeInput(inputId, label, choices, selected = NULL, multiple = FALSE,
        #                options = NULL),
        # or just include "multiple = TRUE"
        #tags$p(textOutput(outputId = "variable_names"))
        wired_select(
             inputId = "clstr",
             label = "Clusters?",
             choices = c("None",
                         "Fixed Effects",
                         "Cluster Standard Errors",
                         "Multilevel Model / LME")
             ),
        tags$br(), 
        tags$br(),
        tags$p(textOutput(outputId = "variable_names"))
    ), #sidebar panel
    mainPanel(
        tabsetPanel(
            tabPanel("About",
                     tags$p("This is intended to be a toy point-and-click-style regression tool to practice R Shiny application development and to enumerate the complexities available in regression analysis. Like R, this tool comes with absolutely no warranty. "),
                     tags$p("Use the tool by uploading your own data set in .csv format (.xlsx functionality coming soon). Browse your data and examine the variables' descriptive statistics, as well as the table of correlations, then create your model to run. "),
                     tags$p("The sketchy nature of the application is intended to deter its use for actual serious purposes and strengthen the feeling of it being a back-of-the-envelope tool for regression analysis.")
                     ),
            tabPanel("Table",
                     DT::dataTableOutput("table")
                     ), 
            tabPanel("Describe Data", 
                     DT::dataTableOutput("description")
                     ),
            tabPanel("Correlations",
                     tags$p("SjPlot's Correlations go here")
                     ),
            tabPanel("Plot"), 
            tabPanel("Output Summary")
        ) #tabset Panel
    ) #main panel
    ) #sidebarlayout
) #fluidpage







server <- function(input, output, session) {
    datasetInput <- reactive({
        infile <- input$FileInput
        if (is.null(infile))
            return(NULL)
        read.csv(infile$datapath, header = TRUE)
    })
    
    output$table = DT::renderDataTable(datasetInput())
  
    
   desc <- reactive({
        if (is.null(datasetInput()))
            return(NULL)
        psych::describe(datasetInput(), fast = T) 
    })
   # output$description = DT::formatRound(
   #     DT::renderDataTable(desc()),
   #     c("mean", "sd", "min", "max", "range", "se"),2)
   output$description =  DT::renderDataTable(desc())
   

   
# thanks to Simon for this:
# https://stackoverflow.com/users/7742981/simon-s-a
     observeEvent(datasetInput(),{
    updateSelectInput(session, "responsevar", choices = names(datasetInput()))
  })

   
   
} #server


shinyApp(ui = ui, server = server)
