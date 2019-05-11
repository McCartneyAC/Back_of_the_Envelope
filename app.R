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
library(dplyr)
library(sjPlot)
# loadfonts()

ui <- fluidPage(
    titlePanel("Back of the Envelope Regression"),
    sidebarLayout(sidebarPanel(
        fileInput("FileInput", "Input Your Data Set (Must be .csv)"),

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
        selectInput(inputId = "responsevar",
                     label = "Dependent Varibale:", 
                     choices = NULL
                     ),
        # checkbox input
        tags$p("Your IV / Predictor Variable(s):"),

        selectizeInput("indevars", "Independent Variable(s)", 
                       choices = NULL, 
                       selected = NULL, 
                       multiple = TRUE,
                       options = NULL),
        # or just include "multiple = TRUE"
        wired_select(
             inputId = "clstr",
             label = "Clusters?",
             choices = c("None",
                         "Fixed Effects",
                         "Cluster Standard Errors",
                         "Multilevel Model / LME")
             ),
        selectInput(inputId = "clust",
                    label = "Cluster Varibale:", 
                    choices = NULL
        )
    ), #sidebar panel
    mainPanel(
        tabsetPanel(
            tabPanel("About",
                     tags$p("This is intended to be a toy point-and-click-style regression tool to practice R Shiny application development and to enumerate the complexities available in regression analysis. Like R itself, this tool comes with absolutely no warranty. "),
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
                     plotOutput("cors")
                     ),
            tabPanel("Plot"), 
            tabPanel("Output Summary", 
                     tags$p("SjPlot's tab_model() goes here."),
                     tags$p("Be sure to include a null_model if LME is selected and if a cluster is chosen"))
        ) #tabset Panel
    ) #main panel
    ) #sidebarlayout
) #fluidpage







server <- function(input, output, session) {
    
    is_extant <-function(x) any(!is.na(x))
    is_numeric<-function(x) any(is.numeric(x))
    
    datasetInput <- reactive({
        infile <- input$FileInput
        if (is.null(infile))
            return(NULL)
        read.csv(infile$datapath, header = TRUE)
    })
    
    output$table = DT::renderDataTable(datasetInput())
    
    output$variable_names <- reactive({
        if (is.null(datasetInput()))
            return(NULL)
        names(datasetInput()) 
        })
    
   desc <- reactive({
        if (is.null(datasetInput()))
            return(NULL)
        psych::describe(datasetInput(), fast = T) 
    })
   # output$description = DT::formatRound(
   #     DT::renderDataTable(desc()),
   #     c("mean", "sd", "min", "max", "range", "se"),2)
   output$description =  DT::renderDataTable(desc())
   
   
   observeEvent(datasetInput(),{
       updateSelectInput(session, "responsevar", choices = names(datasetInput()))
   })
   observeEvent(datasetInput(),{
       updateSelectInput(session, "clust", choices = names(datasetInput()))
   })
   observeEvent(datasetInput(),{
       updateSelectInput(session, "indevars", choices = names(datasetInput()))
   })
   
   
   
   # correlation plot
   output$cors <- renderPlot(
       datasetInput() %>% 
           select_if(is_extant) %>% 
           select_if(is_numeric) %>% 
           sjp.corr(data = ., sort.corr = T, decimals = 2, na.deletion = "pairwise") + 
           theme_xkcd()
       )


   
   
} #server


shinyApp(ui = ui, server = server)
