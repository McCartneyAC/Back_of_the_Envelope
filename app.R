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

source("functions.R")

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
        ),
        wired_toggle(inputId = "rbst", label = "Robust Standard Errors"),
        tags$p("Select your variables for analysis:"),
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
                       # CHANGE ME LATER
                       multiple = FALSE,
                       options = NULL),

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
        ),
        wired_button("regress", label = "Run Regression")
    ), #sidebar panel
    mainPanel(
        tabsetPanel(
            tabPanel("About",
                     tags$p("This is intended to be a toy point-and-click-style regression tool to practice R Shiny application development and to enumerate the complexities available in regression analysis. Like R itself, this tool comes with absolutely no warranty. "),
                     tags$p("Use the tool by uploading your own data set in .csv format (.xlsx functionality coming soon). Browse your data and examine the variables' descriptive statistics, as well as the table of correlations, then create your model to run. "),
                     tags$p("The sketchy nature of the application is intended to deter its use for actual serious purposes and strengthen the feeling of it being a back-of-the-envelope tool for regression analysis."), 
                     tags$p("Back of the Envelope was built with the following R packages: Shiny, wired, xkcd, DT, psych, SjPlot, MASS, mccrr, and the tidyverse.")
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
                     tags$p("Be sure to include a null_model if LME is selected and if a cluster is chosen"),
                     htmlOutput("tabmodel")
                     )
        ) #tabset Panel
    ) #main panel
    ) #sidebarlayout
) #fluidpage







server <- function(input, output, session) {
    # input the data set
    datasetInput <- reactive({
        infile <- input$FileInput
        if (is.null(infile))
            return(NULL)
        read.csv(infile$datapath, header = TRUE)
    })
    
    # display the data
    output$table = DT::renderDataTable(datasetInput())
    
    
    # Variable Descriptions
    output$variable_names <- reactive({
        if (is.null(datasetInput()))
            return(NULL)
        names(datasetInput()) 
        })
   desc <- reactive({
        if (is.null(datasetInput()))
            return(NULL)
        psych::describe(datasetInput(), fast = T) %>% 
            mutate(mean = round(mean, 2)) %>% 
            mutate(sd = round(sd, 2)) %>% 
            mutate(se = round(se, 2))
    })
   output$description =  DT::renderDataTable(desc())
   
   # correlation plot
   output$cors <- renderPlot(
       datasetInput() %>% 
           select_if(is_extant) %>% 
           select_if(is_numeric) %>% 
           sjp.corr(data = ., sort.corr = T, decimals = 2, na.deletion = "pairwise") + 
           theme_xkcd()
   )
   
   
   # UI elements that must listen for an uploaded dataset:
   observeEvent(datasetInput(),{
       updateSelectInput(session, "responsevar", choices = names(datasetInput()))
   })
   observeEvent(datasetInput(),{
       updateSelectInput(session, "clust", choices = names(datasetInput()))
   })
   observeEvent(datasetInput(),{
       updateSelectInput(session, "indevars", choices = names(datasetInput()))
   })
   

   
   
   # Create REgression Formula
  #  lm1 <- reactive({lm(reformulate(input$indevars, input$responsevar), data = datasetInput())})
   # regression formula
   regFormula <- reactive({
       as.formula(paste(input$responsevar, '~', input$indevars))
   })
   
   # bivariate model
   model <- reactive({
       if (input$rbst) {
           MASS::rlm(regFormula(), data = datasetInput())
       } else {
           lm(regFormula(), data = datasetInput())
       }
   })
   
   # model <- reactive({
   #     lm(regFormula(), data = datasetInput())
   # })
   #bivariate model
   output$model <- renderPrint({
       summary(model())
   })

   #Display Regression Model Summary:
output$tabmodel <- renderUI({
    modeltab<- tab_model(model())
    HTML(modeltab$knitr)
})


   
} #server


shinyApp(ui = ui, server = server)
