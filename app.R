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
# , 
source("functions.R")

ui <- fluidPage(
    titlePanel(#title=div(img(src="stamp.jpg"),
                           "Back of the Envelope Regression" #)
    ),
    sidebarLayout(sidebarPanel(
        fileInput("FileInput", "Input Your Data Set (Must be .csv)"),
        wired_icon_button("button", icon = "mail_outline"),
        tags$h3(tags$b("Build your Model:")), 
        wired_radio(
            inputId = "rgrssn", label = "Regression:",
            choices = c("linear" = "linear",
                        "logistic" = "logistic")
        ),
        wired_toggle(inputId = "rbst", label = "Robust Standard Errors"),
        tags$p(tags$b("Select your variables for analysis:")),
        selectInput(inputId = "responsevar",
                     label = "Your DV / Response Variable:", 
                     choices = NULL
                     ),

        selectizeInput("indevars", "Your IV / Predictor Variable(s):", 
                       choices = NULL, 
                       selected = NULL, 
                       # CHANGE ME LATER
                       multiple = TRUE,
                       options = NULL),

        wired_select(
             inputId = "clstr",
             label = "Clusters?",
             choices = c("None",
                         "Fixed Effects",
                         "Cluster Standard Errors",
                         "Multilevel Model / LME"),
             selected = "None"
             ),
        selectInput(inputId = "clust",
                    label = "Cluster Varibale:", 
                    choices = NULL
        )

    ), #sidebar panel
    mainPanel(
        tabsetPanel(
            tabPanel("About",
                     tags$h3("About"),
                     tags$p("This is intended to be a toy point-and-click-style regression tool to practice R Shiny application development and to enumerate the complexities available in regression analysis. Like R itself, this tool comes with absolutely no warranty. "),
                     tags$h3("Use"),
                     tags$p("Use the tool by uploading your own data set in .csv format (.xlsx functionality coming soon). Browse your data and examine the variables' descriptive statistics, as well as the table of correlations, then create your model to run. "),
                     tags$p("The sketchy nature of the application is intended to deter its use for actual serious purposes and strengthen the feeling of it being a back-of-the-envelope tool for regression analysis."), 
                     tags$p(tags$b("What this app doesn't do:"), "This app does not allow for any kind of data preparation. Techniques such as interaction terms, exponential terms, or complex extensions such as regression discontinuity need to be done in whatever data-preparation program you choose to use (e.g. excel) before data can be uploaded and used here."),
                     tags$h3("Credit"),
                     tags$p("Back of the Envelope was built with the following R packages: Shiny, wired, xkcd, DT, psych, SjPlot, MASS, mccrr, and the tidyverse."), 
                     wired_icon_button("button3", icon = "mail")
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
            tabPanel("Plot", 
                     tabsetPanel(type = "tabs",
                                 tabPanel("Bivariate", plotOutput("bivariate")
                                          ),
                                 tabPanel("Multiple Regression" # , tableOutput("table_mult")
                                          )
                     )
                     ), 
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
            mutate(se = round(se, 2)) %>% 
            mutate(min = round(min, 2)) %>% 
            mutate(max = round(max, 2)) %>% 
            mutate(range = round(range, 2))
    })
   output$description =  DT::renderDataTable(desc())
   
   # correlation plot
   output$cors <- renderPlot(
       datasetInput() %>% 
           select_if(is_extant) %>% 
           select_if(is_numeric) %>% 
           sjp_corr(data = ., sort.corr = T, decimals = 2, na.deletion = "pairwise", show.p = FALSE) + 
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
   
   feats <- reactive({
       if(length(input$indevars != 1)) {
           paste(input$indevars, collapse = " + ")
       } else {
           paste(input$indevars)
       }
   })

   
   regFormula <- reactive({
       as.formula(paste(input$responsevar, '~', feats()))
   })
   
   # Linear Model
   model <- reactive({
       if (input$rbst) {
           MASS::rlm(regFormula(), data = datasetInput())
       } else {
           lm(regFormula(), data = datasetInput())
       }
   })
   

   output$model <- renderPrint({
       summary(model())
   })

   #Display Regression Model Summary:
   output$tabmodel <- renderUI({
       modeltab <- tab_model(model())
       HTML(modeltab$knitr)
   })

   
   xrange <- reactive({
       datasetInput() %>% 
       select_(input$indevars) %>% 
       range()
   })
   yrange <- reactive({
       datasetInput() %>% 
       select_(input$responsevar) %>% 
       range()   
   })
   indvariable <- reactive({
       input$indevars
   })
   depvariable <- reactive({
       input$responsevar
   })
   output$bivariate <- renderPlot(
       datasetInput() %>%
           ggplot(aes_string(x = indvariable(), y = depvariable())) +
           geom_jitter() +
           geom_smooth(method = "lm") +
           theme_xkcd() +
           xkcdaxis(xrange(), yrange())
)



} #server


shinyApp(ui = ui, server = server)
