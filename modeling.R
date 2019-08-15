# modeling

# linear regression shiny application
# made with wiredjs and xkcd
# 
# 
# 
# # to implement: 
# display bivariate regression model with ggplot
# , 
source("functions.R")

ui <- fluidPage(
  titlePanel(#title=div(img(src="stamp.jpg"),
    "Back of the Envelope Regression" #)
  ),
  sidebarLayout(sidebarPanel(
    fileInput("FileInput", "Input Your Data Set"),
    tags$p("Dataset must be one of: .csv, .sav, .dta, or .xlsx"),
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
                   multiple = TRUE,
                   options = NULL),
    
    wired_select(
      inputId = "clstr",
      label = "Clustering:",
      choices = c("None",
                  "Fixed Effects",
                  "Cluster Standard Errors",
                  "Multilevel / LME / HLM")
    ), 
    tags$br(), 
    tags$br(),
    selectInput(inputId = "clust",
                label = "Cluster Varibale: (coming soon)", 
                choices = NULL
    )
    
    
  ), #sidebar panel
  mainPanel(
    tabPanel("Output Summary", 
             tags$p("Be sure to include a null_model if LME is selected and if a cluster is chosen"),
             htmlOutput("tabmodel")
             
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
    dat<-use(infile$datapath)
    names(dat) <-  gsub(" ", "_", names(dat), fixed = TRUE) 
    return(dat)
    #readr::read_csv(infile$datapath)
  })
  
  # Update elements of UI for features of data input
  observeEvent(datasetInput(), {
    updateSelectInput(session, "responsevar", choices = names(datasetInput()))
  })
  observeEvent(datasetInput(), {
    updateSelectInput(session, "clust", choices = names(datasetInput()))
  })
  observeEvent(datasetInput(), {
    updateSelectInput(session, "indevars", choices = names(datasetInput()))
  })
  observeEvent(datasetInput(), {
    updateSelectInput(session, "restricted", choices = names(datasetInput()))
  })
  
  
  # Variables
  output$variable_names <- reactive({
    if (is.null(datasetInput()))
      return(NULL)
    gsub(" ", "_", names(datasetInput()), fixed = TRUE)
  })

  # regression formula
  feats <- reactive({
    if (length(input$indevars != 1)) {
      paste(input$indevars, collapse = " + ")
    } else {
      paste(input$indevars)
    }
  })
  
  
  # different formulas for different approaches to clustering. These will need to be 
  # collected into one reactive value of regFormula with 3 IF statements once they have
  # been shown to actually work, 
  regFormula <- reactive({
    as.formula(paste(input$responsevar, ' ~ ', feats()))
  })
  
  regFormula_fe <- reactive({
    as.formula(paste(input$responsevar, ' ~ ', feats(),  ' + factor(', input$clust, ' ) '))
  })
  
  regFormula_hlm<- reactive({
    as.formula(paste(input$responsevar, ' ~ 1 + ', feats(), ' + (1 |', input$clust, ')'))
  })
  
  regFormula_se<- reactive({
    as.formula(paste(input$responsevar, ' ~ ', feats()))
  })
  
  
  # Model Building
  regress_none <- function(){
    
    linear <- reactive ({
      if (input$rbst) {
        MASS::rlm(regFormula(), data = datasetInput())
      } else {
        lm(regFormula(), data = datasetInput())
      }
    })
    
    logistic <- reactive({
      if (input$rbst) {
        robust::glmRob(
          regFormula(),
          data = datasetInput(),
          family = binomial(),
          method = "cubif"
        )
      } else {
        glm(regFormula(), data = datasetInput(), family = "binomial")
      }
    })
    
    if (input$rgrssn == "logistic") {
      logistic()
    } else {
      linear()
    }
    
    
  }
  regress_fe   <- function() {
    linear <- reactive ({
      if (input$rbst) {
        MASS::rlm(regFormula_fe(), data = datasetInput())
      } else {
        lm(regFormula_fe(), data = datasetInput())
      }
    })
    
    logistic <- reactive({
      if (input$rbst) {
        robust::glmRob(
          regFormula_fe(),
          data = datasetInput(),
          family = binomial(),
          method = "cubif"
        )
      } else {
        glm(regFormula_fe(), data = datasetInput(), family = "binomial")
      }
    })
    
    if (input$rgrssn == "logistic") {
      logistic()
    } else {
      linear()
    }
  }
  regress_hlm  <- function(){
    hlmmodel<-reactive({
      lmer(regFormula(), data = datasetInput())
    })
    return(hlmmodel)
  }
  regress_se   <- function(){
    
    clustmodel<-lm.cluster(data = datasetInput(), regFormula_se(), 
                           cluster = input$clust)
    return(clustmodel)
  }
  model <- reactive({
      if (is.null(input$clstr)) {
        regress_none()
      } else {
        switch(
          input$clster,
          "None" = regress_none(),
          "Fixed Effects" = regress_fe(),
          "Cluster Standard Errors" = regress_se(),
          "Multilevel / LME / HLM" = regress_hlm()
        )
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
  
  
} #server


shinyApp(ui = ui, server = server)
# 
# 
# 
# 
# regress <- function() {
#   switch(
#     input$clster,
#     "None" = regress_none(),
#     "Fixed Effects" = regress_fe(),
#     "Cluster Standard Errors" = regress_se(),
#     "Multilevel / LME / HLM" = regress_hlm()
#   )
# }
# 
# 
# regress <- reactive({
#   switch(
#     input$clstr,
#     "None" = regress_none() ,
#     "Fixed Effects" =  regress_fe() ,
#     "Cluster Standard Errors" =  regress_se(),
#     "Multilevel / LME / HLM" =   regress_hlm(),
#   )
# })


