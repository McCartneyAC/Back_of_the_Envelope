## LAST
## STABLE
## VERSION
## DO
## NOT
## EDIT
## 


library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinydashboardPlus)
library(shinyWidgets)
library(wired)
library(xkcd)
# library(extrafont)
library(DT)
library(psych)
library(magrittr)
library(dplyr)
library(sjPlot)
library(MASS)
library(fontawesome)
library(lme4)
library(car)
library(robust)
library(multiwayvcov)
library(miceadds)
library(estimatr)
# loadfonts()



is_extant <-function(x) any(!is.na(x))
is_numeric<-function(x) any(is.numeric(x))

# Data import
use <- function(name) {
  csv <- ".csv"
  xlsx <- ".xlsx"
  dta <- ".dta"
  sav <- ".sav"
  if (grepl(csv, name)) {
    readr::read_csv(name)
  } else if (grepl(xlsx, name)) {
    readxl::read_xlsx(name)
  } else if (grepl(dta, name)) {
    haven::read_dta(name)
  } else if (grepl(sav, name)) {
    haven::read_spss(name)
  } else {
    stop("unknown data type.")
  }
}

gg_added_var <- function(partial, extended, se = TRUE) {
  # In a multiple regression, the added variable plot for a predictor X, say,
  # is the plot showing the residual of Y against all predictors except X against the
  # residual of X on all predictors except X, of course.
  # Adapted from Steven Pollack
  # https://github.com/stevenpollack/stat151a/blob/master/From_Lab/Feb-26-2014.R
  require(ggplot2)
  partial_residuals <- resid(partial)
  full_residuals <- resid(extended)
  avPlot <- ggplot(
    data = data.frame(x = partial_residuals, y = full_residuals),
    aes(x = partial_residuals, y = full_residuals)
  ) +
    geom_point()
  if (se) {
    avPlot <- avPlot +
      stat_smooth(method = "lm")
  } else {
    avPlot <- avPlot +
      stat_smooth(method = "lm", se = FALSE)
  }
  return(avPlot)
}








ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "Back of the Envelope"),
                    dashboardSidebar(
                      sidebarMenu(id = "sidebar",
                                  tags$br(),
                                  menuItem("About", tabName = "grand_about", icon = icon("book")),
                                  tags$h3("Regression:"),
                                  menuItem("Upload & Model", tabName = "reg_about", icon = icon("upload")),
                                  menuItem("Data Set", tabName = "reg_data", icon = icon("superscript")),
                                  menuItem("Describe", tabName = "reg_desc", icon = icon("list-ol")),
                                  menuItem("Correlation Table", tabName = "reg_cor", icon = icon("th")),
                                  menuItem("Plots", tabName = "reg_plot", icon = icon("line-chart"),
                                           badgeLabel = "partial", badgeColor = "orange"),
                                  menuItem("Summary", tabName = "reg_sum", icon = icon("list")),
                                  menuItem("Diagnostics", tabName = "reg_ddx", icon = icon("x-ray"),
                                           badgeLabel = "pending", badgeColor = "red"),
                                  menuItem("Outliers", tabName = "reg_outlier", icon = icon("sliders"),
                                           badgeLabel = "pending", badgeColor = "red"), 
                                  tags$hr(),
                                  socialButton(url = "mailto:mccartneyac@gmail.com", type = "at"),
                                  socialButton(url = "https://www.reddit.com/r/learnrstats/", type = "reddit"),
                                  socialButton(url = "https://www.r-project.org/", type = "r-project"),
                                  socialButton(url = "https://paypal.me/mccartneyac", type = "paypal"),
                                  socialButton(url = "https://github.com/McCartneyAC/average_of_polls/", type = "github")
                                  
                                  
                      )# sidebarmenu
                    ), #sidebar 
                    dashboardBody(
                      tabItems(
                        
                        
                        #masthead
                        tabItem(tabName = "grand_about", 
                                box(title = "About", 
                                    tags$p("Back of the Envelope is the culmination of two (and maybe more?) project ideas that I have worked on in the year 2019. When I first learned to use Shiny R, I couldn't get the idea out of my head that someone should build a point-and-click style regression tool that utilized all and only those presets that I found helpful and that gave its output in ways that I tended to use when doing homework or preparing presentations and publications. Several extant R packages were outputting results in APA format our otherwise had defaults that were best-in-the-industry for a grad student. After leaving grad school, I put this idea into practice. The original version used wired.js and R's xkcd package to make all regression plots and fonts look hand-drawn. In this iteration, I have restored defaults so you can use it directly in publications. You're welcome...but I do miss the sketchiness."),
                                    tags$p("A few months later, I began obsessively following 2020 democratic nomination polls and eventually I got annoyed with all the most popular trackers' issues: no error bars, too wiggly, not displaying enough candidates, can't zoom, etc. Eventually I started plotting the data myself, and it turned into a shiny app."), 
                                    tags$p("What the two ideas have in common is a dangerous amount of statistical sophistication: enough to give it a strong semblance of accuracy, but leaving out the super technical details that might be important for research publication. Back of the Envelope regression is good enough for stat homework and basic pubs. Back of the Envelope 2020 Polling is good for getting a sense of the field that's less sensationalized than what you see on other sites. Neither project is polished, hence the overarching title.")),
                                box(title= "Credit",
                                    tags$p("Back of the Envelope was built with myriad R packages, among them: Shiny & shinydashboard,  DT, psych, SjPlot, MASS, mccrr, and the tidyverse."), 
                                    socialButton(url = "https://github.com/McCartneyAC/average_of_polls/", type = "github")
                                )#box
                        ), #tabItem
                        
                        # Regression
                        # # About
                        tabItem(tabName = "reg_about",
                                box( title = "Upload and Model",
                                     fileInput("FileInput", "Input Your Data Set"),
                                     helpText("Dataset must be one of: .csv, .sav, .dta, or .xlsx"),
                                     tags$h3(tags$b("Build your Model:")), 
                                     radioButtons(
                                       inputId = "rgrssn", label = "Regression:",
                                       choices = c("linear" = "linear",
                                                   "logistic" = "logistic")
                                     ),
                                     #shinywidget
                                     materialSwitch(inputId = "rbst", label = "Robust Standard Errors"),
                                     #wired_toggle(inputId = "rbst", label = "Robust Standard Errors"),
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
                                     
                                     selectInput(
                                       inputId = "clstr",
                                       label = "Clustering:",
                                       choices = c("None",
                                                   "Fixed Effects",
                                                   "Cluster Standard Errors",
                                                   "Multilevel / LME / HLM"),
                                       selected = NA
                                     ), 
                                     selectInput(inputId = "clust",
                                                 label = "Cluster Varibale: (coming soon)", 
                                                 choices = NULL
                                     )
                                ), #upload box
                                box( title = "About",
                                     tags$p("This is intended to be a toy point-and-click-style regression tool to practice R Shiny application development and to enumerate the complexities available in regression analysis. Like R itself, this tool comes with absolutely no warranty. "),
                                     tags$p("The sketchy nature of the application is intended to deter its use for serious purposes and strengthen the feeling of it being a back-of-the-envelope tool for regression analysis. Use the features to quickly explore options for regression and their effect on your analysis, but resist the urge to p-hack."), 
                                     tags$h3("Use"),
                                     tags$p("Use the tool by uploading your own data set in one of the listed formats. Browse your data and examine the variables' descriptive statistics, as well as the table of correlations, then create your model to run."),
                                     tags$p(tags$b("What this app doesn't do:"), "This app does not allow for any kind of data preparation. Techniques such as interaction terms, exponential terms, or complex extensions such as regression discontinuity need to be done in whatever data-preparation program you choose to use (e.g. excel) before data can be uploaded and used here. For example, to include polynomials, create a new variable in your dataset that is x", tags$sup("2"), "and re-upload the dataset to run a new regression.  Fixed effects are supported (coming soon!) but if you wish to choose your reference category, you will need to create dummy variables in your dataset and re-upload."),
                                     
                                     tags$p(tags$b("last updated: 8/12/2019"))
                                ) #box
                        ), #tabItem
                        # # Data Set
                        tabItem(tabName = "reg_data",
                                title = "Dataset",
                                DT::dataTableOutput("reg_data_table")
                        ),
                        # # Describe
                        tabItem(tabName = "reg_desc", title = "Describe", 
                                DT::dataTableOutput("description")),
                        # # Correlation
                        tabItem(tabName = "reg_cor", title = "Correlations",
                                plotOutput("cors")),
                        # # Plot
                        tabItem(tabName = "reg_plot", title = "Plot", 
                                tabsetPanel(type = "tabs",
                                            tabPanel("Marginal Effects",
                                                     plotOutput("marginal")),
                                            tabPanel("One IV", 
                                                     plotOutput("bivariate"), 
                                                     tags$p("residuals:"), 
                                                     plotOutput("bivar_resid")
                                            ),
                                            tabPanel("Two IVs",
                                                     plotOutput("trivariate")),
                                            tabPanel("Added Variable Plots", 
                                                     tags$h4("Added Variable Plots (forthcoming)"),
                                                     selectInput(inputId = "restricted",
                                                                 label = "Select your Predictor", 
                                                                 choices = NULL
                                                     ), 
                                                     plotOutput("avplot")
                                            )
                                ) #tabset panel
                        ), #tabitem 
                        # # Summary
                        tabItem(tabName = "reg_sum", title = "Output Summary", 
                                box(
                                  tags$p("Be sure to include a null_model if LME is selected and if a cluster is chosen"),
                                  tags$br(), 
                                  htmlOutput("tabmodel")
                                ) #box
                        ),
                        # # Outliers
                        tabItem(tabName = "reg_outlier", title = "Outlier Analysis",
                                tabsetPanel(type = "tabs",
                                            tabPanel("Cook's Distance", 
                                                     tags$p("Select one independent variable:")
                                            ), # Cook's Distance
                                            tabPanel("Leverage"),
                                            tabPanel("Influence Index" 
                                                     # car::influenceIndexPlot(model())
                                            )
                                ) #tabset panel
                        )# tab item. (LAST ONE)
                        
                        
                        
                        
                        
                      ) #tabitems
                    ) #Dashboard Body
) #Dashboard Page

server <- function(input, output, session) { 
  
  
  
  # Data Input --------------------------------------------------------------
  
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
  
  # Variable Names (for use by everything)
  output$variable_names <- reactive({
    if (is.null(datasetInput()))
      return(NULL)
    gsub(" ", "_", names(datasetInput()), fixed = TRUE)
  }) 
  
  
  # Data Table --------------------------------------------------------------
  output$reg_data_table = DT::renderDataTable(datasetInput())
  
  
  
  # Describe the Data Set ---------------------------------------------------
  desc <- reactive({
    if (is.null(datasetInput()))
      return(NULL)
    psych::describe(datasetInput(), fast = T) %>%
      add_rownames(var = "Variable") %>%
      mutate(mean = round(mean, 2)) %>%
      mutate(sd = round(sd, 2)) %>%
      mutate(se = round(se, 2)) %>%
      mutate(min = round(min, 2)) %>%
      mutate(max = round(max, 2)) %>%
      mutate(range = round(range, 2))
  })
  
  # description table (psych::describe)
  output$description =  DT::renderDataTable(desc())
  
  
  # Correlation Table -------------------------------------------------------
  output$cors <- renderPlot(
    datasetInput() %>%
      select_if(is_extant) %>%
      select_if(is_numeric) %>%
      sjp.corr(
        data = .,
        sort.corr = T,
        decimals = 2,
        na.deletion = "pairwise",
        show.p = FALSE
      ) +
      theme_light()
  )
  
  
  
  
  # Generate a Regression Formula -------------------------------------------
  feats <- reactive({
    if (length(input$indevars != 1)) {
      paste(input$indevars, collapse = " + ")
    } else {
      paste(input$indevars)
    }
  })
  
  regFormula <- reactive({
    as.formula(paste(input$responsevar, ' ~ ', feats()))
  })
  
  # Model Building
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
  
  model <- reactive({
    if (input$rgrssn == "logistic") {
      logistic()
    } else {
      linear()
    }
  })
  
  
  # Plots (all of them) -----------------------------------------------------
  
  
  # Marginal Effects Plot:
  output$marginal <- renderPlot(
    plot_model(model())+
      theme_light()
  )
  
  
  
  
  # can this be exported to a sourced .R file to clean up the code? 
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
  indvariable1<-reactive({
    input$indevars[1]
  })
  indvariable2<-reactive({
    input$indevars[2]
  })
  depvariable <- reactive({
    input$responsevar
  })
  model_predicted <- reactive({
    predict(model())   # Save the predicted values
  })
  model_residuals <-  reactive({
    residuals(model()) # Save the residual values
  })
  y_range_residual <- reactive({
    model_residuals() %>% 
      range()
  })
  
  
  #refactor this you fool. 
  output$bivariate <- renderPlot(if (input$rgrssn == "linear") {
    datasetInput() %>%
      ggplot(aes_string(x = indvariable(), y = depvariable())) +
      geom_point() +
      geom_smooth(method = "lm") +
      theme_light()
  } else if (input$rgrssn == "logistic") {
    datasetInput() %>%
      ggplot(aes_string(x = indvariable(), y = depvariable())) +
      geom_point() +
      geom_smooth(method = "glm",
                  method.args = list(family = "binomial")) +
      theme_light()
  } else {
    print(NULL)
  })
  
  output$bivar_resid <-  renderPlot(if (input$rgrssn == "linear") {
    datasetInput() %>%
      ggplot(aes_string(x = indvariable(), y = model_residuals())) +
      geom_point() +
      geom_smooth(method = "lm") +
      theme_light()
  } else if (input$rgrssn == "logistic") {
    print("Error is not normally distributed in logistic regression.")
  } else {
    print(NULL)
  })
  
  output$trivariate <- renderPlot(if (input$rgrssn == "linear") {
    datasetInput() %>%
      ggplot(aes_string(
        x = indvariable1(),
        y = depvariable()
      )) +
      # TODO: this is currently broken. 
      geom_point(alpha = 0.6, aes(color = indvariable2())) +
      theme_light() + 
      geom_smooth(method = "lm")
  } else if (input$rgrssn == "logistic") {
    datasetInput() %>%
      ggplot(aes_string(
        x = indvariable1(),
        y = depvariable()
      )) +
      geom_point(alpha = 0.6, aes(color = indvariable2())) +
      geom_smooth(method = "glm",
                  method.args = list(family = "binomial")) +
      theme_light()
  } else {
    print(NULL)
  })
  
  
  # AV PLOT CONSTRUCTION
  rstrctd<-reactive({
    names(input$indevars)[names(input$indevars) != input$restricted]
  })
  rstrctdfeats<- reactive({
    paste(rstrctd(), collapse = " + ")
  })
  rstrctdformula <-  reactive({
    as.formula(paste(input$responsevar, ' ~ ', rstrctdfeats()))
  })
  
  # Model Building
  rstrctdlinear <- reactive ({
    if (input$rbst) {
      MASS::rlm(rstrctdformula(), data = datasetInput())
    } else {
      lm(rstrctdformula(), data = datasetInput())
    }
  })
  rstrctdlogistic <- reactive({
    if (input$rbst) {
      robust::glmRob(
        rstrctdformula(),
        data = datasetInput(),
        family = binomial(),
        method = "cubif"
      )
    } else {
      glm(rstrctdformula(), data = datasetInput(), family = "binomial")
    }
  })
  rstrctdmodel <- reactive({
    if (input$rgrssn == "logistic") {
      rstrctdlogistic()
    } else {
      rstrctdlinear()
    }
  })
  
  
  output$avplot <- renderPlot(
    gg_added_var(partial = rstrctdmodel(), extended = model()) 
    
  )
  
  
  
  # Model Summary -----------------------------------------------------------
  # creates the actual model summary object. 
  output$model <- renderPrint({
    summary(model())
  })
  # passes model summary object into the Sjplot model summary HTML thing. 
  output$tabmodel <- renderUI({
    modeltab <- tab_model(model())
    HTML(modeltab$knitr)
  })  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}

shinyApp(ui, server)
