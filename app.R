library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinydashboardPlus)
library(shinyWidgets)
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
library(ggplot2)




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
                    dashboardHeader(title = "Back of the Envelope"
                                    #dropdownMenuOutput("formula_message")
                    ),
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
                                   menuItem("FAQ", tabName = "FAQ_tab", icon = icon("info"))
                                   
                                   
                                   
                       ), # sidebarmenu
                       tags$hr(),
                       socialButton(url = "mailto:mccartneyac@gmail.com", type = "at"),
                       socialButton(url = "https://www.reddit.com/r/learnrstats/", type = "reddit"),
                       socialButton(url = "https://www.r-project.org/", type = "r-project"),
                       socialButton(url = "https://paypal.me/mccartneyac", type = "paypal"),
                       socialButton(url = "https://github.com/McCartneyAC/average_of_polls/", type = "github")
                    ), #sidebar 
                    dashboardBody(
                       tabItems(
                          
                          
                          #masthead
                          tabItem(tabName = "grand_about", 
                                  box(title = "About",width = 7,
                                      tags$p("This is intended to be a toy point-and-click-style regression tool to practice R Shiny application development and to enumerate the complexities available in regression analysis. Like R itself, this tool comes with absolutely no warranty. Use the features to quickly explore options for regression and their effect on your analysis, but resist the urge to p-hack.")
                                      ),
                                  box(title = "Use", width = 7,
                                      tags$p("Use the tool by uploading your own data set in one of the listed formats. Browse your data and examine the variables' descriptive statistics, as well as the table of correlations, then create your model to run. Check the model diagnostics, distribution of error terms, and your outliers to determine if there are better options for dealing with your data. More sophisticated modeling techniques are being added on an ongoing basis."), tags$p("What this app doesn't do: This app does not allow for any kind of data preparation. Techniques such as interaction terms, exponential terms, or complex extensions such as regression discontinuity need to be done in whatever data-preparation program you choose to use (e.g. excel) before data can be uploaded and used here. For example, to include polynomials, create a new variable in your dataset that is x", 
tags$sup("2"), "and re-upload the dataset to run a new regression.  Fixed effects are supported, but if you wish to choose your reference category, you will need to create dummy variables in your dataset and re-upload.")),# use 
                                  box(title= "Credit", width =7,
                                      tags$p("Back of the Envelope was built with myriad R packages, among them: Shiny & shinydashboard, DT for the tables, psych, SjPlot for model summaries, estimatr and MASS for robust estimations, mccrr, and the tidyverse."), 
                                      tags$p(tags$b("last updated: 5/27/2020")),
                                      socialButton(url = "https://github.com/McCartneyAC/average_of_polls/", type = "github")
                                  ) #box
                          ) , #tabItem
                          
                          # Regression
                          # # About
                          tabItem(tabName = "reg_about",
                                  fluidRow(
                                     column( width = 6,
                                             box( title = "Upload and Model",width = NULL,
                                                  fileInput("FileInput", "Input Your Data Set"),
                                                  helpText("Dataset must be one of: .csv, .sav, .dta, or .xlsx")
                                             ), #upload box
                                             box(title = "Your Model:",width = NULL,
                                                 textOutput("regformula")
                                             ), # model box
                                     ),
                                     column(width = 6,
                                            box(title = "Build Your Model:",width = NULL,
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
                                                   label = "Clustering Type:",
                                                   choices = c("No Clustering",
                                                               "Fixed Effects",
                                                               "Cluster Standard Errors",
                                                               "Multilevel / LME / HLM (coming soon)"),
                                                   selected = NA
                                                ), 
                                                selectInput(inputId = "clust",
                                                            label = "Cluster Variable:", 
                                                            choices = NULL
                                                )#select input
                                            ) #upload box
                                     ) #column
                                  ) #fluidrow
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
                          ),# tab item.
                          tabItem(tabName = "FAQ_tab",
                                  box(
                                     tags$p(tags$b("How did it get started? "), 
                                               "Back of the Envelope is the culmination of two and maybe more?) project ideas that I have worked on in the year 2019. When I first learned to use Shiny R, I couldn't get the idea out of my head that someone should build a point-and-click style regression tool that utilized all and only those presets that I found helpful and that gave its output in ways that I tended to use when doing homework or preparing presentations and publications. Several extant R packages were outputting results in APA format our otherwise had defaults that were best-in-the-industry for a grad student. After leaving grad school, I put this idea into practice. The original version used wired.js and R's xkcd package to make all regression plots and fonts look hand-drawn. In this iteration, I have restored defaults so you can use it directly in publications. You're welcome...but I do miss the sketchiness.

The idea is a dangerous amount of statistical sophistication: enough to give it a strong semblance of accuracy, but leaving out the super technical details that might be important for research publication. Back of the Envelope regression is good enough for stat homework and basic publications." 
),
                                       tags$p(tags$b("How can I support the project?"), "By clicking on the paypal link on the left, you can contribute to the project and keep it up and running."),
                                       tags$p(tags$b("I got an error I don't understand!"), "In general, getting an error means you tried to do something that is either unsupported by the app at this time or impossible under the constraints or definitions of contemporary statistics. For example, supplying a continuous variable for logistic regression will get an error. (Supplying a binary variable to linear regression will get you a linear probability model.)"),    
                                       tags$p(tags$b("I can't run the model I want!"), "As noted above, some things just aren't possible. For example, residuals are not normally distributed around a logistic regression model, so trying to check the residuals graph for logistic models will throw an error. If you're trying to do something that you think should be possible, please contact me (the @ symbol on the left). Binary models with mixed effects are currently unsupported, but it's in the works."),
                                       tags$p(tags$b("Any mathematical notes?"),"Yes. Things like sums of squares vary from package to package, and significantly, between R and Stata. The sums of squares and standard errors found herein may or may not match those of other software. In general, it's best to cite your estimation package. Most of the available regression models herein are from the packages: MASS, CAR, estimatr, or lme4."),
                                     tags$p("Include a diagram of available options (errors, etc)")
                                  ) #box
                          ) # tab item. (LAST ONE)

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
      if (input$clstr == "Fixed Effects"){
         as.formula(paste(input$responsevar, ' ~ ', feats()," + factor(", cluster_var(), ")"))
      } else {
         as.formula(paste(input$responsevar, ' ~ ', feats())) 
      }
   })
   
   # Make the Formula Visual
   output$regformula<-reactive({
      paste(deparse(regFormula(), width.cutoff = 500), collapse="")
   })
   
   
   #### NIX THIS IT DOESN"T WORK ANYWAY
   # output$formula_message <- renderMenu({
   #   badgestat<-reactive({
   #     if("try-error" %in% class(try(
   #       paste(deparse(regFormula(), width.cutoff = 500), collapse="")
   #     ))) {
   #       NULL
   #     } else {
   #       "primary"
   #     }
   #     
   #   })
   #   dropdownMenu(type = "tasks",  badgeStatus = badgestat(),
   #                messageItem(from= "Your Model Is:", icon = icon("chart-bar"),
   #                            message = paste(deparse(regFormula(), width.cutoff = 500), collapse="")))
   #   })
   
   # Clustering Issues  
   
   cluster_var <- reactive({
      if (input$clstr == "No Clustering") {
         NULL
      } else {
         input$clust
      }
   })
   
   # Model Building
   linear <- reactive ({
      if (input$rbst & (input$clstr == "Cluster Standard Errors")) {
         lm_robust(regFormula(), clusters = cluster_var(), data = datasetInput())
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
   # TODO: Fix these models
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
