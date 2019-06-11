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
        selectInput(inputId = "clust",
                    label = "Cluster Varibale: (coming soon)", 
                    choices = NULL
        ),
        wired_select(
             inputId = "clstr",
             label = "Clustering:",
             choices = c("None",
                         "Fixed Effects",
                         "Cluster Standard Errors",
                         "Multilevel / LME / HLM"),
             selected = "None"
             )


    ), #sidebar panel
    mainPanel(
        tabsetPanel(
            tabPanel("About",
                     tags$h3("About"),
                     tags$p("This is intended to be a toy point-and-click-style regression tool to practice R Shiny application development and to enumerate the complexities available in regression analysis. Like R itself, this tool comes with absolutely no warranty. "),
                     tags$h3("Use"),
                     tags$p("Use the tool by uploading your own data set in one of the listed formats. Browse your data and examine the variables' descriptive statistics, as well as the table of correlations, then create your model to run."),
                     tags$p("The sketchy nature of the application is intended to deter its use for serious purposes and strengthen the feeling of it being a back-of-the-envelope tool for regression analysis. Use the features to quickly explore options for regression and their effect on your analysis, but resist the urge to p-hack."), 
                     tags$p(tags$b("What this app doesn't do:"), "This app does not allow for any kind of data preparation. Techniques such as interaction terms, exponential terms, or complex extensions such as regression discontinuity need to be done in whatever data-preparation program you choose to use (e.g. excel) before data can be uploaded and used here. For example, to include polynomials, create a new variable in your dataset that is x^2 and re-upload the dataset to run a new regression.  Fixed effects are supported (coming soon!) but if you wish to choose your reference category, you will need to create dummy variables in your dataset and re-upload."),
                     tags$h3("Credit"),
                     tags$p("Back of the Envelope was built with the following R packages: Shiny, wired, xkcd, DT, psych, SjPlot, MASS, mccrr, and the tidyverse."), 
                     tags$p(tags$b("last updated:",format(Sys.time(), "%a %b %d %X %Y"))),
                     wired_icon_button("button3", icon = "mail")
                     ),
            tabPanel("Dataset",
                     DT::dataTableOutput("table")
                     ), 
            tabPanel("Describe", 
                     DT::dataTableOutput("description")
                     ),
            tabPanel("Correlations",
                     plotOutput("cors")
                     ),
            tabPanel("Plot", 
                     tabsetPanel(type = "tabs",
                                 tabPanel("Marginal Effects",
                                          plotOutput("marginal")),
                                 tabPanel("One IV", 
                                          plotOutput("bivariate")
                                          # , plotOutput("bivar-resid")
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
                     )
                     ), 
            tabPanel("Output Summary", 
                     tags$p("Be sure to include a null_model if LME is selected and if a cluster is chosen"),
                     htmlOutput("tabmodel")
                     ),

            tabPanel("Outlier Analysis",
                     tabsetPanel(type = "tabs",
                                 tabPanel("Cook's Distance"),
                                 tabPanel("Leverage"),
                                 tabPanel("Influence Index" 
                                          # car::influenceIndexPlot(model())
                                          )
                                 ) #tabsetPanel
            )# Outlier Analysis
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
   
   

   
   # display the data
   output$table = DT::renderDataTable(datasetInput())
   
   
   # Variables
   output$variable_names <- reactive({
      if (is.null(datasetInput()))
         return(NULL)
      gsub(" ", "_", names(datasetInput()), fixed = TRUE)
   })
   
   # Describe the dataset
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
   
   # correlation plot
   output$cors <- renderPlot(
      datasetInput() %>%
         select_if(is_extant) %>%
         select_if(is_numeric) %>%
         sjp_corr(
            data = .,
            sort.corr = T,
            decimals = 2,
            na.deletion = "pairwise",
            show.p = FALSE
         ) +
         theme(
            panel.grid.major = element_blank(),
            axis.ticks = element_line(colour = "black"),
            panel.background = element_blank(),
            panel.grid.minor = element_blank(),
            legend.key = element_blank(),
            strip.background = element_blank(),
            text = element_text(size = 16, family = "xkcd")
         )
   )
   
   

   
   # regression formula
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
   
   # Marginal Effects Plot:
   output$marginal <- renderPlot(
      plot_model(model())+
         theme(
            panel.grid.major = element_blank(),
            axis.ticks = element_line(colour = "black"),
            panel.background = element_blank(),
            panel.grid.minor = element_blank(),
            legend.key = element_blank(),
            strip.background = element_blank(),
            text = element_text(size = 16, family = "xkcd")
         ) 
   )
   
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
   indvariable1<-reactive({
      input$indevars[1]
   })
   indvariable2<-reactive({
      input$indevars[2]
   })
   depvariable <- reactive({
      input$responsevar
   })
   
   output$bivariate <- renderPlot(if (input$rgrssn == "linear") {
      datasetInput() %>%
         ggplot(aes_string(x = indvariable(), y = depvariable())) +
         geom_point() +
         geom_smooth(method = "lm") +
         theme(
            panel.grid.major = element_blank(),
            axis.ticks = element_line(colour = "black"),
            panel.background = element_blank(),
            panel.grid.minor = element_blank(),
            legend.key = element_blank(),
            strip.background = element_blank(),
            text = element_text(size = 16, family = "xkcd")
         ) +
         xkcdaxis(xrange(), yrange())
   } else if (input$rgrssn == "logistic") {
      datasetInput() %>%
         ggplot(aes_string(x = indvariable(), y = depvariable())) +
         geom_point() +
         geom_smooth(method = "glm",
                     method.args = list(family = "binomial")) +
         theme(
            panel.grid.major = element_blank(),
            axis.ticks = element_line(colour = "black"),
            panel.background = element_blank(),
            panel.grid.minor = element_blank(),
            legend.key = element_blank(),
            strip.background = element_blank(),
            text = element_text(size = 16, family = "xkcd")
         ) +
         xkcdaxis(xrange(), yrange())
   } else {
      print(NULL)
   })
   
   
   output$trivariate <- renderPlot(if (input$rgrssn == "linear") {
      datasetInput() %>%
         ggplot(aes_string(
            x = indvariable1(),
            y = depvariable(),
            color = indvariable2()
         )) +
         geom_point(alpha = 0.6) +
         theme(
            panel.grid.major = element_blank(),
            axis.ticks = element_line(colour = "black"),
            panel.background = element_blank(),
            panel.grid.minor = element_blank(),
            legend.key = element_blank(),
            strip.background = element_blank(),
            text = element_text(size = 16, family = "xkcd")
         ) +
         xkcdaxis(xrange(), yrange())
   } else if (input$rgrssn == "logistic") {
      datasetInput() %>%
         ggplot(aes_string(
            x = indvariable1(),
            y = depvariable(),
            color = indvariable2()
         )) +
         geom_point(alpha = 0.6) +
         geom_smooth(method = "glm",
                     method.args = list(family = "binomial")) +
         theme(
            panel.grid.major = element_blank(),
            axis.ticks = element_line(colour = "black"),
            panel.background = element_blank(),
            panel.grid.minor = element_blank(),
            legend.key = element_blank(),
            strip.background = element_blank(),
            text = element_text(size = 16, family = "xkcd")
         ) +
         xkcdaxis(xrange(), yrange())
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
         # theme(
         #    panel.grid.major = element_blank(),
         #    axis.ticks = element_line(colour = "black"),
         #    panel.background = element_blank(),
         #    panel.grid.minor = element_blank(),
         #    legend.key = element_blank(),
         #    strip.background = element_blank(),
         #    text = element_text(size = 16, family = "xkcd")
         # ) +
         # xkcdaxis(xrange(), yrange()) + 
         # labs(y = depvariable())
   )
   

} #server


shinyApp(ui = ui, server = server)

    