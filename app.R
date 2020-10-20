
# Packages ---------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinydashboardPlus)
library(shinyWidgets)
library(DT)
library(psych)
library(dplyr)
library(tidyr)
library(sjPlot)
library(MASS)
library(fontawesome)
library(lme4)
library(car)
library(robust)
library(multiwayvcov)
# library(miceadds) (was being used for lm.cluster, but estimatr does this now. consider readding in future if we make a MICE module, (deep future) )
library(estimatr)
library(ggplot2)
library(lindia)
library(knitr)
library(rmarkdown)
library(tibble)
library(gt)


# HEY DUMMY LOOK HERE:
url <- "https://github.com/rstudio/shiny/issues/998"


# functions ---------------------------------------------------------------



is_extant <-function(x) any(!is.na(x))
is_numeric<-function(x) any(is.numeric(x))

# Data import
use <- function(name) {
   # consider future support for .json? 
   if (grepl(".csv", name)) {
      readr::read_csv(name)
   } else if (grepl(".xlsx", name)) {
      readxl::read_xlsx(name)
   } else if (grepl(".dta", name)) {
      haven::read_dta(name)
   } else if (grepl(".sav", name)) {
      haven::read_spss(name)
   } else if (grepl(".rda", name)) {
      load(name)
   } else {
      stop("unknown data type.")
   }
}

gg_added_var <- function(partial, extended, se = TRUE) {
   # Adapted from Steven Pollack
   # https://github.com/stevenpollack/stat151a/blob/master/From_Lab/Feb-26-2014.R
   require(ggplot2)
   partial_residuals <- resid(partial)
   full_residuals <- resid(extended)
   avPlot <- ggplot(
      data = data.frame(x = partial_residuals, y = full_residuals),
      aes(x = partial_residuals, y = full_residuals)
   ) + 
      labs(title = "Added Variable Plot", 
           x = "Residuals Regressing chosen X (on all other predictors)", 
           y = "Residuals Regressing Y (without chosen X)") + 
      geom_point() + 
      theme_light()
   if (se) {
      avPlot <- avPlot +
         stat_smooth(method = "lm")
   } else {
      avPlot <- avPlot +
         stat_smooth(method = "lm", se = FALSE)
   }
   return(avPlot)
}

description<-function(data, group = NULL, fast = TRUE, ...) {
   grp<-paste0(deparse(substitute(group)))
   #print(grp)
   if(is.null(group)) {
      data %>%
         psych::describe(fast = fast, ...) %>%
         tibble::rownames_to_column() %>%
         dplyr::select(-c(vars)) %>%
         dplyr::mutate(dplyr::across(is.numeric, round, 2)) %>%
         gt::gt() %>%
         gt::tab_options(
            column_labels.font.size = "small",
            table.font.size = "small",
            row_group.font.size = "small",
            data_row.padding = px(3)
         ) %>% 
         tab_header(
            title = paste0("Data Description") 
         )
   } else {
      data %>%
         select_if(is.numeric) %>%
         psych::describeBy(group = group, fast = fast, mat= TRUE, ...) %>%
         tibble::rownames_to_column() %>%
         select(-c(item, vars)) %>%
         dplyr::mutate(dplyr::across(is.numeric, round, 2)) %>%
         arrange(group1) %>%
         group_by(group1) %>%
         gt() %>%
         gt::tab_options(
            column_labels.font.size = "small",
            table.font.size = "small",
            row_group.font.size = "small",
            data_row.padding = px(3))
   } %>% 
      tab_header(
         title = paste0("Data Description") ,
         subtitle = paste0("Grouped by: ",  grp )
      )
}
validStatuses <- c("primary", "success", "info", "warning", "danger" )

validateStatus <- function (status) 
{
   if (status %in% validStatuses) {
      return(TRUE)
   }
   stop("Invalid status: ", status, ". Valid statuses are: ", 
        paste(validStatuses, collapse = ", "), ".")
}

dropdownBlock <- function(..., id, icon = NULL, title = NULL, 
                          badgeStatus = "danger") {
   
   if (!is.null(badgeStatus)) 
      validateStatus(badgeStatus)
   items <- c(list(...))
   
   # Make sure the items are li tags
   #lapply(items, tagAssert, type = "li")
   # items <- lapply(1:length(items), FUN = function(i) {
   #   item <- items[[i]]
   #   name <- item$name
   #   if (name != "li") {
   #     wrapper <- shiny::tags$li()
   #     item <- shiny::tagAppendChild(wrapper, item)
   #   }
   # })
   
   dropdownClass <- paste0("dropdown")
   
   numItems <- length(items)
   if (is.null(badgeStatus)) {
      badge <- NULL
   } else {
      badge <- dashboardLabel(status = badgeStatus, numItems)
   }
   
   shiny::tags$li(
      shiny::singleton(
         shiny::tags$head(
            # custom javascript so that the dropdown
            #is not hidden when the user click on it
            shiny::tags$script(
               shiny::HTML(
                  paste0(
                     "$(document).ready(function(){
                $('#", id, "').find('ul').click(function(e){
                  e.stopPropagation();
                });
              });
              "
                  )
               )
            )
         )
      ),
      class = dropdownClass,
      id = id,
      shiny::tags$a(
         href = "#",
         class = "dropdown-toggle",
         `data-toggle` = "dropdown",
         icon,
         title, 
         badge
      ),
      shiny::tags$ul(
         class = "dropdown-menu",
         style = "left: 0; right: auto;",
         shiny::tags$li(
            shiny::tags$ul(
               class = "menu",
               shiny::tags$div(
                  style = "margin-left: auto; margin-right: auto; width: 80%;",
                  items
               )
            )
         )
      )
   )
}

# UI components -----------------------------------------------------------


ui <- dashboardPagePlus(skin = "black",
                        dashboardHeaderPlus(title = "Back of the Envelope",
                                            left_menu = tagList(
                                               dropdownBlock(
                                                  id = "download_dropdown",
                                                  title = "Download Report",
                                                  icon = icon("download"),
                                                  radioButtons('format',
                                                               helpText('Document format'),
                                                               c('PDF', 'HTML', 'Word')),
                                                  downloadButton('downloadReport'),
                                                  badgeStatus = NULL
                                               )
                                            )
                                            #dropdownMenuOutput("formula_message")
                        ),
                        
                        
                        ##  Sidebar -----------------------------------------------------------------
                        
                        
                        dashboardSidebar(
                           sidebarMenu(id = "sidebar",
                                       tags$br(),
                                       tags$h4("Information:"),
                                       menuItem("About", tabName = "grand_about", icon = icon("book")),
                                       menuItem("FAQ", tabName = "FAQ_tab", icon = icon("info")),
                                       

                                       tags$h4("Your Data:"),
                                       menuItem("Upload", tabName = "reg_upload", icon = icon("upload")),
                                       menuItem("Dossier", tabName = "reg_dossier", icon = icon("id-card")),
                                       menuItem("Describe", tabName = "reg_desc", icon = icon("list-ol")),
                                       menuItem("Correlation Table", tabName = "reg_cor", icon = icon("th")),
                                       
                                       tags$h4("Regression:"),
                                       menuItem("Model", tabName = "reg_model", icon = icon("cogs")),
                                       menuItem("Summary", tabName = "reg_sum", icon = icon("list-alt")),
                                       menuItem("Plots", tabName = "reg_plot", icon = icon("line-chart")),
                                       menuItem("Diagnostics", tabName = "reg_ddx", icon = icon("x-ray")),
                                       menuItem("Outliers", tabName = "reg_outlier", icon = icon("wrench"),
                                                badgeLabel = "partial", badgeColor = "orange"), 
                                       menuItem("Mediation",tabName = "reg_path", icon = icon("project-diagram"),
                                                badgeLabel = "future", badgeColor = "red")
                           ), # sidebarmenu
                           tags$hr(),
                           socialButton(url = "mailto:mccartneyac@gmail.com", type = "at"),
                           socialButton(url = "https://www.reddit.com/r/learnrstats/", type = "reddit"),
                           socialButton(url = "https://www.r-project.org/", type = "r-project"),
                           socialButton(url = "https://paypal.me/mccartneyac", type = "paypal"),
                           socialButton(url = "https://github.com/McCartneyAC/Back_of_the_Envelope", type = "github"),
                           tags$br(),tags$br(),  
                           tags$p("Version 0.9.2")
                        ), #sidebar 
                        dashboardBody(
                           tags$head(tags$title("Back of the Envelope")),
                           tabItems(
                              
                              
                              ## about -------------------------------------------------------------------
                              
                              
                              #masthead
                              tabItem(tabName = "grand_about", 
                                      box(title = "About",width = 7,solidHeader = TRUE,
                                          tags$p("This is intended to be a toy point-and-click-style regression tool to practice R Shiny application development and to enumerate the complexities available in regression analysis. Like R itself, this tool comes with absolutely no warranty. Use the features to quickly explore options for regression and their effect on your analysis, but resist the urge to p-hack.")
                                      ) , #box
                                      box(title = "Use", width = 7,solidHeader = TRUE,
                                          tags$p("Use the tool by uploading your own data set in one of the listed formats. Browse your data and examine the variables' descriptive statistics, as well as the table of correlations, then create your model to run. Check the model diagnostics, distribution of error terms, and your outliers to determine if there are better options for dealing with your data. More sophisticated modeling techniques are being added on an ongoing basis."), tags$p("What this app doesn't do: This app does not allow for any kind of data preparation (yet). Techniques such as interaction terms, exponential terms, or complex extensions such as regression discontinuity need to be done in whatever data-preparation program you choose to use (e.g. excel) before data can be uploaded and used here. For example, to include polynomials, create a new variable in your dataset that is x", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            tags$sup("2"), "and re-upload the dataset to run a new regression.  Fixed effects are supported, but if you wish to choose your reference category, you will need to create dummy variables in your dataset and re-upload.")), # use 
                                      box(title= "Credit", width =7,solidHeader = TRUE, #for all the good it does. 
                                          tags$p("Back of the Envelope was built with myriad R packages, among them: Shiny & shinydashboard for the UI, DT for the tables, psych for its ubiquitous describe function, SjPlot for model summaries, correlations, and margins plots, estimatr and MASS for robust estimations, lindia for the diagnostic models, my own package mccrr, and the tidyverse. Thanks to Rich Majerus for some code inspiration."), 
                                          tags$p(tags$b("last updated: 9/1/2020")),
                                          socialButton(url = "https://github.com/McCartneyAC/Back_of_the_Envelope", type = "github")
                                      ) #box
                              ) , #tabItem
                              
                              
                              ## Data Subsection ---------------------------------------------------------
                              # 
                              tabItem(tabName = "reg_upload",
                                      box( title = "Upload and Model",width = 7,
                                           fileInput("FileInput", "Input Your Data Set"),
                                           helpText("Dataset must be one of: .csv, .sav, .dta, .xlsx, or .rda")
                                           # , 
                                           # tags$p(tags$b("Google Sheets:")), 
                                           # textInputIcon(
                                           #    inputId = "googlesheets_link",
                                           #    label = "Link your GoogleSheet",
                                           #    icon = list("https://")
                                           # ),
                                           # actionBttn(
                                           #    inputId = "load_googlesheet",
                                           #    label = "Load", 
                                           #    style = "minimal",
                                           #    color = "primary"
                                           # ),
                                           # helpText("Google Sheets must be open to the public.")
                                      ), #upload box
                                      box(title = "Your Data", width = 12, 
                                          DT::dataTableOutput("reg_data_table")
                                      ) # box (Dataset output)
                                      
                              ), #tabItem (reg upload)
                              # # Variables
                              tabItem(tabName = "reg_dossier", title = "Dossier", 
                                      box(
                                         tags$p("The Dossier function allows you to select an individual observation among your data and observe all of its variables. This can be useful for detecting data problems, examining particular outliers, or seeing your variables at a glance."), 
                                         selectInput("reg_dossier_ID","Choose an ID Variable:", 
                                                     choices = NULL, 
                                                     selected = NULL), 
                                         selectInput("reg_dossier_choice", "Choose an Observation:",
                                                     choices = NULL, 
                                                     selected = NULL)
                                      ), #box
                                      box(
                                         DT::dataTableOutput("dossier") 
                                      ) #box
                              ),# tabItem Dossier
                              # # Describe
                              tabItem(tabName = "reg_desc", title = "Describe",  
                                      box(title = "Data Description", width  = 10,
                                          materialSwitch(
                                             inputId = "ext_desc",
                                             label = "Extended Description", 
                                             value = FALSE,
                                             status = "primary"
                                          ),
                                          # selectInput(
                                          #    inputId = "desc_group",
                                          #    label = "Describe By A Group", 
                                          #    choices = NULL
                                          # ),
                                          gt::gt_output("description"))
                              ),
                              
                              # # Correlation
                              tabItem(tabName = "reg_cor", title = "Correlations",
                                      box(title = "Correlation Matrix", width = 12, height = 700, 
                                          plotOutput("cors")
                                      )# box
                              ), #tabItem
                              
                              
                              
                              
                              ## Model Subsection --------------------------------------------------------
                              
                              tabItem(tabName = "reg_model", title = "Build your Model",
                                      fluidRow(infoBoxOutput("formulabox", width = 7)) ,
                                      box(title = "Build Your Model:", width = 7,
                                          radioGroupButtons(
                                             inputId = "reg_outcome",
                                             label = "Outcome:", 
                                             choices = c("linear" = "linear",
                                                         "logistic" = "logistic"),
                                             status = "primary"
                                          ),
                                          #shinywidget
                                          materialSwitch(inputId = "rbst", 
                                                         label = "Robust Standard Errors", 
                                                         status = "primary"),
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
                                          # selectInput("instrument","Instrumental Variable (forthcoming)", 
                                          #             choices = NULL, 
                                          #             selected = NULL),
                                          pickerInput(
                                             inputId = "clstr",
                                             label = "Clustering Type:", 
                                             choices = c("No Clustering",
                                                         "Fixed Effects",
                                                         "Cluster Standard Errors",
                                                         "Multilevel / LME / HLM (coming soon)"),
                                             options = list(
                                                style = "btn-primary")
                                          ),
                                          selectInput(inputId = "clust",
                                                      label = "Cluster Variable:", 
                                                      choices = NULL
                                          ) #, #select input
                                          # tags$p("Subset by Variable"), 
                                          # selectInput(inputId = "subset_var",
                                          #             label = "Subset Variable:", 
                                          #             choices = NULL
                                          # ), 
                                          # selectInput(inputId = "subset_val",
                                          #             label = "Subset Value:", 
                                          #             choices = NULL
                                          # )
                                      ) #model building box
                                      
                              ), #tabitem model building
                              # # Plot
                              tabItem(tabName = "reg_plot", title = "Plot", 
                                      tabsetPanel(type = "tabs",
                                                  tabPanel("Main Effect", 
                                                           tabBox(title = "Main Effect", width = 9,
                                                                  tabPanel("Relationship", plotOutput("main_relationship")), 
                                                                  tabPanel("Residuals", plotOutput("bivar_resid"), 
                                                                           tags$p("To see the residuals for a different predictor, re-order your independent variables on the model tab."))
                                                           ) #tabBox
                                                           
                                                  ),#tabPanel
                                                  tabPanel("Marginal Effects",
                                                           box(title = "Marginal Effects", width = 9, 
                                                               plotOutput("marginal")) #box
                                                  ),
                                                  tabPanel("Added Variable Plots", 
                                                           box(title = "Added Variable Plots", width = 9, 
                                                               selectInput(inputId = "restricted",
                                                                           label = "Select your Predictor", 
                                                                           choices = NULL
                                                               ),
                                                               helpText("AV Plots require at least two continuous predictor variables. Otherwise you will receive an error."),
                                                               plotOutput("avplot")
                                                               
                                                           ) #box
                                                  ) #tab panel (added variable)
                                      ) #tabset panel (plotting tabs)
                              ), #tabitem (plots page)
                              # # Summary
                              tabItem(tabName = "reg_sum", title = "Output Summary", 
                                      box(
                                         #  TODO: Be sure to include a null_model if LME is selected and if a cluster is chosen
                                         # tags$br(), 
                                         htmlOutput("tabmodel")
                                      ) #box
                              ),
                              # # Diagnostic Plots
                              tabItem(tabName = "reg_ddx", title = "Diagnostic Plots", 
                                      tabsetPanel(type = "tabs",
                                                  tabPanel("Residuals", 
                                                           box(plotOutput("hist_resid"))
                                                  ),
                                                  tabPanel("QQ Plot", 
                                                           box(plotOutput("normal_qq"))
                                                  ),
                                                  tabPanel("Residual vs Fitted", 
                                                           box(plotOutput("resid_v_fitted"))
                                                  )# tab panel
                                      ) #tabset panel
                              ), #reg_ddx tab
                              # # Outliers
                              tabItem(tabName = "reg_outlier", title = "Outlier Analysis",
                                      tabsetPanel(type = "tabs",
                                                  tabPanel("Cook's Distance", 
                                                           box(plotOutput("cooks_d"))
                                                  ), # Cook's Distance
                                                  tabPanel("Leverage", 
                                                           box(
                                                              tags$p("Coming soon, I promise"),
                                                              tags$p("Here: interactive graph to allow for point-and-click deletion of outlying points."),
                                                              plotOutput("brush_plot", height = 350,
                                                                         click = "plot1_click",
                                                                         brush = brushOpts(
                                                                            id = "plot1_brush"
                                                                         )),
                                                              actionBttn(
                                                                 inputId = "exclude_toggle",
                                                                 label = "Toggle points", 
                                                                 style = "minimal",
                                                                 color = "primary"
                                                              ),
                                                              actionBttn(
                                                                 inputId = "exclude_reset",
                                                                 label = "Reset", 
                                                                 style = "minimal",
                                                                 color = "primary"
                                                              )
                                                           ) # box
                                                  ), #tabPanel
                                                  tabPanel("Influence Index", 
                                                           box(
                                                              tags$p("Leverage: extremity on X"),
                                                              tags$p("Discrepancy: extremity on Y"),
                                                              tags$p("Coming soon, I promise"),
                                                              tags$p("First - Studentized Residuals"),
                                                              tags$p("Second - hat values"),
                                                              # car::influenceIndexPlot(model())
                                                           )#box
                                                  )#tabPanel
                                      ) #tabset panel
                              ),# tab item.
                              tabItem(tabName = "reg_path", title = "Mediation Analysis", 
                                       tabsetPanel(type = "tabs",
                                                   tabPanel("model",box()), 
                                                   tabPanel("Summary", box()), 
                                                   tabPanel("Diagram", box())
                                                   )
                                      ),#tab item. 
                              
                              
                              
                              ## FAQ ---------------------------------------------------------------------
                              
                              
                              tabItem(tabName = "FAQ_tab",
                                      tabBox( width = 10, 
                                             tabPanel(title = "Frequently Asked",
                                                      tags$p(tags$b("How did it get started? "), 
                                                             "Back of the Envelope is the culmination of two (and maybe more?) project ideas that I have worked on in the year 2019. When I first learned to use Shiny R, I couldn't get the idea out of my head that someone should build a point-and-click style regression tool that utilized all and only those presets that I found helpful and that gave its output in ways that I tended to use when doing homework or preparing presentations and publications. Several extant R packages were outputting results in APA format our otherwise had defaults that were best-in-the-industry for a grad student. After leaving grad school, I put this idea into practice. The original version used wired.js and R's xkcd package to make all regression plots and fonts look hand-drawn. In this iteration, I have restored defaults so you can use it directly in publications. You're welcome...but I do miss the sketchiness.

The idea is a dangerous amount of statistical sophistication: enough to give it a strong semblance of accuracy, but leaving out the super technical details that might be important for research publication. Back of the Envelope regression is good enough for stat homework and basic publications." 
                                                      ),
                                                      tags$p(tags$b("How can I support the project?"), 
                                                             "By clicking on the paypal link on the left, you can contribute to the project and keep it up and running."),
                                                      tags$p(tags$b("I got an error I don't understand!"), "In general, getting an error means you tried to do something that is either unsupported by the app at this time or impossible under the constraints or definitions of contemporary statistics. For example, supplying a continuous variable for logistic regression will get an error. (Supplying a binary variable to linear regression will get you a linear probability model.). Otherwise, leaving things blank will produce errors--did you forget to upload data or define a model? That's the usual culprit."),    
                                                      tags$p(tags$b("What happens to my Data? Are they confidential?"),
                                                             "Yes. When you load Back of the Envelope, you create a new temporary server that only exists while you are using it. You may notice that after a while of being idle, the server stops running and needs to be restarted. Your data exist only on this server and aren't stored by RStudio, let alone by me. The tool is probably not strictly HIPAA compliant given that your data would need to be uploaded to the web; contact me to learn about running a local version of Back of the Envelope."), 
                                                      tags$p(tags$b("What's coming next? "), 
                                                             "Upcoming features include heirarchical linear modeling, instrumental variables, support for googlesheets data input, editable data tables, and more advanced subsetting."
                                                      )  #tags$p        
                                                      ),  #tabpanel: Frequently Asked
                                             tabPanel(title = "Models Matrix", tags$p("Not every possible configuration of inputs results in a coherent or fully defined model. Use this matrix to determine whether the model you are trying to run is possible (and to check which package is doing the statistics on the back end). ")),
                                             tabPanel(title = "Technical Notes",
                                                      tags$p(tags$b("I can't run the model I want!"), 
                                                             "As noted above, some things just aren't possible. For example, residuals are not normally distributed around a logistic regression model, so trying to check the residuals graph for logistic models will throw an error. If you're trying to do something that you think should be possible, please contact me (the @ symbol on the left). Binary models with mixed effects are currently unsupported, but it's in the works."),
                                                      tags$p(tags$b("I want to run an ANOVA or a t-test. Why is that so hard?"),
                                                             "It's not! T-tests and ANOVAs (and ANCOVA, etc) are all special cases of linear regression. If your dependent variable is continuous, use your grouping variable as the only independent variable and that is equivalent to a t-test. Do the same with more than 2 groups for ANOVA and add covariates for ANCOVA. The only problem is this: your data must be in long format. Repeated-measures ANOVA won't be available until HLM is implemented, which is coming soon."),
                                                      tags$p(tags$b("Any mathematical notes?"),
                                                             "Yes. Things like sums of squares vary from package to package, and significantly, between R and Stata. The sums of squares and standard errors found herein may or may not match those of other software. In general, it's best to cite your estimation package. Most of the available regression models herein are from the packages: MASS, CAR, ", tags$a(href = "https://github.com/rstudio/cheatsheets/raw/master/estimatr.pdf", "estimatr" ), ", or lme4."),
                                                      tags$p(tags$b("An Identification Variable"),
                                                             "For using Back of the Envelope, it's generally best to have ID variables for each row of your data. This allows you to use the dossier tab, but it also allows you to compare results of the cook's distance output (which is row-numbered) and refer these directly back to your data set.")
                                                      )
                                      ), #tabbox
                              ) # tab item. (LAST ONE)
                              
                           ) #tabitems
                        ) #Dashboard Body
) #Dashboard Page














# Server Components -------------------------------------------------------



server <- function(input, output, session) { 
   
   
   
   # Data Input --------------------------------------------------------------
   
   datasetInput <- reactive({
      infile <- input$FileInput
      if (is.null(infile))
         return(NULL)
      dat<-use(infile$datapath)
      names(dat) <-  gsub(" ", "_", names(dat), fixed = TRUE) 
      return(dat)
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
   observeEvent(indvariable(), {
      updateSelectInput(session, "restricted", choices = indvariable())
   })
   observeEvent(datasetInput(), {
      updateSelectInput(session, "instrument", choices = names(datasetInput()))
   })
   observeEvent(datasetInput(), {
      updateSelectInput(session, "desc_group", choices = c("None", names(datasetInput())))
   })

   # Data Table --------------------------------------------------------------
   output$reg_data_table = DT::renderDataTable(datasetInput())
   
   
   
   # Describe the Data Set ---------------------------------------------------
   descgroup <- reactive({
      input$desc_group
   })
   
   desc <- reactive({
      req(datasetInput())
      #desc_formula<- as.formula(substitute(datasetInput()  ~  descgroup()  ))
      #grp<-paste0(deparse(substitute(descgroup()))) 
         datasetInput() %>%
            select_if(is_numeric) %>%
            psych::describe(., fast = !(input$ext_desc)) %>%
            add_rownames(var = "Variable") %>%
            dplyr::select(-c(vars)) %>%
            dplyr::mutate(dplyr::across(is.numeric, round, 2)) %>%
            gt::gt() %>%
            gt::tab_options(
               column_labels.font.size = "small",
               table.font.size = "small",
               row_group.font.size = "small",
               data_row.padding = px(3)
            ) %>%
            gt::tab_header(title = paste0("Data Description"))
})
   
   

   # description table (psych::describe)
  # output$description =  DT::renderDataTable(desc())
   output$description =  gt::render_gt(desc())
   
   # Correlation Table -------------------------------------------------------
   
   output$cors <- renderPlot({
      req(datasetInput())
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
         theme_light() + 
         theme(axis.text.x = element_text(angle = 50, hjust = 1))
   }, height = 650
   )
   
   
   
   
   # Generate a Regression Formula -------------------------------------------
   
   feats <- reactive({
      req(datasetInput())
      paste(input$indevars, collapse = " + ")
   })
   
   regFormula <- reactive({
      if (input$clstr == "Fixed Effects"){
         as.formula(paste(input$responsevar, ' ~ ', feats()," + factor(", cluster_var(), ")"))
      } else {
         as.formula(paste(input$responsevar, ' ~ ', feats())) 
      }
   })
   
   # Make the Formula Visual
   reg_formula_text<-reactive({
      req(indvariable())
      req(depvariable())
      paste(deparse(regFormula(), width.cutoff = 500), collapse="")
   })
   # an infobox for same:
   output$formulabox <- renderInfoBox({
      infoBox(
         "Your Model:", reg_formula_text(), icon = icon("subscript"),
         color = "black", width = 7
      )
   })
   
   # creates the actual model summary object. 
   output$model <- renderPrint({
      req(indvariable())
      req(depvariable())
      summary(model())
   })
   # passes model summary object into the Sjplot model summary HTML thing. 
   output$tabmodel <- renderUI({
      req(model())
      modeltab <- tab_model(model())
      HTML(modeltab$knitr)
   })  
   
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
      if (input$reg_outcome == "logistic") {
         logistic()
      } else {
         linear()
      }
   })
   
   
   # Plots (all of them) -----------------------------------------------------
   # Marginal Effects Plot:
   
   output$marginal <- renderPlot({
      req(datasetInput()) 
      plot_model(model(), vline.color = "grey", show.values = TRUE, value.offset = .3) +
         theme_light()
   }
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
   reg_variables_choice <- reactive({
      input$reg_variables_choice
   })
   
   output$main_relationship <- renderPlot({
      req(model())
      if (length(input$indevars) == 1) {
         if (input$reg_outcome == "linear") {
            datasetInput() %>% 
               ggplot(aes_string(x = indvariable1(), y = depvariable())) +
               theme_light() +
               geom_point() + 
               geom_smooth(method = "lm") 
         } else if (input$reg_outcome == "logistic") {
            datasetInput() %>%
               ggplot(aes_string(x = indvariable1(), y = depvariable())) +
               geom_point() +
               geom_smooth(method = "glm",
                           method.args = list(family = "binomial")) +
               theme_light()
         } else {
            print(NULL)
         }
      } else {
         if (input$reg_outcome == "linear") {
            datasetInput() %>%
               ggplot(aes_string(
                  x = indvariable1(),
                  y = depvariable()
               )) +
               geom_point(alpha = 0.6, aes_string(color = indvariable2())) +
               theme_light() + 
               geom_smooth(method = "lm")
         } else if (input$reg_outcome == "logistic") {
            datasetInput() %>%
               ggplot(aes_string(
                  x = indvariable1(),
                  y = depvariable()
               )) +
               geom_point(alpha = 0.6, aes_string(color = indvariable2())) +
               geom_smooth(method = "glm",
                           method.args = list(family = "binomial")) +
               theme_light()
         } else {
            print(NULL)
         }
      }
   })
   
   
   
   output$bivar_resid <-  renderPlot(if (input$reg_outcome == "linear") {
      req(model())
      datasetInput() %>%
         drop_na() %>% 
         ggplot(aes_string(x = indvariable1(), y = model_residuals())) +
         geom_point() +
         geom_smooth(method = "lm") +
         theme_light()
   } else if (input$reg_outcome == "logistic") {
      datasetInput() %>%
         drop_na() %>% 
         ggplot(aes_string(x = indvariable1(), y = depvariable())) +
         geom_point() +
         annotate("text", x = mean(xrange()), y = mean(yrange()), label = "Error is not normally distributed in logistic regression.") +
         theme_light()
   } else {
      print(NULL)
   })
   
   
   
   # DIagnostic Plots! library(Lindia)
   
   output$hist_resid <-renderPlot({
      req(model())
      gg_reshist(model())+
         theme_light()
   }
   )
   
   output$resid_v_fitted<-renderPlot({
      req(model())
      gg_resfitted(model())+
         theme_light()
   })
   
   output$normal_qq<-renderPlot({
      req(model())
      gg_qqplot(model())+
         theme_light()
   }
   )
   
   
   output$cooks_d <- renderPlot({
      req(model())
      gg_cooksd(model()) + 
         theme_light()
   })
   
   # AV PLOT CONSTRUCTION
   # Hey COOL : ( mod_vars = all.vars( formula(fit1) )[-1] ) grabs all the X variables. 
   
   restricted_choice <- reactive({
      input$restricted
   })
   
   rstrctd_list<-reactive({
      indvariable()[indvariable() != restricted_choice()]
   })
   rstrctdfeats<- reactive({
      paste(rstrctd_list(), collapse = " + ")
   })
   fullformula <-  reactive({
      as.formula(paste(depvariable(), ' ~ ', rstrctdfeats()))
   })
   
   partialformula <-reactive({
      as.formula(paste(restricted_choice(), ' ~ ', rstrctdfeats()))
   })
   
   # Model for Y ~ all but chosen X val
   partiallinear <- reactive ({
      if (input$rbst & (input$clstr == "Cluster Standard Errors")) {
         lm_robust(partialformula(), clusters = cluster_var(), data = datasetInput())
      } else {
         lm(partialformula(), data = datasetInput())
      }
   })
   partiallogistic <- reactive({
      if (input$rbst) {
         robust::glmRob(
            partialformula(),
            data = datasetInput(),
            family = binomial(),
            method = "cubif"
         )
      } else {
         glm(partialformula(), data = datasetInput(), family = "binomial")
      }
   })
   partialdmodel <- reactive({
      if (input$reg_outcome == "logistic") {
         partiallogistic()
      } else {
         partiallinear()
      }
   })
   
   # model for x_chosen ~ all other x values
   fulllinear <- reactive ({
      if (input$rbst & (input$clstr == "Cluster Standard Errors")) {
         datasetInput() %>% 
            drop_na() %>% 
            lm_robust(fullformula(), clusters = cluster_var(), data = .)
      } else {
         datasetInput() %>% 
            drop_na() %>% 
            lm(fullformula(), data = .)
      }
   })
   
   fulllogistic <- reactive({
      if (input$rbst) {
         datasetInput() %>% 
            drop_na() %>% 
            robust::glmRob(
               fullformula(),
               data = . ,
               family = binomial(),
               method = "cubif"
            )
      } else {
         datasetInput() %>% 
            drop_na() %>% 
            glm(fullformula(), data = ., family = "binomial")
      }
   })
   
   
   fullmodel <- reactive({
      if (input$reg_outcome == "logistic") {
         fulllogistic()
      } else {
         fulllinear()
      }
   })
   
   
   # final choice
   output$avplot <- renderPlot({
      req(model())
      gg_added_var(extended = fullmodel(),  partial= partialdmodel()) 
   }
   )
   
   
   
   
   # Dossier Code ------------------------------------------------------------
   reg_dossier_id <- reactive({
      input$reg_dossier_ID
   })
   reg_dossier_choice <- reactive({
      input$reg_dossier_choice
   })
   id_var <- reactive({
      if (is.null(datasetInput())) {
         return(NULL)
      } else {
         datasetInput() %>% 
            dplyr::select(reg_dossier_id())
      }
   })
   observeEvent(datasetInput(), {
      updateSelectInput(session, "reg_dossier_ID", choices = names(datasetInput()))
   })
   observeEvent(reg_dossier_id(), {
      updateSelectInput(session, "reg_dossier_choice", choices = id_var())
   })
   
   
   output$dossier <-  DT::renderDataTable({
      req(datasetInput())
      datasetInput() %>% 
         dplyr::filter(.data[[reg_dossier_id()]] == !!(reg_dossier_choice())) %>% 
         t() %>% 
         as.data.frame() %>% 
         tibble::rownames_to_column(var = "variable") %>% 
         rename("Value" = "V1")}
   )
   

# download report ---------------------------------------------------------
   output$downloadReport <- downloadHandler(
      filename = function() {
         paste('RegressionOutput', sep = '.', switch(
            input$format, "PDF" = 'pdf', "HTML" = 'html', "Word" = 'docx'
         ))
      },
      content = function(file) {
         src <- normalizePath('report.Rmd')
         owd <- setwd(tempdir())
         on.exit(setwd(owd))
         file.copy(src, 'report.Rmd')
         
         library(rmarkdown)
         out <- render('report.Rmd', switch(
            input$format,
            PDF = pdf_document(), HTML = html_document(), Word = word_document()
         ))
         file.rename(out, file)
      })

   
   
   
   # details for brushing leverage -------------------------------------------
   
   vals <- reactive({
      keeprows = rep(TRUE, nrow(datasetInput()))
   })
   
   
   output$brush_plot <- renderPlot({
      # Plot the kept and excluded points as two separate data sets
      keep    <- bind_cols(datasetInput(), vals$keeprows)
      exclude <- bind_cols(datasetInput(), vals$keeprows)
      
      p<- ggplot(keep, aes(y = depvariable(), x = indvariable1())) +
         geom_smooth(method = "lm", fullrange = TRUE, color = "black") +
         geom_point(data = exclude, shape = 21, fill = NA, color = "black", alpha = 0.25) +
         coord_cartesian(xlim = xrange() , ylim = yrange())
      
      p
   })
   
   
   
   # Toggle points that are clicked
   observeEvent(input$plot1_click, {
      res <- nearPoints(datasetInput(), input$plot1_click, allRows = TRUE)
      
      vals$keeprows <- xor(vals$keeprows, res$selected_)
   })
   
   # Toggle points that are brushed, when button is clicked
   observeEvent(input$exclude_toggle, {
      res <- brushedPoints(datasetInput(), input$plot1_brush, allRows = TRUE)
      
      vals$keeprows <- xor(vals$keeprows, res$selected_)
   })
   
   # Reset all points
   observeEvent(input$exclude_reset, {
      vals$keeprows <- rep(TRUE, nrow(datasetInput()))
   })
   

   
   
}

shinyApp(ui, server)
