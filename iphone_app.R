library(shiny)
library(shinyMobile)
library(echarts4r)
library(shinyWidgets)

shiny::shinyApp(
  ui = f7Page(
    
    
    title = "Back of the Envelope Regression",
    
    
    
    
    
    
    
    
    
    
    f7TabLayout(
      panels = tagList(
        f7Panel(title = "Upload and Model", side = "left", theme = "light", #icon("function")
                # Tab Pieces go Here
                     fileInput("FileInput", f7Button(
                       inputId = "FileInputButt", 
                       label = "Upload your Data", 
                       color = "deeporange"
                     )), #icon("tray_arrow_up_fill")
                     tags$h3(tags$b("Build your Model:")), 
                
                
                      f7Radio(
                       inputId = "rgrssn", label = "Regression:",
                       choices = c("linear" = "linear",
                                   "logistic" = "logistic"), 
                       selected = "linear"
                     ),
                     #shinywidget
                f7Toggle(inputId = "rbst", label = "Robust Standard Errors"),
                     #wired_toggle(inputId = "rbst", label = "Robust Standard Errors"),
                     tags$p(tags$b("Select your variables for analysis:")),
                f7SmartSelect(inputId = "responsevar",
                                 label = "Your DV / Response Variable:", 
                                 choices = NULL
                     ),
                f7SmartSelect("indevars", "Your IV / Predictor Variable(s):", 
                                    choices = NULL, 
                                    selected = NULL, 
                                    multiple = TRUE,
                                #    options = NULL
                              ),
                     
                f7SmartSelect(
                       inputId = "clstr",
                       label = "Clustering:",
                       choices = c("None",
                                   "Fixed Effects",
                                   "Cluster Standard Errors",
                                   "Multilevel / LME / HLM"),
                       #selected = NA, 
                       multiple = FALSE
                     ), 
                f7SmartSelect(inputId = "clust",
                                 label = "Cluster Varibale: (coming soon)", 
                                 choices = NULL, 
                                 multiple = FALSE
                     ),
        
                
                
                
                effect = "cover")
        
      ),
      
      
      
      
      
      
      
      
      
      
      navbar = f7Navbar(
        title = "Regression",
        hairline = FALSE,
        shadow = TRUE,
        left_panel = TRUE,
        right_panel = TRUE
      ),
      
      
      
      
      
      
      
      f7Tabs(
        animated = TRUE,
        #swipeable = TRUE,
      
      
        f7Tab(
          tabName = "About",
          icon = f7Icon("book"),
          active = TRUE,
          f7Shadow(
            intensity = 10,
            hover = TRUE,
            f7Card(
              title = "About Back of the Envelope",
              tags$h3("Old about text:"),
              tags$p("This is intended to be a toy point-and-click-style regression tool to practice R Shiny application development and to enumerate the complexities available in regression analysis. Like R itself, this tool comes with absolutely no warranty."),

              tags$p("The sketchy nature of the application is intended to deter its use for serious purposes and strengthen the feeling of it being a back-of-the-envelope tool for regression analysis. Use the features to quickly explore options for regression and their effect on your analysis, but resist the urge to p-hack."),

"Use",
tags$p("Use the tool by uploading your own data set in one of the listed formats. Browse your data and examine the variables' descriptive statistics, as well as the table of correlations, then create your model to run."),

       tags$p("What this app doesn't do: This app does not allow for any kind of data preparation. Techniques such as interaction terms, exponential terms, or complex extensions such as regression discontinuity need to be done in whatever data-preparation program you choose to use (e.g. excel) before data can be uploaded and used here. For example, to include polynomials, create a new variable in your dataset that is x^2 and re-upload the dataset to run a new regression. Fixed effects are supported (coming soon!) but if you wish to choose your reference category, you will need to create dummy variables in your dataset and re-upload."),
              tags$a(href="https://github.com/McCartneyAC/Back_of_the_Envelope", f7Icon("logo_github"))

            )
          )
        ) ,
        
        
        
        

        
        f7Tab(
          tabName = "Dataset",
          icon = f7Icon("table"),
          active = FALSE,

        ),

        f7Tab(
          tabName = "Description",
          icon = f7Icon("chart_bar_square_fill"),
          active = FALSE,
        ),
        f7Tab(
          tabName = "Correlation",
          icon = f7Icon("square_grid_4x3_fill"),
          active = FALSE,
        ),
        f7Tab(
          tabName = "Plot",
          icon = f7Icon("graph_square_fill"),
          active = FALSE,
        ),
        f7Tab(
          tabName = "Summary",
          icon = f7Icon("list_number_rtl"),
          active = FALSE,
        )
        # f7Tab(
        #   tabName = "Outliers",
        #   icon = f7Icon("minus_slash_plus"),
        #   active = FALSE,
        # ),
        
        
        
        
        
      )
    )
  ),
  
  
  
  server = function(input, output) {
    
    


  }
  
  
)
