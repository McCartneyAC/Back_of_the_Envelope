fa("envelope", fill = "steelblue")
fa("envelope-open", fill = "steelblue")
fa("envelope-open-text", fill = "steelblue")
fa("chart-line", fill = "steelblue")
fa("chart-bar", fill = "steelblue")
fa("chart-area", fill = "steelblue")
fa("r-project", fill = "steelblue")


# Model Building
linear <- reactive ({
  if (input$rbst) {
                MASS::rlm(regFormula(), data = datasetInput())
            } else {
                lm(regFormula(), data = datasetInput())
    }
  })
          
logistic <- reactive({
    glm(regFormula(), data = datasetInput(), family = "binomial")
})
          
model <- reactive({
  if (input$rgrssn == "logistic") {
    logistic()
    } else {
    linear()
    }
})


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


   output$bivariate <- renderPlot(
       datasetInput() %>%
           ggplot(aes_string(x = indvariable(), y = depvariable())) +
           geom_jitter() +
           geom_smooth(method = "lm") +
           theme_xkcd() +
           xkcdaxis(xrange(), yrange())
)





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
                    label = "Cluster Varibale: (coming soon)", 
                    choices = NULL
        )


lme_model <-lmer(lnwg ~ 1 + (1 |id), data = hours)




















  output$trivariate <- renderPlot(
    datasetInput() %>%
        ggplot(aes_string(x = ...
