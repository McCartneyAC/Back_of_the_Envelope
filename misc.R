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
