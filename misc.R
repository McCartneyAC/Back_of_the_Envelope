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
                          theme(panel.grid.major = element_blank(), axis.ticks = element_line(colour = "black"),  
                     panel.background = element_blank(), panel.grid.minor = element_blank(), 
                     legend.key = element_blank(), strip.background = element_blank(), 
                     text = element_text(size = 16, family = "xkcd")) +
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










regress<- function(df, cluster = NA, ...) {
  require(miceadds)
  require(multiwayvcov)
  if (is.na(cluster)) {
    summary(
      lm(data = df, ...)
    )
  } else {
    summary(
      lm.cluster(data = df, cluster = cluster, ...)
    )
  }
}



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
   indvariables <- reactive({
       input$indevars
   })
    iv1 <- reactive({
      indevariables()[1]
      })
    iv2 <- reactive({
      indevariables()[2]
      })
   depvariable <- reactive({
       input$responsevar
   })

  output$trivariate <- renderPlot(
    datasetInput() %>%
        ggplot(aes_string(x = iv1(), y = depvariable(), color = iv2())) + 
    # and if fixed effects are selected, make color = factor(iv2())
          geom_jitter() +
          geom_smooth(method = "lm") +
                         theme(panel.grid.major = element_blank(), axis.ticks = element_line(colour = "black"),  
                     panel.background = element_blank(), panel.grid.minor = element_blank(), 
                     legend.key = element_blank(), strip.background = element_blank(), 
                     text = element_text(size = 16, family = "xkcd")) +
          xkcdaxis(xrange(), yrange())
    )
 plot <- renderPlot({
   if (length(indevariables() == 1)) {
     output$bivariate
     } else if (length(indeariables() == 2)) {
     output$trivariate 
     } else {
     NULL 
     }
   })
