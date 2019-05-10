select2Input("select2Input1","This is a multiple select2Input. The items are re-arrangeable",
                        choices=c("a","b","c"),
                        selected=c("b","a"))
         )


f()<-reactive({
  as.formula(paste0(input$dependentvar, " ~ ", input$indevar))
  })
model<-reactive({
  if (robust = FALSE){
  lm(f(), data = dataInput())
    }
  else {
    rlm(f(), data = dataInput())
    }
  })

fa("envelope", fill = "steelblue")
fa("envelope-open", fill = "steelblue")
fa("envelope-open-text", fill = "steelblue")
fa("chart-line", fill = "steelblue")
fa("chart-bar", fill = "steelblue")
fa("chart-area", fill = "steelblue")
fa("r-project", fill = "steelblue")

# robust:
library(MASS)
rlm_mod <- rlm(stack.loss ~ ., stackloss, psi = psi.bisquare)  # robust reg model
summary(rlm_mod)
