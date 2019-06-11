# 🖂  Back of the Envelope 📉 
can we build a basic point-and-click regression analysis tool with R shiny? 


## The goal:

Build a general purpose regression tool, incorporating extensions to regression such as heteroskedasticity-robust standard errors, clustered standard errors, multilevel modeling, and logistic regression. 

## Subgoal: 

Use combinations of `wired.js` and the R `xkcd` package to make the entire thing look goofy and hand drawn so that no one takes it seriously. 



With sufficient on-your-own data preparation, this tool should be sufficient for basic regression analyses for beginner-to-intermediate level social-science use. I have no plans to implement latent-variable / structural equation modeling at this time. 

## To Do: 
* ~Output should include `SjPlot`'s `tab_model()` output for APA-style regression tables~
* `ggplot2` representations of the model
* * ~bivariate~
* * two independent variables
* * added variable plots
* ~Additionally, need to figure out how to modify the DT with `%>%` to round `psych::describe()` to two decimal places.~
* ~include correlation table from `SjPlot`~
* ~include data table~
* ~include `psych::describe()`~
* ~Robust Specification~ 
* ~Logistic Regression up and running~
* ~Multivariate Regression Up and running~
* clusters:
* * Linear Mixed
* * fixed effects (just make it add `factor()` of whatever variable to the data, then update the model to include this)
* * standard errors
* ~Additional common filetypes supported. (stata, spss, csv, excel)~
* ~fix odd error with spaces in variable names~
* Instrumental variables / two-stage least squares? 
* ~margins plots~
* Outlier Analysis:
* * Cook's Distance Calculator (onHover over ggplot)
* * Leverage Calculator?
* * Influence index plot (from `car`, can it be remade in `ggplot2` though?)

With a sufficient amount of effort, this could actually be a pretty good tool for multilevel linear modeling, provided I can figure out what exactly the error was with `mice`'s MCMC imputation. 

## Better names:
www.backoftheenvelope.com  ?

## reading material:
* https://stackoverflow.com/questions/38878113/implementing-reactive-values-in-regression-using-shiny?rq=1
* https://stackoverflow.com/questions/56068825/use-selectbox-to-create-regression-formula-from-user-input-dataset/56069365#56069365
* https://stackoverflow.com/questions/43102554/build-linear-regression-with-dynamic-inputs-in-r-shiny?rq=1

### it's been done:
but not with user-input data:
https://rich.shinyapps.io/regression/
(use this for model work)
