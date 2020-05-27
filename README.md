# 🖂  Back of the Envelope 📉 
can we build a basic point-and-click regression analysis tool with R shiny? 


## The goal:

Build a general purpose regression tool, incorporating extensions to regression such as heteroskedasticity-robust standard errors, clustered standard errors, multilevel modeling, and logistic regression. 

## Subgoal: 

Use combinations of `wired.js` and the R `xkcd` package to make the entire thing look goofy and hand drawn so that no one takes it seriously. 



With sufficient on-your-own data preparation, this tool should be sufficient for basic regression analyses for beginner-to-intermediate level social-science use. I have no plans to implement latent-variable / structural equation modeling at this time. 

## To Do: 
* ~Output should include `SjPlot`'s `tab_model()` output for APA-style regression tables~
* * ~there be dragons with clustered standard errors~
* * Dragons Slayed. 
* `ggplot2` representations of the model
* * ~bivariate~
* * ~bivariate residual plot~
* * two independent variables (close!)
* * added variable plots
* * Plot residuals. (it's just [predicted v actual] + ~[residual v fitted]~)
* ~Additionally, need to figure out how to modify the DT with `%>%` to round `psych::describe()` to two decimal places.~
* ~include correlation table from `SjPlot`~
* ~include data table~
* ~include `psych::describe()`~
* ~Robust Specification~ 
* ~Logistic Regression up and running~
* ~Multivariate Regression Up and running~
* clusters:
* * Linear Mixed
* * ~fixed effects (just make it add `factor()` of whatever variable to the data, then update the model to include this)~
* * * option to eliminate coefficients with `felm()`
* * ~standard errors (this will make error for SjPlot see his tweet reply on this topic)~
* ~Additional common filetypes supported. (stata, spss, csv, excel)~
* * need to add support for google sheets ????
* ~fix odd error with spaces in variable names~
* Instrumental variables / two-stage least squares? (Save for 1.2)
* ~margins plots~
* Outlier Analysis:
* * follow this for outlier removal: https://www.shinyapps.org/apps/p-hacker/
* * Cook's Distance Calculator (onHover over ggplot)
* * Leverage Calculator?
* * Influence index plot (from `car`, can it be remade in `ggplot2` though?)
* ~Adjust `SjPlot`'s marginsplot to include a dotted line at 0 for reference.~ 
* * ~or 1 for logistic obv.~ 
* * ~`plot_model(m1, vline.color = "red")` (it's already built in to SjP)~
* * ~Also: `plot_model(m1, show.values = TRUE, value.offset = .3)`~
* editable data tables: https://github.com/jbryer/DTedit
* Quantiles? `geom_quantile()`
* Pure description + plot all variables against each other a la https://drsimonj.svbtle.com/plot-some-variables-against-many-others
* `ggvis` overhaul, at least for main two or three plots? 
* ~rearrange upload / model page (incorporates text of current model)~
* * drop-down message of current model (nixed due to error logging)

## User Feedback:
* For the Correlation table, you may want to rotate your x-axis labels 45 or 90 degrees.  Getting a lot of overlap for files with > 20 factors


With a sufficient amount of effort, this could actually be a pretty good tool for multilevel linear modeling, ~provided I can figure out what exactly the error was with `mice`'s MCMC imputation.~ imputation is gonna have to wait for v1.2 at the absolute earliest. 

## Better names:
www.backoftheenvelope.com  ?

## reading material:
* https://stackoverflow.com/questions/38878113/implementing-reactive-values-in-regression-using-shiny?rq=1
* https://stackoverflow.com/questions/56068825/use-selectbox-to-create-regression-formula-from-user-input-dataset/56069365#56069365
* https://stackoverflow.com/questions/43102554/build-linear-regression-with-dynamic-inputs-in-r-shiny?rq=1
* Reformulate: http://novicemetrics.blogspot.com/2011/04/forming-formulas.html?utm_source=feedburner&utm_medium=feed&utm_campaign=Feed%3A+CoffeeAndEconometricsInTheMorning+%28Coffee+and+Econometrics+in+the+Morning%29

### it's been done:
but not with user-input data:
https://rich.shinyapps.io/regression/
(use this for model work)
