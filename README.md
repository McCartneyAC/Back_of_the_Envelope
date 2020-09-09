# 🖂  Back of the Envelope 📉 

https://acm9q.shinyapps.io/Back_of_the_Envelope/

Can we build a ~basic point-and-click regression analysis tool~ replacement to general purpose statistical analysis tools with R shiny? 


# The goal:

Build a general purpose regression tool, incorporating extensions to regression such as heteroskedasticity-robust standard errors, clustered standard errors, multilevel modeling, and logistic regression. 

With sufficient on-your-own data preparation, this tool should be sufficient for basic regression analyses for beginner-to-intermediate level social-science use. I have no plans to implement latent-variable / structural equation modeling at this time, but path analysis could be in the distant future. 

# To Do Items
## 1.0 To Do List: 
* ~Output should include `SjPlot`'s `tab_model()` output for APA-style regression tables~
  * ~there be dragons with clustered standard errors~  Dragons Slain. 
* ~`ggplot2` representations of the model~
  * ~bivariate~
  * ~bivariate residual plot~
  * ~two independent variables (close! close? finished!)~
  * ~added variable plots~ Take that, math! 
  * ~Plot residuals. (it's just [predicted v actual] + ~[residual v fitted]~)~
* ~Additionally, need to figure out how to modify the DT with `%>%` to round `psych::describe()` to two decimal places.~
* ~include correlation table from `SjPlot`~
* ~include data table~
* ~include `psych::describe()`~
* ~Robust Specification~ 
* ~Logistic Regression up and running~
* ~Multivariate Regression Up and running~
* clusters:
  * ~fixed effects (just make it add `factor()` of whatever variable to the data, then update the model to include this)~
    * option to eliminate coefficients with `felm()`
  * ~standard errors (this will make error for SjPlot see his tweet reply on this topic)~
* ~Additional common filetypes supported. (stata, spss, csv, excel)~
* ~fix odd error with spaces in variable names~
* ~margins plots~
* Outlier Analysis:
  * follow this for outlier removal: https://www.shinyapps.org/apps/p-hacker/
    * why doesn't this work? 
  * ~Cook's Distance~
  * Leverage Calculator?
  * Influence index plot (from `car`, can it be remade in `ggplot2` though?)
* ~Adjust `SjPlot`'s marginsplot to include a dotted line at 0 for reference.~ 
  * ~or 1 for logistic obv.~ 
  * ~`plot_model(m1, vline.color = "red")` (it's already built in to SjP)~
  * ~Also: `plot_model(m1, show.values = TRUE, value.offset = .3)`~
* ~rearrange upload / model page (incorporates text of current model)~
  * drop-down message of current model (nixed due to error logging)
* ~sassy message when they ask for logistic regression residuals.~ 
* ~Model Diagnostics~
  * ~QQ plot~
  * ~resid v fitted~
  * ~histogram of residuals~
* fix issue with missing points whenever there are residuals? what's that about?? 
  * something like, for each variable selected, select_if(is_extant)? this should already be working in the back end. Annoying. 
  * https://tidyr.tidyverse.org/reference/drop_na.html
  * This is dropping too much data--needs to only drop the chosen variables
* ~rotate `SJP.corr()` table variables~ :/ 
* ~nix the variables page and add a dossier page instead :)~ 
  * ~`dossier()` numeric return error.~ 
* ~overhaul the plot outputs so it's just:~
  * ~original plot (follows programmatically based on variables) with second tab for residuals~
    * ~set "1 IV" and "2 IV" plots to be a logical when `length(indevars) == 1{} else if length(indevars == 2{} else NULL`~
  * ~marginal effects plot~
  * ~Added Variable Plots.~ Make sure to deal with issue of missing data with AV plots (and residuals above for that matter)
* `estimatr` redo of all models, including a fixed-effects absorption. 
  * triple-check that cluster standard errors and robust standard errors are properly specified. 
* ~`Fast = TRUE` toggle for `psych::describe()` so nerds can see skew/kurtosis.~
* ~missing data error messages~ lol get rekt. 
* Available Models Matrix
  * https://gt.rstudio.com/ use this one to make it due to the double-headers :) 

* solidify color theme
* purchase logo design
* Publish

## 1.1 To Do List
* Linear Mixed Effects
* Two-Stage Estimation
  * Instrumental Variables
  * https://declaredesign.org/r/estimatr/articles/getting-started.html#iv_robust
* need to add support for google sheets via {googlesheets4} this should be fairly trivial now: including a textinput for the link and a 'go' button, probably, then a check that blocks having two data sources at once. 
  * oh my god what an absolute nightmare
  * https://stackoverflow.com/questions/44980757/remote-server-authentication-to-read-googlesheets-from-r-script-not-using-servic/59910070#59910070
* fix left-right scroll on dataTableOutput. 
* bookmark current state
  * https://shiny.rstudio.com/articles/bookmarking-state.html
* Allow `describe_by()` groupings with a material switch and a dropdown menu. 
  *   ~verbatimTextOutput("info")~ NO
  * use gt() output instead. 
    * https://stackoverflow.com/questions/63177033/tidyeval-in-a-non-quasiquotation-context-psychdescribebygroup
  * consider switch to gt_summary() for regression tables:
   * https://www.danieldsjoberg.com/gtsummary/reference/theme_gtsummary.html
   * especially if `theme_APA` ever becomes a thing. 
* get download report code to work: 

```
#ui

radioButtons('format', h5('Document format'), c('PDF', 'HTML', 'Word'), inline = TRUE),
downloadButton('downloadReport'),

#server
   output$downloadReport <- downloadHandler(
      filename = function() {
         paste('my-report', sep = '.', switch(
            input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
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
```


## 1.2 To Do List
* editable data tables: 
  * idea one: https://github.com/jbryer/DTedit
  * idea two: https://www.r-bloggers.com/shinymatrix-matrix-input-for-shiny-apps/
  * idea three: https://cran.r-project.org/web/packages/rhandsontable/vignettes/intro_rhandsontable.html
  * idea four: Common generic variable adjustments as options:
    * gen dummies
    * center
    * standardize
    * drop
    * square
    * interact 
    * ... ? 
* Quantiles? `geom_quantile()` 
  * https://cran.r-project.org/web/packages/quantreg/vignettes/rq.pdf
* Pure description + plot all variables against each other a la https://drsimonj.svbtle.com/plot-some-variables-against-many-others
  * surprisingly difficult. return to this later. 
* `ggvis` overhaul, at least for main two or three plots? 
  * surprisingly difficult. return to this later. 
  * problem with `prop("x", as.name(indvariable()))` ? What's up with that. 
* Mutate variables for common issues: (1) generate dummy variables of factors, (2) center, (3) standardize.
* `dplyr::filter()` regression on data subgroups. 
* JSON support.
* multiple simultaneous models

## The Deep Future To Do List
* Binary Outcome Mixed Effects
* path analysis
  * https://advstats.psychstat.org/book/path/index.php
  * https://data.library.virginia.edu/introduction-to-mediation-analysis/
* Multiple Imputation with Chained Equations
  
  
## User Feedback:
* ~For the Correlation table, you may want to rotate your x-axis labels 45 or 90 degrees.  Getting a lot of overlap for files with > 20 factors~
* Is there a way that you can override or modify the error messages?  Instead of "contact the app author", maybe provide a URL to a message board or email?
  * MAKE ERRORS GREAT AGAIN
* ~my big suggestion is just to clarify and restructure the flow of the user interface. I think my user preference is that I'd want to upload a dataset, look around in it, and then decide on a model~
  * ~so maybe separate the Upload and Model pieces entirely. Then restructure the left-hand nav to be something like Upload > View Data Set > Descriptive Statistics > Correlation Table > Model > Summary > Plots > Diagnostics~
  * ~some suggestions in there to make the nav header more descriptive, and put summary before plots just so we get the immediate output of the model.~ with this, might even be worthwhile to put the model + summary on the same page actually
* my last thought would be that it might be cool to allow for dplyr-style filtering of the uploaded dataset - I think it would be relatively straightforward, but also legit if you don't want to include that functionality as it could also be an enormous pain in the ass to try and catch edge-cases
  * (in re dplyr style filtering: editable data tables are now possible, but that's a feature I have planned to work on after I squash all the inital bugs. it's gonna go: squash bugs, ~added variable plots~, finish outliers, HLM, instrumental variables, THEN data processing)
* ~ooh, before I forget: I might also have a disclaimer or something re: what you do with the uploaded datasets. could spook some people when you actually have people using it for not just testing purposes~






With a sufficient amount of effort, this could actually be a pretty good tool for multilevel linear modeling, ~provided I can figure out what exactly the error was with `mice`'s MCMC imputation.~ imputation is gonna have to wait for v1.2 at the absolute earliest. 


# Domain issues
## Better names:
www.backoftheenvelope.com  (is currently taken)




# material:
## Reading Material
* https://stackoverflow.com/questions/38878113/implementing-reactive-values-in-regression-using-shiny?rq=1
* https://stackoverflow.com/questions/56068825/use-selectbox-to-create-regression-formula-from-user-input-dataset/56069365#56069365
* https://stackoverflow.com/questions/43102554/build-linear-regression-with-dynamic-inputs-in-r-shiny?rq=1
* Reformulate: http://novicemetrics.blogspot.com/2011/04/forming-formulas.html?utm_source=feedburner&utm_medium=feed&utm_campaign=Feed%3A+CoffeeAndEconometricsInTheMorning+%28Coffee+and+Econometrics+in+the+Morning%29

## Watching Material
https://rstudio.com/resources/webinars/testing-shiny-applications-with-shinytest-shiny-developers-now-have-tools-for-automated-testing-of-complete-applications/
https://rstudio.com/resources/webinars/introducing-shiny-gadgets-interactive-tools/
https://rstudio.com/resources/webinars/interactive-graphics-with-shiny/
https://rstudio.com/resources/webinars/help-me-help-you-creating-reproducible-examples/
https://rstudio.com/resources/webinars/scaling-shiny-apps-with-asynchronous-programming/

## it's been done:
but not with user-input data:
https://rich.shinyapps.io/regression/
(use this for model work)
