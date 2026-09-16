# 🖂  Back of the Envelope 📉 

https://acm9q.shinyapps.io/Back_of_the_Envelope/

Can we build a ~basic point-and-click regression analysis tool~ replacement to general purpose statistical analysis tools with R shiny? 

# Back of the Envelope — To-Do List (revised September 2026)

## Context
estimatr 2.0.0 was released this week with native `broom::augment()` 
support for `lm_robust`. This resolves the central blocking issue that 
stalled the project in 2021. Code has not yet been updated to reflect this.

---

## 🔴 Critical Bugs (do these first)

- [x] ~~Fix `& → |` bug in `linear()` model building block~~
      — one character change, massive downstream consequences
      — this is the original sin; everything else depends on it
- [ ] Update `estimatr` to 2.0.0 (`remotes::install_github("DeclareDesign/estimatr")` 
      until CRAN accepts it)
- [ ] Replace `augment(fit)` calls with new 2.0 syntax (no `newdata=` required anymore)

---

## 🟠 Core Functionality (v1.0 blockers)

### Model Diagnostics
- [ ] Rewrite diagnostic plots using `augment(model())` now that 
      `lm_robust` is supported — replaces all the `lindia` workarounds
  - [ ] Histogram of residuals
  - [ ] QQ plot  
  - [ ] Residuals vs. fitted
  - [ ] Cook's distance (now extractable cleanly via augment)
- [ ] Fix missing data issue in residual plots — `drop_na()` is 
      dropping too aggressively; should only drop on modeled variables

### Added Variable Plots
- [ ] Rewrite `gg_added_var()` using `augment()` — now unblocked
- [ ] Handle missing data correctly in AV plot construction

### Outlier Analysis
- [ ] Fix `reactiveValues()` bug in brushing/leverage tab
      (`vals` is declared as `reactive()` but mutated as if `reactiveValues()`)
- [ ] Complete Cook's Distance plot (now unblocked via augment)
- [ ] Leverage plot — interactive point exclusion 
      (reference: https://gallery.shinyapps.io/106-plot-interaction-exclude/)
- [ ] Decide: ship Cook's D only for v1.0, defer leverage interactivity?

### Standard Errors
- [ ] Verify robust SE and cluster SE are correctly specified after `& → |` fix
- [ ] Test all model type combinations after estimatr 2.0 update
- [ ] Update `geom_smooth()` calls to use `method = "lm_robust"` 
      when `input$rbst == TRUE`

---

## 🟡 UI / Architecture (v1.0 quality-of-life)

### Framework Migration
- [ ] Migrate from `shinydashboard` + `shinydashboardPlus` to `bs4Dash`
  - [ ] Swap library calls and fix renamed functions
  - [ ] Replace `socialButton()` with `tags$a()` + fontawesome equivalents
  - [ ] Replace `dashboardLabel()` with styled `span` equivalents
  - [ ] `dropdownBlock()` is already custom — verify it survives migration

### Code Modernization  
- [ ] Replace deprecated `aes_string()` with `.data[[]]` or `aes()`
- [ ] Replace deprecated `select_()` with `pick()` or `.data[[]]`
- [ ] Replace `add_rownames()` with `tibble::rownames_to_column()`

### Modular Overhaul
- [ ] Refactor server into Shiny modules — one per tab
      (reference: https://rviews.rstudio.com/2021/10/20/a-beginner-s-guide-to-shiny-modules/)
- [ ] This is an architecture improvement but not a v1.0 blocker — 
      consider doing during bs4Dash rewrite since you're touching everything anyway

### Error Handling
- [ ] Replace generic Shiny error messages with informative, 
      friendly messages per error type
- [ ] Add specific message for logistic regression residuals tab

---

## 🟢 Available Models Matrix (tracking tool)

- [ ] Build a gt() table showing which model × feature combinations 
      are supported, in-progress, or broken
- [ ] Use double-headers via gt for model type × clustering type axes
- [ ] Keep this current as fixes land — it's the scoreboard

---

## ✅ Confirmed Working (do not touch)

- Model formula generation including fixed effects via `factor()`
- `tab_model()` HTML output via sjPlot
- Bivariate and multivariate main effect plots
- Marginal effects plot
- Correlation matrix
- Descriptive statistics via gt
- Dossier tab
- Upload and data table display
- Logistic regression model fitting
- Basic robust SE specification (modulo the `& → |` bug)

---

## 🔵 v1.1 (post-ship, not blocking)
- [ ] Model results stored somewhere so that a second model can be created and compared (incremental F, e.g.) This is important for several future features.
- [ ] `{report}` package integration for model summary narrative
      — now potentially unblocked by estimatr 2.0, needs testing
- [ ] `broom::glance() %>% gt()` summary statistics panel
- [ ] `{equatiomatic}` equation display
- [ ] Linear Mixed Effects / HLM
- [ ] Instrumental Variables via `iv_robust()` 
      — estimatr 2.0 has augment support for this too
- [ ] Incremental F test 
- [ ] ANOVA table output tab
- [ ] Bookmark state
- [ ] Downloadable individual plots via `ggsave()`
- [ ] Fix horizontal scroll on `dataTableOutput`

---

## 🔵 v1.2 (future)

- [ ] Editable data tables / in-app variable transformations 
      via `{datawizard}`
- [ ] GLM linking function selection (Poisson etc.)
- [ ] Multiple simultaneous model comparison
- [ ] Data subsetting / dplyr-style filtering
- [ ] Google Sheets support (non-trivial — see prior notes)
- [ ] JSON support

---

## ⚫ Deep Future / Maybe Never

- [ ] Binary outcome mixed effects
- [ ] Multiple imputation (MICE)
- [ ] Path analysis / mediation
- [ ] Downloadable report (PDF/HTML/Word) — deprioritized; 
      high pain, low user value
- [ ] Plot ID variables instead of anonymous points

---

## Recommended Sequencing 

1. `& → |` fix + estimatr 2.0 update
2. Rewrite diagnostics and AV plots using new `augment()`
3. Fix `reactiveValues()` for brushing
4. bs4Dash migration (touches UI only, server intact)
5. Deprecated tidy eval cleanup
6. Modular refactor (can overlap with step 4)
7. Ship v1.0


# Back of the Envelope — Available Models Matrix (post & → | fix)

Key: ✅ works | ⚠️ partial / untested | ❌ broken | 🚧 not yet implemented

---

## Linear Outcome (`lm` / `lm_robust`)

| Configuration | Robust SEs | Model Function | SE Type | augment() | Diagnostics | AV Plots | Notes |
|---|---|---|---|---|---|---|---|
| No clustering, no robust | OFF | `lm()` | OLS | ✅ | ✅ | ✅ | Baseline case; fully working |
| No clustering, robust | ON | `lm_robust(clusters = NULL)` | HC2 | ⚠️ | ❌ | ❌ | Now correctly reaches `lm_robust()`. `clusters = NULL` should give HC2 but needs verification with estimatr 2.0. Diagnostics/AV plots still break on `lm_robust` class until lindia is replaced |
| Cluster SEs, no robust | OFF | `lm_robust(clusters = cluster_var())` | CR2 | ⚠️ | ❌ | ❌ | Now correctly reaches `lm_robust()`. Previously silently used plain OLS — this is a meaningful fix. SE type needs verification |
| Cluster SEs + robust | ON | `lm_robust(clusters = cluster_var())` | CR2 | ⚠️ | ❌ | ❌ | Was the only case that worked before; behavior unchanged. Both conditions true, `\|` still routes here correctly |
| Fixed Effects | OFF | `lm()` | OLS | ✅ | ✅ | ✅ | FE via `factor()` in formula, plain `lm()`. Works but no SE adjustment for FE |
| Fixed Effects + robust | ON | `lm_robust(clusters = NULL)` | HC2 | ⚠️ | ❌ | ❌ | Now reaches `lm_robust()` correctly. estimatr 2.0 default for FE without clusters is HC2 — needs verification |
| Fixed Effects + cluster SEs | either | `lm_robust(clusters = cluster_var())` | CR2 | ⚠️ | ❌ | ❌ | Routes correctly now. Note: FE is via `factor()` in formula, not absorbed — coefficient list will be large |
| HLM / Multilevel | — | 🚧 | — | 🚧 | 🚧 | 🚧 | UI option exists, routes nowhere |

---

## Logistic Outcome (`glm` / `glmRob`)

| Configuration | Robust SEs | Model Function | SE Type | augment() | Diagnostics | AV Plots | Notes |
|---|---|---|---|---|---|---|---|
| No robust | OFF | `glm(..., family = "binomial")` | MLE | ✅ | ⚠️ | ⚠️ | Unchanged. Diagnostics run but residuals are not meaningful for logistic |
| Robust logistic | ON | `robust::glmRob(..., method = "cubif")` | Mallows/cubif | ❌ | ❌ | ❌ | Unchanged. No augment support for `glmRob` |
| Clustered logistic | any | 🚧 | — | — | — | — | Unchanged. `logistic()` reactive has no clustering path at all — `cluster_var()` is never passed to it |
| Fixed effects logistic | any | 🚧 | — | — | — | — | `regFormula()` does append `factor(cluster_var)` for FE, so formula is correct — but clustering type and robust toggle are still ignored inside `logistic()` |

---

## Standard Error Types

| SE Label | Estimator | When Active | HC Variant | Verified? |
|---|---|---|---|---|
| OLS | `lm()` | No robust, no clustering | N/A | ✅ |
| HC2 | `lm_robust(clusters = NULL)` | Robust ON, no clustering | HC2 (estimatr default) | ⚠️ needs testing post-fix |
| CR2 | `lm_robust(clusters = cluster_var())` | Cluster SEs selected (with or without robust toggle) | CR2 (estimatr default) | ⚠️ needs testing post-fix |
| HC2 (FE) | `lm_robust(clusters = NULL)` | FE + robust, no clustering | HC2 (estimatr 2.0 FE default) | ⚠️ needs testing |
| CR0 | — | FE + clustered | CR0 (estimatr 2.0 FE+cluster default, changed from CR2 in 1.x) | ⚠️ **breaking change from estimatr 1.x — see note** |
| Mallows/cubif | `robust::glmRob()` | Logistic + robust | cubif | ✅ |

**estimatr 2.0 breaking change note:** For fixed effects with clustering, estimatr 2.0 changed the default from CR2 to CR0, and issues a once-per-session warning. If you want CR2 back, pass `se_type = "CR2"` explicitly. Worth deciding which default you want and making it explicit in the code.

---

## End-to-End Summary

| | Plain OLS | Robust OLS (HC2) | Clustered OLS (CR2) | Plain Logistic | Robust Logistic |
|---|---|---|---|---|---|
| Formula display | ✅ | ✅ | ✅ | ✅ | ✅ |
| Model fits | ✅ | ✅ | ✅ | ✅ | ✅ |
| tab_model() summary | ✅ | ⚠️ | ⚠️ | ✅ | ⚠️ |
| Main effect plot | ✅ | ✅ | ✅ | ✅ | ✅ |
| Marginal effects | ✅ | ⚠️ | ⚠️ | ✅ | ⚠️ |
| Residual plot | ✅ | ❌ | ❌ | ⚠️ | ❌ |
| Diagnostics | ✅ | ❌ | ❌ | ⚠️ | ❌ |
| AV plots | ✅ | ❌ | ❌ | ⚠️ | ❌ |
| Cook's distance | ✅ | ❌ | ❌ | ❌ | ❌ |
| Outlier brushing | ❌ | ❌ | ❌ | ❌ | ❌ |

---

## What Changed vs. Pre-Fix

The fix converts three silent failures into correct behavior:
- Robust ON + no clustering: was silently OLS, now correctly HC2
- Cluster SEs + robust OFF: was silently OLS, now correctly CR2  
- Cluster SEs + robust ON: behavior unchanged, was already the one working case

The ❌ items in diagnostics/AV plots are now **honest failures** 
(lm_robust class hitting lindia) rather than silent misfires 
(plain lm being used when user asked for robust). That's progress.








# The goal:

Build a general purpose regression tool, incorporating extensions to regression such as heteroskedasticity-robust standard errors, clustered standard errors, multilevel modeling, and logistic regression. 

With sufficient on-your-own data preparation, this tool should be sufficient for basic regression analyses for beginner-to-intermediate level social-science use. I have no plans to implement latent-variable / structural equation modeling at this time, but path analysis could be in the distant future. 

# To Do Items
## 1.0 To Do List: 
- [x] ~Output should include `SjPlot`'s `tab_model()` output for APA-style regression tables~
  - [x] ~there be dragons with clustered standard errors~  
- [x] ~`ggplot2` representations of the model~
  - [x] ~bivariate~
  - [x] ~bivariate residual plot~
  - [x] ~two independent variables (close! close? finished!)~
  - [x] added variable plots Take that, math! 
  - [x] Plot residuals. (it's just [predicted v actual] + ~[residual v fitted]~)
    - [ ] when using {estimatr}, can do this:
- [x] ~Robust Specification~ 
- [x] ~Logistic Regression up and running~
- [x] ~Multivariate Regression Up and running~
- [ ] clusters:
  - [x] ~fixed effects (just make it add `factor()` of whatever variable to the data, then update the model to include this)~
    - [x] ~option to eliminate coefficients with `felm()`~
    - [x] ~idk that seems like a real pain given the four part formula: https://www.rdocumentation.org/packages/lfe/versions/2.8-5.1/topics/felm~
    - [x] ~doesn't need to be four parts: `summary(felm(y ~ x + x2 + Q + W | id + firm))`~
  - [x] ~standard errors (this will make error for SjPlot see his tweet reply on this topic)~
- [x] ~margins plots~
- [ ] Outlier Analysis:
  - [ ] follow this for outlier removal: https://www.shinyapps.org/apps/p-hacker/
  - [ ] that was the wrong outlier removal link. follow this code instead: https://gallery.shinyapps.io/106-plot-interaction-exclude/
    - [ ] why doesn't this work? 
  - [ ] Cook's Distance
  - [ ] Leverage Calculator?
  - [ ] Influence index plot (from `car`, can it be remade in `ggplot2` though?)
- [x] ~rearrange upload / model page (incorporates text of current model)~
  - [ ]  but can we prettify this? 
- [x] ~sassy message when they ask for logistic regression residuals.~ 
- [ ] Model Diagnostics 
  * `lindia` is being a jerk. maybe try `ggfortify` or `gglm`
    * {ggfortify} can't handle these either. 
    * {gglm} only handles classes 'lm' and 'glm' wow even fewer. 
  * QQ plot
  * resid v fitted
  * histogram of residuals
* fix issue with missing points whenever there are residuals? what's that about?? 
  * something like, for each variable selected, select_if(is_extant)? this should already be working in the back end. Annoying. 
  * https://tidyr.tidyverse.org/reference/drop_na.html
  * This is dropping too much data--needs to only drop the chosen variables
- [x] ~overhaul the plot outputs so it's just:~
  - [x] ~original plot (follows programmatically based on variables) with second tab for residuals~
    * ~set "1 IV" and "2 IV" plots to be a logical when `length(indevars) == 1{} else if length(indevars == 2{} else NULL`~
  - [x] ~marginal effects plot~
  - [ ] Added Variable Plots. Make sure to deal with issue of missing data with AV plots (and residuals above for that matter)
- [ ] ~`estimatr` redo of all models, including a fixed-effects absorption.~ 
  - [x] ~triple-check that cluster standard errors and robust standard errors are properly specified.~
  - [] nope you idiot. you did & instead of |
  - [ ] HUGE DOWNSTREAM EFFECTS ON MODEL DIAGNOSTICS AND AVPLOTS
- [ ] Available Models Matrix **THIS IS NOW PRIORITY #1 AS IT WILL TRACK PROGRESS ON EVERYTHING ELSE** 
  - [ ] https://gt.rstudio.com/ use this one to make it due to the double-headers :) 
- [ ] fix downstream issues from `lm_robust()` 
  - [ ] av plots
  - [ ] residuals
  - [ ] model diagnostics 
  - [ ] don't forget to adjust `geom_smooth(method = "lm_robust")` in the function call if `input$rbst == TRUE`
- [ ] model summary extra tab for results as ANOVA (no package has good output of ANOVA table to HTML for a REASON)
- [ ] MODULAR OVERHAUL (https://rviews.rstudio.com/2021/10/20/a-beginner-s-guide-to-shiny-modules/)
  - [ ] This should make every individual tab its own module for simplicity's sake on the main page, which is like 800 lines now jeez.
- [ ] solidify color theme
- [ ] purchase logo design
- [ ] Publish

## Current Problem:

### Honestly
The key may to take all the model output objects that are generated and to standardize their output into my own bespoke formatting, and then render that into everything else... But it sure does sound like a pain. 

### Status of what doesn't work: 
- [ ] Added Variable Plots (needs `broom::augment()` for lm_robust) https://github.com/DeclareDesign/estimatr/issues/377
- [ ] see this response; https://github.com/DeclareDesign/estimatr/issues/377#issuecomment-923105361
- [ ] Plot Residuals (needs `broom::augment()` for lm_robust) https://github.com/DeclareDesign/estimatr/issues/377
- [ ] model diagnostics: (needs `broom::augment()` for lm_robust)
  - [ ] QQ plot 
  - [ ] resid v fitted https://github.com/DeclareDesign/estimatr/issues/377
  - [ ] histogram of residuals https://github.com/DeclareDesign/estimatr/issues/377
  - [ ] cook's distance (https://stackoverflow.com/questions/62107571/how-to-manually-calculate-cooks-distance) 
- [ ] {report}

### The Plan:
- [ ] Just switch to using {robustbase} and drop {estimatr} altogether, since it doesn't play nice with anyone? 
  - [ ] would be a shame to lose their ability to do two-stage least squares, but let's see where this takes us without using {estimatr} :( 
  - [ ] https://cran.r-project.org/web/packages/robustbase/robustbase.pdf
- [ ] generate a standardized `augmented` table for every model based on `if_then()` for regular `lm` or `lm_robust` 
   - [ ] how do you even do this with robust standard errors, though? 
   - [ ] `broom` has methods for `augment` for types `lmrob` and `lmRob` but not `lm_robust` and they are passing this problem on to the maintainers of the `estimatr` package
- [ ] call the standardized augmented table for every function that requires it; personalize all the {lindia} functions for this workflow
- [ ] re-write my own AV plots and Residuals for this
- [ ] dig into whether can coerce `lm_robust` class into a class {report} can read. 
- [ ] https://easystats.github.io/insight/reference/index.html Use this to extract predicted & residuals and get as close as possible? 
- [ ] Diagnostics from here might be helpful? https://github.com/easystats/performance
- [ ] anything that requires prediction intervals with type "lm_robust" is going to be bogus anyway: 
  - [ ]  https://stats.stackexchange.com/questions/175127/prediction-intervals-with-heteroscedasticity
  - [ ]  https://stats.stackexchange.com/questions/186953/calculating-prediction-intervals-from-heteroscedastic-data?noredirect=1&lq=1

## 1.1 To Do List
- [ ] use {report} package to report models on summary page
  - [ ] outputs of {estimatr} class `lm_robust` aren't supported by report. 
  - [ ] include `broom::glance() %>% gt()` with this please. 
- [ ] https://datalorax.github.io/equatiomatic/articles/intro-equatiomatic.html for same
- [ ] An easy feature: https://twitter.com/Dom_Makowski/status/1321825702040100864 
  - [ ] https://github.com/easystats/report
- [ ] Linear Mixed Effects
- [ ] Incremental F test so Vivian will finally love me. .
- [ ] use {equatiomatic} to produce an equation on the page (this will probably bounce with LaTeX somewhere)
- [ ] Two-Stage Estimation
  - [ ] Instrumental Variables
  - [ ] https://john-d-fox.github.io/ivreg/articles/ivreg.html
  - [ ] https://declaredesign.org/r/estimatr/articles/getting-started.html#iv_robust
  - [ ] this should...actually be easy now given `varselectinput()` ??
- [ ] need to add support for google sheets via {googlesheets4} this should be fairly trivial now: including a textinput for the link and a 'go' button, probably, then a check that blocks having two data sources at once. 
  - [ ] oh my god what an absolute nightmare. not trivial at all. 
  - [ ] https://stackoverflow.com/questions/44980757/remote-server-authentication-to-read-googlesheets-from-r-script-not-using-servic/59910070#59910070
- [ ] fix left-right scroll on dataTableOutput. 
  - [ ] https://stackoverflow.com/questions/30765338/how-to-make-the-horizontal-scrollbar-visible-in-dtdatatable/30765558
- [ ] bookmark current state
  - [ ] https://shiny.rstudio.com/articles/bookmarking-state.html
- [ ] consider switch to gt_summary() for regression tables:
  - [ ] https://www.danieldsjoberg.com/gtsummary/reference/theme_gtsummary.html
  - [ ] especially if `theme_APA` ever becomes a thing. 
  - [ ] No. Use package {apaTables} instead! WAIT NO, this only prints to msWord, it doesn't print to HTML.
- [ ] path analysis 
  - [ ] https://advstats.psychstat.org/book/path/index.php
  - [ ] https://data.library.virginia.edu/introduction-to-mediation-analysis/
- [ ] get download report code to work: 

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
### Downloadable plots: 
Example: 
```
plot_server <- function(id, df, vbl, threshhold = NULL) {
  
  moduleServer(id, function(input, output, session) {
    
    plot <- reactive({viz_monthly(df(), vbl, threshhold)})
    output$plot <- renderPlot({plot()})
    output$dnld <- downloadHandler(
      filename = function() {paste0(vbl, '.png')},
      content = function(file) {ggsave(file, plot())}
    )
    
  })
}
```

## 1.2 To Do List
- [ ] User choice standard errors with `estimatr`
- [ ] editable data tables: 
  - [ ] idea one: https://github.com/jbryer/DTedit
  - [ ] idea two: https://www.r-bloggers.com/shinymatrix-matrix-input-for-shiny-apps/
  - [ ] idea three: https://cran.r-project.org/web/packages/rhandsontable/vignettes/intro_rhandsontable.html
  - [ ] idea four: Common generic variable adjustments as options: (use new {datawizard} to do this?)
    - [ ] gen dummies
    - [ ] center
      - [ ] median
      - [ ] mean
    - [ ] standardize
      - [ ] z-transform? 
      - [ ] other's from library(SuppDists) 
        - [ ] http://finzi.psych.upenn.edu/library/SuppDists/html/00Index.html
    - [ ] drop
    - [ ] square (or root? what about a generic power function?) 
    - [ ] interact 
    - [ ] Brian's subsetting maybe? 
    - [ ] ... ? 
- [ ] Quantiles? `geom_quantile()` 
  - [ ] https://cran.r-project.org/web/packages/quantreg/vignettes/rq.pdf
- [ ] instead of "linear v logistic" do "lm" versus "glm" and allow selection of linking function (e.g. to allow poisson, etc)
- [ ] `ggvis` overhaul, at least for main two or three plots? 
  - [ ] surprisingly difficult. return to this later. 
  - [ ] problem with `prop("x", as.name(indvariable()))` ? What's up with that. 
- [ ] `dplyr::filter()` regression on data subgroups. Brian wanted this but tbh is it worth it? 
- [ ] JSON support.
- [ ] multiple simultaneous models

## The Deep Future To Do List
- [ ] Binary Outcome Mixed Effects
- [ ] Multiple Imputation with Chained Equations
- [ ] specify which variable is the ID variable; then allow users to plot Id variables instead of points. 
  
  
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
www.envelope.fyi
www.envelo.pe


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
(~use this for model work~) bruh use this for *downloadable reports work*

