# sketchy_regression
can we build a basic point-and-click regression analysis tool with R shiny? 


## The goal:

Build a general purpose regression tool, incorporating extensions to regression such as heteroskedasticity-robust standard errors, clustered standard errors, multilevel modeling, and logistic regression. 

## Subgoal: 

Use combinations of `wired.js` and the R `xkcd` package to make the entire thing look goofy and hand drawn so that no one takes it seriously. 



With sufficient on-your-own data preparation, this tool should be sufficient for basic regression analyses for beginner-to-intermediate level social-science use. I have no plans to implement latent-variable / structural equation modeling at this time. 

## To Do: 
Output should include `SjPlot`'s tab_model output for APA-style regression tables (if you can change the font back to Times New Roman) and `ggplot2` representations of the model, included added-variable plots for multiple regression models. Additionally, need to figure out how to modify the DT with `%>%` to round `psych::describe()` to two decimal places. 

With a sufficient amount of effort, this could actually be a pretty good tool for multilevel linear modeling, provided I can figure out what exactly the error was with `mice`'s MCMC imputation. 

## Better names:
www.backoftheenvelope.com  ?
