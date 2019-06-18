# Automatized statistics in r
A set of helper functions to automatize a statistical analysis in R, from cleaning to t-tests, linear models and Anovas.

`helperfunctions.R` includes all functions, namely:
- `modelAuto`, a function that runs a number of t-tests, linear models and anovas for all combinations of `response` and `explanatory`. The function expects `response` and `explanatory` to be of the following form:
  ```
  response <- c("literal name 1", "literal name 2")
  names(response) <- c("name in df 1", "name in df 2)
  ```
  It then returns a
  - a barplot for the t-test
  - a line plot for the linear model
  - a boxplot for the anova
  
- `lmp` a function to extract p values from a linear model object as proposed by [Stephen Turner](https://gist.github.com/stephenturner/722049#file-pvalue-from-lm-object-r)
- `binFun` a function that recodes likert scale items to 0/1 (1 if x >=3, 0 if x < 3)
