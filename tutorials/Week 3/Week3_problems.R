# Remove objects
rm(list=ls())

# Detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# Load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# Load any necessary packages
lapply(c("readr", "ggplot2", "dplyr", "viridis", "foreign", "haven"),  pkgTest)

# Set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Agenda
# (1.) Load & inspect data
# (2.) Descriptive analysis
# (3.) Confidence intervals
# (4.) Significance test for a mean
# (5.) Significance test for a difference in means
# (6.) Extra activity: real-world data (Polity scores)


### Research Question -----------
# Is there a relationship between education and income?

# -------------------------------#
# 1. Load & Inspect Data
# -------------------------------#

df <- read_csv("../../datasets/fictional_data.csv")

# Quick overview
head(df)     
str(df)      
summary(df)  

# Variables:
# - income: Monthly net income (numeric)
# - edu: University-level education in years (numeric)
# - cap: Binary variable (1 = lives in capital, 0 = otherwise)

# How many observations do we have?
# HINT: nrow(), or length() of one column.
# This matters later: a small sample is one reason to use the t distribution.


# -------------------------------#
# 2. Descriptive Statistics
# -------------------------------#

# Recap of the four quantities we need:

# EXERCISE: Find the mean,standard deviation and standard error
# of income. Store each one in an object so we can reuse them later.
mean_income <- 
sd_income   <- 
se_income   <- 

mean_income; sd_income; se_income


# -------------------------------#
# 3. Visualizing the Distribution
# -------------------------------#

# EXERCISE: Create a histogram of income, with a title and x-axis label.


# EXERCISE: Create a density plot of income, with a title and x-axis label.
# HINT: density() computes the curve, plot() draws it.



# -----------------------------------------#
# 4. Sampling Distribution & Standard Error
# -----------------------------------------#
# Which kind of inferences can we make with regards to the population,
# based on the sample data, specifically the sample mean and SE?


# Why do we need the standard error?


# -------------------------------#
# 5. Confidence Intervals
# -------------------------------#
# Definition: Point estimate +/- Margin of error,
# where margin of error is a multiple of the standard error:
#
#   CI = mean +/- critical value * SE
#
# The "multiple" (critical value) depends on the confidence level and the
# distribution we use. First we need to learn how to find it in R.

# ---- Finding critical values with qnorm() (normal distribution) ----
# qnorm(p) answers: "which value of the normal distribution has a
# proportion p of the distribution BELOW it?"
# By default it uses the STANDARD normal (mean = 0, sd = 1).
# Arguments: qnorm(p, mean = 0, sd = 1, lower.tail = TRUE)
?qnorm

# For a 95% CI we leave 2.5% in each tail, so we need the values at
# p = 0.025 and p = 0.975:
qnorm(0.025) # value with the first 2.5% of the distribution below it
qnorm(0.975) # value with 97.5% below it (i.e. the last 2.5% above it)

# We can also shift and scale the distribution with mean and sd.
# Here the distribution has mean 2 and sd 0.4:
qnorm(0.025, mean=2, sd=0.4)
# Now the same quantile is expressed in the units of that distribution.
qnorm(0.975, mean=2, sd=0.4)

# If we plug in mean = our sample mean and sd = our SE, the result is directly
# the lower end of the CI (no need to multiply by hand).


# EXERCISE: Calculate the 95% CI for mean income using qnorm().
lower_95_n <- 
upper_95_n <- 

lower_95_n; mean_income; upper_95_n

# Check yourself: does the same result come from
# mean_income -/+ 1.96 * se_income ?


# ---- Finding critical values with qt() (t distribution) ----
# When the population SD is unknown and estimated from the sample (which is
# almost always the case!), the t distribution is the appropriate one.
# It has heavier tails than the normal, especially for small samples; with a
# large n it becomes almost identical to the normal.
# It needs one extra argument: degrees of freedom, df = n - 1.
# qt(p, df, lower.tail = TRUE)
?qt

# For a 95% CI, we leave 2.5% in each tail (for df = 18)
qt(0.025, df=length(df$income)-1)
qt(0.975, df=length(df$income)-1)

# For a 99% CI, we leave 0.5% in each tail (for df = 18)
qt(0.005, df=length(df$income)-1)
qt(0.995, df=length(df$income)-1) 

# The lower.tail argument lets us ask for the upper tail directly:
qt(0.005, df=length(df$income)-1, lower.tail=FALSE) # same as the previous line

# Compare with the normal distribution: qnorm(0.995). Which is larger?
# What does this mean for the width of the CI?

t_score <- qt(0.995, df = length(df$income) - 1)

# EXERCISE: Re-calculate the 99% CI around mean_income using t_score.
lower_99_t <- 
upper_99_t <- 

lower_99_t; mean_income; upper_99_t


# -------------------------------#
# 6. Significance Tests
# -------------------------------#

# In statistics, a **significance test** checks whether an observed sample
# could plausibly have come from a population with a hypothesized parameter value.
# Here we focus on:
#   (a) Testing a single population mean
#   (b) Testing the difference between two group means


# ---- (a) Testing a single mean ----
# t.test(x, mu = mu0) computes the t statistic, degrees of freedom, p-value
# and a CI for the mean. Useful arguments:
#   mu          the hypothesized value under H0 (default 0)
#   alternative "two.sided", "less" or "greater"
#   conf.level  confidence level of the reported CI (default 0.95)
?t.test

# ---------------------------------------------#
# Question:
# Is the average monthly income in our sample
# different from the population mean in Ireland (from Google: 3034)?

# Hypotheses: Should our test be one or two-sided?
# Write H0 and HA here
# H0: 
# HA: 
 
# EXERCISE: Conduct the appropriate test (using the built-in R function).


# What is our conclusion?


# EXERCISE: Now test a one-sided hypothesis: is the mean LESS than 3034?




# ---- (b) Testing a difference in means ----
# Two-sample t-test: compares the means of two independent groups.

# By default, t.test() uses Welch's t-test, which does NOT assume equal variances.
# H0: the two group means are equal (difference = 0).

# ---------------------------------------------#
# Question:
#   Do people living in the capital earn different
#   incomes than those living elsewhere?
#
# Hypotheses: Should our test be one or two-sided?
# Write H0 and HA here:
# H0: 
# HA:

# First, a quick descriptive check: the group means.
# We need to select the income of only one group at a time, which is
# subsetting. Step by step:
df$cap                   # see the variable
df$cap == 0              # logical test: TRUE/FALSE for each row
df[df$cap == 0, ]        # keep only the rows where the test is TRUE (non-capital)
df[df$cap == 0, ]$income # select the income column of those rows


# EXERCISE: Calculate the mean income of each group.
# Non-capital:

# Capital:


# EXERCISE: Conduct a two-sample t-test (Welch), two-sided.



# Conclusion? What does the CI tell us about the direction of the difference?


# EXERCISE: On average, do people earn more in the capital
# compared to people who do not reside in the capital?
# Conduct the one-sided test.


# Interpretation:
# - If p-value < 0.05 : reject H0 (the means differ significantly)
# - In the one-sided case, a small p-value supports the specific direction
#   stated in HA, but says nothing about the opposite direction.


# -----------------------------------------------------------#
### Extra activity with real-world data (difference in means):
# -----------------------------------------------------------#

# Goal: Test whether mean Polity scores differ between
#       Eastern Europe vs Western Europe & North America.
# Data: polity.dta — Polity score (0–10), higher = more democratic.

# Why not load("polity.dta")?
# - load() is for .RData/.rda (R's serialized objects), not Stata files.
# - Use a function made for .dta files: foreign::read.dta() (used here) or
#   haven::read_dta() (newer alternative).
data <- read.dta("../../datasets/polity.dta")

# Quick look
head(data)
glimpse(data)
table(data$region) # how many countries per region?

# Variable of interest: fh_polity2 - numeric Polity score (0-10)

# Subset the two regions of interest:
west <- data$fh_polity2[data$region == "Western Europe and North America"]
east <- data$fh_polity2[data$region == "Eastern Europe"]

# Quick descriptive statistics - careful for missing values!
# If a variable has NAs, mean() and sd() return NA unless you add na.rm = TRUE.
# The same holds for length(): it counts NAs too.

# Check first whether there are any missing values:
sum(is.na(west)); sum(is.na(east))

# EXERCISE: Compute the mean, n and SD for each group.
mean_west <- 
mean_east <- 
n_west    <- 
n_east    <- 
sd_west   <- 
sd_east   <- 

mean_west; mean_east
n_west; n_east
sd_west; sd_east

# EXERCISE: Calculate the SEs
# SE = sample SD / sqrt(n)
se_west <- 
se_east <-
  
se_west; se_east

# -------------------------------------#
#  Analytical CI (Normal Approximation)
# -------------------------------------#
# For a difference in means, the standard error combines the uncertainty of
# both groups:
#   Diff = mean_west - mean_east
#   SE_diff = sqrt(Var_west/n_west + Var_east/n_east)
#   95% CI = Diff ± 1.96 * SE_diff

# EXERCISE: Compute the CI by hand using the formulas above.
diff_hat <- 
se_diff  <- 

ci_low_analytic <- 
ci_up_analytic  <- 
ci_analytic     <- c(ci_low_analytic, ci_up_analytic)

diff_hat
se_diff
ci_analytic

# ------------------------#
#  Welch Two-Sample t-test
# ------------------------#
# We already know t.test(). When given two numeric vectors instead of a
# formula, it compares their means: t.test(x, y).
# The result is an object; we can store it and extract pieces with $,
# e.g. res$conf.int, res$p.value, res$statistic.

# EXERCISE: Conduct a two-sided diff-in-means test and store it in t_test_res.
t_test_res <- 
t_test_res

# EXERCISE: Extract the CI from the stored test.
ci_t <- 
ci_t

# Does it match your by-hand CI? Why is it slightly different?

# Conclusion?
