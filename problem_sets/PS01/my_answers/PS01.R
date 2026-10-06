#####################
library(ggplot2)
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", 
                      "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

# 1. Find a 90% confidence interval for the average student IQ in the school (As a hint, you first need the mean and SD to create a CI):

n <- length(y)
y_bar <- mean(y)
s <- sd(y)
se <- s/sqrt(n)
cat(" 'y' has",n,"observations, \n The sample mean is :",y_bar, 
    " \n The standard deviation is: ", s, 
    " \n And the standard error is: ", se)

critical_t <- qt(p = 0.95, df = n - 1) # df = 25 -1 = 24
critical_t

ci90 <- c(y_bar - critical_t * se,
          y_bar + critical_t * se)
print(c("Lower" = round(ci90, 2)[1], "Upper" = round(ci90, 2)[2]))

# Next, the school counselor was curious  whether  the average student IQ in her school is higher than the average IQ score (100) among all the schools in the country.

t.test(y, mu = 100, alternative = "greater")

#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2026/main/datasets/expenditure.txt", header=T)

# Explore the 'expenditure' data set and import data into R:

print(head(expenditure)) # Look at the first rows in the data set.
print(dim(expenditure)) # Check the structure (dimensions) of the data set.
print(sum(is.na(expenditure))) # Check if the data set has NA values.

# Please plot the relationships among Y, X1, X2, and X3? What are the correlations among them (you just need to describe the graph and the relationships among them)?

# Create a folder for the images to be used in the TexStudio document.
dir.create("PS01_figures", showWarnings = FALSE)

# 1. Plot the relationships among Y, X1, X2 and X3.

variables <- expenditure[c("Y", "X1", "X2", "X3")] # Extract only the required variables to another data set.

pdf("PS01_figures/scatterplot_matrix.pdf", width = 7, height = 5)
pairs(variables, pch = 16, col = "blue") # Plot the variables.
dev.off()

correlations <- cor(variables) # Get the correlation coefficients.
print(round(correlations, 3))

# 2. Please plot the relationship between Y and Region? On average, which region has the highest per capita expenditure on housing assistance?

expenditure$Region_name <- factor(expenditure$Region, # Set Region as factors as a new variable. 
                                  levels = 1:4,
                                  labels = c("Northeast", 
                                             "North Central", 
                                             "South", 
                                             "West"))

region_means <- tapply(expenditure$Y, expenditure$Region_name, mean) # Get the mean for each region.

pdf("PS01_figures/spending_by_region.pdf", width = 7, height = 7) # Create image.
boxplot(Y ~ Region_name, data = expenditure,
        col = "lightgrey", xlab = "Region",
        ylab = "Per capita expenditure (Y)")
points(1:4, region_means, col = "red", pch = 18) # Add a red dot where the mean is in the boxplot for each region.
legend("topleft", legend = "Mean", col = "red", pch = 18,
       bty = "n")
dev.off()

print(round(region_means, 2))

# 3. Please plot the relationship between Y and X1? Describe this graph and the relationship. Reproduce the above graph including one more variable Region and display different regions with different types of symbols and colors.

income_plot <- ggplot(expenditure, aes(x = X1, y = Y)) +
  geom_point(colour = "blue", size = 2) +
  labs(x = "Per capita personal income (X1)",
       y = "Per capita expenditure (Y)") +
  theme_minimal()
ggsave("PS01_figures/spending_and_income.pdf", plot = income_plot,
       width = 7, height = 4)

expenditure[expenditure$STATE %in% c("WY", "MI"), c("STATE", "X1", "Y")]

# Adding Region to the same graph.

region_income_plot <- ggplot(expenditure, aes(x = X1, y = Y, colour = Region_name, shape = Region_name)) +
  geom_point(size = 2.5) +
  scale_colour_manual(values = c("blue", "darkgreen", "orange", "red")) +
  scale_shape_manual(values = c(15, 16, 17, 18)) +
  labs(x = "Per capita personal income (X1)",
       y = "Per capita expenditure (Y)",
       colour = "Region", shape = "Region") +
  theme_minimal()
ggsave("PS01_figures/income_and_spending_by_region.pdf",
       plot = region_income_plot, width = 7, height = 4)