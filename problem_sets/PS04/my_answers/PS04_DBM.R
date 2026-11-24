# Applied Statistics I, Problem Set 4
# Barrera Munoz Diego


##### Question 1) Economics

### (a) Recode occupation type as professional or non-professional.



library(car) # install.packages(car) if needed.
library(stargazer)
data(Prestige)
help(Prestige)

# Code professionals as 1 and blue and white collar occupations as 0.
Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)
type_check <- with(Prestige, table(type, professional, useNA = "ifany"))
rownames(type_check) <- c(levels(Prestige$type), "Missing")
colnames(type_check) <- c("0", "1", "Missing")
stargazer(unclass(type_check), summary = FALSE, digits = 0, float = FALSE)

### (b) Regress prestige on income, professional, and their interaction.

# Fit a separate income slope for each occupation group.
prestige_model <- lm(prestige ~ income * professional, data = Prestige)
stargazer(prestige_model, digits = 6, single.row = TRUE,  # Create regression table.
          report = "vc*st", omit.stat = c("ser", "f"), float = FALSE)

### (c) Write the prediction equation.

# Read the coefficients for the prediction equation.
b <- coef(prestige_model)
equation <- formatC(b, format = "f", digits = 8)  # Keep the small slopes visible.
stargazer(t(equation), summary = FALSE, digits = 8, float = FALSE)  # Create coefficient table.

### (d) Interpret the coefficient for income.

# Express the non-professional income slope per $1,000.
print(1000 * b[["income"]])

### (e) Interpret the coefficient for professional.

# Compare professional and non-professional occupations at zero income.
print(b[["professional"]])

### (f) Calculate the change in prestige for a $1,000 income increase among professionals.

# Add the income and interaction coefficients to get the professional slope.
professional_gain <- 1000 * (b[["income"]] + b[["income:professional"]])
print(professional_gain)

### (g) Compare professional and non-professional occupations at an income of $6,000.

# Predict prestige for both groups and subtract the non-professional prediction.
income_6000 <- data.frame(income = 6000, professional = c(0, 1))
predicted <- predict(prestige_model, newdata = income_6000)
comparison <- c("Non-professional" = predicted[[1]], Professional = predicted[[2]],
                Difference = predicted[[2]] - predicted[[1]])
stargazer(t(comparison), summary = FALSE, digits = 4, float = FALSE)


##### Question 2) Political Science

### (a) Test whether assigning signs to a precinct affects vote share.

# Test the assignment coefficient against zero using both tails.
df_signs <- 131 - 2 - 1  # Two slopes and one intercept.
t_assigned <- 0.042 / 0.016  # Assigned estimate divided by its standard error.
p_assigned <- 2 * pt(-abs(t_assigned), df = df_signs)
assigned_test <- formatC(c(t = t_assigned, p = p_assigned),
                         format = "f", digits = 8)
stargazer(t(assigned_test), summary = FALSE, float = FALSE)

### (b) Test whether being next to a precinct assigned signs affects vote share.

# Use the standard error for adjacency and the same residual degrees of freedom.
t_adjacent <- 0.042 / 0.013  # Adjacent estimate divided by its standard error.
p_adjacent <- 2 * pt(-abs(t_adjacent), df = df_signs)
adjacent_test <- formatC(c(t = t_adjacent, p = p_adjacent),
                         format = "f", digits = 8)
stargazer(t(adjacent_test), summary = FALSE, float = FALSE)

### (d) Evaluate the model fit.

# Calculate the percentages of precinct-level variation explained and left unexplained.
r_squared <- 0.094
fit_percent <- 100 * c(Explained = r_squared, Unexplained = 1 - r_squared)
stargazer(t(fit_percent), summary = FALSE, digits = 1, float = FALSE)