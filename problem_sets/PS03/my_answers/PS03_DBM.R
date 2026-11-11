# Applied Statistics I, Problem Set 3
# Barrera Munoz Diego




##### Question 1)

### 1.1) Run a regression where the outcome variable is voteshare and the explanatory variable is difflog.

# Read and summarise the election data.
library(stargazer)
incumbents <- read.csv("incumbents_subset.csv", row.names = 1)
incumbents <- incumbents[c("voteshare", "difflog", "presvote")]
descriptive <- rbind(Elections = nrow(incumbents),
                     Missing = colSums(is.na(incumbents)),
                     sapply(incumbents, summary))
stargazer(t(descriptive),  # Create the descriptive statistics table.
          summary = FALSE, digits = 4, float = FALSE)

# Fit incumbent vote share on the log spending gap.
vote_spending <- lm(voteshare ~ difflog, data = incumbents)
stargazer(vote_spending, digits = 6, single.row = TRUE,  # Create regression table.
          report = "vc*st", omit.stat = c("ser", "f"), float = FALSE)

### 1.2) Make a scatterplot of the two variables and add the regression line.

# Calculate the fitted mean and its pointwise 95% limits.
library(ggplot2)
spending_gap <- seq(min(incumbents$difflog), max(incumbents$difflog),
                    length.out = 200)
vote_grid <- data.frame(difflog = spending_gap)
vote_grid <- cbind(vote_grid, predict(vote_spending, vote_grid,
                              interval = "confidence", level = 0.95))

# Draw and save the scatterplot.
vote_spending_plot <- ggplot(vote_grid, aes(x = difflog)) +
  geom_point(data = incumbents, aes(y = voteshare),
             size = 0.8, alpha = 0.28, colour = "grey35") +
  geom_line(aes(y = lwr), colour = "red4", linetype = "dashed") +
  geom_line(aes(y = upr), colour = "red4", linetype = "dashed") +
  geom_line(aes(y = fit), colour = "steelblue2", linewidth = 0.7) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(title = "Incumbent vote share and campaign spending",
       subtitle = "Fitted mean in blue with dashed 95% confidence limits",
       x = "difflog (Log incumbent spending minus log challenger spending)",
       y = "voteshare (Incumbent vote share)") +
  theme_classic(base_size = 12)
ggsave("PS03_Q1_vote_spending.pdf", vote_spending_plot,
       width = 7, height = 4)

### 1.3) Save the residuals of the model in a separate object.

# Save the residuals and check the first election.
vote_resid <- resid(vote_spending)
first_election <- c(observed = incumbents$voteshare[1],
                    fitted = unname(fitted(vote_spending)[1]),
                    residual = unname(vote_resid[1]))
stargazer(t(first_election), summary = FALSE, digits = 6, float = FALSE)  # Create output table.

### 1.4) Write the prediction equation.

# Read the coefficients for the prediction equation.
equation <- round(coef(vote_spending), 6) # Rounded to 6 decimals.
stargazer(t(equation), summary = FALSE, digits = 6, float = FALSE)  # Create coefficient table.

##### Question 2)

### 2.1) Run a regression where the outcome variable is presvote and the explanatory variable is difflog.

# Fit presidential vote share on the log spending gap.
pres_spending <- lm(presvote ~ difflog, data = incumbents)
stargazer(pres_spending, digits = 6, single.row = TRUE,  # Create regression table.
          report = "vc*st", omit.stat = c("ser", "f"), float = FALSE)

### 2.2) Make a scatterplot of the two variables and add the regression line.

# Calculate the fitted mean and its pointwise 95% limits.
pres_grid <- data.frame(difflog = spending_gap)
pres_grid <- cbind(pres_grid, predict(pres_spending, pres_grid,
                              interval = "confidence", level = 0.95))

# Draw and save the scatterplot.
pres_spending_plot <- ggplot(pres_grid, aes(x = difflog)) +
  geom_point(data = incumbents, aes(y = presvote),
             size = 0.8, alpha = 0.28, colour = "grey35") +
  geom_line(aes(y = lwr), colour = "red4", linetype = "dashed") +
  geom_line(aes(y = upr), colour = "red4", linetype = "dashed") +
  geom_line(aes(y = fit), colour = "steelblue2", linewidth = 0.7) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(title = "Presidential vote share and campaign spending",
       subtitle = "Fitted mean in blue with dashed 95% confidence limits",
       x = "difflog (Log incumbent spending minus log challenger spending)",
       y = "presvote (Presidential vote share)") +
  theme_classic(base_size = 12)
ggsave("PS03_Q2_pres_spending.pdf", pres_spending_plot,
       width = 7, height = 4)

### 2.3) Save the residuals of the model in a separate object.

# Save the residuals and check the first election.
pres_resid <- resid(pres_spending)
first_election <- c(observed = incumbents$presvote[1],
                    fitted = unname(fitted(pres_spending)[1]),
                    residual = unname(pres_resid[1]))
stargazer(t(first_election), summary = FALSE, digits = 6, float = FALSE)  # Create output table.

### 2.4) Write the prediction equation.

# Read the coefficients for the prediction equation.
equation <- round(coef(pres_spending), 6) # Rounded to 6 decimals.
stargazer(t(equation), summary = FALSE, digits = 6, float = FALSE)  # Create coefficient table.

##### Question 3)

### 3.1) Run a regression where the outcome variable is voteshare and the explanatory variable is presvote.

# Fit incumbent vote share on presidential vote share.
vote_pres <- lm(voteshare ~ presvote, data = incumbents)
stargazer(vote_pres, digits = 6, single.row = TRUE,  # Create regression table.
          report = "vc*st", omit.stat = c("ser", "f"), float = FALSE)

### 3.2) Make a scatterplot of the two variables and add the regression line.

# Calculate the fitted mean and its pointwise 95% limits.
pres_share <- seq(min(incumbents$presvote), max(incumbents$presvote),
                  length.out = 200)
support_grid <- data.frame(presvote = pres_share)
support_grid <- cbind(support_grid, predict(vote_pres, support_grid,
                                    interval = "confidence", level = 0.95))

# Draw and save the scatterplot.
vote_pres_plot <- ggplot(support_grid, aes(x = presvote)) +
  geom_point(data = incumbents, aes(y = voteshare),
             size = 0.8, alpha = 0.28, colour = "grey35") +
  geom_line(aes(y = lwr), colour = "red4", linetype = "dashed") +
  geom_line(aes(y = upr), colour = "red4", linetype = "dashed") +
  geom_line(aes(y = fit), colour = "steelblue2", linewidth = 0.7) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(title = "Incumbent and presidential vote shares",
       subtitle = "Fitted mean in blue with dashed 95% confidence limits",
       x = "presvote (Presidential vote share)",
       y = "voteshare (Incumbent vote share)") +
  theme_classic(base_size = 12)
ggsave("PS03_Q3_vote_pres.pdf", vote_pres_plot,
       width = 7, height = 4)

### 3.3) Write the prediction equation.

# Read the coefficients for the prediction equation.
equation <- round(coef(vote_pres), 6) # Rounded to 6 decimals.
stargazer(t(equation), summary = FALSE, digits = 6, float = FALSE)  # Create coefficient table.

##### Question 4)

### 4.1) Run a regression where the outcome variable is the residuals from Question 1 and the explanatory variable is the residuals from Question 2.

# Regress the incumbent residual on the presidential residual.
residual_data <- data.frame(vote_resid, pres_resid)
resid_model <- lm(vote_resid ~ pres_resid, data = residual_data)
stargazer(resid_model, digits = 6, single.row = TRUE,  # Create regression table.
          report = "vc*st", omit.stat = c("ser", "f"), float = FALSE)

### 4.2) Make a scatterplot of the two residuals and add the regression line.

# Calculate the fitted mean and its pointwise 95% limits.
pres_gap <- seq(min(residual_data$pres_resid), max(residual_data$pres_resid),
                length.out = 200)
resid_grid <- data.frame(pres_resid = pres_gap)
resid_grid <- cbind(resid_grid, predict(resid_model, resid_grid,
                                interval = "confidence", level = 0.95))

# Draw and save the scatterplot.
resid_plot <- ggplot(resid_grid, aes(x = pres_resid)) +
  geom_point(data = residual_data, aes(y = vote_resid),
             size = 0.8, alpha = 0.28, colour = "grey35") +
  geom_line(aes(y = lwr), colour = "red4", linetype = "dashed") +
  geom_line(aes(y = upr), colour = "red4", linetype = "dashed") +
  geom_line(aes(y = fit), colour = "steelblue2", linewidth = 0.7) +
  # To read the residuals in percentage points, we multiply the axis labels by 100.
  scale_x_continuous(labels = scales::label_number(scale = 100)) +
  scale_y_continuous(labels = scales::label_number(scale = 100)) +
  labs(title = "Added-variable plot for incumbent vote share",
       subtitle = "Fitted mean in blue with dashed 95% confidence limits",
       x = "Presidential vote-share residual (percentage points)",
       y = "Incumbent vote-share residual (percentage points)") +
  theme_classic(base_size = 12)
ggsave("PS03_Q4_added_variable.pdf", resid_plot,
       width = 7, height = 4)

### 4.3) Write the prediction equation.

# Read the coefficients for the prediction equation.
equation <- round(coef(resid_model), 6) # Rounded to 6 decimals.
stargazer(t(equation), summary = FALSE, digits = 6, float = FALSE)  # Create coefficient table.

##### Question 5)

### 5.1) Run a regression where the outcome variable is the incumbent’s voteshare and the explanatory variables are difflog and presvote.

# Fit incumbent vote share on both predictors.
full_model <- lm(voteshare ~ difflog + presvote, data = incumbents)
stargazer(full_model, digits = 6, single.row = TRUE,  # Create regression table.
          report = "vc*st", omit.stat = c("ser", "f"), float = FALSE)

### 5.2) Write the prediction equation.

# Read the coefficients for the prediction equation.
equation <- round(coef(full_model), 6) # Rounded to 6 decimals.
stargazer(t(equation), summary = FALSE, digits = 6, float = FALSE)  # Create coefficient table.

### 5.3) What is it in this output that is identical to the output in Question 4? Why do you think this is the case?

# Print the two fitted models side by side, as in the Week 9 tutorial.
old_options <- options(digits = 15)  # Show extra digits for the two slopes.
stargazer(resid_model, full_model,  # Create the model comparison table.
          digits = 10, single.row = TRUE,
          omit.stat = "f", float = FALSE)

# Check the unrounded slopes and the complete residual vectors.
print(all.equal(coef(resid_model)[["pres_resid"]], # Comparing slopes.
                coef(full_model)[["presvote"]]))
print(all.equal(resid(resid_model), resid(full_model))) # Comparing residuals.
options(old_options)