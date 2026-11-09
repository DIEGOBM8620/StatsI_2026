# 1(a) Import the data and check the three variables.
incumbents <- read.csv("incumbents_subset.csv", row.names = 1)
incumbents <- incumbents[c("voteshare", "difflog", "presvote")]
print(dim(incumbents))
print(colSums(is.na(incumbents)))
summary(incumbents)

# 1(a) Regress voteshare on difflog.
vote_spending <- lm(voteshare ~ difflog, data = incumbents)
summary(vote_spending)

# 1(b) Calculate 95% confidence limits for the fitted mean.
library(ggplot2)
dir.create("Figures", showWarnings = FALSE)
spending_gap <- seq(min(incumbents$difflog), max(incumbents$difflog),
                    length.out = 200)
vote_grid <- data.frame(difflog = spending_gap)
vote_grid <- cbind(vote_grid, predict(vote_spending, vote_grid,
                              interval = "confidence", level = 0.95))

# 1(b) Plot the observations, fitted line and confidence limits.
vote_spending_plot <- ggplot(vote_grid, aes(x = difflog)) +
  geom_point(data = incumbents, aes(y = voteshare),
             size = 0.8, alpha = 0.28, colour = "grey35") +
  geom_line(aes(y = lwr), colour = "sienna3", linetype = "dashed") +
  geom_line(aes(y = upr), colour = "sienna3", linetype = "dashed") +
  geom_line(aes(y = fit), colour = "steelblue4", linewidth = 0.7) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(title = "Incumbent vote share and campaign spending",
       subtitle = "Fitted mean in blue with dashed 95% confidence limits",
       x = "Log spending gap (incumbent minus challenger)",
       y = "Incumbent vote share") +
  theme_classic(base_size = 12)
ggsave("Figures/Q1_vote_spending.pdf", vote_spending_plot,
       width = 7, height = 4)

# 1(c) Save the residuals and check the first election.
vote_resid <- resid(vote_spending)
first_election <- c(observed = incumbents$voteshare[1],
                    fitted = unname(fitted(vote_spending)[1]),
                    residual = unname(vote_resid[1]))
print(round(first_election, 6))

# 1(d) Get the coefficients for the prediction equation.
round(coef(vote_spending), 6)

# 2(a) Regress presvote on difflog.
pres_spending <- lm(presvote ~ difflog, data = incumbents)
summary(pres_spending)

# 2(b) Calculate 95% confidence limits for the fitted mean.
pres_grid <- data.frame(difflog = spending_gap)
pres_grid <- cbind(pres_grid, predict(pres_spending, pres_grid,
                              interval = "confidence", level = 0.95))

# 2(b) Plot the observations, fitted line and confidence limits.
pres_spending_plot <- ggplot(pres_grid, aes(x = difflog)) +
  geom_point(data = incumbents, aes(y = presvote),
             size = 0.8, alpha = 0.28, colour = "grey35") +
  geom_line(aes(y = lwr), colour = "sienna3", linetype = "dashed") +
  geom_line(aes(y = upr), colour = "sienna3", linetype = "dashed") +
  geom_line(aes(y = fit), colour = "steelblue4", linewidth = 0.7) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(title = "Presidential vote share and campaign spending",
       subtitle = "Fitted mean in blue with dashed 95% confidence limits",
       x = "Log spending gap (incumbent minus challenger)",
       y = "Presidential vote share") +
  theme_classic(base_size = 12)
ggsave("Figures/Q2_pres_spending.pdf", pres_spending_plot,
       width = 7, height = 4)

# 2(c) Save the residuals and check the first election.
pres_resid <- resid(pres_spending)
first_election <- c(observed = incumbents$presvote[1],
                    fitted = unname(fitted(pres_spending)[1]),
                    residual = unname(pres_resid[1]))
print(round(first_election, 6))

# 2(d) Get the coefficients for the prediction equation.
round(coef(pres_spending), 6)

# 3(a) Regress voteshare on presvote.
vote_pres <- lm(voteshare ~ presvote, data = incumbents)
summary(vote_pres)

# 3(b) Calculate 95% confidence limits for the fitted mean.
pres_share <- seq(min(incumbents$presvote), max(incumbents$presvote),
                  length.out = 200)
support_grid <- data.frame(presvote = pres_share)
support_grid <- cbind(support_grid, predict(vote_pres, support_grid,
                                    interval = "confidence", level = 0.95))

# 3(b) Plot the observations, fitted line and confidence limits.
vote_pres_plot <- ggplot(support_grid, aes(x = presvote)) +
  geom_point(data = incumbents, aes(y = voteshare),
             size = 0.8, alpha = 0.28, colour = "grey35") +
  geom_line(aes(y = lwr), colour = "sienna3", linetype = "dashed") +
  geom_line(aes(y = upr), colour = "sienna3", linetype = "dashed") +
  geom_line(aes(y = fit), colour = "steelblue4", linewidth = 0.7)

# Label the axes and save the plot.
vote_pres_plot <- vote_pres_plot +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(title = "Incumbent and presidential vote shares",
       subtitle = "Fitted mean in blue with dashed 95% confidence limits",
       x = "Presidential vote share",
       y = "Incumbent vote share") +
  theme_classic(base_size = 12)
ggsave("Figures/Q3_vote_pres.pdf", vote_pres_plot,
       width = 7, height = 4)

# 3(c) Get the coefficients for the prediction equation.
round(coef(vote_pres), 6)

# 4(a) Regress vote_resid on pres_resid.
residual_data <- data.frame(vote_resid, pres_resid)
resid_model <- lm(vote_resid ~ pres_resid, data = residual_data)
summary(resid_model)

# 4(b) Calculate 95% confidence limits for the fitted mean.
pres_gap <- seq(min(residual_data$pres_resid), max(residual_data$pres_resid),
                length.out = 200)
resid_grid <- data.frame(pres_resid = pres_gap)
resid_grid <- cbind(resid_grid, predict(resid_model, resid_grid,
                                interval = "confidence", level = 0.95))

# 4(b) Plot the observations, fitted line and confidence limits.
resid_plot <- ggplot(resid_grid, aes(x = pres_resid)) +
  geom_point(data = residual_data, aes(y = vote_resid),
             size = 0.8, alpha = 0.28, colour = "grey35") +
  geom_line(aes(y = lwr), colour = "sienna3", linetype = "dashed") +
  geom_line(aes(y = upr), colour = "sienna3", linetype = "dashed") +
  geom_line(aes(y = fit), colour = "steelblue4", linewidth = 0.7)

# Label the axes and save the plot.
resid_plot <- resid_plot +
  scale_x_continuous(labels = scales::label_number(scale = 100)) +
  scale_y_continuous(labels = scales::label_number(scale = 100)) +
  labs(title = "Vote shares after accounting for campaign spending",
       subtitle = "Fitted mean in blue with dashed 95% confidence limits",
       x = "Presidential vote-share residual (percentage points)",
       y = "Incumbent vote-share residual (percentage points)") +
  theme_classic(base_size = 12)
ggsave("Figures/Q4_added_variable.pdf", resid_plot,
       width = 7, height = 4)

# 4(c) Get the coefficients for the prediction equation.
round(coef(resid_model), 6)

# 5(a) Regress incumbent vote share on both predictors.
full_model <- lm(voteshare ~ difflog + presvote, data = incumbents)
summary(full_model)

# 5(b) Get the coefficients for the prediction equation.
round(coef(full_model), 6)

# 5(c) Compare the slopes and residuals from Questions 4 and 5.
slopes <- c(Q4 = unname(coef(resid_model)["pres_resid"]),
            Q5 = unname(coef(full_model)["presvote"]))
print(slopes, digits = 10)
print(all.equal(unname(resid(resid_model)), unname(resid(full_model))))
