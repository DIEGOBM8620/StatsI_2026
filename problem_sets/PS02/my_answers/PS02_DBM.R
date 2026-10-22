# Problem Set 2
# Barrera Munoz Diego

##### Question 1: Political Science

### (a) Calculate the chi-squared test statistic manually.

# Input the data.
observed <- matrix(c(14, 6, 7, 7, 7, 1), nrow = 2, byrow = TRUE)
rownames(observed) <- c("Upper class", "Lower class")
colnames(observed) <- c("Not stopped", "Bribe", "Warning")
print(observed)

# Calculate each row total, each column total and the grand total.
t_upper <- observed[1, 1] + observed[1, 2] + observed[1, 3]
t_lower <- observed[2, 1] + observed[2, 2] + observed[2, 3]

t_not_stopped <- observed[1, 1] + observed[2, 1]
t_bribe <- observed[1, 2] + observed[2, 2]
t_warning <- observed[1, 3] + observed[2, 3]

t <- t_upper + t_lower

t_observed <- rbind(
  cbind(observed, Total = c(t_upper, t_lower)),
  Total = c(t_not_stopped, t_bribe, t_warning, t))

print(t_observed)

# Expected counts: row total times column total, divided by t (42).
E11 <- (t_upper * t_not_stopped) / t
E12 <- (t_upper * t_bribe) / t
E13 <- (t_upper * t_warning) / t
E21 <- (t_lower * t_not_stopped) / t
E22 <- (t_lower * t_bribe) / t
E23 <- (t_lower * t_warning) / t
expected <- matrix(c(E11, E12, E13, E21, E22, E23),
                   nrow = 2, byrow = TRUE, dimnames = dimnames(observed))
print(round(expected, 4)) # Rounded to 4 decimal points.

# Subtract the expected count from the observed count in each cell.
D11 <- observed[1, 1] - E11
D12 <- observed[1, 2] - E12
D13 <- observed[1, 3] - E13
D21 <- observed[2, 1] - E21
D22 <- observed[2, 2] - E22
D23 <- observed[2, 3] - E23
differences <- matrix(c(D11, D12, D13, D21, D22, D23),
                      nrow = 2, byrow = TRUE, 
                      dimnames = dimnames(observed))
print(round(differences, 4)) # Rounded to 4 decimal points.

# Square each of the six differences.
S11 <- D11^2
S12 <- D12^2
S13 <- D13^2
S21 <- D21^2
S22 <- D22^2
S23 <- D23^2
squared_differences <- matrix(c(S11, S12, S13, S21, S22, S23),
                              nrow = 2, byrow = TRUE,
                              dimnames = dimnames(observed))
print(round(squared_differences, 4)) # Rounded to 4 decimal points.

# Divide each squared difference by its own expected count.
C11 <- S11 / E11
C12 <- S12 / E12
C13 <- S13 / E13
C21 <- S21 / E21
C22 <- S22 / E22
C23 <- S23 / E23
div_squared_differences <- matrix(c(C11, C12, C13, C21, C22, C23),
                                  nrow = 2, byrow = TRUE, 
                                  dimnames = dimnames(observed))
print(round(div_squared_differences, 4)) # Rounded to 4 decimal points.

# Add the six values.
chi_squared <- C11 + C12 + C13 + C21 + C22 + C23
cat("Therefore, the chi-squared test statistic is:", chi_squared)



### (b) Calculate the degrees of freedom and the p-value.

df_chi <- (2 - 1) * (3 - 1)
p_chi <- pchisq(chi_squared, df = df_chi, lower.tail = FALSE)
cat("The p-value is:", p_chi)

### (c) Calculate each adjusted standardized residual separately.

# Calculate each row proportion and each column proportion.
r1 <- t_upper / t
r2 <- t_lower / t
c1 <- t_not_stopped / t
c2 <- t_bribe / t
c3 <- t_warning / t
print(c(r1 = r1, r2 = r2, c1 = c1, c2 = c2, c3 = c3))

# Calculate the denominator.
SE11 <- (E11 * (1 - r1) * (1 - c1))^0.5
SE12 <- (E12 * (1 - r1) * (1 - c2))^0.5
SE13 <- (E13 * (1 - r1) * (1 - c3))^0.5
SE21 <- (E21 * (1 - r2) * (1 - c1))^0.5
SE22 <- (E22 * (1 - r2) * (1 - c2))^0.5
SE23 <- (E23 * (1 - r2) * (1 - c3))^0.5
mat_se <- matrix(c(SE11, SE12, SE13, SE21, SE22, SE23),
                 nrow = 2, byrow = TRUE, dimnames = dimnames(observed))
print(round(mat_se, 4)) # Rounded to 4 decimal points.

# Calculate each residual using that cell's observed count and denominator.
z11 <- (observed[1, 1] - E11) / SE11
z12 <- (observed[1, 2] - E12) / SE12
z13 <- (observed[1, 3] - E13) / SE13
z21 <- (observed[2, 1] - E21) / SE21
z22 <- (observed[2, 2] - E22) / SE22
z23 <- (observed[2, 3] - E23) / SE23
standardized_residuals <- matrix(c(z11, z12, z13, z21, z22, z23),
                                 nrow = 2, byrow = TRUE,
                                 dimnames = dimnames(observed))
print(round(standardized_residuals, 3)) # Rounded to 4 decimal points.




#### Question 2: Economics -----------------------------------------------------

### (b) Import the data and fit the bivariate regression.

# Import the data.
women <- read.csv(paste0("https://raw.githubusercontent.com/",
                         "kosukeimai/qss/master/PREDICTION/women.csv"))
print(dim(women))
print(sum(is.na(women)))
print(table(women$reserved))

# Estimate the bivariate regression.
water_model <- lm(water ~ reserved, data = women)
summary(water_model)

# Reproduce the two-sided slope test from the regression output.
beta_1 <- coef(water_model)["reserved"]
se_beta_1 <- coef(summary(water_model))["reserved", "Std. Error"]
t_slope <- unname(beta_1 / se_beta_1)
df_slope <- df.residual(water_model)
p_slope <- 2 * pt(abs(t_slope), df = df_slope, lower.tail = FALSE)
print(c(t = t_slope, df = df_slope, p_value = p_slope))

# Calculate the 95% confidence interval.
t_critical <- qt(0.975, df = df_slope)
ci_slope <- unname(beta_1) + c(-1, 1) * t_critical * se_beta_1
names(ci_slope) <- c("Lower", "Upper")
print(ci_slope)

# Check the CI using confint.
print(confint(water_model, "reserved", level = 0.95))

# Plot for OLS
library(ggplot2)

plot_data <- data.frame(reserved = c(0, 1))
plot_data <- cbind(plot_data, predict(water_model, plot_data, interval = "confidence"))

water_plot <- ggplot(plot_data, aes(factor(reserved), fit, ymin = lwr, ymax = upr)) +
  geom_errorbar(width = 0.12, colour = "steelblue") +
  geom_point(size = 3, colour = "steelblue") +
  scale_x_discrete(labels = c("Not reserved", "Reserved")) +
  labs(title = "Drinking-water facilities by reservation status",
       subtitle = "OLS fitted means with conventional 95% confidence intervals",
       x = "Reservation status", y = "Mean drinking-water facilities") +
  theme_classic()
ggsave("PS02_figures/PS02_group_means.pdf", water_plot, width = 7, height = 5)



### (c) Interpret the policy coefficient as a difference in group means.

mean_water <- tapply(women$water, women$reserved, mean)
print(mean_water)
mean_difference <- unname(mean_water["1"] - mean_water["0"])
print(mean_difference)