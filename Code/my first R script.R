# =============================================================================
# ECON20110/30370 Econometrics
# Practical 2: Reading regression output in R
#
# How to use this script
#   * Parts A and B (Q1-Q5): run these on your own BEFORE the session.
#   * Parts C to E (Q6-Q11): we work through these together in the session.
#   * Part F (Q12-Q17): further practice on your own.
# Run the script line by line (Ctrl+Enter on Windows, Cmd+Enter on Mac).
# Write each answer as a comment under "# Answer:", the line after the code
# that produced it. The questions in full are in the Practical 2 PDF.
# =============================================================================


# ---- Setup ------------------------------------------------------------------

# EDIT THIS LINE: put the path to the folder where you saved this script and
setwd("C:/Users/h75391rk/OneDrive/manchester/teaching/econ20110/content/practical/2")

getwd()        # the folder R is working in
list.files()   # must include "wage2.csv"; if not, check the folder


# =============================================================================
# PART A: LOAD AND LOOK  (on your own, before the session)
# =============================================================================

# ---- Q1. Load the data ------------------------------------------------------

wage2 <- read.csv("wage2.csv")   # reads the file into a data frame

dim(wage2)    # number of rows (observations) and columns (variables)
head(wage2)   # the first 6 rows

# (a) How many observations and variables?
# Answer:

# (b) What is the unit of observation? Why is this a cross section?
# Answer:


# ---- Q2. Summarise and plot wages -------------------------------------------

# (a) Summary statistics. sapply() applies the function to each variable;
# t() turns the result so each variable is a row.
t(sapply(wage2[c("wage", "educ", "IQ")], function(x)
  c(Mean = mean(x), Median = median(x), SD = sd(x), Min = min(x), Max = max(x))))
# Answer:

# (b) Is the mean wage above or below the median? What does that say about shape?
# Answer:

# (c) Create log wage and plot both histograms side by side.
wage2$lwage <- log(wage2$wage)   # log() is the natural log

par(mfrow = c(1, 2))             # two plots side by side
hist(wage2$wage,  breaks = 30, main = "", xlab = "Monthly earnings (dollars)")
hist(wage2$lwage, breaks = 30, main = "", xlab = "Log monthly earnings")
par(mfrow = c(1, 1))             # back to one plot
# Answer:


# =============================================================================
# PART B: A REGRESSION IN LEVELS  (on your own, before the session)
# =============================================================================

# ---- Q3. Regress wage on education ------------------------------------------

reg_lev <- lm(wage ~ educ, data = wage2)   # OLS of wage on educ, with an intercept
coef(reg_lev)                              # intercept and slope

# (a) Write down the fitted line.
# Answer:

# (b) Interpret the slope, with units.
# Answer:

# (c) Interpret the intercept. Does anyone have zero years of education?
min(wage2$educ)
# Answer:


# ---- Q4. Visualise the fitted line and predict -------------------------------

# (a) Scatter plot with the fitted line.
plot(wage2$educ, wage2$wage, pch = 16, col = rgb(0, 0, 0, 0.3),
     xlab = "Years of education", ylab = "Monthly earnings (dollars)")
abline(reg_lev, col = "firebrick", lwd = 2)

# (b) Predicted wage at 12 and 16 years of education.
pred <- predict(reg_lev, newdata = data.frame(educ = c(12, 16)))
pred
# Answer:

# (c) Difference between the predictions, and four times the slope.
pred[2] - pred[1]
4 * coef(reg_lev)["educ"]
# Answer:


# ---- Q5. Measure the fit -----------------------------------------------------

# (a) R-squared from the output.
summary(reg_lev)$r.squared
# Answer:

# (b) Squared sample correlation.
cor(wage2$wage, wage2$educ)^2
# Answer:

# (c) R-squared by hand: 1 - SSR/SST.
SSR <- sum(resid(reg_lev)^2)                     # sum of squared residuals
SST <- sum((wage2$wage - mean(wage2$wage))^2)    # total sum of squares
1 - SSR / SST
# Answer:

# (d) "R-squared is low, so education is unrelated to wages in this sample." Right?
# Answer:


# =============================================================================
# PART C: REGRESSIONS IN LOGS  (in the session)
# =============================================================================

# ---- Q6. Log-level -------------------------------------------------------------

reg_log <- lm(lwage ~ educ, data = wage2)
coef(reg_log)

# (a) Approximate percentage: 100 x slope.
100 * coef(reg_log)["educ"]
# Answer:

# (b) Exact percentage: 100 x (exp(slope) - 1).
100 * (exp(coef(reg_log)["educ"]) - 1)
# Answer:


# ---- Q7. Compare levels and logs visually -------------------------------------

# (a) Two scatter plots side by side, each with its fitted line.
par(mfrow = c(1, 2))
plot(wage2$educ, wage2$wage, pch = 16, col = rgb(0, 0, 0, 0.3),
     xlab = "Years of education", ylab = "Monthly earnings (dollars)")
abline(reg_lev, col = "firebrick", lwd = 2)
plot(wage2$educ, wage2$lwage, pch = 16, col = rgb(0, 0, 0, 0.3),
     xlab = "Years of education", ylab = "Log monthly earnings")
abline(reg_log, col = "firebrick", lwd = 2)
par(mfrow = c(1, 1))

# (b) In which plot is the spread around the line more even?
# Answer:

# (c) Compare the two R-squared values. Can they tell you which model fits better?
summary(reg_lev)$r.squared
summary(reg_log)$r.squared
# Answer:


# =============================================================================
# PART D: A BINARY REGRESSOR  (in the session)
# =============================================================================

# ---- Q8. Urban and non-urban workers ------------------------------------------

# (a) Regress wage on urban.
reg_urb <- lm(wage ~ urban, data = wage2)
coef(reg_urb)
# Answer:

# (b) Mean wage in each group. tapply() computes the mean separately for urban = 0 and 1.
tapply(wage2$wage, wage2$urban, mean)
# Answer:


# =============================================================================
# PART E: INFERENCE  (in the session)   We use reg_log throughout.
# =============================================================================

# ---- Q9. Read the output -------------------------------------------------------

summary(reg_log)
# Estimate, standard error, t value and p-value for educ:
# Answer:
# Residual standard error and its degrees of freedom. Why n - 2?
# Answer:


# ---- Q10. Test whether education is related to log wages -----------------------

# (a) State H0 and H1.
# Answer:

# (b) t statistic by hand, and the 5% critical value.
b1   <- coef(summary(reg_log))["educ", "Estimate"]
se1  <- coef(summary(reg_log))["educ", "Std. Error"]
t_0  <- b1 / se1
t_0
qt(0.975, df = df.residual(reg_log))   # two-sided 5% critical value
# Answer:

# (c) p-value by hand.
2 * pt(-abs(t_0), df.residual(reg_log))
# Answer:


# ---- Q11. Confidence interval ---------------------------------------------------

# (a) 95% confidence interval by hand, and from R.
crit <- qt(0.975, df = df.residual(reg_log))
c(lower = b1 - crit * se1, upper = b1 + crit * se1)
confint(reg_log)
# Answer:

# (b) Draw the interval and mark 0, 0.05 and 0.10.
ci <- confint(reg_log)["educ", ]
plot(NA, xlim = c(0, 0.12), ylim = c(0, 1), yaxt = "n",
     xlab = "slope on educ", ylab = "")
segments(ci[1], 0.5, ci[2], 0.5, lwd = 4)
points(coef(reg_log)["educ"], 0.5, pch = 19)
abline(v = c(0, 0.05, 0.10), lty = 2)

# (c) Which values lie inside? What would two-sided 5% tests of each conclude?
# Answer:

# (d) "If we drew many samples and built a 95% confidence interval from each, ..."
# Answer:


# =============================================================================
# PART F: FURTHER PRACTICE  (on your own, not covered in the session)
# =============================================================================

# ---- Q12. Log-log ----------------------------------------------------------------

reg_ll <- lm(lwage ~ log(IQ), data = wage2)
coef(reg_ll)
# Answer:


# ---- Q13. Rebuild the standard error by hand -------------------------------------

n         <- nobs(reg_log)
sigma_hat <- sqrt(sum(resid(reg_log)^2) / (n - 2))   # residual standard error
sigma_hat
SST_x     <- sum((wage2$educ - mean(wage2$educ))^2)
sigma_hat / sqrt(SST_x)                               # standard error of the slope
# Answer:


# ---- Q14. Test other values, and visualise the tests ------------------------------

# (a) t statistics for H0: slope = 0.05 and H0: slope = 0.10.
t_05 <- (b1 - 0.05) / se1
t_10 <- (b1 - 0.10) / se1
c(t_05 = t_05, t_10 = t_10)
# Answer:

# (b) The t distribution under H0, the critical values and the two statistics.
dfree <- df.residual(reg_log)
crit  <- qt(0.975, dfree)
curve(dt(x, dfree), from = -8, to = 8, xlab = "t", ylab = "density")
abline(v = c(-crit, crit), lty = 2)              # 5% rejection cut-offs
abline(v = c(t_05, t_10), lwd = 2, col = c("darkgreen", "firebrick"))
# Answer:

# (c) "We do not reject 0.05, so the slope is exactly 0.05." What is wrong?
# Answer:


# ---- Q15. Repeated samples -----------------------------------------------------------

set.seed(20110)                        # makes the random draws reproducible
R <- 1000
slopes <- numeric(R)                   # an empty vector to store the slopes
for (r in 1:R) {
  rows      <- sample(nrow(wage2), 50) # 50 men, drawn at random
  slopes[r] <- coef(lm(lwage ~ educ, data = wage2[rows, ]))[2]
}
hist(slopes, breaks = 40, main = "", xlab = "slope on educ, one per sample")
abline(v = coef(reg_log)["educ"], lwd = 2)

# (a) Mean and SD of the slopes, against the slope in reg_log.
c(mean = mean(slopes), sd = sd(slopes), full_data = unname(coef(reg_log)["educ"]))
# Answer:

# (b) Repeat with samples of 200 men: change 50 to 200 above and run again.
# Answer:


# ---- Q16. Look at the residuals ---------------------------------------------------

plot(wage2$educ, resid(reg_log), pch = 16, col = rgb(0, 0, 0, 0.3),
     xlab = "Years of education", ylab = "Residual")
abline(h = 0, lty = 2, lwd = 2)
# Answer:


# ---- Q17. Optional, a taste of Week 4: add IQ --------------------------------------

reg_iq <- lm(lwage ~ educ + IQ, data = wage2)
coef(reg_iq)
cor(wage2$educ, wage2$IQ)
# Answer:
