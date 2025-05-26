library(ggplot2)
library(e1071)
library(gridExtra)
library(tidyr)
# Data Simulation
set.seed(123)
n <- 1000
p <- 50
gen_skewed <- function(n) exp(rnorm(n))   
X <- data.frame(x1 = rbinom(n, 1, 0.5))   
for (j in 2:49) {
  X[[paste0("x", j)]] <-
    if (j %% 3 == 0) gen_skewed(n) else rnorm(n)
}
X$x50 <- rnorm(n)   

# Y1
sigma_y1 <- 1
mu_y1 <- 3 +
  2  * X$x1 -
  0.5 * log(X$x3 + 1) +
  1  * X$x5 +
  1  * X$x6 +
  1.5 * X$x5 * X$x6 -
  1  * X$x10 +
  1  * X$x13 +
  1  * X$x14 +
  2  * X$x13 * X$x14
y1 <- mu_y1 + rnorm(n, 0, sigma_y1)     

# Y2
eta_y2 <- -2 +
  1.2 * X$x1 +
  0.7 * log(X$x15 + 1) -
  1   * X$x20 +
  1   * X$x21 +
  1.1 * X$x20 * X$x21 +
  0.5 * X$x25

p_y2 <- plogis(eta_y2)                   
y2   <- rbinom(n, 1, p_y2)   
simdat <- cbind(X, y1, y2) 
write.csv(simdat, file = "Data.csv", row.names = FALSE)

#Skewness

# Check Skewness of Variables
library(ggplot2)
library(gridExtra)
library(e1071)

# Check Skewness of Variables 
boxplot(simdat[,-c(1, 51,52)])

check_skewness <- function(data, vars = paste0("x", 1:50), threshold = 1) {
  plots <- list()
  skew_tbl <- numeric()
  
  for (var in vars) {
    if (!is.numeric(data[[var]])) next
    skew <- round(skewness(data[[var]], na.rm = TRUE), 2)
    skew_tbl[var] <- skew
  
    p1 <- ggplot(data, aes_string(x = var)) +
      geom_histogram(bins = 30, fill = "steelblue", color = "white") +
      ggtitle(paste("Histogram of", var, "\nSkewness:", skew)) +
      theme_classic()
    
    p2 <- ggplot(data, aes(sample = .data[[var]])) +
      stat_qq() + stat_qq_line(color = "red") +
      ggtitle(paste("QQ Plot of", var)) +
      theme_classic()
    
    plots[[length(plots) + 1]] <- grid.arrange(p1, p2, ncol = 2)
  }
  
  high_skew_vars <- names(skew_tbl[abs(skew_tbl) > threshold])
  print(high_skew_vars)
  
  invisible(plots)
}

check_skewness(simdat)
