

data <- read.csv("forsikringskrav.txt")  # No header since there's only one column

colnames(data) <- c("Kr")

# Extract the data from the data-frame
data_numeric <- data$Kr

# Calculate the mean, sample variance, and population variance
mean_value <- mean(data_numeric)  
variance <- var(data_numeric) 
population_variance <- variance * (length(data_numeric) - 1) / length(data_numeric)  # Population variance

# Print the results
print(paste("Mean:", formatC(mean_value, format = "f", digits = 2)))
print(paste("Sample Variance:", formatC(variance, format = "f", digits = 2)))
print(paste("Population Variance:", formatC(population_variance, format = "f", digits = 2)))

alpha = mean_value^2 / variance
gamma = mean_value / variance
cat("\n")
cat("TASK a)\n\n")
print(paste("alpha moment estimated:", formatC(alpha, format = "f", digits = 2)))
print(paste("gamma moment estimated:", formatC(gamma, format = "f", digits = 2)))

# Log-likelihood function for Gamma distribution
log_likelihood_gamma <- function(data, alpha, gamma) {
  n <- length(data)
  
  # Calculate log-likelihood, Notice the use of lgamma for handling log of the Gamma function
  ll <- n * (alpha * log(gamma) - lgamma(alpha)) + 
        (alpha - 1) * sum(log(data)) - 
        gamma * sum(data)
  
  return(ll)
}

log_likelihood_value <- log_likelihood_gamma(data, alpha, gamma)
cat("\n")
cat("TASK b)\n\n")
print(paste("log_likelihood_value:", formatC(log_likelihood_value, format = "f", digits = 2)))

negloglikgamma <- function(logalpha, x=x) {
  n = length(x)
  alpha = exp(logalpha)
  gamma = alpha / mean(x)
  logL = n * alpha * log(gamma) - n * lgamma(alpha) +
         (alpha - 1) * sum(log(x)) - gamma * sum(x)
  -logL
}

x = data_numeric

# Initial guess is moment-estimator
alpha0 <- alpha 

fit.ml <- optim(log(alpha0), negloglikgamma, x=x, method="BFGS")

optimized_alpha = fit.ml$par
cat("\n")
cat("TASK d)\n\n")
print(paste("optimized alpha:", formatC(optimized_alpha, format = "f", digits = 2)))

negloglikgamma_gamma_const <- function(loggamma, x=x) {
  n = length(x)
  gamma = exp(loggamma)
  alpha = mean(x) * gamma
  logL = n * alpha * log(gamma) - n * lgamma(alpha) +
         (alpha - 1) * sum(log(x)) - gamma * sum(x)
  -logL
}

# Initial guess is moment-estimator
gamma0 <- gamma 

fit.ml_gamma <- optim(log(gamma0), negloglikgamma_gamma_const, x=x, method="BFGS")

optimized_gamma = fit.ml_gamma$par
print(paste("optimized gamma:", formatC(optimized_gamma, format = "f", digits = 2)))

diff_optim_moment_alpha = optimized_alpha - alpha
diff_optim_moment_gamma = optimized_gamma - gamma

print(paste("Difference between optimized gamma and moment estimated gamma:", 
            formatC(optimized_gamma, format = "f", digits = 2), 
            "-", formatC(gamma, format = "f", digits = 2), 
            "=", formatC(diff_optim_moment_gamma, format = "f", digits = 2)))

print(paste("Difference between optimized alpha and moment estimated alpha:", 
            formatC(optimized_alpha, format = "f", digits = 2), 
            "-", formatC(alpha, format = "f", digits = 2), 
            "=", formatC(diff_optim_moment_alpha, format = "f", digits = 2)))



neg_logliklihood_value <- function(x=x, optimized_alpha,optimized_gamma) {
  n = length(x)
  alpha = optimized_alpha
  gamma = exp(optimized_gamma)
  print(paste("gamma: ", gamma)) #Some issues here, need to figure out if i should take exp or not
  logL = n * alpha * log(gamma) - n * lgamma(alpha) +
         (alpha - 1) * sum(log(x)) - gamma * sum(x)
  -logL
}

value_loglikelihood = neg_logliklihood_value

print(paste("Log Likelihood value with optimized parameters: ", value_loglikelihood(x = x, optimized_alpha=optimized_alpha,optimized_gamma=optimized_gamma)))



number_bootstrap_samples <- 500
sample_size = 500


bootstrap_alpha <- numeric(number_bootstrap_samples)
bootstrap_gamma <- numeric(number_bootstrap_samples)



for (i in 1:number_bootstrap_samples) {

  sample = sample(data_numeric, size = sample_size, replace=TRUE)

  fit.ml_bootstrap_sample <- optim(log(alpha0), negloglikgamma, x=sample, method="BFGS")

  fit.ml_gamma_bootstrap_sample <- optim(log(gamma0), negloglikgamma_gamma_const, x=sample, method="BFGS")

  bootstrap_alpha[i] <- fit.ml_bootstrap_sample$par
  bootstrap_gamma[i] <- fit.ml_gamma_bootstrap_sample$par


}

std_alpha = sd(bootstrap_alpha)
std_gamma = sd(bootstrap_gamma)

cat("\n")
cat("TASK e)\n\n")
print(paste("Standard Devation Alpha: ", std_alpha))
print(paste("Standard Devation Gamma: ", std_gamma))

alpha_quantiles <- quantile(bootstrap_alpha, c(0.025, 0.975))
gamma_quantiles <- quantile(bootstrap_gamma, c(0.025, 0.975))

# Print the results with formatting
cat("95% Confidence Interval for Alpha:\n")
cat("Lower Bound:", formatC(alpha_quantiles[1], format = "f", digits = 2), "\n")
cat("Upper Bound:", formatC(alpha_quantiles[2], format = "f", digits = 2), "\n\n")

cat("95% Confidence Interval for Gamma:\n")
cat("Lower Bound:", formatC(gamma_quantiles[1], format = "f", digits = 2), "\n")
cat("Upper Bound:", formatC(gamma_quantiles[2], format = "f", digits = 2), "\n\n")


mu = bootstrap_alpha / bootstrap_gamma

mu_quantiles <- quantile(mu, c(0.025, 0.975))

cat(" TASK f))\n\n")
cat("95% Confidence Interval for mu:\n")
cat("Lower Bound:", formatC(mu_quantiles[1], format = "f", digits = 2), "\n")
cat("Upper Bound:", formatC(mu_quantiles[2], format = "f", digits = 2), "\n\n")