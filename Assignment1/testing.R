

data <- read.csv("forsikringskrav.txt")  # No header since there's only one column

colnames(data) <- c("Kr")

# Extract the data from the data-frame
data_numeric <- data$Kr

# Calculate the mean, sample variance, and population variance
mean_value <- mean(data_numeric)  
variance <- var(data_numeric) 
population_variance <- variance * (length(data_numeric) - 1) / length(data_numeric)  # Population variance

# Print the results
print(paste("Mean:", mean_value))
print(paste("Sample Variance:", variance))
print(paste("Population Variance:", population_variance))

alpha = mean_value ** 2 / variance
gamma = mean_value / variance

print(paste("alpha: ", alpha))
print(paste("gamma: ", gamma))

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
print(paste("log_likelihood_value: ", log_likelihood_value))