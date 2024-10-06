data <- read.csv("antocyaner.txt")  # No header since there's only one column

colnames(data) <- c("mg_per_100")

# Extract the data from the data-frame
data_numeric <- data$mg_per_100


# t-test 
result <- t.test(data_numeric, conf.level = 0.95)


lower_bound <- round(result$conf.int[1], 2)
upper_bound <- round(result$conf.int[2], 2)
mean_value <- round(result$estimate, 2)



print("a)")
#cat("Mean:", mean_value, "\n")
cat("95% Confidence Interval: [", lower_bound, ",", upper_bound, "]\n\n")


print("b)")

# Generate 10,000 datasets, each with 15 random samples
datasets <- matrix(rnorm(15 * 10000, mean = 558, sd = 30), nrow = 10000, ncol = 15)



t_test <- apply(datasets, 1, function(x) t.test(x))

t_test_conf_int <- sapply(t_test, function(x) x$conf.int)
t_test_conf_int <- t(t_test_conf_int) #Lower and upper bounds as columns


expected_value <- 558

is_within_confi_interval <- (expected_value >= t_test_conf_int[, 1]) & (expected_value <= t_test_conf_int[, 2])



number_of_times_expected_value_within_confid_intervall = sum(is_within_confi_interval, na.rm = TRUE)

print(paste("Number of times the expected value", expected_value, "is within Confidence Intervall:"))
print(number_of_times_expected_value_within_confid_intervall)
cat("\n\n")

cat("c)")

sample_means = apply(datasets, 1, function(x) mean(x))

standard_devations = apply(datasets, 1, function(x) sd(x))

lower_bound_Confid_large_intervals = sample_means - 1.96 * standard_devations / (sqrt(15))
upper_bound_Confid_large_intervals = sample_means + 1.96 * standard_devations / (sqrt(15))

Confidence_interval_matrix = cbind(lower_bound_Confid_large_intervals, upper_bound_Confid_large_intervals)


is_within_confi_interval_large <- (expected_value >= lower_bound_Confid_large_intervals & (expected_value <= upper_bound_Confid_large_intervals))



number_of_times_expected_value_within_confid_intervall_large = sum(is_within_confi_interval_large, na.rm = TRUE)

print(paste("Number of times the expected value", expected_value, "is within approximated interval for large samples:"))
print(number_of_times_expected_value_within_confid_intervall_large)
cat("\n\n")

cat("The expected value was within the standard confidence interval", 
    number_of_times_expected_value_within_confid_intervall, "times,\n")
cat("while it was within the larger  confidence interval sample", 
    number_of_times_expected_value_within_confid_intervall_large, "times.\n\n")


cat("d)")

n = 15 #Population size, ie 15 elements in each row of datasets

alpha = 0.05 #Confid -> 95%

chi_sq_lower <- qchisq(alpha / 2, df = n - 1)
chi_sq_upper <- qchisq(1 - alpha / 2, df = n - 1)

expected_value_var = 30


confid_intervals_var_lower <- apply(datasets, 1, function(x) {((n - 1) * var(x)**2) / chi_sq_lower})  # Corrected formula for lower bound
confid_intervals_var_upper <- apply(datasets, 1, function(x) {((n - 1) * var(x)**2 )/ chi_sq_upper})

is_within_confi_interval_var <- (expected_value_var >= confid_intervals_var_lower & (expected_value_var <= confid_intervals_var_upper))



number_of_times_expected_value_within_confid_intervall_var = sum(is_within_confi_interval_var, na.rm = TRUE)

print(paste("Number of times the expected variance", expected_value_var, "is within variance confidence interval:"))
print(number_of_times_expected_value_within_confid_intervall_var)
cat("\n\n")

cat("e)")
z_matrix = matrix(rt(15*10000, 7), nrow=10000, ncol = 15)

mu_e = 558
var_e = 30

x_matrix = mu_e + var_e * z_matrix


t_test_e <- apply(x_matrix, 1, function(x) t.test(x))

t_test_conf_int_e <- sapply(t_test_e, function(x) x$conf.int)
t_test_conf_int_e <- t(t_test_conf_int_e) #Lower and upper bounds as columns


is_within_confi_interval_e <- (expected_value >= t_test_conf_int_e[, 1]) & (expected_value <= t_test_conf_int_e[, 2])



number_of_times_expected_value_within_confid_intervall_e = sum(is_within_confi_interval_e, na.rm = TRUE)

print(paste("Number of times the expected value", expected_value, "is within Confidence Interval when distrubtion has 8 less dof:"))
print(number_of_times_expected_value_within_confid_intervall_e)
cat("\n\n")


cat("Number of times expexted value was within the Confidence Interval: \n")
cat("Unrestricted DOF: ", number_of_times_expected_value_within_confid_intervall, "\n")
cat("Restricted DOF: ", number_of_times_expected_value_within_confid_intervall_e,"\n\n")

cat("f)")

n = 15 #Population size, ie 15 elements in each row of datasets (still 15 elements in each row)

alpha = 0.05 #Confid -> 95%

chi_sq_lower <- qchisq(alpha / 2, df = n - 1) #DOF is still 14. altough the DOF has been limited to 7 ?
chi_sq_upper <- qchisq(1 - alpha / 2, df = n - 1)

expected_value_var = 30

confid_intervals_var_lower_f <- apply(x_matrix, 1, function(x) {((n - 1) * 1.4*var(x)**2) / chi_sq_lower})  # Corrected formula for lower bound
confid_intervals_var_upper_f <- apply(x_matrix, 1, function(x) {((n - 1) * 1.4* var(x)**2 )/ chi_sq_upper})

is_within_confi_interval_var_f <- (expected_value_var >= confid_intervals_var_lower_f & (expected_value_var <= confid_intervals_var_upper_f))



cat("Number of times the expected value was inside the confidence interval :")
number_of_times_expected_value_within_confid_intervall_var_f = sum(is_within_confi_interval_var_f, na.rm = TRUE)

print(number_of_times_expected_value_within_confid_intervall_var_f)

