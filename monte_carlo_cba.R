# MONTE CARLO METHODOLOGY FOR COST BENEFIT ANALYSIS ----

#> Packages ----

library(readxl)
library(tidyverse)
library(plotly)

#> Data ----

#> Set Seed ----

# Set seed for reproducibility
set.seed(123)

# Create a data frame with 100 rows and 3 columns
data <- data.frame(
  project_id = 1:100,
  low = runif(100, min = 50, max = 100),
  central = runif(100, min = 100, max = 200),
  high = runif(100, min = 200, max = 300)
)

# Add some extreme values for certain projects
extreme_low <- sample(1:100, 5)
data$low[extreme_low] <- rnorm(5, mean = 300, sd = 50)

extreme_central <- sample(1:100, 3)
data$central[extreme_central] <- rnorm(3, mean = 400, sd = 75)

extreme_high <- sample(1:100, 8)
data$high[extreme_high] <- rnorm(8, mean = 500, sd = 100)

# Ensure that the 'low' values are always lower than the 'central' values, and the 'central' values are always lower than the 'high' values
data <- data %>% 
  mutate(low = ifelse(low >= central, central - runif(1, (central - 1)/2, central - 1), low),
         high = ifelse(high <= central, central + runif(1, (high - 1)/2, high - 1), high))

# Check the structure of the data
str(data)



# DISTRIBUTION FUNCTIONS ----

#> Creating functions that create different distributions based on 
#> specified parameters.
#> 
#> Each function creates a sequence of possible costs at the project level
#> based on the user specified "high" and "low" arguments. Depending on the 
#> assumption of distribution, a probability distribution function is then 
#> used to create a vector of probabilities. This is used in the sample() 
#> function to sample from the sequence of possible cost values, with 
#> replacement, using the probability distribution assumed. 
#> 
#> Some distribution assumptions require parameters, which use added
#> arguments.
#> 
#> To find total cost distributions, the function is then applied
#> over the project data set and the totals found. This creates a vector of 
#> possible costs that can then be plotted as a distribution.


# 1) Uniform Distribution ----

#> Project costs are modeled using a uniform distribution spanning low to 
#> high.

uniform_1 <- function(low, high){
  
  # Set of possible costs
  sequence <- seq(from = 0, to = sum(data$high), by = 1)
  
  # Uniform Probability distribution function
  distribution <- dunif(sequence, min = low, max = high)
  
  # Sampling from possible costs using the assumed distribution function
  sample(x = sequence, size = 10000, replace = T, prob = distribution)
  
}

#> Choosing the first project and looking at the distribution

# uniform_1(low = data$low[1], high = data$high[1]) %>%
#   as.data.frame() %>% 
#   ggplot() + 
#   geom_density(aes(x = .), lwd = 1.5) +
#   theme_minimal() +
#   labs(title = "Project Cost Distribution",
#        subtitle = "Uniform",
#        y  = "Likelihood",
#        x  = "Total Cost (£)") +
#   geom_vline(aes(xintercept = sum(data$central[1])), color = "red") +
#   geom_vline(aes(xintercept = sum(data$low[1])), color = "blue") +
#   geom_vline(aes(xintercept = sum(data$high[1])), color = "blue") 


#> Applying the function to the data and finding the sum of each row 
#> gives the total cos across 10000 different simulations.
#> This provides a cost distribution which is then plotted below.

# mapply(uniform_1, data$low, data$high) %>% 
#   rowSums() %>%
#   as.data.frame() %>% 
#   ggplot() + 
#   theme_minimal() +
#   geom_density(aes(x = .), lwd = 1.5) +
#   labs(title = "Cost Distribution",
#        subtitle = "Uniform",
#        y  = "Likelihood",
#        x  = "Total Cost (£)") +
#   geom_vline(aes(xintercept = sum(data$central)), color = "red") +
#   geom_vline(aes(xintercept = sum(data$low)), color = "blue") +
#   geom_vline(aes(xintercept = sum(data$high)), color = "blue") 

#> This provides a normally distributed cost estimate at
#> due to the central limit theorem.
#> 
#> As the only parameters used to model the distribution of project costs
#> were the high and low estimates, this means that the total cost does 
#> not represent any skew caused by the central estimate. 



# 2) Normal Distribution (without central) ----

#> Project costs are modeled using a normal distribution with a mean 
#> defined as the midpoint between high and low, and a standard deviation 
#> that is 1/4 of the distance between high and low.
#> 
#> This means that, if the data is truly normally distributed, then the 
#> low and high estimates represent the 95% confidence interval for an
#> individual project's cost.

normal_2 <- function(low, high){
  
  # Set of possible costs
  sequence <- seq(from = 0, to = sum(data$high), by = 1)
  
  # Mean equal to the midpoint between low and high
  mean_x = (high-low)/2+low
  
  # Standard Deviation equal to 1/4 of the distance between low and high
  sd_x = (high-low)/4
  
  # Normal Probability Distribution Function
  distribution <- dnorm(sequence, mean = mean_x, sd = sd_x)
  
  # Sampling from possible costs using the assumed distribution function
  sample(x = sequence, size = 10000, replace = T, prob = distribution)
  
}

#> Choosing the first project and looking at the distribution

# normal_2(low = data$low[1], high = data$high[1]) %>%
#   as.data.frame() %>% 
#   ggplot() + 
#   geom_density(aes(x = .)) +
#   theme_minimal() +
#   labs(title = "Project Cost Distribution",
#        subtitle = "Normal (without central)",
#        y  = "Likelihood",
#        x  = "Total Cost (£)") +
#   geom_vline(aes(xintercept = sum(data$central[1])), color = "red") 


#> Applying the function to the data and finding the sum of each row 
#> gives the total Track 1 cost across 10000 different simulations.
#> This provides a cost distribution which is then plotted below.

# mapply(normal_2, data$low, data$high) %>% 
#   rowSums() %>%
#   as.data.frame() %>% 
#   ggplot() + 
#   geom_density(aes(x = .)) +
#   ##theme_beis() +
#   labs(title = "Total Cost Distribution",
#        subtitle = "Normal (without central)",
#        y  = "Likelihood",
#        x  = "Total Cost (£)") +
#   geom_vline(aes(xintercept = sum(data$central)), color = "red") +
#   geom_vline(aes(xintercept = sum(data$low)), color = "blue") +
#   geom_vline(aes(xintercept = sum(data$high)), color = "blue") 

#> This provides a normally distributed total cost estimate which is
#> tighter than if sampled from a set of uniformly distributed project
#> level costs.
#> 
#> Again, this does not involved the central cost estimate therefore 
#> gives a normal distribution which is centred around the mid point between 
#> low and high, but with a 90% confidence interval smaller than example 1)



# 3) Normal Distribution (with central) ----

#> As above, except the mean of the normal distribution is assumed to be 
#> the central value.

normal_3 <- function(low, central, high){
  
  # Set of possible costs
  sequence <- seq(from = 0, to = sum(data$high), by = 1)
  
  # Mean equal to the central project cost estimate
  mean_x = central
  
  # Standard Deviation equal to 1/4 of the distance between low and high
  sd_x = (high-low)/4
  
  # Normal Probability Distribution Function
  distribution <- dnorm(sequence, mean = mean_x, sd = sd_x)
  
  # Sampling from possible costs using the assumed distribution function
  sample(x = sequence, size = 10000, replace = T, prob = distribution)
  
}

#> Choosing the first project and looking at the distribution

# normal_3(low = data$low[1], 
#          central = data$central[1], 
#          high = data$high[1]) %>%
#   as.data.frame() %>% 
#   ggplot() + 
#   geom_density(aes(x = .)) +
#   ##theme_beis() +
#   labs(title = "Project Cost Distribution",
#        subtitle = "Normal (with central)",
#        y  = "Likelihood",
#        x  = "Total Cost (£)") +
#   geom_vline(aes(xintercept = sum(data$central[1])), color = "red") +
#   geom_vline(aes(xintercept = sum(data$low[1])), color = "blue") +
#   geom_vline(aes(xintercept = sum(data$high[1])), color = "blue")


#> Applying the function to the data and finding the sum of each row 
#> gives the total Track 1 cost across 10000 different simulations.
#> This provides a cost distribution which is then plotted below.

# mapply(normal_3, data$low, data$central, data$high) %>% 
#   rowSums() %>%
#   as.data.frame() %>% 
#   ggplot() + 
#   geom_density(aes(x = .)) +
#   #theme_beis() +
#   labs(title = "Total Cost Distribution",
#        subtitle = "Normal (with central)",
#        y  = "Likelihood",
#        x  = "Total Cost (£)") +
#   geom_vline(aes(xintercept = sum(data$central)), color = "red") +
#   geom_vline(aes(xintercept = sum(data$low)), color = "blue") +
#   geom_vline(aes(xintercept = sum(data$high)), color = "blue") 


#> This provides a normally distributed Track 1 cost estimate which is 
#> anchored to the central cost estimate. 
#> 
#> Due to the central limit theorem, this is still a symmetric cost 
#> distribution and treats low and high estimates as cost limits.


# 4) Log-normal Distribution ----

#> The Log-Normal distribution allows for a right skew, whilst also being 
#> able to use the same input parameters as a normal distribution.
#> 
#> Using the formula for the Cumulative Density Function of the Log-Normal
#> distribution, we can calculate the mu and sigma parameters necessary to 
#> achieve a distribution where approximately 95% of estimates fall between
#> the low and high cost estimates.
#> 
#> To do this we can specify the relation between our central project cost
#> estimate and any statistic we can fully specify with the relevant formula.
#> 
#> This does, however, require an assumption about what the central estimate
#> represents.
#> 
#> One possible statistic that relates our 3 project cost estimates to the 
#> parameters of the distribution is the mode. If we assume that our central
#> cost estimate represents the most likely outcome, it would be the peak of 
#> the probability distribution and therefore the mode. 
#> 
#> The mode of the Log-Normal distribution function is given by:
#> 
#>    mode = exp(mu - sigma^2) = central
#>    
#> Making mu the subject of the expression gives:
#> 
#>    mu = log(mode) + sigma^2 = log(central) + sigma^2
#>    
#> Therefore we need to find the sigma which gets closest to 95% of our 
#> project cost estimates falling in-between the high cost and low cost
#> estimates.
#> 
#> This can be calculated by the difference between the Log-Normal 
#> Cumulative Distribution Function as evaluated at the High cost 
#> estimate and evaluated at the Low cost estimate.
#> 
#> To illustrate this point we can use the data from the first project.

#> Testing the Log-Normal ----

# First defining an open function
f <- function(sigma){
  
  # The relationship between mode (central), mu and sigma
  mu <- log(data$central[1]) + sigma^2
  
  # The difference between the CDF at high and CDF at low where 95% 
  # of estimates fall
  abs(plnorm(data$high[1], mu, sigma) - plnorm(data$low[1], mu, sigma) - 0.95)
  
}

# Next using optimize to search the interval from lower to upper for a 
# minimum of the function f with respect to the first argument, sigma.
optimize(f, lower = 0, upper = 1)

# Selecting the minimum from the tibble, this is the optimal sigma
sigma_test <- optimize(f, lower = 0, upper = 1)$minimum

# Plugging this back into the formula for the mean 
mu_test <- (log(data$central[1]) + sigma_test^2) 

# Now using these to simulate a distribution
N <- 10000000
nums <- rlnorm(N, mu_test, sigma_test)

# Now testing how many values lie between Low and High
sum(data$low[1] < nums & nums < data$high[1]) / N

# This shows that approximately 95% of the distribution lies between
# the high and low cost estimates !


#> Implementing the Log-Normal ----

log_normal_4 <- function(low, central, high){
  
  # Set of possible costs
  sequence <- seq(from = 0, to = sum(data$high), by = 1)
  
  # Function that defines parameter relationships
  f <- function(sigma) {
    
    # The relationship between central (the mode) and 
    mu <- log(central) + sigma^2
    
    # The difference between the CDF at high and CDF at low
    abs(plnorm(high, mu, sigma) - plnorm(low, mu, sigma) - 0.95)
    
  }
  
  # The optimize function searches the interval from lower to upper
  # for a minimum of the function f with respect to its first argument.
  sigma_x <- optimize(f, lower = 0, upper = 1)$minimum
  
  # Then plugging the minimum sigma back into the definition of the mean
  mu_x <- (log(central) + sigma_x^2) 
  
  # Using the Log-Normal Distribution Function
  distribution <- dlnorm(sequence, meanlog = mu_x, sdlog = sigma_x)
  
  # Sampling from possible costs using the assumed distribution function
  sample(x = sequence, size = 10000, replace = T, prob = distribution)
  
}

#> Choosing the first project and looking at the distribution

# log_normal_4(low = data$low[1],
#              central = data$central[1],
#              high = data$high[1]) %>%
#   as.data.frame() %>%
#   ggplot() +
#   geom_density(aes(x = .), lwd=1.5) +
#   #theme_beis() +
#   labs(title = "Project Cost Distribution",
#        subtitle = "Log-Normal",
#        y  = "Likelihood",
#        x  = "Total Cost (£)") +
#   geom_vline(aes(xintercept = sum(data$central[1])), color = "red") +
#   geom_vline(aes(xintercept = sum(data$low[1])), color = "blue") +
#   geom_vline(aes(xintercept = sum(data$high[1])), color = "blue")


#> Applying the function to the data and finding the sum of each row 
#> gives the total Track 1 cost across 10000 different simulations.
#> This provides a cost distribution which is then plotted below.

# mapply(log_normal_4, data$low, data$central, data$high) %>%
#   rowSums() %>%
#   as.data.frame() %>%
#   ggplot() +
#   geom_density(aes(x = .), lwd=1.5) +
#   #theme_beis() +
#   labs(title = "Track 1 Cost Distribution",
#        subtitle = "Log-Normal",
#        y  = "Likelihood",
#        x  = "Total Cost (£m)") +
#   geom_vline(aes(xintercept = sum(data$central)), color = "red") +
#   geom_vline(aes(xintercept = sum(data$low)), color = "blue") +
#   geom_vline(aes(xintercept = sum(data$high)), color = "blue")


# CREDIBLE INTERVALS ----

#> After calculating a total cost distribution based on the 4 different 
#> cost assumptions, the highest density interval (HDI) can then be calculated.
#> 
#> Unlike equal-tailed intervals that typically exclude 2.5% from each tail
#> of the distribution and always include the median, the HDI is not 
#> equal-tailed and therefore always includes the mode of the distribution.
#> 
#> All points within this interval have a higher probability density than
#> points outside of the interval. The HDI can be used in the context of 
#> uncertainty characterisation of a posterior distribution. 
#> 
#> The current analytical consensus is that the 95% or 89% credible intervals
#> (CI) are two reasonable ranges to characterise the uncertainty associated 
#> with the estimation.

# # Bayes Tests
# library(bayestestR)
# 
# #> Uniform ----
# 
# mapply(uniform_1, data$low, data$high) %>%
#   rowSums() %>%
#   hdi(ci = c(0.95, 0.89))
# 
# #> Normal (without central) ----
# 
# mapply(normal_2, data$low, data$high) %>%
#   rowSums() %>%
#   hdi(ci = c(0.95, 0.89))
# 
# #> Normal (with central) ----
# 
# mapply(normal_3, data$low, data$central, data$high) %>%
#   rowSums() %>%
#   hdi(ci = c(0.95, 0.89))
# 
# #> Log-Normal ----  
# 
# mapply(log_normal_4, data$low, data$central, data$high) %>%
#   rowSums() %>%
#   hdi(ci = c(0.95, 0.89))



# PLOTTING ----

# # Creating aggregated data set  
# 
# # 1) Uniform
# dist_dat_1 <- data.frame(Distribution = "Uniform",
#                          Cost = mapply(uniform_1,
#                                        data$low,
#                                        data$high) %>%
#                            rowSums())
# 
# # 2) Normal (without central)
# dist_dat_2 <- data.frame(Distribution = "Normal (without central)",
#                          Cost = mapply(normal_2,
#                                        data$low,
#                                        data$high) %>%
#                            rowSums())
# 
# # 3) Normal (with central)
# dist_dat_3 <- data.frame(Distribution = "Normal (with central)",
#                          Cost = mapply(normal_3,
#                                        data$low,
#                                        data$central,
#                                        data$high) %>%
#                            rowSums())
# 
# # 3) Log-Normal
# dist_dat_4 <- data.frame(Distribution = "Log-Normal",
#                          Cost = mapply(log_normal_4,
#                                        data$low,
#                                        data$central,
#                                        data$high) %>%
#                            rowSums())
# 
# # Joining
# dist_dat <- rbind(dist_dat_1, dist_dat_2, dist_dat_3, dist_dat_4)
# 
# 
# # Plotting aggregated cost distributions by project level cost distribution
# # assumption.
# 
# dist_dat %>%
#   #filter(Distribution %in% c("Uniform", "Log-Normal")) %>%
#   ggplot() +
#   geom_density(aes(x = Cost, color = Distribution), lwd=1.5) +
#   geom_vline(aes(xintercept = sum(data$central)), color = "red") +
#   geom_vline(aes(xintercept = sum(data$low)), color = "blue") +
#   geom_vline(aes(xintercept = sum(data$high)), color = "blue") +
#   theme_minimal() +
#   labs(title = "Total Cost Distribution",
#        y  = "Likelihood",
#        x  = "Total Cost")
# 
# 
# # As seen below, the data is skewed to the right
# sum(data$low)
# sum(data$central)
# sum(data$high)


