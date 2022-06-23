
# Readers will notice the distribution is somewhat skewed and has a large 
# variance. Even if the estimated coefficient is unbiased, the statistical 
# properties are improved if the underlying data has had more time to reach 
# equilibrium. In other words, the way we simulated the data above means the 
# variances of the outcome over time, as well as the covariances of adjacent 
# columns of the outcome are changing rapidly near the beginning of the process 
# [@andersen2021]. If we first allow the process to reach equilibrium 
# (variances, covariances, as well as means are fairly constant over time), then
# the sampling distribution becomes more normal, and the variance is reduced. We
# can show this by first simulating a 'spin-up' phase for the outcome, and then 
# focusing in on the causal model at a later point in time.

sim_func2 = function(rho = 0.3, beta = 0.4, gamma = 0.6, delta = 0.5) {
  
  # Set large sample size
  n <- 1000L
  
  # Time-invariant unobserved heterogeneity
  V = rnorm(n, 0, 1)
  
  # Simulate spin-up phase to allow Yt to reach equilibrium
  Y0 = 1 * V + rnorm(n, 0, 1)
  Y1 = rho * Y0 + 1 * V + rnorm(n, 0, 1)
  Y2 = rho * Y1 + 1 * V + rnorm(n, 0, 1)
  Y3 = rho * Y2 + 1 * V + rnorm(n, 0, 1)
  Y4 = rho * Y3 + 1 * V + rnorm(n, 0, 1)
  Y5 = rho * Y4 + 1 * V + rnorm(n, 0, 1)
  
  # Time-varying confounder
  U = rnorm(n, 0, 1)
  
  # Causal variable
  D = gamma * U + rnorm(n, 0, 1)
  
  # Focus on effect D -> Y7, holding Y6 constant
  Y6 = delta * U + rho * Y5 + 1 * V + rnorm(n, 0, 1)
  Y7 = beta * D + rho * Y6 + 1 * V + rnorm(n, 0, 1)
  
  # Put into dataframe
  df = data.frame(Y0, Y1, Y2, Y3, Y4, Y5, Y6, Y7, D, V, U)
  
  # Fit the model
  mx = "
    # Individual effects to account for V
    alpha =~ 1*Y6 + 1*Y7
    # Regressions
    Y7 ~ beta*D + rho*Y6
    Y6 ~ rho*Y5
    # Allow initial outcome to correlate with unit effects
    alpha ~~ Y5
    # Account for U, common cause of Y6 and D
    D ~~ Y6
    "
  mx.fit = sem(model = mx, data = df, estimator = "ML")
  
  # Get estimate of beta
  est = lavInspect(mx.fit, "list") %>%
    filter(op == "~") %>%
    filter(label == "beta") %>%
    select(est) %>%
    as.numeric()
  
  # Return estimate of beta
  return(est)
}

res2 = replicate(n = 5000L, expr = sim_func2())

# The estimate of the causal effect is still unbiased: 

mean(res2); median(res2)
sd(res2)

# But now the sampling distribution better approximates the normal distribution 
# and the spread of the estimated coefficients is tighter:

hist(res2, main = NULL, xlab = NULL, breaks = 30)