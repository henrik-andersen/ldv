# Set seed 
set.seed(45678)

# Load packages 
library(lavaan)
library(dplyr)


# Generate data -----------------------------------------------------------

# Set large sample size 
n <- 1000L

rho   = 0.3 # Autoregressive effect
gamma = 0.6 # Effect U -> D
delta = 0.5 # Effect U -> Y6
beta  = 0.4 # Causal effect of interest

# Time-invariant unobserved heterogeneity 
V = rnorm(n, 0, 1)

# Simulate initial realization of outcome
Y0 = 1 * V + rnorm(n, 0, 1)

# Time-varying confounder 
U = rnorm(n, 0, 1)

# Causal variable 
D = gamma * U + rnorm(n, 0, 1)

# Remaining realizations of outcome 
Y1 = delta * U + rho * Y0 + 1 * V + rnorm(n, 0, 1)
Y2 = beta  * D + rho * Y1 + 1 * V + rnorm(n, 0, 1)

# Put into dataframe
df = data.frame(Y0, Y1, Y2, 
                D, V, U)


# Model 1: All observed variables -----------------------------------------

m1 = "
  Y2 ~ beta*D + rho*Y1 + V 
  Y1 ~ delta*U + V
  D  ~ gamma*U 
"
m1.fit = sem(model = m1, data = df, estimator = "ML") %>%
  summary()


# Model 2: Ignoring time-invariant unob. hetero.  -------------------------

m2 = "
  Y2 ~ beta*D + rho*Y1 
  Y1 ~ delta*U 
  D  ~ gamma*U 
"
m2.fit = sem(model = m2, data = df, estimator = "ML") %>%
  summary()


# Model 3: Time-invariant unob. hetero. as latent variable ----------------

m3 = "
  # Individual effects to account for V
  alpha =~ 1*Y1 + 1*Y2
  # Regressions 
  Y2 ~ beta*D + rho*Y1
  Y1 ~ rho*Y0
  # Allow initial outcome to correlate with unit effects
  alpha ~~ Y0
  # Account for U, common cause of Y6 and D 
  D ~~ Y1
"
m3.fit = sem(model = m3, data = df, estimator = "ML") %>%
  summary()


# Repeated sampling -------------------------------------------------------

sim_func = function(rho = 0.3, beta = 0.4, gamma = 0.6, delta = 0.5) {
  
  # Set large sample size
  n = 1000L
  
  # Time-invariant unobserved heterogeneity 
  V = rnorm(n, 0, 1)
  
  # Simulate initial realization of outcome
  Y0 = 1 * V + rnorm(n, 0, 1)
  
  # Time-varying confounder 
  U = rnorm(n, 0, 1)
  
  # Causal variable 
  D = gamma * U + rnorm(n, 0, 1)
  
  # Remaining realizations of outcome 
  Y1 = delta * U + rho * Y0 + 1 * V + rnorm(n, 0, 1)
  Y2 = beta  * D + rho * Y1 + 1 * V + rnorm(n, 0, 1)
  
  # Put into dataframe
  df = data.frame(Y0, Y1, Y2, 
                  D, V, U)
  
  # Fit the model 
  mx = "
    # Individual effects to account for V
    alpha =~ 1*Y1 + 1*Y2
    # Regressions 
    Y2 ~ beta*D + rho*Y1
    Y1 ~ rho*Y0
    # Allow initial outcome to correlate with unit effects
    alpha ~~ Y0
    # Account for U, common cause of Y6 and D 
    D ~~ Y1
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

res = replicate(n = 10000L, expr = sim_func())