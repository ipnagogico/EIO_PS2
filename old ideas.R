## After Uniroot:

# Trying around with the gmm() function. It's weird.

gmm_op <- function(alpha_k, x) {
  omega = op_data2$phi_hat - alpha_k * op_data2$k # Calculation of omega
  lag.omega = op_data2$lag.phi_hat - alpha_k * x # Calculation of lagged omega
  
  gmm_df <- tibble(omega, lag.omega) %>% 
    mutate(lag.omega2 = lag.omega * lag.omega,
           lag.omega3 = lag.omega * lag.omega * lag.omega) # regression preparation
  regression <- lm(omega ~ lag.omega + lag.omega2 + lag.omega3, data = gmm_df)
  omega_hat <- predict.lm(regression, gmm_df) # calculation of omega hat (right hand side of equation on sl. 20 in the middle)
  
  xi = omega - omega_hat
  return(xi)
} 

gmm_op <- function(alpha_k, x) {
  omega = op_data2$phi_proxy - alpha_k * op_data2$k
  lag.omega = op_data2$lag.phi_proxy - alpha_k * op_data2$lag.k
  
  gmm_df <- tibble(omega, lag.omega) %>% 
    mutate(lag.omega2 = lag.omega * lag.omega,
           lag.omega3 = lag.omega * lag.omega * lag.omega)
  regression <- lm(omega ~ lag.omega + lag.omega2 + lag.omega3, data = gmm_df)
  omega_hat <- predict.lm(regression, gmm_df)
  
  xi = omega - omega_hat
  return(xi)
}

op2 <- gmm(g = gmm_op, x = op_data2$lag.k, t0=capital_coefficient, type = "iterative")
op2

### After optimx

optimx(par = c(0.23, 0.71),
       fn = optimize_lp,
       method = c('Nelder-Mead', 
                  'BFGS', 
                  'CG', 
                  'L-BFGS-B', 
                  'nlm', 
                  'nlminb', 
                  'spg', 
                  'ucminf', 
                  'newuoa', 
                  'bobyqa', 
                  'nmkb', 
                  'hjkb', 
                  'Rcgmin',
                  'Rvmmin'))

#carina: vlt so ab?ndern: (l?uft noch nicht)

#####start
optimize_lp_c <- function(m,k) {
  omega <- lp2_data$phi_proxy - m * lp2_data$m - k * lp2_data$k
  lag.omega <- lp2_data$lag.phi_proxy - m * lp2_data$lag.m - k * lp2_data$lag.k
  
  gmm_df <- tibble(omega, lag.omega) %>% 
    mutate(lag.omega2 = lag.omega * lag.omega,
           lag.omega3 = lag.omega * lag.omega * lag.omega)
  regression <- lm(omega ~ lag.omega + lag.omega2 + lag.omega3, data = gmm_df)
  omega_hat <- predict.lm(regression, gmm_df) # calculation of omega hat (right hand side of equation on sl. 20 in the middle)
  
  xi <- omega - omega_hat
  g1 <- as.vector(xi %*% lp2_data$lag.k)
  g2 <- as.vector(xi %*% lp2_data$lag.m)
  
  g_abs <- abs(g1) + abs(g2) # ensure that we have a minimum where both cost functions are zero
  return(g_abs)
}

optimx(par = c(coeffs_lp1["m"], coeffs_lp1["k"]),
       fn = optimize_lp_c)
#####ende



# Bootstrap idea:
reps <- 1000
boot_coeffs <- matrix(nrow = nrow(op_data), ncol = 3)

for (i in seq(reps)) {
  # to make it replicable, we use set.seed to have replicable randomness
  set.seed(i)
  # We take a random sample to be used as indices for the bootstrap observations
  boot_sample <- sample(op_data, op_data, replace = T)
  boot_observations <- op_data[boot_sample, ]
  
  # Implement 1st and 2nd Stage of OP algorithm
  
  
  boot_coeffs[i, ] <- coeffs_op
}


#add coefficients to the table:
#coeffs_lp <- c(coeffs_lp["l"], coeffs_lp2["m"], coeffs_lp2["k"]) 
#coeff_matrix[, "LP"] <- round(coeffs_lp, round_coeffs)