## Preparation

#wd <- setwd("your wd")

rm(list=ls())

# Load packages & install where needed 
packages <- c("dplyr", 
              "ggplot2",
              "haven",
              "stargazer",
              "vtable",
              "gmm",
              "optimx") 



lapply(packages[!(packages %in% installed.packages())], install.packages)
lapply(packages, library, character.only = TRUE)

## Data Loading

# For a look at the original dataset and the variables
naics <- read_dta("naics5809.dta")
lapply(naics, function(x) attributes(x)$label)

# Provided dataset
data <- read_dta("AssignmentProd.dta")
lapply(data, function(x) attributes(x)$label)


## Descriptive Statistics


variables <- data %>% 
  select(ln_labor, ln_capital, ln_matcostR, ln_outputR)

# Visual check of the distribution of the variables
for (var in colnames(variables)) {
  print(ggplot(data = variables, aes_string(x = var)) + geom_density())
}

# time period covered with the data:
range(data$year) #this is the time period we are interested in in the task, no cuts needed 

# ln investment: 

exp(min(data$ln_investR)) #minimum expenditure is still >0
#ln invest was total expenditures (new and used) from 1997 on, but in the dataset here it is adjusted with a constructed fraction new expenditures to reflect investments 

#industries & observations in the dataset:
length(unique(data$naics))
length(unique(data$year))
nrow(data) #473 unique industries over 13 years yield 6149 observations in total 


### task 2: 

# Create summary table
sumtable(variables,
         summ=c('notNA(x)',
                'mean(x)',
                'sd(x)',
                'min(x)',
                'median(x)',
                'max(x)'),
         summ.names = c('N',
                        'Mean',
                        'Std. Dev',
                        'Min',
                        'Median',
                        'Max'),
         labels = c('log(Labor)',
                    'log(Capital)',
                    'log(Material Cost)',
                    'log(Output)'),
         out = 'viewer')



### task 3: 


## OLS Regression
reg_data_ols <- data %>% 
  rename(vad = ln_vaddR,
         y = ln_outputR,
         l = ln_labor,
         k = ln_capital,
         m = ln_matcostR,
         i = ln_investR)

ols <- lm(y ~ l + m + k, reg_data_ols)
summary(ols)

coeffs_ols <- ols$coefficients[2:4]

# We create a Matrix of coefficients to store our results in
coeff_matrix <- matrix(data = NA, nrow = 3, ncol = 4)
rownames(coeff_matrix) <- c("Labor", "Material", "Capital")
colnames(coeff_matrix) <- c("OLS", "FE", "OP", "LP")
round_coeffs <- 3

coeff_matrix[,"OLS"] <- round(coeffs_ols, round_coeffs)

#get the 95% confidence intervalls
labor_ci_ols <- round(confint(ols, 'l', level=0.95),3)
material_ci_ols <- round(confint(ols, 'm', level=0.95),3)
capital_ci_ols <- round(confint(ols, 'k', level=0.95),3)

#for visualization: creat html chunk: 
stargazer(ols, type = "html", title = "OLS Regression", ci = T)



###task 4: 


## Fixed Effects

# first difference approach:

# Creating the lags and first differences:

reg_data_fe <- reg_data_ols %>% 
  group_by(naics) %>% 
  mutate(lag.y = lag(y, n = 1, default = NA),
         lag.l = lag(l, n = 1, default = NA),
         lag.k = lag(k, n = 1, default = NA),
         lag.m = lag(m, n = 1, default = NA),
         diff.y = y - lag.y,
         diff.l = l - lag.l,
         diff.k = k - lag.k,
         diff.m = m - lag.m) 

fe <- lm(diff.y ~ 0 + diff.l + diff.m + diff.k, 
         data = reg_data_fe %>% filter(!is.na(lag.y)))  # for the first year, we don't have a referential previous period. These get filtered out
summary(fe)

coeffs_fe <- fe$coefficients
coeff_matrix[,"FE"] <- round(coeffs_fe, round_coeffs)

# get confidence intervalls
labor_ci_fe <- round(confint(fe, 'diff.l', level=0.95),3)
material_ci_fe <- round(confint(fe, 'diff.m', level=0.95),3)
capital_ci_fe <- round(confint(fe, 'diff.k', level=0.95),3)

#for visualization: creat html chunk: 
stargazer(fe, type = "html", title = "Fixed Effect Regression (First Difference)",  ci = T)

#we see, the number of observations is exactly the total number minus the count of the first observatiosn per industry




#(additionally: mean difference approach for comparison: )

means <-data%>% group_by(naics) %>% summarise(labor_mean = mean(ln_labor), capital_mean = mean(ln_capital), material_mean = mean(ln_matcostR), output_mean = mean(ln_outputR))
reg_data_fe_mean <- left_join(data, means, by= "naics")
reg_data_fe_mean <-  reg_data_fe_mean %>% mutate(y_dif = ln_outputR- output_mean,l_dif = ln_labor -labor_mean, capital_dif = ln_capital-capital_mean , mat_dif = ln_matcostR-material_mean )
#include fixed effects for the firms:
#fixed effect estimation with the mean differences approach is able to solve the selection bias and simultanety problem, which would exist normally 
fe_mean <- lm(y_dif ~ l_dif + mat_dif + capital_dif + 0, reg_data_fe_mean)
summary(fe_mean)
summary(fe)
stargazer(fe_mean, type = "html", title= "Fixed Effect Regression (Mean Difference)",  ci=T)


###task 6: 


## Olley & Pakes

#add all polynoms and interactions to the data: 

op_data <- reg_data_fe  %>% 
  mutate(k2 = k*k,
         k3 = k*k*k,
         i2 = i*i,
         i3 = i*i*i,
         ik = i*k,
         i2k = i*i*k,
         ik2 = i*k*k,
         lag.k2 = lag.k*lag.k,
         lag.k3 = lag.k*lag.k*lag.k,
         lag.ik = i*lag.k,
         lag.i2k = i*i*lag.k,
         lag.ik2 = i*lag.k*lag.k)

#first stage: ols linear regression of l, m, k and w proxy:

op1 <- lm(y ~ l + m + 
            k + 
            lag.k + lag.k2 + lag.k3 + #from here on we approximate w (productivity) with a polynom of lagged capital and investment plus their interactions
            i + i2 + i3 + 
            lag.ik + lag.i2k + lag.ik2 + 
            t1 + t2 + t3 + t4 + t5 + t6 + t7 + t8 + t9 + t10 + t11 + t12, 
          data = op_data %>% filter(!is.na(lag.y),
                                    i > 0))

summary(op1)

coeffs_op1 <- op1$coefficients #save the coefficients of stage 1
#relevant are the labor and material coefficient (rest is eather uninteresting (times dummies) or (more seriously) biased because not separable here

#for visualization: creat html chunk: (only display labor and material)
stargazer(op1, type = "html", title= "OP first stage", keep= c("l","m"), ci=T)



#prepare for stage 2 with GMM: 

op_data$y_hat = predict(op1, op_data) # Add predicted output, to rid phi of error term

op_data2 <- op_data %>% 
  mutate(phi_proxy = y_hat - coeffs_op1["l"] * l - coeffs_op1["m"] * m,
         lag.phi_proxy = lag(phi_proxy, n = 1, default = NA)) %>% 
  filter(!is.na(lag.phi_proxy),
         i > 0)

## Test for GMM starts here

# self-done gmm function: takes the desired alpha k as input, output is the difference between omega and omega head = g

optimize_op <- function(alpha_k) {
  omega = op_data2$phi_proxy - alpha_k * op_data2$k
  lag.omega = op_data2$lag.phi_proxy - alpha_k * op_data2$lag.k
  
  gmm_df <- tibble(omega, lag.omega) %>% 
    mutate(lag.omega2 = lag.omega * lag.omega,
           lag.omega3 = lag.omega * lag.omega * lag.omega)
  regression <- lm(omega ~ lag.omega + lag.omega2 + lag.omega3, data = gmm_df)
  omega_hat <- predict.lm(regression, gmm_df)
  
  xi = omega - omega_hat
  
  g = as.vector(xi %*% op_data2$lag.k)
  return(g)
}

# Alter the argument (alpha_k) of the function optimize_op such that you make the return value g zero
# this is the aim of the uniroot function to get g=0, it searches for alpha k value in the borders lower-upper

op2 <- uniroot(optimize_op, lower = 0, upper = 5, tol = 0.0001) 
coeffs_op2 <- op2$root

#add coefficients to the table:
coeffs_op <- c(coeffs_op1["l"], coeffs_op1["m"], coeffs_op2) 
coeff_matrix[, "OP"] <- round(coeffs_op, round_coeffs)

# Visualization of g function
test_values <- seq(-5,5, by = 0.01)
test_result <- numeric(length = length(test_values))

for (i in seq(test_values)) {
  test_result[i] <- optimize_op(test_values[i])
}

ggplot(tibble(test_values, test_result), aes(x = test_values, y = test_result)) + 
  geom_line() + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")





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



###task 7: 


## Levinson & Petrin
lp_data <- reg_data_fe %>% 
  mutate(k2 = k*k,
         k3 = k*k*k,
         m2 = m*m,
         m3 = m*m*m,
         mk = m*k,
         m2k = m*m*k,
         mk2 = m*k*k,
         lag.k2 = lag.k*lag.k,
         lag.k3 = lag.k*lag.k*lag.k,
         lag.ik = i*lag.k,
         lag.i2k = i*i*lag.k,
         lag.ik2 = i*lag.k*lag.k)

##first stage: 

lp1 <- lm(y ~ l +
            k + m +
            k2 + k3 + m2 + m3 + mk + m2k + mk2 +
            t1 + t2 + t3 + t4 + t5 + t6 + t7 + t8 + t9 + t10 + t11 + t12 + t13,
          data = lp_data)
summary(lp1)

#save coefficients: 

coeffs_lp1 <- lp1$coefficients


#prepare for second stage:

lp_data$y_hat = predict(lp1, lp_data) #get fitted values on the basis of model from first stage


lp2_data <- lp_data %>%  #here we calculate the phi proxy, which is the differnce of y hat and (the only correctly identified) regressor for labor
  mutate(phi_proxy = y_hat - coeffs_lp1["l"] * l, #furthermore, we lag phi proxy
         lag.phi_proxy = lag(phi_proxy, n = 1, default = NA)) %>% 
  filter(!is.na(lag.phi_proxy))


#as in op, we define the function of the coefficients which are to be chosen optimal to achieve a g function result of zero
#therefor the input is now not only one, but two coefficients:

optimize_lp <- function(coeff) {
  omega <- lp2_data$phi_proxy - coeff[1] * lp2_data$m - coeff[2] * lp2_data$k
  lag.omega <- lp2_data$lag.phi_proxy - coeff[1] * lp2_data$lag.m - coeff[2] * lp2_data$lag.k
  
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
       fn = optimize_lp)

# Does not return a minimum of value 0, which is what we would want
#TODO: Try different methods or alter function


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



## task 8/9 (result table):

#here, we depict again the estimation results: 

coeff_matrix

