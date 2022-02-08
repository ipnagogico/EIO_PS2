## Preparation

#wd <- setwd("your wd")

rm(list=ls())

# Load packages & install where needed 
packages <- c("dplyr", 
              "ggplot2",
              "haven",
              "stargazer",
              "vtable",
              "optimx",
              "plotly")



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

# Check the time period covered with the data:
range(data$year) #this is the time period we are interested in in the task, no cuts needed 

# Check the investment
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



### task 4: 


## Fixed Effects

# first difference approach:

# Creating the lags and first differences:

reg_data_fe <- reg_data_ols %>% 
  group_by(naics) %>% 
  mutate(lag.y = lag(y, n = 1, default = NA),
         lag.l = lag(l, n = 1, default = NA),
         lag.m = lag(m, n = 1, default = NA),
         lag.k = lag(k, n = 1, default = NA),
         diff.y = y - lag.y,
         diff.l = l - lag.l,
         diff.m = m - lag.m,
         diff.k = k - lag.k) 

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

#we see, the number of observations is exactly the total number minus the count of the first year observations per industry

#(additionally: mean difference approach for comparison: )

means <- data %>% 
  group_by(naics) %>% 
  summarise(y_mean = mean(ln_outputR),
            l_mean = mean(ln_labor), 
            m_mean = mean(ln_matcostR), 
            k_mean = mean(ln_capital))          

reg_data_fe_mean <- left_join(data, means, by= ("naics"))

reg_data_fe_mean <-  reg_data_fe_mean %>% 
  mutate(diff.y_mean = ln_outputR  - y_mean,
         diff.l_mean = ln_labor - l_mean, 
         diff.m_mean = ln_matcostR - m_mean,
         diff.k_mean = ln_capital - k_mean)
#include fixed effects for the firms:
#fixed effect estimation with the mean differences approach is able to solve the selection bias and simultanety problem, which would exist normally 
fe_mean <- lm(diff.y_mean ~ 0 + diff.l_mean + diff.m_mean + diff.k_mean, reg_data_fe_mean)
summary(fe_mean)
summary(fe)

stargazer(fe_mean, type = "html", title = "Fixed Effect Regression (Mean Difference)",  ci=T)


### task 6: 


## Olley & Pakes

#add all polynomials and interactions to the data: 

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

# How many observations do we lose?
op_data %>% 
  filter(i <= 0) # Lost due to negative investment: All of them occur in the years 2007, 2008, 2009

nrow(op_data %>% 
       filter(year == "1997")) # Lost since we have no referential t-1

nrow(op_data %>% 
       filter(year != "1997",
              i > 0)) # First Stage N

nrow(op_data %>% 
       filter(year == "1998")) # Lost in second stage
       
# so in stage 2 we use 5198 observations:

nrow(op_data %>% 
       filter(year != "1997",
              i > 0)) -nrow(op_data %>% 
                              filter(year == "1998"))

nrow(op_data %>% 
       filter(year != "1997",
              year != "1998", 
              i > 0)) # Second Stage N


#first stage: ols linear regression of l, m, and phi:
op1 <- lm(y ~ l + m + 
            k + 
            lag.k + lag.k2 + lag.k3 + #from here on we approximate omega as h with a polynomial of lagged capital and investment plus their interactions
            i + i2 + i3 + 
            lag.ik + lag.i2k + lag.ik2 + 
            t2 + t3 + t4 + t5 + t6 + t7 + t8 + t9 + t10 + t11 + t12, 
          data = op_data %>% filter(!is.na(lag.y),
                                    i > 0))

summary(op1)

coeffs_op1 <- op1$coefficients #save the coefficients of stage 1
#relevant are the labor and material coefficient (rest is either uninteresting (times dummies) or (more seriously) biased because not separable here

#for visualization: creat html chunk: (only display labor and material)
stargazer(op1, type = "html", title = "OP first stage", keep= c("l","m"), ci=T)



#prepare for stage 2 with GMM: 

op_data$y_hat = predict(op1, op_data) # Add predicted output, to rid phi of error term

op_data2 <- op_data %>% 
  mutate(phi_proxy = y_hat - coeffs_op1["l"] * l - coeffs_op1["m"] * m,
         lag.phi_proxy = lag(phi_proxy, n = 1, default = NA)) %>% 
  filter(!is.na(lag.phi_proxy),
         i > 0)

# Stage  2
# We write a function that takes the capital coefficients as input, 
# Output is the dot product of Xi and our instrument, lagged capital, which we want to be zero

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

# Alter the argument of the function optimize_op such that you make the return value g zero
# this is the aim of the uniroot function to get g=0, it searches for alpha k value in the borders lower-upper

op2 <- uniroot(optimize_op, lower = 0, upper = 5, tol = 0.0001) 
coeffs_op2 <- op2$root

#we see, the found coefficient results in a g value close to zero:
optimize_op(coeffs_op2)   
       
#add coefficients to the table:
coeffs_op <- c(coeffs_op1["l"], coeffs_op1["m"], coeffs_op2) 
coeff_matrix[, "OP"] <- round(coeffs_op, round_coeffs)

# Visualization of g function
test_values_op <- seq(-5,5, by = 0.01)
test_result_op <- numeric(length = length(test_values_op))

for (i in seq(test_values_op)) {
  test_result_op[i] <- optimize_op(test_values_op[i])
}

ggplot(tibble(test_values_op, test_result_op), aes(x = test_values_op, y = test_result_op)) + 
  geom_line() + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Capital Coefficient",
       y = "Objective function value")



### Task 7: 

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
            t1 + t2 + t3 + t4 + t5 + t6 + t7 + t8 + t9 + t10 + t11 + t12,
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
#therefor the input is now not only one, but two coefficients (in a vector):

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
  
  g_squares <- g1^2 + g2^2# ensure that we have a minimum where both cost functions are zero
  return(g_squares)
}

# Find a value for both coefficients so that g becomes minimal, i.e. 0. Warning: It takes some time since it tries every method
lp2 <- optimx(par = c(coeffs_lp1["m"], coeffs_lp1["k"]),
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

lp2
# Show method with the minimum target value:
best_method <- lp2[which.min(lp2$value),]
best_method

# Store the coefficients
coeff_matrix[, "LP"] <- round(c(coeffs_lp1["l"], best_method$m, best_method$k), 3)

# Visualization of the objective function
# Create each combination of alpha_m and alpha_k between 0 and 1
test_values_lp <- seq(0, 1, by = 0.01)
test_values_lp1 <- rep(test_values_lp, each = length(test_values_lp))
test_values_lp2 <- rep(test_values_lp, times = length(test_values_lp))
test_values_lp_matrix <- cbind(test_values_lp1, test_values_lp2)
colnames(test_values_lp_matrix) <- c("m", "k")

test_result_lp <- numeric(length = nrow(test_values_lp_matrix))

# Get results for each combination 
# Caution: Can take a while
for (i in seq(test_result_lp)) {
  test_result_lp[i] <- optimize_lp(test_values_lp_matrix[i,])
}

test_data_lp <- tibble(test_values_lp1, test_values_lp2, test_result_lp)

# 3D Visualization of the Objective function
test_result_lp_matrix <- matrix(test_result_lp, 
                             nrow = length(test_values_lp), 
                             byrow = T,
                             dimnames = list(as.character(test_values_lp),
                                             as.character(test_values_lp)))


axx <- list(title = "Capital coefficient")
axy <- list(title = "Material coefficient")
axz <- list(title = "Objective function value")

plot_ly(x = test_values_lp,
        y = test_values_lp,
        z = test_result_lp_matrix,
        type = 'surface') %>% 
  layout(scene = list(xaxis = axx,yaxis = axy, zaxis = axz))


## task 8/9 (result table):
#here, we depict again the estimation results: 

coeff_matrix
