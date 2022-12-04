rm(list = ls())
library(rstan)
library(shinystan)
library(faraway)
#View(sat)
?sat
sat_scores = sat
J<- 50
y <- sat_scores$total
n_all<-length(y)
mu0 <- 0
tau <- 1e6
nu<- 1
lambda<- 20 
expend<- sat_scores$expend
ratio<- sat_scores$ratio
salary<- sat_scores$salary
takers<- sat_scores$takers

pooled_args<- list(J = J, y = y, mu0 = mu0, tau = tau, nu = nu, lambda = lambda,
                   expend = expend, ratio = ratio, salary = salary, takers = takers)
hierarchical_model <- 
  rstan::stan_model(file = "hierarchal1.stan")
fit1 <- 
  rstan::sampling(object = hierarchical_model,
                  data = pooled_args)
?sampling
rstan::summary(fit1)[[1]]
names(fit1)

samples <- 
  rstan::extract(object = fit1,
                 pars = c("beta_0", "beta_1", "beta_2","beta_3","beta_4","sigma"))
names(samples)
#traceplots
class(fit1)
traceplot(fit1)
pairs(fit1,pars = c("beta_0", "beta_1", "beta_2","beta_3","beta_4"))
plot(fit1,pars = c("beta_0", "beta_1", "beta_2","beta_3","beta_4","sigma"))
#extracting posterior samples
beta_0_post<- samples[["beta_0"]]
beta_1_post<- samples[["beta_1"]]
beta_2_post<- samples[["beta_2"]]
beta_3_post<- samples[["beta_3"]]
beta_4_post<- samples[["beta_4"]]
sigma_post<-samples[["sigma"]]
#histograms of samples
par(mfrow=c(2,3))
hist(beta_0_post)
hist(beta_1_post)
hist(beta_2_post)
hist(beta_3_post)
hist(beta_4_post)

hist(sigma_post)

#Visualizing uncertainity and posterior samples
n_samples<- 4000
y_post_pred_samples <- matrix(NA, nrow = n_samples, ncol = n_all)
for(i in 1:n_all){
  y_post_pred_samples[,i] <-
    beta_0_post + 
    beta_1_post * expend[i] +
    beta_2_post * ratio[i] +
    beta_3_post * salary[i] +
    beta_4_post * takers[i] +
    sigma_post * rnorm(n = n_samples, mean = 0, sd = 1)
}
#posterior predictive mean
y_post_pred_mean <- apply(y_post_pred_samples, 
                          MARGIN = 2, FUN = mean)
#SAT score VS Expenditure
x_lim<- range(expend)
y_lim<- range(sat_scores$total)
par(mfrow=c(1,1))
par(mar = c(3,3,2,1), mgp = c(1.8, 0.5, 0))
plot(1, type = "n", xlim = x_lim, ylim = y_lim, 
     main = "SAT scores vs Expend", xlab = "Expenditure", 
     ylab = "SAT score")
points(expend, sat_scores$total, pch = 16)
points(expend, y_post_pred_mean, pch = 17, col = "orange")
legend("bottomright", legend = c("Observed", "Fitted"),
       pch = c(16, 17), col = c("black", "orange"))

#Observed vs fitted
par(mar = c(3,3,2,1), mgp = c(1.8, 0.5, 0))
plot(sat_scores$total, y_post_pred_mean,
     xlim = range(c(sat_scores$total, y_post_pred_mean)),
     ylim = range(c(sat_scores$total, y_post_pred_mean)),
     pch = 16, xlab = "Observed", ylab = "Fitted")
abline(a = 0, b = 1, col = 'red')
cor(y, y_post_pred_mean)

#Posterior Uncertainity visualization
y_post_pred_l95 <- apply(y_post_pred_samples, MARGIN = 2, 
                         FUN = quantile, probs = 0.025)

y_post_pred_u95 <- apply(y_post_pred_samples, MARGIN = 2, 
                         FUN = quantile, probs = 0.975)

in_interval <- ( y >= y_post_pred_l95 & y <= y_post_pred_u95)
order_ix <- order(y)

par(mar = c(3,3,2,1), mgp = c(1.8, 0.5, 0))
plot(1, type = "n", xaxt = "n", xlim = c(0, n_all+1),
     ylim = range(c(y_post_pred_l95, y_post_pred_u95, y)),
     xlab = "", ylab = "Total SAT scores")
for(i in 1:n_all){
  plot_ix <- order_ix[i]
  lines(x = c(i,i),
        y = c(y_post_pred_l95[plot_ix],y_post_pred_u95[plot_ix]))
  points(x = i, y = y_post_pred_mean[plot_ix], pch = 16, cex = 0.5)
  
  points(x = i, y = y[plot_ix], pch = 16, cex = 0.5,
         col = ifelse(in_interval[plot_ix], 'red', 'black'))
}
mean(in_interval)




#Shinystan visualizations
m_hier<-stan(file="hierarchal1.stan",data=pooled_args)
launch_shinystan(m_hier)

########################################
#####Diagnosis and confidence checks
#######################################
tmp_samples <- 
  rstan::extract(object = pooled_fit,
                 pars = c("std_alpha", "std_beta", "std_sigma"))
names(tmp_samples)

post_sigma_samples <- tmp_samples[["std_sigma"]] * y_sd
post_beta_samples <- tmp_samples[["std_beta"]] * y_sd/x_sd
post_alpha_samples <- tmp_samples[["std_alpha"]] * y_sd + 
  y_mean - x_mean * tmp_samples[["std_beta"]] * y_sd/x_sd


hist(post_alpha_samples, breaks = 50)
hist(post_beta_samples, breaks = 50)


######################
#polygon plot
#####################
####################
# One more visualization
N_grid <- 100
expend_gen <- seq(min(expend), max(expend), length = N_grid)
ratio_gen <- seq(min(ratio), max(ratio), length = N_grid)
salary_gen <- seq(min(salary), max(salary), length = N_grid)
takers_gen <- seq(min(takers), max(takers), length = N_grid)


post_pred_grid_samples <- matrix(NA, nrow = n_samples, ncol = N_grid)
post_line_samples <- matrix(NA, nrow = n_samples, ncol = N_grid)
for(i in 1:n_samples){
  post_pred_grid_samples[i,] <- 
    beta_0_post[i] + expend_gen * beta_1_post[i] +
    ratio_gen * beta_2_post[i] +
    salary_gen * beta_3_post[i] +
    takers_gen * beta_4_post[i] +
    sigma_post[i] * rnorm(n = N_grid, mean = 0, sd = 1)
  post_line_samples[i,] <-beta_0_post[i] + expend_gen * beta_1_post[i] +
    ratio_gen * beta_2_post[i] +
    salary_gen * beta_3_post[i] +
    takers_gen * beta_4_post[i] 
}
post_pred_grid_mean <-
  apply(post_pred_grid_samples, MARGIN = 2, 
        FUN = mean)

post_pred_grid_l95 <-
  apply(post_pred_grid_samples, MARGIN = 2, 
        FUN = quantile, probs = 0.025)
post_pred_grid_u95 <-
  apply(post_pred_grid_samples, MARGIN = 2, 
        FUN = quantile, probs = 0.975)

post_line_mean <-
  apply(post_line_samples, MARGIN = 2, 
        FUN = mean)

post_line_l95 <-
  apply(post_line_samples, MARGIN = 2, 
        FUN = quantile, probs = 0.025)
post_line_u95 <-
  apply(post_line_samples, MARGIN = 2, 
        FUN = quantile, probs = 0.975)

y_lim_line <- range(c(post_pred_grid_l95, post_pred_grid_u95, y))
par(mar = c(3,3,2,1), mgp = c(1.8, 0.5, 0))
plot(1, type = "n", xlim = x_lim, 
     ylim = range(c(post_line_l95, post_line_u95, y)), 
     main = "Posterior of regression line", xlab = "Expenditure", 
     ylab = "Total SAT SCORE")
polygon(x = c(expend_gen, rev(expend_gen)),
        y = c(post_line_l95, rev(post_line_u95)),
        col = rgb(0.8, 0.8, 0.8, 0.3), border = NA)
lines(expend_gen, post_line_mean, col = "black", lwd = 2)
# points(expend_gen,post_pred_grid_mean , pch = 16, cex = 0.5,
#        col = ifelse(in_interval, 'black', 'red'))

##########################
par(mar = c(3,3,2,1), mgp = c(1.8, 0.5, 0))
plot(1, type = "n", xlim = x_lim, ylim = y_lim_line, 
     main = "Posterior predictive check", xlab = "Expenditure", 
     ylab = "Total SAT SCORES")
polygon(x = c(expend_gen, rev(expend_gen)),
        y = c(post_pred_grid_l95, rev(post_pred_grid_u95)),
        col = rgb(0.8, 0.8, 0.8, 0.3), border = NA)
lines(expend_gen, post_pred_grid_mean, col = "black", lwd = 2)
points(expend, , pch = 16, cex = 0.5,
       col = ifelse(in_interval, 'black', 'red'))

#################################
#SAT score VS takers
x_lim<- range(takers)
y_lim<- range(sat_scores$total)

par(mar = c(3,3,2,1), mgp = c(1.8, 0.5, 0))
plot(1, type = "n", xlim = x_lim, ylim = y_lim, 
     main = "SAT scores vs Takers", xlab = "No of students appeared", 
     ylab = "SAT score")
points(takers, sat_scores$total, pch = 16)
points(takers, y_post_pred_mean, pch = 17, col = my_colors[2])
legend("bottomright", legend = c("Observed", "Fitted"),
       pch = c(16, 17), col = c("black", my_colors[2]))
#Expenditure vs takers
x_lim<- range(takers)
y_lim<- range(expend)
plot(1, type = "n", xlim = x_lim, ylim = y_lim, 
     main = "expenditure vs Takers", xlab = "No of students appeared", 
     ylab = "Expen")
points(takers, expend, pch = 16)
points(takers, y_post_pred_mean, pch = 17, col = my_colors[2])
legend("bottomright", legend = c("Observed", "Fitted"),
       pch = c(16, 17), col = c("black", my_colors[2]))
