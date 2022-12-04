data {
  int<lower = 0> J;
  vector[J] y;
  real mu0;
  real<lower = 0> tau;
  real nu;
  real lambda;
  vector[J] expend;
  vector[J] ratio;
  vector[J] salary;
  vector[J] takers;
  
 
}


parameters {
  real beta_0;
  real beta_1;
  real beta_2;
  real beta_3;
  real beta_4;
  real<lower = 0> sigma_squared;
}
transformed parameters{
  real<lower = 0> sigma;
  sigma = sqrt(sigma_squared);
}


model {
  beta_0 ~ normal(mu0,tau);
  beta_1 ~ normal(mu0,tau);
  beta_2 ~ normal(mu0,tau);
  beta_3 ~ normal(mu0,tau);
  beta_4 ~ normal(mu0,tau);
  sigma_squared ~ inv_gamma(0.5*nu, 0.5*nu*lambda);
  for(j in 1:J){
    y[j] ~ normal(beta_0 + beta_1*expend[j] + beta_2*ratio[j] + beta_3*salary[j] 
    + beta_4*takers[j], sigma);
  }
  
}


