data {
int<lower=0> K;
int<lower=0> N_obs;
int<lower=0> N_mis;
real y_obs[N_obs];
matrix[N_obs,K] x_obs;
matrix[N_mis,K] x_mis;
}
parameters {
vector[K] beta;
real mu;
real<lower=0> sigma;
real y_mis[N_mis];
}
model {
y_obs ~ normal(mu + x_obs*beta, sigma);
y_mis ~ normal(mu + x_mis*beta, sigma);
}
