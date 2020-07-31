data {
  int<lower=0> n;
  real mu;
  real<lower=0> s2;
}

parameters {
  real logit_p;        // vector of logit_p
}

model {
  target += normal_lpdf(logit_p | mu, sqrt(s2));
  target += binomial_lpmf(0 | n, inv_logit(logit_p));
}
