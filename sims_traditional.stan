data {
  int<lower = 1> n; // n. observations
  int y[n]; // binary choice

  real offset;

  // DATA RANDOM INTERCEPTS

  int V1_id[n];
  int<lower=1> V1_N;

  int V2_id[n];
  int<lower=1> V2_N;

  int V3_id[n];
  int<lower=1> V3_N;

  int area_id[n]; // index of areas in the observed data
  int<lower=1> area_N; // no. of areal units

  // AREA-LEVEL COVARIATE

  vector[n] Z;
}

parameters {
  real alpha_star; // baseline rate of choice

  // PARAMETER RANDOM INTERCEPTS

  vector[V1_N] eta_V1;
  real<lower = 0> eta_V1_scale;

  vector[V2_N] eta_V2;
  real<lower = 0> eta_V2_scale;

  vector[V3_N] eta_V3;
  real<lower = 0> eta_V3_scale;

  vector[area_N] gamma;
  real<lower = 0> gamma_scale;

  // AREA-LEVEL FIXED-EFFECT
  real beta;
}

transformed parameters {
  vector[n] mu; // latent propensity for choice j

  // NON-CENTERED PARAMETRISATION

  vector[V1_N] eta_V1_star = eta_V1 * eta_V1_scale;
  vector[V2_N] eta_V2_star = eta_V2 * eta_V2_scale;
  vector[V3_N] eta_V3_star = eta_V3 * eta_V3_scale;
  vector[area_N] gamma_star = gamma * gamma_scale;

  // LINEAR PREDICTOR

  mu =
    offset + 
    alpha_star +
    eta_V1_star[V1_id] + eta_V2_star[V2_id] + eta_V3_star[V3_id] +
    gamma_star[area_id] + beta*Z;
}

model {

    // IID PRIORS

    alpha_star ~ normal(0,10);

    // UNSTRUCTURED RANDOM EFFECTS

    eta_V1 ~ std_normal();
    eta_V1_scale ~ std_normal();

    eta_V2 ~ std_normal();
    eta_V2_scale ~ std_normal();

    eta_V3 ~ std_normal();
    eta_V3_scale ~ std_normal();

    gamma ~ std_normal();
    gamma_scale ~ std_normal();

    // FIXED-EFFECT

    beta ~ std_normal();

    // LIKELIHOOD

    y ~ bernoulli_logit(mu) ;
}
