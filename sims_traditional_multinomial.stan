data {
  int<lower = 1> n; // n. observations
  int<lower = 1> J; 
  int y[n]; // categorical choice

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


transformed data {
  vector[V1_N] eta_V1_zeros = rep_vector(0, V1_N);
  vector[V2_N] eta_V2_zeros = rep_vector(0, V2_N);
  vector[V3_N] eta_V3_zeros = rep_vector(0, V3_N);
  vector[area_N] gamma_zeros = rep_vector(0, area_N);
  real beta_zeros = 0;
  real alpha_zeros = 0;
}

parameters {
  vector[J-1] alpha_tilde; // baseline rate of choice

  // PARAMETER RANDOM INTERCEPTS

  matrix[V1_N,J-1] eta_V1;
  vector<lower = 0>[J-1] eta_V1_scale;

  matrix[V2_N,J-1] eta_V2;
  vector<lower = 0>[J-1] eta_V2_scale;

  matrix[V3_N,J-1] eta_V3;
  vector<lower = 0>[J-1] eta_V3_scale;

  matrix[area_N,J-1] gamma;
  vector<lower = 0>[J-1] gamma_scale;

  // AREA-LEVEL FIXED-EFFECT
  vector[J-1] beta_tilde;
}


transformed parameters {
  matrix[n,J] mu; // latent propensity for choice j

  // NON-CENTERED PARAMETRISATION

  matrix[V1_N,J-1] eta_V1_tilde;
  matrix[V2_N,J-1] eta_V2_tilde;
  matrix[V3_N,J-1] eta_V3_tilde;
  matrix[area_N,J-1] gamma_tilde;

  matrix[V1_N,J] eta_V1_star;
  matrix[V2_N,J] eta_V2_star;
  matrix[V3_N,J] eta_V3_star;
  matrix[area_N,J] gamma_star;
  vector[J] alpha_star;
  vector[J] beta;


for(j in 1:(J-1)){

  eta_V1_tilde[,j] = eta_V1[,j] * eta_V1_scale[j];
  eta_V2_tilde[,j] = eta_V2[,j] * eta_V2_scale[j];
  eta_V3_tilde[,j] = eta_V3[,j] * eta_V3_scale[j];
  gamma_tilde[,j] = gamma[,j] * gamma_scale[j];

}

  eta_V1_star = append_col(eta_V1_tilde, eta_V1_zeros);
  eta_V2_star = append_col(eta_V2_tilde, eta_V2_zeros);
  eta_V3_star = append_col(eta_V3_tilde, eta_V3_zeros);
  gamma_star = append_col(gamma_tilde, gamma_zeros);
  beta = append_row(beta_tilde, beta_zeros);
  alpha_star = append_row(alpha_tilde, alpha_zeros);



  // LINEAR PREDICTOR
for(j in 1:J){

  mu[,j] =
    alpha_star[j] +
    eta_V1_star[V1_id,j] + eta_V2_star[V2_id,j] + eta_V3_star[V3_id,j] +
    gamma_star[area_id,j] + beta[j]*Z;

}

}

model {

    // IID PRIORS

    alpha_tilde ~ normal(0,1);

    // UNSTRUCTURED RANDOM EFFECTS

    to_vector(eta_V1) ~ std_normal();
    eta_V1_scale ~ std_normal();

    to_vector(eta_V2) ~ std_normal();
    eta_V2_scale ~ std_normal();

    to_vector(eta_V3) ~ std_normal();
    eta_V3_scale ~ std_normal();

    to_vector(gamma) ~ std_normal();
    gamma_scale ~ std_normal();

    // FIXED-EFFECT

    beta_tilde ~ std_normal();

    // LIKELIHOOD

  for (i in 1:n) {

    y[i] ~ categorical_logit(mu[i]');

  }

}
