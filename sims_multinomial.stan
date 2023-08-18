functions {
// spatial functions from : https://github.com/ConnorDonegan/Stan-IAR
#include icar-functions.stan
}

data {

  int<lower = 1> n; // n. observations
  int<lower = 1> J; // number of choices in the choice-set
  int y[n]; // categorical choice

  // DATA RANDOM INTERCEPTS

  int V1_id[n];
  int<lower=1> V1_N;

  int V2_id[n];
  int<lower=1> V2_N;

  int V3_id[n];
  int<lower=1> V3_N;

  // AREA-LEVEL COVARIATE

  vector[n] Z;

  // DATA SPATIAL MODEL

  int area_id[n]; // index of areas in the observed data
  int<lower=1> area_N; // no. of spatial units

  int<lower=1> k; // no. of separate inner-connected groups
  int group_size[k]; // observational units per group
  int group_idx[area_N]; // index of observations, ordered by group

  int<lower=1> N_edges; // number of adjacency instances
  int<lower=1, upper=area_N> node1[N_edges]; // node1[i] adjacent to node2[i]
  int<lower=1, upper=area_N> node2[N_edges]; // node1[i] < node2[i]
  int<lower=1, upper=k> comp_id[area_N]; // ids of groups by areas

  matrix[k,1] inv_sqrt_scaling_factor ; // BYM2 scale factor, with singletons represented by 1
}


transformed data {

// ZERO VECTORS FOR IDENTIFIABILITY

  vector[V1_N] eta_V1_zeros = rep_vector(0, V1_N);
  vector[V2_N] eta_V2_zeros = rep_vector(0, V2_N);
  vector[V3_N] eta_V3_zeros = rep_vector(0, V3_N);
  vector[area_N] gamma_zeros = rep_vector(0, area_N);
  real beta_zeros = 0;
  real alpha_zeros = 0;


// turn phi on to include unstructured random effect in BYM2 spec.
  int<lower=0,upper=1> has_phi=1; 

// This is needed because stan doesn't like vectors of length one as inputs 
  vector[k] f;
    for(i in 1:k){ f[i] = inv_sqrt_scaling_factor[i,1]; }
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

  // AREA-LEVEL FIXED-EFFECT
  vector[J-1] beta_tilde;

  // SPATIAL MODEL PARAMETERS

  matrix[area_N,J-1] phi; // unstructured area effect
  matrix[area_N,J-1] psi; // spatial (ICAR) area effect
  vector<lower=0,upper = 1>[J-1] omega; // mixing parameter for structured/unstructured area effects
  vector<lower=0>[J-1] spatial_scale; // scale for structured area effects
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
  gamma_tilde[,j] = convolve_bym2(psi[,j], phi[,j], spatial_scale[j], area_N, k, group_size, group_idx, omega[j], f);
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

    // SPATIAL PRIOR
    for(j in 1:(J-1)){
      
      psi[,j] ~ icar_normal(spatial_scale[j],node1, node2, k, group_size, group_idx, has_phi);
      
      phi[,j] ~ std_normal();
      
      omega[j] ~ beta(0.5,0.5);
      
      spatial_scale[j] ~ std_normal();

    }

    // FIXED-EFFECT

    beta_tilde ~ std_normal();

    // LIKELIHOOD

  for (i in 1:n) {

    y[i] ~ categorical_logit(mu[i]');

  }

}
