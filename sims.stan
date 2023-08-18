functions {
// spatial functions from : https://github.com/ConnorDonegan/Stan-IAR
#include icar-functions.stan
}

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
// turn phi on to include unstructured random effect in BYM2 spec.
  int<lower=0,upper=1> has_phi=1; 
// This is needed because stan doesn't like vectors of length one as inputs 
vector[k] f;
  for(i in 1:k){ f[i] = inv_sqrt_scaling_factor[i,1]; }
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

  real beta;

  // SPATIAL MODEL PARAMETERS

  vector[area_N] phi; // unstructured area effect
  vector[area_N] psi; // spatial (ICAR) area effect
  real<lower=0,upper = 1> omega; // mixing parameter for structured/unstructured area effects
  real<lower=0> spatial_scale; // scale for structured area effects
}

transformed parameters {
  vector[n] mu; // latent propensity for choice j

  // NON-CENTERED PARAMETRISATION

  vector[V1_N] eta_V1_star = eta_V1 * eta_V1_scale;
  vector[V2_N] eta_V2_star = eta_V2 * eta_V2_scale;
  vector[V3_N] eta_V3_star = eta_V3 * eta_V3_scale;

  vector[area_N] gamma_star = convolve_bym2(psi, phi, spatial_scale, area_N, k, group_size, group_idx, omega, f);


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

    beta ~ std_normal();

    // SPATIAL PRIOR

    psi ~ icar_normal(spatial_scale,node1, node2, k, group_size, group_idx, has_phi);
    phi ~ std_normal();
    omega ~ beta(0.5,0.5);
    spatial_scale ~ std_normal();

    // LIKELIHOOD

    y ~ bernoulli_logit(mu) ;
}