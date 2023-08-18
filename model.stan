functions {
// spatial functions from : https://github.com/ConnorDonegan/Stan-IAR
#include icar-functions.stan
}

data{

    int<lower = 1> N; // n. observations
    int Y[N]; // binary choice
    real offset; // king-zeng offset parameter

    int<lower = 1> P; // n. state- and day- level fxed effects
    matrix[N, P] X; // state- and day-  level covariate matrix

    int gender_id[N]; // gender level id
    int<lower=1> gender_N; // number of gender levels 

    int ethnicity_id[N]; // ethnicity level id
    int<lower=1> ethnicity_N; // number of ethnicity levels 

    int age_id[N]; // age level id
    int<lower=1> age_N; // number of age levels 

    int edu_id[N]; // education level id
    int<lower=1> edu_N; // number of education levels 

    int income_id[N]; // income level id
    int<lower=1> income_N; // number of income levels 

    int vote2016_id[N]; // 2016 vote level id
    int<lower=1> vote2016_N; // number of 2016 vote levels 

    int dte_id[N]; // days-to-election id
    int<lower=1> dte_N; // max number of days-to-election

    // SPATIAL-COMPONENT DATA

        int area_id[N]; // index of areas in the observed data
        int<lower=1> area_N; // no. of spatial units
        int<lower=1> k; // no. of separate inner-connected groups
        int group_size[k]; // observational units per group
        int group_idx[area_N]; // index of observations, ordered by group

        int<lower=1> N_edges; // number of adjacency instances
        int<lower=1, upper=area_N> node1[N_edges]; // node1[i] adjacent to node2[i]
        int<lower=1, upper=area_N> node2[N_edges]; // node1[i] < node2[i]
        int<lower=1, upper=k> comp_id[area_N]; // ids of groups by areas

        vector[k] inv_sqrt_scaling_factor ; // BYM2 scale factor, with singletons represented by 1

}

transformed data {

    int<lower=0,upper=1> has_phi=1; // turn phi on to include unstructured random effect in BYM2 spec.

}


parameters{

    real alpha_star; // baseline rate of choice
    
    vector[P] beta_star; // fixed-effects
    
    vector[gender_N] gamma_gender; // gender unstructured random effects
    real<lower=0> gamma_gender_scale; // gender effects' scale

    vector[ethnicity_N] gamma_ethnicity; // ethnicity unstructured random effects
    real<lower=0> gamma_ethnicity_scale; // ethnicity effects' scale

    vector[age_N] gamma_age; // age autoregressive random effects
    real<lower=0>  gamma_age_scale; // age effects' scale

    vector[edu_N] gamma_edu; // education unstructured random effects
    real<lower=0>  gamma_edu_scale; // education effects' scale

    vector[income_N] gamma_income; // income autoregressive random effects
    real<lower=0>  gamma_income_scale; // income effects' scale

    vector[vote2016_N] gamma_vote2016; // 2016 vote unstructured random effects
    real<lower=0> gamma_vote2016_scale;// 2016 vote effects' scale

    vector[dte_N] delta; // days-to-election autoregressive random effects
    real<lower=0> delta_scale;// days-to-election effects' scale

    vector[area_N] phi; // unstructured area effect
    vector[area_N] psi; // spatial (ICAR) area effect
    real<lower=0,upper = 1> omega; // mixing parameter for structured/unstructured area effects
    real<lower=0> spatial_scale; // scale for structured area effects
  
}


transformed parameters{
    
    vector[N] mu; // latent propensity for choice j
    
    // NON-CENTERED PARAMETRISATION 
    
        vector[gender_N] gamma_gender_star = gamma_gender * gamma_gender_scale;
  
        vector[ethnicity_N] gamma_ethnicity_star = gamma_ethnicity * gamma_ethnicity_scale;
        
        vector[age_N] gamma_age_star = gamma_age * gamma_age_scale;
  
        vector[edu_N] gamma_edu_star = gamma_edu * gamma_edu_scale;
  
        vector[income_N] gamma_income_star = gamma_income * gamma_income_scale;

        vector[vote2016_N] gamma_vote2016_star = gamma_vote2016 * gamma_vote2016_scale;

        vector[dte_N] delta_star = delta * delta_scale;

        vector[area_N] lambda_star = convolve_bym2(psi, phi, spatial_scale, area_N, k, group_size, group_idx, omega, inv_sqrt_scaling_factor);
        
    // LINEAR PREDICTOR 
  
        mu =
            offset +
            alpha_star +
            X*beta_star +
            gamma_gender_star[gender_id] +
            gamma_ethnicity_star[ethnicity_id] +
            gamma_age_star[age_id] +
            gamma_edu_star[edu_id] +
            gamma_income_star[income_id] +
            gamma_vote2016_star[vote2016_id] +
            lambda_star[area_id] +
            delta_star[dte_id];
            
}


model{

    // IID PRIORS

        alpha_star ~ std_normal();
    
        to_vector(beta_star) ~ std_normal();

    // UNSTRUCTURED RANDOM EFFECTS

        to_vector(gamma_gender) ~ std_normal();
        gamma_gender_scale ~ std_normal();

        to_vector(gamma_ethnicity) ~ std_normal();
        gamma_ethnicity_scale ~ std_normal();

        to_vector(gamma_edu) ~ std_normal();
        gamma_edu_scale ~ std_normal();

        to_vector(gamma_vote2016) ~ std_normal();
        gamma_vote2016_scale ~ std_normal();

    // STRUCTURED AUTOREGRESSIVE PRIORS 

        sum(gamma_income) ~ normal(0, 0.01 * income_N); // sum-to-0 constraint
        for(i in 2:income_N){ 
            gamma_income[i] ~ normal(gamma_income[i-1],1);
        };
        gamma_income_scale ~ std_normal();

        sum(gamma_age) ~ normal(0, 0.01 * age_N); // sum-to-0 constraint
        for(i in 2:age_N){ 
            gamma_age[i] ~ normal(gamma_age[i-1],1);
        };
        gamma_age_scale ~ std_normal();

        sum(delta) ~ normal(0, 0.01 * dte_N); // sum-to-0 constraint
        for(i in 2:dte_N){ 
            delta[i] ~ normal(delta[i-1],1); 
        };
        delta_scale ~ std_normal();

        psi ~ icar_normal(spatial_scale,node1, node2, k, group_size, group_idx, has_phi);
        phi ~ std_normal();
        omega ~ beta(0.5,0.5);
        spatial_scale ~ std_normal();
        
    // LIKELIHOOD

        Y ~ bernoulli_logit(mu) ;

}