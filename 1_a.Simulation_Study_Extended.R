# clean workspace
rm(list = ls())
# clean garbadge
gc()
# clear graphics device
# dev.off()
# set decimals to digits instead of scientific
options(scipen = 999)
# set timeout limit (laxed)
options(timeout = 10000)
# set work directory
setwd(dir = "~/Desktop/Artificially Intelligent Opinion Polling/")

# load useful package
library(data.table)
library(dplyr)
library(MASS)
library(MCMCprecision)
estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}

# prepare scoring functions
bias <- function(pred,obs){mean(pred - obs)}
rmse <- function(pred,obs){sqrt(mean((pred-obs)^2))}
pearson <- function(pred,obs){cor(y = obs,x = pred)}
cover <- function(pred_lo,pred_hi,obs){ mean(pred_lo<obs & pred_hi>obs) }

# softmax function
softmax <- function(x){ exp(x) / rowSums(exp(x)) }

# load stan and options
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Sample function is useful but buggy -
# if you specify a single integer it returns a sequence up to that integer
sample = function(x, size, replace = F, prob = NULL) {
  if (length(x) == 1) return(x)
  base::sample(x, size = size, replace = replace, prob = prob)
}

# load spatial utils
library(sf)
library(INLA)
library(geostan)
library(ape)

#' compute scaling factor for adjacency matrix, accounting for differences in spatial connectivity #'
#' @param C connectivity matrix
#'
#' @details
#'
#' Requires the following packages:
#'
#' library(Matrix)
#' library(INLA);
#' library(spdep)
#' library(igraph)
#'
#' @source
#'
#' Morris, Mitzi (2017). Spatial Models in Stan: Intrinsic Auto-Regressive Models for Areal Data. <ht #'
scale_c <- function(C) {
  geometric_mean <- function(x) exp(mean(log(x)))
  N = dim(C)[1]
  Q =  Diagonal(N, rowSums(C)) - C
  Q_pert = Q + Diagonal(N) * max(diag(Q)) * sqrt(.Machine$double.eps)
  Q_inv = inla.qinv(Q_pert, constr=list(A = matrix(1,1,N),e=0))
  scaling_factor <- geometric_mean(Matrix::diag(Q_inv))
  return(scaling_factor)
}

# stan model fit faciliatation function
# fit the model
get_id <- function(dt,SF,var.name,model.var.name = NA){

  if( is.na(model.var.name) ){
    model.var.name <- var.name
  }

  id <-
    match(
      dt[[var.name]],
      levels(
        as.factor(
          SF[[var.name]]
        )
      )
    )

  N <-
    nlevels(
      as.factor(
        SF[[var.name]]
      )
    )

  object <-
    list(
      id = id,
      N = N
    )

  names(object) <- paste(model.var.name,names(object),sep="_")

  return(object)
}


# # # SETUP SPATIAL STRUCTURE

# load US states map
shape_US <- sf::read_sf(dsn = 'data_auxiliary/cb_2018_us_state_500k/cb_2018_us_state_500k.shp')

# match names to FIPS names
shape_US$small.area <- usmap::fips_info()$full[match(shape_US$STATEFP,usmap::fips_info()$fips)]

# drop non relevant areas
shape_US <- shape_US[!is.na(shape_US$small.area),]

# get adjacency matrix
C <- shape2mat(shape_US, style = "B")

# prep icar data
icar.data <- geostan::prep_icar_data(C)

# get scaling factor for BYM2 model

# calculate the scale factor for each of k connected group of nodes,
# using the scale_c function from M. Morris (see geostan functions)
scale_factor <- vector(mode = "numeric", length = icar.data$k)
for (j in 1:icar.data$k) {
  g.idx <- which(icar.data$comp_id == j)
  if (length(g.idx) == 1) {
    scale_factor[j] <- 1
    next
  }
  Cg <- C[g.idx, g.idx]
  scale_factor[j] <- scale_c(Cg)
}
# use inverse square root of scale factor
icar.data$inv_sqrt_scale_factor <- 1 / sqrt( scale_factor )

# # # INITIALISE SIMULATION
n.sims <- 335
#store.list <- list()

for(sim in 149:n.sims){

# # # # # # # # #
# # # # # # # # # DATA PREP:
# # # # # # # # # POPULATION
# # # # # # # # #
{
  N <- 1000000 # population size
  J <- 3 # number of choices
  P <- 3 # number of individual-level covariates

  # model parameters
  alpha <- rnorm(n = J,mean = 0,sd = 1) # individual level baseline
  rho_X <- runif(n = 1,min = 0,max = 1) # correlation amongst covariates
  Sigma <- diag(P)
  Sigma[Sigma!=1] <- rho_X

  X <- mvrnorm(n = N,mu = rep(0,P),Sigma = Sigma)

  # discrete random effect IDs
  X_id <-
    apply(
      X,
      2,
      function(x){ as.integer( cut(x, c( min(x)-1, -1,0,1, max(x)+1) ) ) }
    )

  # generate random effects for each covariate, for each choice
  # in the MrsP study, a random effect variance ~ 0.5 is suggested
  var.eta <- 1
  eta <-
    lapply(
      1:J,
      function(x){
        as.data.table(mvrnorm(n = 4,mu = rep(0,P),Sigma = diag(P)*var.eta))
      } )

  # generate area-level effect from BYM2 DGP
  area_N =  dim(shape_US)[1] # total number of areas

  # generate random size of states - good to have variability to represent
  # real-life data
  prob <- rdirichlet(n = 1,rep(1,area_N))

  area_id =
    sample(
      x = c(1:area_N),
      size = N,
      replace = TRUE,
      prob = prob
    ) # randomly assign area

  # simulate spatially correlated effects
  # simulate via SAR as more stable
  # get distance matrix
  W <- shape2mat(shape_US, style = "W")
  D <- diag(rowSums(W))

  # create highly spatially correlated data (this is the ICAR component of the
  # BYM2 model).

  # I want to keep moran stable - sometimes it goes as low as 0.4. Sample 1000
  # etas and pick the one with the highest moran every time.
  n.spat.samples <- 1000
  psi_samples <-
    sim_sar(
      m = n.spat.samples,
      w = W[rowSums(D)!=0,rowSums(D)!=0],
      rho = 0.99,
      mu = rep(0, sum(rowSums(D)!=0)),
      sigma = 0.1
      # keep sigma small such that resulting spatial data has max sd = 1
    )

  # check moran
  moran_samples <-
    apply(
      psi_samples,
      1,
      function(x){
        Moran.I(
          x = x,
          weight = as.matrix(C)[rowSums(D)!=0,rowSums(D)!=0]
        )$observed
      }
    )
  # this is meant to be a highly correlated effect (ICAR) -
  # pick the largest J and randomly assign to chocies
  moran_psi <-
    moran_samples[moran_samples %in% rev(sort(moran_samples))[1:J]]

  order.J <- sample(1:J,size = 3,replace = FALSE)

  # put together spatial effect with islands
  psi <- array(0,dim = c(area_N,J))
  for(j in 1:J){
    psi[rowSums(D)!=0,j] <-
      as.numeric(psi_samples[moran_samples==moran_psi[order.J[j]],])
  }

  # unstructured area-level effects  effect
  phi <-
    t(
      mvrnorm(
        n = J,
        mu = rep(0,area_N),
        Sigma = diag(area_N)
      )
    )
  # random effect sigma = 1

  # mixing parameter
  omega <- runif(n = J,min = 0,max = 1)

  # BYM2 Effect
  inv_sqrt_scale_factor <- rep(1,area_N)
  inv_sqrt_scale_factor[rowSums(D)!=0] <- icar.data$inv_sqrt_scale_factor[1]
  gamma <- array(0,dim = c(area_N,J))
  tau.2 <- 1
  tau <- sqrt(tau.2)

  for(j in 1:J){
    gamma[,j] <-
    tau*(sqrt(1-omega[j])*phi[,j] + sqrt(omega[j]/inv_sqrt_scale_factor)*psi[,j])
  }

  # area predictor
  Z <- rnorm(n = area_N,mean = 0,sd = 1)
  beta <- runif(n = J,min = -1,max = 1) #rnorm(n = J,mean = 0,sd = 1)

  # # # Generate ground-truth stratification frame
  frame <- cbind(as.data.table(X_id),area_id,Z = Z[area_id])
  # create weights
  frame$w <- 1
  frame <- frame[,lapply(.SD,sum),by = c(names(frame[,!'w'])),.SDcols = c('w')]

  # generate predicted probabilities for each party
  mu_tilde_m <- as.data.table(array(NA,c(dim(frame)[1],J)))
  names(mu_tilde_m) <- paste('mu_tilde_',1:J,sep='')
  for(j in 1:J){
    mu_tilde_m[[j]] <-
      as.numeric(
        unlist(
          alpha[j] +
            eta[[j]][frame$V1,1] + eta[[j]][frame$V2,2] + eta[[j]][frame$V3,3] +
            gamma[frame$area_id,j] + beta[j]*frame$Z
        ) )
  }
  pi_tilde_m <- softmax(mu_tilde_m)
  names(pi_tilde_m) <- paste('pi_tilde',1:J,sep='_')

  # generate area-level stratified truth
  tmp <- cbind(pi_tilde_m,frame)
  theta_tilde <-
    tmp[,
        lapply(.SD,function(x){
          sum(x*w)/sum(w)
        }),
        by = c('area_id'),
        .SDcols = c('pi_tilde_1','pi_tilde_2','pi_tilde_3')
    ]

  theta_tilde <-theta_tilde [order(theta_tilde $area_id)]
}
  # # # # # # # # #
  # # # # # # # # # DATA PREP:
  # # # # # # # # # RANDOM SAMPLE
  # # # # # # # # #
{
  n <- round(runif(n = 1,min = 100,max = 10000)) # sample size

  # get selection probability for each cell by party
  Pr.S_j <-
    as.data.table(
      sapply(1:J,function(j){ pi_tilde_m[[j]]*frame$w})
    )
  names(Pr.S_j) <- paste('j=',1:J,sep='')

  # sample according to this protocol
  # reframe the frame in terms of the observable preferences
  reframe <- data.table()
  for(j in 1:J){
    reframe.tmp <- cbind(frame,j = j,s = Pr.S_j[[j]])
    reframe <- rbindlist(list(reframe,reframe.tmp ))
  }

  sample_id <-
    sample(
      x = 1:dim(reframe)[1],
      size = n,
      replace = TRUE,
      prob = reframe$s
    )

  random_sample <- reframe[sample_id]
  for(j in 1:J){
    random_sample <- cbind(random_sample,tmp = ifelse(random_sample$j==j,1,0))
    names(random_sample)[names(random_sample)=='tmp'] = paste('j',j,sep='_')
  }
  random_sample <- random_sample[,!'j']

  # observe preference
  y <- random_sample[,grepl('j',names(random_sample)),with=FALSE]

  # calculate sample counts per choice
  n1.RS <- apply(y,2,sum)
  n0.RS <- n - n1.RS

  # generate random effect ids
  var.name.list <- paste('V',1:P,sep='')
  id_list <- list()
  for(i in 1:length(var.name.list)) {

    temp <-
      get_id(
        dt = random_sample,
        SF = frame,
        var.name = var.name.list[i]
      )

    id_list <- append(id_list, temp)
  }
}
  # # # # # # # # #
  # # # # # # # # # FIT ROUTINE:
  # # # # # # # # # RANDOM SAMPLE
  # # # # # # # # # STRUCTURED MODEL
  # # # # # # # # # NO-OFFSET
  # # # # # # # # # BERNOULLI
  # # # # # # # # #
{
  fit_object <- list()
  for(j in 1:J){

    train_data_list <-
      list(
        n = n,
        y = random_sample[[paste('j',j,sep="_")]],
        offset = 0,

        Z = random_sample$Z,

        area_id = random_sample$area_id,
        area_N =  area_N,

        k = icar.data$k,
        group_size = icar.data$group_size,
        group_idx = icar.data$group_idx,
        N_edges = icar.data$n_edges,
        node1 = icar.data$node1,
        node2 = icar.data$node2,
        comp_id = icar.data$comp_id,

        inv_sqrt_scaling_factor = as.matrix(icar.data$inv_sqrt_scale_factor)
      )

    train_data_list <- append(train_data_list, id_list)

    pars =
      c('alpha_star',
        'eta_V1_star','eta_V2_star','eta_V3_star',
        'gamma_star','beta',
        'psi','phi','omega','spatial_scale'
      )

    fit_object_temp <-
      stan(
        file = "sims.stan",
        pars = pars,
        data = train_data_list,
        iter = 500,
        warmup = 250,
        refresh = 1,
        thin = 4,
        cores = 8,
        chains = 8,
        control = list(max_treedepth = 10,adapt_delta = 0.8),
        verbose = TRUE
      )

    fit_object <- append(fit_object,list(fit_object_temp))

  }

  # extract parameters
  pars_sims <-
    lapply(1:J,
           function(j){
             rstan::extract(
               fit_object[[j]],
               pars = pars,
               inc_warmup = FALSE
             )
           } )

  # make predictions for each simulation
  pi_tilde_m_est_list <- list()
  theta_tilde_est_list <- list()

  for(s in 1:length(pars_sims[[1]]$alpha_star)){
    # calculate latent propensity
    mu_tilde_est <-
      sapply(
        1:J,
        function(j){
          pars_sims[[j]]$alpha_star[s] +
            pars_sims[[j]]$eta_V1_star[s,frame$V1] +
            pars_sims[[j]]$eta_V2_star[s,frame$V2] +
            pars_sims[[j]]$eta_V3_star[s,frame$V3] +
            pars_sims[[j]]$gamma_star[s,frame$area_id] +
            pars_sims[[j]]$beta[s]*frame$Z
        }
      )
    # calculate cell-level choice probability
    pi_tilde_m_est <- exp(mu_tilde_est)/(1+exp(mu_tilde_est))
    colnames(pi_tilde_m_est) <- paste('pi_tilde_m_est',1:J,sep="_")

    # store simulations for cell-level probability
    pi_tilde_m_est_list <-
      append(
        pi_tilde_m_est_list,
        list(as.data.table(pi_tilde_m_est))
      )

    # calculate stratified estimate
    theta_tilde_est <-
      as.data.table(
        cbind(pi_tilde_m_est,frame)
      )[,
        lapply(.SD,function(x){sum(x*w)/sum(w)}),
        by = c('area_id'),
        .SDcols = c(colnames(pi_tilde_m_est))
      ]
    theta_tilde_est <- theta_tilde_est[order(theta_tilde_est $area_id)]

    # store simulations for area-level chocie probability
    theta_tilde_est_list <-
      append(
        theta_tilde_est_list,
        list(theta_tilde_est)
      )
  }

  # calculate summaries over simulations
  estimate_summary_pi_tilde_m <-
    lapply(
      1:J,
      function(j){
        t(
          apply(
            sapply(
              1:length(pars_sims[[1]]$alpha_star),
              function(s){
                pi_tilde_m_est_list[[s]][[paste('pi_tilde_m_est',j,sep='_')]]
              }
            ),
            1,
            function(x){
              quantile(x,c(0.05,0.5,0.95))
            }
          )
        )
      }
    )

  estimate_summary_theta_tilde <-
    lapply(
      1:J,
      function(j){
        t(
          apply(
            sapply(
              1:length(pars_sims[[1]]$alpha_star),
              function(s){
                theta_tilde_est_list[[s]][[paste('pi_tilde_m_est',j,sep='_')]]
              }
            ),
            1,
            function(x){
              quantile(x,c(0.05,0.5,0.95))
            }
          )
        )
      }
    )

  # # # CALCULATE PERFORMANCE

  # setup scoring for pi_m
  pred_pi <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_pi_tilde_m[[j]])[['50%']]
        } ) )
  pred_pi_lo <-
    as.data.table(
      sapply(
        1:J,
        function(j){as.data.table(estimate_summary_pi_tilde_m[[j]])[['5%']]
        } ) )
  pred_pi_hi <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_pi_tilde_m[[j]])[['95%']]
        } ) )
  obs_pi <- pi_tilde_m

  # setup scoring for theta
  pred_theta <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_theta_tilde[[j]])[['50%']]
        } ) )
  pred_theta_lo <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_theta_tilde[[j]])[['5%']]
        } ) )
  pred_theta_hi <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_theta_tilde[[j]])[['95%']]
        } ) )
  obs_theta <- theta_tilde[,c('pi_tilde_1','pi_tilde_2','pi_tilde_3'),with=F]

  # bias
  bias_pi_RS_str_Bern <-
    sapply(1:J,function(j){bias(pred = pred_pi[[j]],obs = obs_pi[[j]])})
  bias_theta_RS_str_Bern <-
    sapply(1:J,function(j){bias(pred = pred_theta[[j]],obs = obs_theta[[j]])})

  # rmse
  rmse_pi_RS_str_Bern <-
    sapply(1:J,function(j){rmse(pred = pred_pi[[j]],obs = obs_pi[[j]])})
  rmse_theta_RS_str_Bern <-
    sapply(1:J,function(j){rmse(pred = pred_theta[[j]],obs = obs_theta[[j]])})

  # correlation
  pearson_pi_RS_str_Bern <-
    sapply(1:J,function(j){pearson(pred = pred_pi[[j]],obs = obs_pi[[j]])})
  pearson_theta_RS_str_Bern <-
    sapply(1:J,function(j){pearson(pred = pred_theta[[j]],obs = obs_theta[[j]])})

  # coverage
  cover_pi_RS_str_Bern <-
    sapply(1:J,function(j){cover(pred_lo = pred_pi_lo[[j]],pred_hi = pred_pi_hi[[j]],obs = obs_pi[[j]])})
  cover_theta_RS_str_Bern <-
    sapply(1:J,function(j){cover(pred_lo = pred_theta_lo[[j]],pred_hi = pred_theta_hi[[j]],obs = obs_theta[[j]])})
}
  # # # # # # # # #
  # # # # # # # # # FIT ROUTINE:
  # # # # # # # # # RANDOM SAMPLE
  # # # # # # # # # STRUCTURED MODEL
  # # # # # # # # # NO-OFFSET
  # # # # # # # # # MULTINOMIAL
  # # # # # # # # #
{
  multi.list <- list(
    y =
      apply(
        random_sample[,grepl('j',names(random_sample)),with=F],
        1,
        function(x){which(x==1)}
      ),
    J = J
  )

  train_data_list <-
    append(
      train_data_list[names(train_data_list)!='y'],
      multi.list
    )

  pars =
    c('alpha_star',
      'eta_V1_star','eta_V2_star','eta_V3_star',
      'gamma_star','beta',
      'psi','phi','omega','spatial_scale'
    )

  fit_object <-
    stan(
      file = "sims_multinomial.stan",
      pars = pars,
      data = train_data_list,
      iter = 500,
      warmup = 250,
      refresh = 1,
      thin = 4,
      cores = 8,
      chains = 8,
      control = list(max_treedepth = 10,adapt_delta = 0.8),
      verbose = TRUE
    )

  # extract parameters
  pars_sims <-
    rstan::extract(
      fit_object,
      pars = pars,
      inc_warmup = FALSE
    )

  # make predictions for each simulation
  pi_tilde_m_est_list <- list()
  theta_tilde_est_list <- list()


  for(s in 1:dim(pars_sims$alpha_star)[1]){
    # calculate latent propensity
    mu_tilde_est <-
      sapply(
        1:J,
        function(j){
          pars_sims$alpha_star[s,j] +
            pars_sims$eta_V1_star[s,frame$V1,j] +
            pars_sims$eta_V2_star[s,frame$V2,j] +
            pars_sims$eta_V3_star[s,frame$V3,j] +
            pars_sims$gamma_star[s,frame$area_id,j] +
            pars_sims$beta[s,j]*frame$Z
        }
      )
    # calculate cell-level choice probability
    pi_tilde_m_est <- softmax(mu_tilde_est)
    colnames(pi_tilde_m_est) <- paste('pi_tilde_m_est',1:J,sep="_")

    # store simulations for cell-level probability
    pi_tilde_m_est_list <-
      append(
        pi_tilde_m_est_list,
        list(as.data.table(pi_tilde_m_est))
      )

    # calculate stratified estimate
    theta_tilde_est <-
      as.data.table(
        cbind(pi_tilde_m_est,frame)
      )[,
        lapply(.SD,function(x){sum(x*w)/sum(w)}),
        by = c('area_id'),
        .SDcols = c(colnames(pi_tilde_m_est))
      ]
    theta_tilde_est <- theta_tilde_est[order(theta_tilde_est $area_id)]

    # store simulations for area-level chocie probability
    theta_tilde_est_list <-
      append(
        theta_tilde_est_list,
        list(theta_tilde_est)
      )
  }

  # calculate summaries over simulations
  estimate_summary_pi_tilde_m <-
    lapply(
      1:J,
      function(j){
        t(
          apply(
            sapply(
              1:dim(pars_sims$alpha_star)[1],
              function(s){
                pi_tilde_m_est_list[[s]][[paste('pi_tilde_m_est',j,sep='_')]]
              }
            ),
            1,
            function(x){
              quantile(x,c(0.05,0.5,0.95))
            }
          )
        )
      }
    )

  estimate_summary_theta_tilde <-
    lapply(
      1:J,
      function(j){
        t(
          apply(
            sapply(
              1:dim(pars_sims$alpha_star)[1],
              function(s){
                theta_tilde_est_list[[s]][[paste('pi_tilde_m_est',j,sep='_')]]
              }
            ),
            1,
            function(x){
              quantile(x,c(0.05,0.5,0.95))
            }
          )
        )
      }
    )

  # # # CALCULATE PERFORMANCE

  # setup scoring for pi_m
  pred_pi <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_pi_tilde_m[[j]])[['50%']]
        } ) )
  pred_pi_lo <-
    as.data.table(
      sapply(
        1:J,
        function(j){as.data.table(estimate_summary_pi_tilde_m[[j]])[['5%']]
        } ) )
  pred_pi_hi <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_pi_tilde_m[[j]])[['95%']]
        } ) )
  obs_pi <- pi_tilde_m

  # setup scoring for theta
  pred_theta <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_theta_tilde[[j]])[['50%']]
        } ) )
  pred_theta_lo <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_theta_tilde[[j]])[['5%']]
        } ) )
  pred_theta_hi <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_theta_tilde[[j]])[['95%']]
        } ) )
  obs_theta <- theta_tilde[,c('pi_tilde_1','pi_tilde_2','pi_tilde_3'),with=F]

  # bias
  bias_pi_RS_str_Multi <-
    sapply(1:J,function(j){bias(pred = pred_pi[[j]],obs = obs_pi[[j]])})
  bias_theta_RS_str_Multi <-
    sapply(1:J,function(j){bias(pred = pred_theta[[j]],obs = obs_theta[[j]])})

  # rmse
  rmse_pi_RS_str_Multi <-
    sapply(1:J,function(j){rmse(pred = pred_pi[[j]],obs = obs_pi[[j]])})
  rmse_theta_RS_str_Multi <-
    sapply(1:J,function(j){rmse(pred = pred_theta[[j]],obs = obs_theta[[j]])})

  # correlation
  pearson_pi_RS_str_Multi <-
    sapply(1:J,function(j){pearson(pred = pred_pi[[j]],obs = obs_pi[[j]])})
  pearson_theta_RS_str_Multi <-
    sapply(1:J,function(j){pearson(pred = pred_theta[[j]],obs = obs_theta[[j]])})

  # coverage
  cover_pi_RS_str_Multi <-
    sapply(1:J,function(j){cover(pred_lo = pred_pi_lo[[j]],pred_hi = pred_pi_hi[[j]],obs = obs_pi[[j]])})
  cover_theta_RS_str_Multi <-
    sapply(1:J,function(j){cover(pred_lo = pred_theta_lo[[j]],pred_hi = pred_theta_hi[[j]],obs = obs_theta[[j]])})
}
  # # # # # # # # #
  # # # # # # # # # FIT ROUTINE:
  # # # # # # # # # RANDOM SAMPLE
  # # # # # # # # # UNSTRUCTURED MODEL
  # # # # # # # # # NO-OFFSET
  # # # # # # # # # BERNOULLI
  # # # # # # # # #
{
  fit_object <- list()
  for(j in 1:J){

    train_data_list <-
      list(
        n = n,
        y = random_sample[[paste('j',j,sep="_")]],
        offset = 0,

        Z = random_sample$Z,

        area_id = random_sample$area_id,
        area_N =  area_N,

        k = icar.data$k,
        group_size = icar.data$group_size,
        group_idx = icar.data$group_idx,
        N_edges = icar.data$n_edges,
        node1 = icar.data$node1,
        node2 = icar.data$node2,
        comp_id = icar.data$comp_id,

        inv_sqrt_scaling_factor = as.matrix(icar.data$inv_sqrt_scale_factor)
      )

    train_data_list <- append(train_data_list, id_list)

    pars =
      c('alpha_star',
        'eta_V1_star','eta_V2_star','eta_V3_star',
        'gamma_star','beta'
      )

    fit_object_temp <-
      stan(
        file = "sims_traditional.stan",
        pars = pars,
        data = train_data_list,
        iter = 500,
        warmup = 250,
        refresh = 1,
        thin = 4,
        cores = 8,
        chains = 8,
        control = list(max_treedepth = 10,adapt_delta = 0.8),
        verbose = TRUE
      )

    fit_object <- append(fit_object,list(fit_object_temp))

  }

  # extract parameters
  pars_sims <-
    lapply(1:J,
           function(j){
             rstan::extract(
               fit_object[[j]],
               pars = pars,
               inc_warmup = FALSE
             )
           } )

  # make predictions for each simulation
  pi_tilde_m_est_list <- list()
  theta_tilde_est_list <- list()

  for(s in 1:length(pars_sims[[1]]$alpha_star)){
    # calculate latent propensity
    mu_tilde_est <-
      sapply(
        1:J,
        function(j){
          pars_sims[[j]]$alpha_star[s] +
            pars_sims[[j]]$eta_V1_star[s,frame$V1] +
            pars_sims[[j]]$eta_V2_star[s,frame$V2] +
            pars_sims[[j]]$eta_V3_star[s,frame$V3] +
            pars_sims[[j]]$gamma_star[s,frame$area_id] +
            pars_sims[[j]]$beta[s]*frame$Z
        }
      )
    # calculate cell-level choice probability
    pi_tilde_m_est <- exp(mu_tilde_est)/(1+exp(mu_tilde_est))#softmax(mu_tilde_est)
    colnames(pi_tilde_m_est) <- paste('pi_tilde_m_est',1:J,sep="_")

    # store simulations for cell-level probability
    pi_tilde_m_est_list <-
      append(
        pi_tilde_m_est_list,
        list(as.data.table(pi_tilde_m_est))
      )

    # calculate stratified estimate
    theta_tilde_est <-
      as.data.table(
        cbind(pi_tilde_m_est,frame)
      )[,
        lapply(.SD,function(x){sum(x*w)/sum(w)}),
        by = c('area_id'),
        .SDcols = c(colnames(pi_tilde_m_est))
      ]
    theta_tilde_est <- theta_tilde_est[order(theta_tilde_est $area_id)]

    # store simulations for area-level chocie probability
    theta_tilde_est_list <-
      append(
        theta_tilde_est_list,
        list(theta_tilde_est)
      )
  }

  # calculate summaries over simulations
  estimate_summary_pi_tilde_m <-
    lapply(
      1:J,
      function(j){
        t(
          apply(
            sapply(
              1:length(pars_sims[[1]]$alpha_star),
              function(s){
                pi_tilde_m_est_list[[s]][[paste('pi_tilde_m_est',j,sep='_')]]
              }
            ),
            1,
            function(x){
              quantile(x,c(0.05,0.5,0.95))
            }
          )
        )
      }
    )

  estimate_summary_theta_tilde <-
    lapply(
      1:J,
      function(j){
        t(
          apply(
            sapply(
              1:length(pars_sims[[1]]$alpha_star),
              function(s){
                theta_tilde_est_list[[s]][[paste('pi_tilde_m_est',j,sep='_')]]
              }
            ),
            1,
            function(x){
              quantile(x,c(0.05,0.5,0.95))
            }
          )
        )
      }
    )

  # # # CALCULATE PERFORMANCE

  # setup scoring for pi_m
  pred_pi <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_pi_tilde_m[[j]])[['50%']]
        } ) )
  pred_pi_lo <-
    as.data.table(
      sapply(
        1:J,
        function(j){as.data.table(estimate_summary_pi_tilde_m[[j]])[['5%']]
        } ) )
  pred_pi_hi <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_pi_tilde_m[[j]])[['95%']]
        } ) )
  obs_pi <- pi_tilde_m

  # setup scoring for theta
  pred_theta <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_theta_tilde[[j]])[['50%']]
        } ) )
  pred_theta_lo <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_theta_tilde[[j]])[['5%']]
        } ) )
  pred_theta_hi <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_theta_tilde[[j]])[['95%']]
        } ) )
  obs_theta <- theta_tilde[,c('pi_tilde_1','pi_tilde_2','pi_tilde_3'),with=F]

  # bias
  bias_pi_RS_unstr_Bern <-
    sapply(1:J,function(j){bias(pred = pred_pi[[j]],obs = obs_pi[[j]])})
  bias_theta_RS_unstr_Bern <-
    sapply(1:J,function(j){bias(pred = pred_theta[[j]],obs = obs_theta[[j]])})

  # rmse
  rmse_pi_RS_unstr_Bern <-
    sapply(1:J,function(j){rmse(pred = pred_pi[[j]],obs = obs_pi[[j]])})
  rmse_theta_RS_unstr_Bern <-
    sapply(1:J,function(j){rmse(pred = pred_theta[[j]],obs = obs_theta[[j]])})

  # correlation
  pearson_pi_RS_unstr_Bern <-
    sapply(1:J,function(j){pearson(pred = pred_pi[[j]],obs = obs_pi[[j]])})
  pearson_theta_RS_unstr_Bern <-
    sapply(1:J,function(j){pearson(pred = pred_theta[[j]],obs = obs_theta[[j]])})

  # coverage
  cover_pi_RS_unstr_Bern <-
    sapply(1:J,function(j){cover(pred_lo = pred_pi_lo[[j]],pred_hi = pred_pi_hi[[j]],obs = obs_pi[[j]])})
  cover_theta_RS_unstr_Bern <-
    sapply(1:J,function(j){cover(pred_lo = pred_theta_lo[[j]],pred_hi = pred_theta_hi[[j]],obs = obs_theta[[j]])})
}
  # # # # # # # # #
  # # # # # # # # # FIT ROUTINE:
  # # # # # # # # # RANDOM SAMPLE
  # # # # # # # # # UNSTRUCTURED MODEL
  # # # # # # # # # NO-OFFSET
  # # # # # # # # # MULTINOMIAL
  # # # # # # # # #
{
  multi.list <- list(
    y =
      apply(
        random_sample[,grepl('j',names(random_sample)),with=F],
        1,
        function(x){which(x==1)}
        ),
    J = J
  )

    train_data_list <-
      append(
        train_data_list[names(train_data_list)!='y'],
        multi.list
        )

    pars =
      c('alpha_star',
        'eta_V1_star','eta_V2_star','eta_V3_star',
        'gamma_star','beta'
      )

    fit_object <-
      stan(
        file = "sims_traditional_multinomial.stan",
        pars = pars,
        data = train_data_list,
        iter = 500,
        warmup = 250,
        refresh = 1,
        thin = 4,
        cores = 8,
        chains = 8,
        control = list(max_treedepth = 10,adapt_delta = 0.8),
        verbose = TRUE
      )

  # extract parameters
  pars_sims <-
    rstan::extract(
      fit_object,
      pars = pars,
      inc_warmup = FALSE
      )

  # make predictions for each simulation
  pi_tilde_m_est_list <- list()
  theta_tilde_est_list <- list()


  for(s in 1:dim(pars_sims$alpha_star)[1]){
    # calculate latent propensity
    mu_tilde_est <-
      sapply(
        1:J,
        function(j){
          pars_sims$alpha_star[s,j] +
            pars_sims$eta_V1_star[s,frame$V1,j] +
            pars_sims$eta_V2_star[s,frame$V2,j] +
            pars_sims$eta_V3_star[s,frame$V3,j] +
            pars_sims$gamma_star[s,frame$area_id,j] +
            pars_sims$beta[s,j]*frame$Z
        }
      )
    # calculate cell-level choice probability
    pi_tilde_m_est <- softmax(mu_tilde_est)
    colnames(pi_tilde_m_est) <- paste('pi_tilde_m_est',1:J,sep="_")

    # store simulations for cell-level probability
    pi_tilde_m_est_list <-
      append(
        pi_tilde_m_est_list,
        list(as.data.table(pi_tilde_m_est))
      )

    # calculate stratified estimate
    theta_tilde_est <-
      as.data.table(
        cbind(pi_tilde_m_est,frame)
      )[,
        lapply(.SD,function(x){sum(x*w)/sum(w)}),
        by = c('area_id'),
        .SDcols = c(colnames(pi_tilde_m_est))
      ]
    theta_tilde_est <- theta_tilde_est[order(theta_tilde_est $area_id)]

    # store simulations for area-level chocie probability
    theta_tilde_est_list <-
      append(
        theta_tilde_est_list,
        list(theta_tilde_est)
      )
  }

  # calculate summaries over simulations
  estimate_summary_pi_tilde_m <-
    lapply(
      1:J,
      function(j){
        t(
          apply(
            sapply(
              1:dim(pars_sims$alpha_star)[1],
              function(s){
                pi_tilde_m_est_list[[s]][[paste('pi_tilde_m_est',j,sep='_')]]
              }
            ),
            1,
            function(x){
              quantile(x,c(0.05,0.5,0.95))
            }
          )
        )
      }
    )

  estimate_summary_theta_tilde <-
    lapply(
      1:J,
      function(j){
        t(
          apply(
            sapply(
              1:dim(pars_sims$alpha_star)[1],
              function(s){
                theta_tilde_est_list[[s]][[paste('pi_tilde_m_est',j,sep='_')]]
              }
            ),
            1,
            function(x){
              quantile(x,c(0.05,0.5,0.95))
            }
          )
        )
      }
    )

  # # # CALCULATE PERFORMANCE

  # setup scoring for pi_m
  pred_pi <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_pi_tilde_m[[j]])[['50%']]
        } ) )
  pred_pi_lo <-
    as.data.table(
      sapply(
        1:J,
        function(j){as.data.table(estimate_summary_pi_tilde_m[[j]])[['5%']]
        } ) )
  pred_pi_hi <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_pi_tilde_m[[j]])[['95%']]
        } ) )
  obs_pi <- pi_tilde_m

  # setup scoring for theta
  pred_theta <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_theta_tilde[[j]])[['50%']]
        } ) )
  pred_theta_lo <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_theta_tilde[[j]])[['5%']]
        } ) )
  pred_theta_hi <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_theta_tilde[[j]])[['95%']]
        } ) )
  obs_theta <- theta_tilde[,c('pi_tilde_1','pi_tilde_2','pi_tilde_3'),with=F]

  # bias
  bias_pi_RS_unstr_Multi <-
    sapply(1:J,function(j){bias(pred = pred_pi[[j]],obs = obs_pi[[j]])})
  bias_theta_RS_unstr_Multi <-
    sapply(1:J,function(j){bias(pred = pred_theta[[j]],obs = obs_theta[[j]])})

  # rmse
  rmse_pi_RS_unstr_Multi <-
    sapply(1:J,function(j){rmse(pred = pred_pi[[j]],obs = obs_pi[[j]])})
  rmse_theta_RS_unstr_Multi <-
    sapply(1:J,function(j){rmse(pred = pred_theta[[j]],obs = obs_theta[[j]])})

  # correlation
  pearson_pi_RS_unstr_Multi <-
    sapply(1:J,function(j){pearson(pred = pred_pi[[j]],obs = obs_pi[[j]])})
  pearson_theta_RS_unstr_Multi <-
    sapply(1:J,function(j){pearson(pred = pred_theta[[j]],obs = obs_theta[[j]])})

  # coverage
  cover_pi_RS_unstr_Multi <-
    sapply(1:J,function(j){cover(pred_lo = pred_pi_lo[[j]],pred_hi = pred_pi_hi[[j]],obs = obs_pi[[j]])})
  cover_theta_RS_unstr_Multi <-
    sapply(1:J,function(j){cover(pred_lo = pred_theta_lo[[j]],pred_hi = pred_theta_hi[[j]],obs = obs_theta[[j]])})
}
  # # # # # # # # #
  # # # # # # # # # DATA PREP:
  # # # # # # # # # SELECTED SAMPLE
  # # # # # # # # #
{
  # get selection probability for each cell by party
  Pr.S_j <-
    as.data.table(
      sapply(1:J,function(j){ pi_tilde_m[[j]]*frame$w})
    )
  names(Pr.S_j) <- paste('j=',1:J,sep='')

  # # # calculate heterogeneous online selection penalty

  # expected penalty
  penalty <- rbeta(n = J,shape1 = 1,shape2 = 1)

  # maximum variance associated with the penalty
  max.var <- penalty*(1 -penalty)

  # sample a value for the variance in this draw
  var.penalty <- runif(n = J,min = rep(0,J),max = max.var)

  # estimate beta paramters which guarantee this level of variance and mean at every iteration
  penalty_m_pars <- estBetaParams(mu = penalty,var = var.penalty )

  # draw from this distribution a cell-specific penalty
  penalty_m <-
    sapply(1:J,function(j){
      rbeta(
        n = dim(Pr.S_j)[1],
        shape1 = penalty_m_pars$alpha[j],
        shape2 = penalty_m_pars$beta[j]
      ) } )

  # calculate new selection probability
  penalised_Pr.S <- as.data.table(sapply(1:J,function(j){(1-penalty_m[,j])*Pr.S_j[[j]]}))

  # sample according to this protocol
  # reframe the frame in terms of the observable preferences
  reframe <- data.table()
  for(j in 1:J){
    reframe.tmp <- cbind(frame,j = j,s = penalised_Pr.S[[j]])
    reframe <- rbindlist(list(reframe,reframe.tmp ))
  }

  sample_id <-
    sample(
      x = 1:dim(reframe)[1],
      size = n,
      replace = TRUE,
      prob = reframe$s
    )

  selected_sample <- reframe[sample_id]
  for(j in 1:J){
    selected_sample <- cbind(selected_sample,tmp = ifelse(selected_sample$j==j,1,0))
    names(selected_sample)[names(selected_sample)=='tmp'] = paste('j',j,sep='_')
  }
  selected_sample <- selected_sample[,!'j']

  # calculate the offset parameter
  N1 <- c()
  N0 <- c()
  for(j in 1:J){
    N1 <- c(N1,sum(pi_tilde_m[[paste('pi_tilde',j,sep='_')]]*frame$w))
    N0 <- c(N0,N - N1[j])
  }
  pi_tilde <- N1/sum(N1)

  # calculate sample counts per choice
  n1 <- apply(selected_sample[,c('j_1','j_2','j_3')],2,sum)
  n0 <- n -n1

  # offset parameter
  offset <- log((n1/N1)/(n0/N0))

  # generate random effect ids
  var.name.list <- paste('V',1:P,sep='')
  id_list <- list()
  for(i in 1:length(var.name.list)) {

    temp <-
      get_id(
        dt = selected_sample,
        SF = frame,
        var.name = var.name.list[i]
      )

    id_list <- append(id_list, temp)
  }
}
  # # # # # # # # #
  # # # # # # # # # FIT ROUTINE:
  # # # # # # # # # SELECTED SAMPLE
  # # # # # # # # # STRUCTURED MODEL
  # # # # # # # # # NO-OFFSET
  # # # # # # # # # BERNOULLI
  # # # # # # # # #
{
  fit_object <- list()
  for(j in 1:J){

    train_data_list <-
      list(
        n = n,
        y = selected_sample[[paste('j',j,sep="_")]],
        offset = 0,

        Z = selected_sample$Z,

        area_id = selected_sample$area_id,
        area_N =  area_N,

        k = icar.data$k,
        group_size = icar.data$group_size,
        group_idx = icar.data$group_idx,
        N_edges = icar.data$n_edges,
        node1 = icar.data$node1,
        node2 = icar.data$node2,
        comp_id = icar.data$comp_id,

        inv_sqrt_scaling_factor = as.matrix(icar.data$inv_sqrt_scale_factor)
      )

    train_data_list <- append(train_data_list , id_list)

    pars =
      c('alpha_star',
        'eta_V1_star','eta_V2_star','eta_V3_star',
        'gamma_star','beta',
        'psi','phi','omega','spatial_scale'
      )

    fit_object_temp <-
      stan(
        file = "sims.stan",
        pars = pars,
        data = train_data_list,
        iter = 500,
        warmup = 250,
        refresh = 1,
        thin = 4,
        cores = 8,
        chains = 8,
        control = list(max_treedepth = 10,adapt_delta = 0.8),
        verbose = TRUE
      )

    fit_object <- append(fit_object,list(fit_object_temp))

  }

  # make stratified prediction

  # extract parameters
  pars_sims <-
    lapply(1:J,
           function(j){
             rstan::extract(
               fit_object[[j]],
               pars = pars,
               inc_warmup = FALSE
             )
           } )

  # make predictions for each simulation
  pi_tilde_m_est_list <- list()
  theta_tilde_est_list <- list()

  for(s in 1:length(pars_sims[[1]]$alpha_star)){
    # calculate latent propensity
    mu_tilde_est <-
      sapply(
        1:J,
        function(j){
          pars_sims[[j]]$alpha_star[s] +
            pars_sims[[j]]$eta_V1_star[s,frame$V1] +
            pars_sims[[j]]$eta_V2_star[s,frame$V2] +
            pars_sims[[j]]$eta_V3_star[s,frame$V3] +
            pars_sims[[j]]$gamma_star[s,frame$area_id] +
            pars_sims[[j]]$beta[s]*frame$Z
        }
      )
    # calculate cell-level choice probability
    pi_tilde_m_est <- exp(mu_tilde_est)/(1+exp(mu_tilde_est))
    colnames(pi_tilde_m_est) <- paste('pi_tilde_m_est',1:J,sep="_")

    # store simulations for cell-level probability
    pi_tilde_m_est_list <-
      append(
        pi_tilde_m_est_list,
        list(as.data.table(pi_tilde_m_est))
      )

    # calculate stratified estimate
    theta_tilde_est <-
      as.data.table(
        cbind(pi_tilde_m_est,frame)
      )[,
        lapply(.SD,function(x){sum(x*w)/sum(w)}),
        by = c('area_id'),
        .SDcols = c(colnames(pi_tilde_m_est))
      ]
    theta_tilde_est <- theta_tilde_est[order(theta_tilde_est $area_id)]

    # store simulations for area-level chocie probability
    theta_tilde_est_list <-
      append(
        theta_tilde_est_list,
        list(theta_tilde_est)
      )
  }

  # calculate summaries over simulations
  estimate_summary_pi_tilde_m <-
    lapply(
      1:J,
      function(j){
        t(
          apply(
            sapply(
              1:length(pars_sims[[1]]$alpha_star),
              function(s){
                pi_tilde_m_est_list[[s]][[paste('pi_tilde_m_est',j,sep='_')]]
              }
            ),
            1,
            function(x){
              quantile(x,c(0.05,0.5,0.95))
            }
          )
        )
      }
    )

  estimate_summary_theta_tilde <-
    lapply(
      1:J,
      function(j){
        t(
          apply(
            sapply(
              1:length(pars_sims[[1]]$alpha_star),
              function(s){
                theta_tilde_est_list[[s]][[paste('pi_tilde_m_est',j,sep='_')]]
              }
            ),
            1,
            function(x){
              quantile(x,c(0.05,0.5,0.95))
            }
          )
        )
      }
    )

  # # # CALCULATE PERFORMANCE

  # setup scoring for pi_m
  pred_pi <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_pi_tilde_m[[j]])[['50%']]
        } ) )
  pred_pi_lo <-
    as.data.table(
      sapply(
        1:J,
        function(j){as.data.table(estimate_summary_pi_tilde_m[[j]])[['5%']]
        } ) )
  pred_pi_hi <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_pi_tilde_m[[j]])[['95%']]
        } ) )
  obs_pi <- pi_tilde_m

  # setup scoring for theta
  pred_theta <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_theta_tilde[[j]])[['50%']]
        } ) )
  pred_theta_lo <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_theta_tilde[[j]])[['5%']]
        } ) )
  pred_theta_hi <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_theta_tilde[[j]])[['95%']]
        } ) )
  obs_theta <- theta_tilde[,c('pi_tilde_1','pi_tilde_2','pi_tilde_3'),with=F]

  # bias
  bias_pi_SS_str_Bern <-
    sapply(1:J,function(j){bias(pred = pred_pi[[j]],obs = obs_pi[[j]])})
  bias_theta_SS_str_Bern <-
    sapply(1:J,function(j){bias(pred = pred_theta[[j]],obs = obs_theta[[j]])})

  # rmse
  rmse_pi_SS_str_Bern <-
    sapply(1:J,function(j){rmse(pred = pred_pi[[j]],obs = obs_pi[[j]])})
  rmse_theta_SS_str_Bern <-
    sapply(1:J,function(j){rmse(pred = pred_theta[[j]],obs = obs_theta[[j]])})

  # correlation
  pearson_pi_SS_str_Bern <-
    sapply(1:J,function(j){pearson(pred = pred_pi[[j]],obs = obs_pi[[j]])})
  pearson_theta_SS_str_Bern <-
    sapply(1:J,function(j){pearson(pred = pred_theta[[j]],obs = obs_theta[[j]])})

  # coverage
  cover_pi_SS_str_Bern <-
    sapply(1:J,function(j){cover(pred_lo = pred_pi_lo[[j]],pred_hi = pred_pi_hi[[j]],obs = obs_pi[[j]])})
  cover_theta_SS_str_Bern <-
    sapply(1:J,function(j){cover(pred_lo = pred_theta_lo[[j]],pred_hi = pred_theta_hi[[j]],obs = obs_theta[[j]])})
}
  # # # # # # # # #
  # # # # # # # # # FIT ROUTINE:
  # # # # # # # # # SELECTED SAMPLE
  # # # # # # # # # STRUCTURED MODEL
  # # # # # # # # # NO-OFFSET
  # # # # # # # # # MULTINOMIAL
  # # # # # # # # #
{
  multi.list <- list(
    y =
      apply(
        selected_sample[,grepl('j',names(selected_sample)),with=F],
        1,
        function(x){which(x==1)}
      ),
    J = J
  )

  train_data_list <-
    append(
      train_data_list[names(train_data_list)!='y'],
      multi.list
    )

  pars =
    c('alpha_star',
      'eta_V1_star','eta_V2_star','eta_V3_star',
      'gamma_star','beta',
      'psi','phi','omega','spatial_scale'
    )

  fit_object <-
    stan(
      file = "sims_multinomial.stan",
      pars = pars,
      data = train_data_list,
      iter = 500,
      warmup = 250,
      refresh = 1,
      thin = 4,
      cores = 8,
      chains = 8,
      control = list(max_treedepth = 10,adapt_delta = 0.8),
      verbose = TRUE
    )

  # extract parameters
  pars_sims <-
    rstan::extract(
      fit_object,
      pars = pars,
      inc_warmup = FALSE
    )

  # make predictions for each simulation
  pi_tilde_m_est_list <- list()
  theta_tilde_est_list <- list()


  for(s in 1:dim(pars_sims$alpha_star)[1]){
    # calculate latent propensity
    mu_tilde_est <-
      sapply(
        1:J,
        function(j){
          pars_sims$alpha_star[s,j] +
            pars_sims$eta_V1_star[s,frame$V1,j] +
            pars_sims$eta_V2_star[s,frame$V2,j] +
            pars_sims$eta_V3_star[s,frame$V3,j] +
            pars_sims$gamma_star[s,frame$area_id,j] +
            pars_sims$beta[s,j]*frame$Z
        }
      )
    # calculate cell-level choice probability
    pi_tilde_m_est <- softmax(mu_tilde_est)
    colnames(pi_tilde_m_est) <- paste('pi_tilde_m_est',1:J,sep="_")

    # store simulations for cell-level probability
    pi_tilde_m_est_list <-
      append(
        pi_tilde_m_est_list,
        list(as.data.table(pi_tilde_m_est))
      )

    # calculate stratified estimate
    theta_tilde_est <-
      as.data.table(
        cbind(pi_tilde_m_est,frame)
      )[,
        lapply(.SD,function(x){sum(x*w)/sum(w)}),
        by = c('area_id'),
        .SDcols = c(colnames(pi_tilde_m_est))
      ]
    theta_tilde_est <- theta_tilde_est[order(theta_tilde_est $area_id)]

    # store simulations for area-level chocie probability
    theta_tilde_est_list <-
      append(
        theta_tilde_est_list,
        list(theta_tilde_est)
      )
  }

  # calculate summaries over simulations
  estimate_summary_pi_tilde_m <-
    lapply(
      1:J,
      function(j){
        t(
          apply(
            sapply(
              1:dim(pars_sims$alpha_star)[1],
              function(s){
                pi_tilde_m_est_list[[s]][[paste('pi_tilde_m_est',j,sep='_')]]
              }
            ),
            1,
            function(x){
              quantile(x,c(0.05,0.5,0.95))
            }
          )
        )
      }
    )

  estimate_summary_theta_tilde <-
    lapply(
      1:J,
      function(j){
        t(
          apply(
            sapply(
              1:dim(pars_sims$alpha_star)[1],
              function(s){
                theta_tilde_est_list[[s]][[paste('pi_tilde_m_est',j,sep='_')]]
              }
            ),
            1,
            function(x){
              quantile(x,c(0.05,0.5,0.95))
            }
          )
        )
      }
    )

  # # # CALCULATE PERFORMANCE

  # setup scoring for pi_m
  pred_pi <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_pi_tilde_m[[j]])[['50%']]
        } ) )
  pred_pi_lo <-
    as.data.table(
      sapply(
        1:J,
        function(j){as.data.table(estimate_summary_pi_tilde_m[[j]])[['5%']]
        } ) )
  pred_pi_hi <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_pi_tilde_m[[j]])[['95%']]
        } ) )
  obs_pi <- pi_tilde_m

  # setup scoring for theta
  pred_theta <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_theta_tilde[[j]])[['50%']]
        } ) )
  pred_theta_lo <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_theta_tilde[[j]])[['5%']]
        } ) )
  pred_theta_hi <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_theta_tilde[[j]])[['95%']]
        } ) )
  obs_theta <- theta_tilde[,c('pi_tilde_1','pi_tilde_2','pi_tilde_3'),with=F]

  # bias
  bias_pi_SS_str_Multi <-
    sapply(1:J,function(j){bias(pred = pred_pi[[j]],obs = obs_pi[[j]])})
  bias_theta_SS_str_Multi <-
    sapply(1:J,function(j){bias(pred = pred_theta[[j]],obs = obs_theta[[j]])})

  # rmse
  rmse_pi_SS_str_Multi <-
    sapply(1:J,function(j){rmse(pred = pred_pi[[j]],obs = obs_pi[[j]])})
  rmse_theta_SS_str_Multi <-
    sapply(1:J,function(j){rmse(pred = pred_theta[[j]],obs = obs_theta[[j]])})

  # correlation
  pearson_pi_SS_str_Multi <-
    sapply(1:J,function(j){pearson(pred = pred_pi[[j]],obs = obs_pi[[j]])})
  pearson_theta_SS_str_Multi <-
    sapply(1:J,function(j){pearson(pred = pred_theta[[j]],obs = obs_theta[[j]])})

  # coverage
  cover_pi_SS_str_Multi <-
    sapply(1:J,function(j){cover(pred_lo = pred_pi_lo[[j]],pred_hi = pred_pi_hi[[j]],obs = obs_pi[[j]])})
  cover_theta_SS_str_Multi <-
    sapply(1:J,function(j){cover(pred_lo = pred_theta_lo[[j]],pred_hi = pred_theta_hi[[j]],obs = obs_theta[[j]])})
}
  # # # # # # # # #
  # # # # # # # # # FIT ROUTINE:
  # # # # # # # # # SELECTED SAMPLE
  # # # # # # # # # UNSTRUCTURED MODEL
  # # # # # # # # # NO-OFFSET
  # # # # # # # # # BERNOULLI
  # # # # # # # # #
{
  fit_object <- list()
  for(j in 1:J){

    train_data_list <-
      list(
        n = n,
        y = selected_sample[[paste('j',j,sep="_")]],
        offset = 0,

        Z = selected_sample$Z,

        area_id = selected_sample$area_id,
        area_N =  area_N,

        k = icar.data$k,
        group_size = icar.data$group_size,
        group_idx = icar.data$group_idx,
        N_edges = icar.data$n_edges,
        node1 = icar.data$node1,
        node2 = icar.data$node2,
        comp_id = icar.data$comp_id,

        inv_sqrt_scaling_factor = as.matrix(icar.data$inv_sqrt_scale_factor)
      )

    train_data_list <- append(train_data_list, id_list)

    pars =
      c('alpha_star',
        'eta_V1_star','eta_V2_star','eta_V3_star',
        'gamma_star','beta'
      )

    fit_object_temp <-
      stan(
        file = "sims_traditional.stan",
        pars = pars,
        data = train_data_list,
        iter = 500,
        warmup = 250,
        refresh = 1,
        thin = 4,
        cores = 8,
        chains = 8,
        control = list(max_treedepth = 10,adapt_delta = 0.8),
        verbose = TRUE
      )

    fit_object <- append(fit_object,list(fit_object_temp))

  }

  # extract parameters
  pars_sims <-
    lapply(1:J,
           function(j){
             rstan::extract(
               fit_object[[j]],
               pars = pars,
               inc_warmup = FALSE
             )
           } )

  # make predictions for each simulation
  pi_tilde_m_est_list <- list()
  theta_tilde_est_list <- list()

  for(s in 1:length(pars_sims[[1]]$alpha_star)){
    # calculate latent propensity
    mu_tilde_est <-
      sapply(
        1:J,
        function(j){
          pars_sims[[j]]$alpha_star[s] +
            pars_sims[[j]]$eta_V1_star[s,frame$V1] +
            pars_sims[[j]]$eta_V2_star[s,frame$V2] +
            pars_sims[[j]]$eta_V3_star[s,frame$V3] +
            pars_sims[[j]]$gamma_star[s,frame$area_id] +
            pars_sims[[j]]$beta[s]*frame$Z
        }
      )
    # calculate cell-level choice probability
    pi_tilde_m_est <- exp(mu_tilde_est)/(1+exp(mu_tilde_est))#softmax(mu_tilde_est)
    colnames(pi_tilde_m_est) <- paste('pi_tilde_m_est',1:J,sep="_")

    # store simulations for cell-level probability
    pi_tilde_m_est_list <-
      append(
        pi_tilde_m_est_list,
        list(as.data.table(pi_tilde_m_est))
      )

    # calculate stratified estimate
    theta_tilde_est <-
      as.data.table(
        cbind(pi_tilde_m_est,frame)
      )[,
        lapply(.SD,function(x){sum(x*w)/sum(w)}),
        by = c('area_id'),
        .SDcols = c(colnames(pi_tilde_m_est))
      ]
    theta_tilde_est <- theta_tilde_est[order(theta_tilde_est $area_id)]

    # store simulations for area-level chocie probability
    theta_tilde_est_list <-
      append(
        theta_tilde_est_list,
        list(theta_tilde_est)
      )
  }

  # calculate summaries over simulations
  estimate_summary_pi_tilde_m <-
    lapply(
      1:J,
      function(j){
        t(
          apply(
            sapply(
              1:length(pars_sims[[1]]$alpha_star),
              function(s){
                pi_tilde_m_est_list[[s]][[paste('pi_tilde_m_est',j,sep='_')]]
              }
            ),
            1,
            function(x){
              quantile(x,c(0.05,0.5,0.95))
            }
          )
        )
      }
    )

  estimate_summary_theta_tilde <-
    lapply(
      1:J,
      function(j){
        t(
          apply(
            sapply(
              1:length(pars_sims[[1]]$alpha_star),
              function(s){
                theta_tilde_est_list[[s]][[paste('pi_tilde_m_est',j,sep='_')]]
              }
            ),
            1,
            function(x){
              quantile(x,c(0.05,0.5,0.95))
            }
          )
        )
      }
    )

  # # # CALCULATE PERFORMANCE

  # setup scoring for pi_m
  pred_pi <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_pi_tilde_m[[j]])[['50%']]
        } ) )
  pred_pi_lo <-
    as.data.table(
      sapply(
        1:J,
        function(j){as.data.table(estimate_summary_pi_tilde_m[[j]])[['5%']]
        } ) )
  pred_pi_hi <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_pi_tilde_m[[j]])[['95%']]
        } ) )
  obs_pi <- pi_tilde_m

  # setup scoring for theta
  pred_theta <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_theta_tilde[[j]])[['50%']]
        } ) )
  pred_theta_lo <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_theta_tilde[[j]])[['5%']]
        } ) )
  pred_theta_hi <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_theta_tilde[[j]])[['95%']]
        } ) )
  obs_theta <- theta_tilde[,c('pi_tilde_1','pi_tilde_2','pi_tilde_3'),with=F]

  # bias
  bias_pi_SS_unstr_Bern <-
    sapply(1:J,function(j){bias(pred = pred_pi[[j]],obs = obs_pi[[j]])})
  bias_theta_SS_unstr_Bern <-
    sapply(1:J,function(j){bias(pred = pred_theta[[j]],obs = obs_theta[[j]])})

  # rmse
  rmse_pi_SS_unstr_Bern <-
    sapply(1:J,function(j){rmse(pred = pred_pi[[j]],obs = obs_pi[[j]])})
  rmse_theta_SS_unstr_Bern <-
    sapply(1:J,function(j){rmse(pred = pred_theta[[j]],obs = obs_theta[[j]])})

  # correlation
  pearson_pi_SS_unstr_Bern <-
    sapply(1:J,function(j){pearson(pred = pred_pi[[j]],obs = obs_pi[[j]])})
  pearson_theta_SS_unstr_Bern <-
    sapply(1:J,function(j){pearson(pred = pred_theta[[j]],obs = obs_theta[[j]])})

  # coverage
  cover_pi_SS_unstr_Bern <-
    sapply(1:J,function(j){cover(pred_lo = pred_pi_lo[[j]],pred_hi = pred_pi_hi[[j]],obs = obs_pi[[j]])})
  cover_theta_SS_unstr_Bern <-
    sapply(1:J,function(j){cover(pred_lo = pred_theta_lo[[j]],pred_hi = pred_theta_hi[[j]],obs = obs_theta[[j]])})
}
  # # # # # # # # #
  # # # # # # # # # FIT ROUTINE:
  # # # # # # # # # SELECTED SAMPLE
  # # # # # # # # # UNSTRUCTURED MODEL
  # # # # # # # # # NO-OFFSET
  # # # # # # # # # MULTINOMIAL
  # # # # # # # # #
{
  multi.list <- list(
    y =
      apply(
        selected_sample[,grepl('j',names(selected_sample)),with=F],
        1,
        function(x){which(x==1)}
      ),
    J = J
  )

  train_data_list <-
    append(
      train_data_list[names(train_data_list)!='y'],
      multi.list
    )

  pars =
    c('alpha_star',
      'eta_V1_star','eta_V2_star','eta_V3_star',
      'gamma_star','beta'
    )

  fit_object <-
    stan(
      file = "sims_traditional_multinomial.stan",
      pars = pars,
      data = train_data_list,
      iter = 500,
      warmup = 250,
      refresh = 1,
      thin = 4,
      cores = 8,
      chains = 8,
      control = list(max_treedepth = 10,adapt_delta = 0.8),
      verbose = TRUE
    )

  # extract parameters
  pars_sims <-
    rstan::extract(
      fit_object,
      pars = pars,
      inc_warmup = FALSE
    )

  # make predictions for each simulation
  pi_tilde_m_est_list <- list()
  theta_tilde_est_list <- list()


  for(s in 1:dim(pars_sims$alpha_star)[1]){
    # calculate latent propensity
    mu_tilde_est <-
      sapply(
        1:J,
        function(j){
          pars_sims$alpha_star[s,j] +
            pars_sims$eta_V1_star[s,frame$V1,j] +
            pars_sims$eta_V2_star[s,frame$V2,j] +
            pars_sims$eta_V3_star[s,frame$V3,j] +
            pars_sims$gamma_star[s,frame$area_id,j] +
            pars_sims$beta[s,j]*frame$Z
        }
      )
    # calculate cell-level choice probability
    pi_tilde_m_est <- softmax(mu_tilde_est)
    colnames(pi_tilde_m_est) <- paste('pi_tilde_m_est',1:J,sep="_")

    # store simulations for cell-level probability
    pi_tilde_m_est_list <-
      append(
        pi_tilde_m_est_list,
        list(as.data.table(pi_tilde_m_est))
      )

    # calculate stratified estimate
    theta_tilde_est <-
      as.data.table(
        cbind(pi_tilde_m_est,frame)
      )[,
        lapply(.SD,function(x){sum(x*w)/sum(w)}),
        by = c('area_id'),
        .SDcols = c(colnames(pi_tilde_m_est))
      ]
    theta_tilde_est <- theta_tilde_est[order(theta_tilde_est $area_id)]

    # store simulations for area-level chocie probability
    theta_tilde_est_list <-
      append(
        theta_tilde_est_list,
        list(theta_tilde_est)
      )
  }

  # calculate summaries over simulations
  estimate_summary_pi_tilde_m <-
    lapply(
      1:J,
      function(j){
        t(
          apply(
            sapply(
              1:dim(pars_sims$alpha_star)[1],
              function(s){
                pi_tilde_m_est_list[[s]][[paste('pi_tilde_m_est',j,sep='_')]]
              }
            ),
            1,
            function(x){
              quantile(x,c(0.05,0.5,0.95))
            }
          )
        )
      }
    )

  estimate_summary_theta_tilde <-
    lapply(
      1:J,
      function(j){
        t(
          apply(
            sapply(
              1:dim(pars_sims$alpha_star)[1],
              function(s){
                theta_tilde_est_list[[s]][[paste('pi_tilde_m_est',j,sep='_')]]
              }
            ),
            1,
            function(x){
              quantile(x,c(0.05,0.5,0.95))
            }
          )
        )
      }
    )

  # # # CALCULATE PERFORMANCE

  # setup scoring for pi_m
  pred_pi <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_pi_tilde_m[[j]])[['50%']]
        } ) )
  pred_pi_lo <-
    as.data.table(
      sapply(
        1:J,
        function(j){as.data.table(estimate_summary_pi_tilde_m[[j]])[['5%']]
        } ) )
  pred_pi_hi <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_pi_tilde_m[[j]])[['95%']]
        } ) )
  obs_pi <- pi_tilde_m

  # setup scoring for theta
  pred_theta <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_theta_tilde[[j]])[['50%']]
        } ) )
  pred_theta_lo <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_theta_tilde[[j]])[['5%']]
        } ) )
  pred_theta_hi <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_theta_tilde[[j]])[['95%']]
        } ) )
  obs_theta <- theta_tilde[,c('pi_tilde_1','pi_tilde_2','pi_tilde_3'),with=F]

  # bias
  bias_pi_SS_unstr_Multi <-
    sapply(1:J,function(j){bias(pred = pred_pi[[j]],obs = obs_pi[[j]])})
  bias_theta_SS_unstr_Multi <-
    sapply(1:J,function(j){bias(pred = pred_theta[[j]],obs = obs_theta[[j]])})

  # rmse
  rmse_pi_SS_unstr_Multi <-
    sapply(1:J,function(j){rmse(pred = pred_pi[[j]],obs = obs_pi[[j]])})
  rmse_theta_SS_unstr_Multi <-
    sapply(1:J,function(j){rmse(pred = pred_theta[[j]],obs = obs_theta[[j]])})

  # correlation
  pearson_pi_SS_unstr_Multi <-
    sapply(1:J,function(j){pearson(pred = pred_pi[[j]],obs = obs_pi[[j]])})
  pearson_theta_SS_unstr_Multi <-
    sapply(1:J,function(j){pearson(pred = pred_theta[[j]],obs = obs_theta[[j]])})

  # coverage
  cover_pi_SS_unstr_Multi <-
    sapply(1:J,function(j){cover(pred_lo = pred_pi_lo[[j]],pred_hi = pred_pi_hi[[j]],obs = obs_pi[[j]])})
  cover_theta_SS_unstr_Multi <-
    sapply(1:J,function(j){cover(pred_lo = pred_theta_lo[[j]],pred_hi = pred_theta_hi[[j]],obs = obs_theta[[j]])})
}
  # # # # # # # # #
  # # # # # # # # # FIT ROUTINE:
  # # # # # # # # # SELECTED SAMPLE
  # # # # # # # # # STRUCTURED MODEL
  # # # # # # # # # OFFSET
  # # # # # # # # # BERNOULLI
  # # # # # # # # #
{
  fit_object <- list()
  for(j in 1:J){

    train_data_list <-
      list(
        n = n,
        y = selected_sample[[paste('j',j,sep="_")]],
        offset = offset[paste('j',j,sep = '_')],

        Z = selected_sample$Z,

        area_id = selected_sample$area_id,
        area_N =  area_N,

        k = icar.data$k,
        group_size = icar.data$group_size,
        group_idx = icar.data$group_idx,
        N_edges = icar.data$n_edges,
        node1 = icar.data$node1,
        node2 = icar.data$node2,
        comp_id = icar.data$comp_id,

        inv_sqrt_scaling_factor = as.matrix(icar.data$inv_sqrt_scale_factor)
      )

    train_data_list <- append(train_data_list , id_list)

    pars =
      c('alpha_star',
        'eta_V1_star','eta_V2_star','eta_V3_star',
        'gamma_star','beta',
        'psi','phi','omega','spatial_scale'
      )

    fit_object_temp <-
      stan(
        file = "sims.stan",
        pars = pars,
        data = train_data_list,
        iter = 500,
        warmup = 250,
        refresh = 1,
        thin = 4,
        cores = 8,
        chains = 8,
        control = list(max_treedepth = 10,adapt_delta = 0.8),
        verbose = TRUE
      )

    fit_object <- append(fit_object,list(fit_object_temp))

  }

  # make stratified prediction

  # extract parameters
  pars_sims <-
    lapply(1:J,
           function(j){
             rstan::extract(
               fit_object[[j]],
               pars = pars,
               inc_warmup = FALSE
             )
           } )

  # make predictions for each simulation
  pi_tilde_m_est_list <- list()
  theta_tilde_est_list <- list()

  for(s in 1:length(pars_sims[[1]]$alpha_star)){
    # calculate latent propensity
    mu_tilde_est <-
      sapply(
        1:J,
        function(j){
          pars_sims[[j]]$alpha_star[s] +
            pars_sims[[j]]$eta_V1_star[s,frame$V1] +
            pars_sims[[j]]$eta_V2_star[s,frame$V2] +
            pars_sims[[j]]$eta_V3_star[s,frame$V3] +
            pars_sims[[j]]$gamma_star[s,frame$area_id] +
            pars_sims[[j]]$beta[s]*frame$Z
        }
      )
    # calculate cell-level choice probability
    pi_tilde_m_est <- exp(mu_tilde_est)/(1+exp(mu_tilde_est))
    colnames(pi_tilde_m_est) <- paste('pi_tilde_m_est',1:J,sep="_")

    # store simulations for cell-level probability
    pi_tilde_m_est_list <-
      append(
        pi_tilde_m_est_list,
        list(as.data.table(pi_tilde_m_est))
      )

    # calculate stratified estimate
    theta_tilde_est <-
      as.data.table(
        cbind(pi_tilde_m_est,frame)
      )[,
        lapply(.SD,function(x){sum(x*w)/sum(w)}),
        by = c('area_id'),
        .SDcols = c(colnames(pi_tilde_m_est))
      ]
    theta_tilde_est <- theta_tilde_est[order(theta_tilde_est $area_id)]

    # store simulations for area-level chocie probability
    theta_tilde_est_list <-
      append(
        theta_tilde_est_list,
        list(theta_tilde_est)
      )
  }

  # calculate summaries over simulations
  estimate_summary_pi_tilde_m <-
    lapply(
      1:J,
      function(j){
        t(
          apply(
            sapply(
              1:length(pars_sims[[1]]$alpha_star),
              function(s){
                pi_tilde_m_est_list[[s]][[paste('pi_tilde_m_est',j,sep='_')]]
              }
            ),
            1,
            function(x){
              quantile(x,c(0.05,0.5,0.95))
            }
          )
        )
      }
    )

  estimate_summary_theta_tilde <-
    lapply(
      1:J,
      function(j){
        t(
          apply(
            sapply(
              1:length(pars_sims[[1]]$alpha_star),
              function(s){
                theta_tilde_est_list[[s]][[paste('pi_tilde_m_est',j,sep='_')]]
              }
            ),
            1,
            function(x){
              quantile(x,c(0.05,0.5,0.95))
            }
          )
        )
      }
    )

  # # # CALCULATE PERFORMANCE

  # setup scoring for pi_m
  pred_pi <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_pi_tilde_m[[j]])[['50%']]
        } ) )
  pred_pi_lo <-
    as.data.table(
      sapply(
        1:J,
        function(j){as.data.table(estimate_summary_pi_tilde_m[[j]])[['5%']]
        } ) )
  pred_pi_hi <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_pi_tilde_m[[j]])[['95%']]
        } ) )
  obs_pi <- pi_tilde_m

  # setup scoring for theta
  pred_theta <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_theta_tilde[[j]])[['50%']]
        } ) )
  pred_theta_lo <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_theta_tilde[[j]])[['5%']]
        } ) )
  pred_theta_hi <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_theta_tilde[[j]])[['95%']]
        } ) )
  obs_theta <- theta_tilde[,c('pi_tilde_1','pi_tilde_2','pi_tilde_3'),with=F]

  # bias
  bias_pi_SS.correct_str_Bern <-
    sapply(1:J,function(j){bias(pred = pred_pi[[j]],obs = obs_pi[[j]])})
  bias_theta_SS.correct_str_Bern <-
    sapply(1:J,function(j){bias(pred = pred_theta[[j]],obs = obs_theta[[j]])})

  # rmse
  rmse_pi_SS.correct_str_Bern <-
    sapply(1:J,function(j){rmse(pred = pred_pi[[j]],obs = obs_pi[[j]])})
  rmse_theta_SS.correct_str_Bern <-
    sapply(1:J,function(j){rmse(pred = pred_theta[[j]],obs = obs_theta[[j]])})

  # correlation
  pearson_pi_SS.correct_str_Bern <-
    sapply(1:J,function(j){pearson(pred = pred_pi[[j]],obs = obs_pi[[j]])})
  pearson_theta_SS.correct_str_Bern <-
    sapply(1:J,function(j){pearson(pred = pred_theta[[j]],obs = obs_theta[[j]])})

  # coverage
  cover_pi_SS.correct_str_Bern <-
    sapply(1:J,function(j){cover(pred_lo = pred_pi_lo[[j]],pred_hi = pred_pi_hi[[j]],obs = obs_pi[[j]])})
  cover_theta_SS.correct_str_Bern <-
    sapply(1:J,function(j){cover(pred_lo = pred_theta_lo[[j]],pred_hi = pred_theta_hi[[j]],obs = obs_theta[[j]])})
}
# # # # # # # # #
# # # # # # # # # FIT ROUTINE:
# # # # # # # # # SELECTED SAMPLE
# # # # # # # # # UNSTRUCTURED MODEL
# # # # # # # # # OFFSET
# # # # # # # # # BERNOULLI
# # # # # # # # #
{
  fit_object <- list()
  for(j in 1:J){

    train_data_list <-
      list(
        n = n,
        y = selected_sample[[paste('j',j,sep="_")]],
        offset = offset[paste('j',j,sep = '_')],

        Z = selected_sample$Z,

        area_id = selected_sample$area_id,
        area_N =  area_N,

        k = icar.data$k,
        group_size = icar.data$group_size,
        group_idx = icar.data$group_idx,
        N_edges = icar.data$n_edges,
        node1 = icar.data$node1,
        node2 = icar.data$node2,
        comp_id = icar.data$comp_id,

        inv_sqrt_scaling_factor = as.matrix(icar.data$inv_sqrt_scale_factor)
      )

    train_data_list <- append(train_data_list , id_list)

    pars =
      c('alpha_star',
        'eta_V1_star','eta_V2_star','eta_V3_star',
        'gamma_star','beta'
      )

    fit_object_temp <-
      stan(
        file = "sims_traditional.stan",
        pars = pars,
        data = train_data_list,
        iter = 500,
        warmup = 250,
        refresh = 1,
        thin = 4,
        cores = 8,
        chains = 8,
        control = list(max_treedepth = 10,adapt_delta = 0.8),
        verbose = TRUE
      )

    fit_object <- append(fit_object,list(fit_object_temp))

  }

  # make stratified prediction

  # extract parameters
  pars_sims <-
    lapply(1:J,
           function(j){
             rstan::extract(
               fit_object[[j]],
               pars = pars,
               inc_warmup = FALSE
             )
           } )

  # make predictions for each simulation
  pi_tilde_m_est_list <- list()
  theta_tilde_est_list <- list()

  for(s in 1:length(pars_sims[[1]]$alpha_star)){
    # calculate latent propensity
    mu_tilde_est <-
      sapply(
        1:J,
        function(j){
          pars_sims[[j]]$alpha_star[s] +
            pars_sims[[j]]$eta_V1_star[s,frame$V1] +
            pars_sims[[j]]$eta_V2_star[s,frame$V2] +
            pars_sims[[j]]$eta_V3_star[s,frame$V3] +
            pars_sims[[j]]$gamma_star[s,frame$area_id] +
            pars_sims[[j]]$beta[s]*frame$Z
        }
      )
    # calculate cell-level choice probability
    pi_tilde_m_est <- exp(mu_tilde_est)/(1+exp(mu_tilde_est))
    colnames(pi_tilde_m_est) <- paste('pi_tilde_m_est',1:J,sep="_")

    # store simulations for cell-level probability
    pi_tilde_m_est_list <-
      append(
        pi_tilde_m_est_list,
        list(as.data.table(pi_tilde_m_est))
      )

    # calculate stratified estimate
    theta_tilde_est <-
      as.data.table(
        cbind(pi_tilde_m_est,frame)
      )[,
        lapply(.SD,function(x){sum(x*w)/sum(w)}),
        by = c('area_id'),
        .SDcols = c(colnames(pi_tilde_m_est))
      ]
    theta_tilde_est <- theta_tilde_est[order(theta_tilde_est $area_id)]

    # store simulations for area-level chocie probability
    theta_tilde_est_list <-
      append(
        theta_tilde_est_list,
        list(theta_tilde_est)
      )
  }

  # calculate summaries over simulations
  estimate_summary_pi_tilde_m <-
    lapply(
      1:J,
      function(j){
        t(
          apply(
            sapply(
              1:length(pars_sims[[1]]$alpha_star),
              function(s){
                pi_tilde_m_est_list[[s]][[paste('pi_tilde_m_est',j,sep='_')]]
              }
            ),
            1,
            function(x){
              quantile(x,c(0.05,0.5,0.95))
            }
          )
        )
      }
    )

  estimate_summary_theta_tilde <-
    lapply(
      1:J,
      function(j){
        t(
          apply(
            sapply(
              1:length(pars_sims[[1]]$alpha_star),
              function(s){
                theta_tilde_est_list[[s]][[paste('pi_tilde_m_est',j,sep='_')]]
              }
            ),
            1,
            function(x){
              quantile(x,c(0.05,0.5,0.95))
            }
          )
        )
      }
    )

  # # # CALCULATE PERFORMANCE

  # setup scoring for pi_m
  pred_pi <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_pi_tilde_m[[j]])[['50%']]
        } ) )
  pred_pi_lo <-
    as.data.table(
      sapply(
        1:J,
        function(j){as.data.table(estimate_summary_pi_tilde_m[[j]])[['5%']]
        } ) )
  pred_pi_hi <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_pi_tilde_m[[j]])[['95%']]
        } ) )
  obs_pi <- pi_tilde_m

  # setup scoring for theta
  pred_theta <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_theta_tilde[[j]])[['50%']]
        } ) )
  pred_theta_lo <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_theta_tilde[[j]])[['5%']]
        } ) )
  pred_theta_hi <-
    as.data.table(
      sapply(
        1:J,
        function(j){
          as.data.table(estimate_summary_theta_tilde[[j]])[['95%']]
        } ) )
  obs_theta <- theta_tilde[,c('pi_tilde_1','pi_tilde_2','pi_tilde_3'),with=F]

  # bias
  bias_pi_SS.correct_unstr_Bern <-
    sapply(1:J,function(j){bias(pred = pred_pi[[j]],obs = obs_pi[[j]])})
  bias_theta_SS.correct_unstr_Bern <-
    sapply(1:J,function(j){bias(pred = pred_theta[[j]],obs = obs_theta[[j]])})

  # rmse
  rmse_pi_SS.correct_unstr_Bern <-
    sapply(1:J,function(j){rmse(pred = pred_pi[[j]],obs = obs_pi[[j]])})
  rmse_theta_SS.correct_unstr_Bern <-
    sapply(1:J,function(j){rmse(pred = pred_theta[[j]],obs = obs_theta[[j]])})

  # correlation
  pearson_pi_SS.correct_unstr_Bern <-
    sapply(1:J,function(j){pearson(pred = pred_pi[[j]],obs = obs_pi[[j]])})
  pearson_theta_SS.correct_unstr_Bern <-
    sapply(1:J,function(j){pearson(pred = pred_theta[[j]],obs = obs_theta[[j]])})

  # coverage
  cover_pi_SS.correct_unstr_Bern <-
    sapply(1:J,function(j){cover(pred_lo = pred_pi_lo[[j]],pred_hi = pred_pi_hi[[j]],obs = obs_pi[[j]])})
  cover_theta_SS.correct_unstr_Bern <-
    sapply(1:J,function(j){cover(pred_lo = pred_theta_lo[[j]],pred_hi = pred_theta_hi[[j]],obs = obs_theta[[j]])})
}
# # # # # # # # #
# # # # # # # # # STORE RESULTS:
# # # # # # # # #
{
  store.list =
    rbindlist(
      list(

        store.list,

        data.table(

          # simulation id
          sim_id = sim,

          # population parameters
          prevalence.pop = N1/N,
          rho_X = rep(rho_X,J),
          alpha = alpha,
          omega = omega,
          beta = beta,

          # sample parameters
          n = rep(n,J),
          prevalence.RS = n1.RS/n,
          penalty = penalty,
          var.penalty = var.penalty,
          prevalence.SS = n1/n,

          # BIAS
          bias_pi_RS_str_Bern = bias_pi_RS_str_Bern,
          bias_theta_RS_str_Bern = bias_theta_RS_str_Bern,
          bias_pi_SS_str_Bern = bias_pi_SS_str_Bern,
          bias_theta_SS_str_Bern = bias_theta_SS_str_Bern,
          bias_pi_SS.correct_str_Bern = bias_pi_SS.correct_str_Bern,
          bias_theta_SS.correct_str_Bern = bias_theta_SS.correct_str_Bern,

          bias_pi_RS_unstr_Bern = bias_pi_RS_unstr_Bern,
          bias_theta_RS_unstr_Bern = bias_theta_RS_unstr_Bern,
          bias_pi_SS_unstr_Bern = bias_pi_SS_unstr_Bern,
          bias_theta_SS_unstr_Bern = bias_theta_SS_unstr_Bern,
          bias_pi_SS.correct_unstr_Bern = bias_pi_SS.correct_unstr_Bern,
          bias_theta_SS.correct_unstr_Bern = bias_theta_SS.correct_unstr_Bern,

          bias_pi_RS_str_Multi = bias_pi_RS_str_Multi,
          bias_theta_RS_str_Multi = bias_theta_RS_str_Multi,
          bias_pi_SS_str_Multi = bias_pi_SS_str_Multi,
          bias_theta_SS_str_Multi = bias_theta_SS_str_Multi,

          bias_pi_RS_unstr_Multi = bias_pi_RS_unstr_Multi,
          bias_theta_RS_unstr_Multi = bias_theta_RS_unstr_Multi,
          bias_pi_SS_unstr_Multi = bias_pi_SS_unstr_Multi,
          bias_theta_SS_unstr_Multi = bias_theta_SS_unstr_Multi,

          # RMSE
          rmse_pi_RS_str_Bern = rmse_pi_RS_str_Bern,
          rmse_theta_RS_str_Bern = rmse_theta_RS_str_Bern,
          rmse_pi_SS_str_Bern = rmse_pi_SS_str_Bern,
          rmse_theta_SS_str_Bern = rmse_theta_SS_str_Bern,
          rmse_pi_SS.correct_str_Bern = rmse_pi_SS.correct_str_Bern,
          rmse_theta_SS.correct_str_Bern = rmse_theta_SS.correct_str_Bern,

          rmse_pi_RS_unstr_Bern = rmse_pi_RS_unstr_Bern,
          rmse_theta_RS_unstr_Bern = rmse_theta_RS_unstr_Bern,
          rmse_pi_SS_unstr_Bern = rmse_pi_SS_unstr_Bern,
          rmse_theta_SS_unstr_Bern = rmse_theta_SS_unstr_Bern,
          rmse_pi_SS.correct_unstr_Bern = rmse_pi_SS.correct_unstr_Bern,
          rmse_theta_SS.correct_unstr_Bern = rmse_theta_SS.correct_unstr_Bern,

          rmse_pi_RS_str_Multi = rmse_pi_RS_str_Multi,
          rmse_theta_RS_str_Multi = rmse_theta_RS_str_Multi,
          rmse_pi_SS_str_Multi = rmse_pi_SS_str_Multi,
          rmse_theta_SS_str_Multi = rmse_theta_SS_str_Multi,

          rmse_pi_RS_unstr_Multi = rmse_pi_RS_unstr_Multi,
          rmse_theta_RS_unstr_Multi = rmse_theta_RS_unstr_Multi,
          rmse_pi_SS_unstr_Multi = rmse_pi_SS_unstr_Multi,
          rmse_theta_SS_unstr_Multi = rmse_theta_SS_unstr_Multi,

          # PEARSON CORRELATION
          pearson_pi_RS_str_Bern = pearson_pi_RS_str_Bern,
          pearson_theta_RS_str_Bern = pearson_theta_RS_str_Bern,
          pearson_pi_SS_str_Bern = pearson_pi_SS_str_Bern,
          pearson_theta_SS_str_Bern = pearson_theta_SS_str_Bern,
          pearson_pi_SS.correct_str_Bern = pearson_pi_SS.correct_str_Bern,
          pearson_theta_SS.correct_str_Bern = pearson_theta_SS.correct_str_Bern,

          pearson_pi_RS_unstr_Bern = pearson_pi_RS_unstr_Bern,
          pearson_theta_RS_unstr_Bern = pearson_theta_RS_unstr_Bern,
          pearson_pi_SS_unstr_Bern = pearson_pi_SS_unstr_Bern,
          pearson_theta_SS_unstr_Bern = pearson_theta_SS_unstr_Bern,
          pearson_pi_SS.correct_unstr_Bern = pearson_pi_SS.correct_unstr_Bern,
          pearson_theta_SS.correct_unstr_Bern = pearson_theta_SS.correct_unstr_Bern,

          pearson_pi_RS_str_Multi = pearson_pi_RS_str_Multi,
          pearson_theta_RS_str_Multi = pearson_theta_RS_str_Multi,
          pearson_pi_SS_str_Multi = pearson_pi_SS_str_Multi,
          pearson_theta_SS_str_Multi = pearson_theta_SS_str_Multi,

          pearson_pi_RS_unstr_Multi = pearson_pi_RS_unstr_Multi,
          pearson_theta_RS_unstr_Multi = pearson_theta_RS_unstr_Multi,
          pearson_pi_SS_unstr_Multi = pearson_pi_SS_unstr_Multi,
          pearson_theta_SS_unstr_Multi = pearson_theta_SS_unstr_Multi,

          # COVERAGE OF THE 90% INTERVAL
          cover_pi_RS_str_Bern = cover_pi_RS_str_Bern,
          cover_theta_RS_str_Bern = cover_theta_RS_str_Bern,
          cover_pi_SS_str_Bern = cover_pi_SS_str_Bern,
          cover_theta_SS_str_Bern = cover_theta_SS_str_Bern,
          cover_pi_SS.correct_str_Bern = cover_pi_SS.correct_str_Bern,
          cover_theta_SS.correct_str_Bern = cover_theta_SS.correct_str_Bern,

          cover_pi_RS_unstr_Bern = cover_pi_RS_unstr_Bern,
          cover_theta_RS_unstr_Bern = cover_theta_RS_unstr_Bern,
          cover_pi_SS_unstr_Bern = cover_pi_SS_unstr_Bern,
          cover_theta_SS_unstr_Bern = cover_theta_SS_unstr_Bern,
          cover_pi_SS.correct_unstr_Bern = cover_pi_SS.correct_unstr_Bern,
          cover_theta_SS.correct_unstr_Bern = cover_theta_SS.correct_unstr_Bern,

          cover_pi_RS_str_Multi = cover_pi_RS_str_Multi,
          cover_theta_RS_str_Multi = cover_theta_RS_str_Multi,
          cover_pi_SS_str_Multi = cover_pi_SS_str_Multi,
          cover_theta_SS_str_Multi = cover_theta_SS_str_Multi,

          cover_pi_RS_unstr_Multi = cover_pi_RS_unstr_Multi,
          cover_theta_RS_unstr_Multi = cover_theta_RS_unstr_Multi,
          cover_pi_SS_unstr_Multi = cover_pi_SS_unstr_Multi,
          cover_theta_SS_unstr_Multi = cover_theta_SS_unstr_Multi

        )
      )
    )

}
  print(store.list)
  save(store.list,file = 'data_generated/simulations_extended.RData',compress = TRUE)
}



