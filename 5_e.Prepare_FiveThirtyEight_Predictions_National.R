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

# load 538 predictions
pred <- fread(file = 'data_auxiliary/f38_Model_Data_and_Predictions/presidential_national_toplines_2020.csv')

# focus on last 30 days of the campaign
pred$modeldate <- as.Date(pred$modeldate,'%m/%d/%Y')
pred$dte <- difftime(as.Date("11/03/2020",'%m/%d/%Y'),pred$modeldate,units = 'days')
pred <- pred[dte %in% 0:30]

# clean names
names(pred) <- gsub('national_|nat_','',names(pred))

vars <- 
  c('dte',
    'voteshare_inc_lo','voteshare_inc','voteshare_inc_hi',
    'voteshare_chal_lo','voteshare_chal','voteshare_chal_hi',
    'voteshare_other_lo','voteshare_other','voteshare_other_hi'
  )
pred <- pred[,..vars]

names(pred) <- gsub('inc','R',names(pred))
names(pred) <- gsub('chal','D',names(pred))


# For compatibility with our own method, I want to simulate from the posterior 
# impled by the 538 interval estimates. 
# This requires somehow to estimate the distribution from these estimates:
# I use the ideas outlined here: 
# https://stats.stackexchange.com/questions/327062/selecting-alpha-and-beta-parameters-for-a-beta-distribution-based-on-a-mode-and
# to derive a beta distribution from which I can simulate, which is uniquely
# compatible with the interval produced by 538.
#
# Define a few functions to do this:
#
# The objective function to minimise
#
objective.function <- function(params,q) {
  intended.quantiles <- q
  calculated.quantiles <- 
    qbeta(p=c(0.1, 0.5, 0.9), shape1=params[1], shape2=params[2])
  squared.error.quantiles <- sum((intended.quantiles - calculated.quantiles)^2)
  return(squared.error.quantiles)
}
#
# Derive useful starting values from normal approx
#
estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}

# for each prediction, extract the relevant quantiles


pred$shape1_R <- NA
pred$shape2_R <- NA
pred$rmse_R <- NA

pred$shape1_D <- NA
pred$shape2_D <- NA
pred$rmse_D <- NA

pred$shape1_other <- NA
pred$shape2_other <- NA
pred$rmse_other <- NA

for(j in c('R','D','other')){
  for(i in 1:nrow(pred)){
    
    q <- 
      as.numeric(
        pred[,grepl(paste('voteshare_',j,sep=''),names(pred)),with = FALSE][i]
      )/100
    
    # for the initial values we assume normality and estimate variance from 
    # 90% pred interval
    var <- ((q[3] -  q[1])/(qnorm(0.9)*2))^2
    med <- q[2] 
    
    # extract shape parameters from this hypothetical normal dist. 
    shape <- estBetaParams(mu = med,var = var)
    starting.params <- unlist(shape)
    
    # pass to numerical solver 
    nlm.result <- 
      nlm(
        f = objective.function, 
        p = starting.params, 
        q = q,
        print.level = 2,
        gradtol = 1e-100,
        steptol = 1e-100
      )
    
    # check final numbers to ensure approximation has worked 
    rmse <- 
      sqrt(
        mean(
          (
            q-
              qbeta(
                p = c(0.1, 0.5, 0.9), 
                shape1 = nlm.result$estimate[1], 
                shape2 = nlm.result$estimate[2]
              )
          )^2
        )
      )
    
    # store shapes 
    pred[[paste('shape1_',j,sep='')]][i] <-  nlm.result$estimate[1]
    pred[[paste('shape2_',j,sep='')]][i] <-  nlm.result$estimate[2]
    pred[[paste('rmse_',j,sep='')]][i] <- rmse
    
  } }



# simulate in the same format we do for the nat preds

n_sims <- 5000
f38_nat.pred <- data.table()
for(i in 1:nrow(pred)){
  temp <- 
    data.table(
      dte = pred$dte[i],
      D = 
        rbeta(
          n = n_sims,
          shape1 = pred[[
            names(pred)[grepl('D',names(pred)) & grepl('shape1',names(pred))]
          ]][i],
          shape2 = pred[[
            names(pred)[grepl('D',names(pred)) & grepl('shape2',names(pred))]
          ]][i]
        ),
      R = 
        rbeta(
          n = n_sims,
          shape1 = pred[[
            names(pred)[grepl('R',names(pred)) & grepl('shape1',names(pred))]
          ]][i],
          shape2 = pred[[
            names(pred)[grepl('R',names(pred)) & grepl('shape2',names(pred))]
          ]][i]
        ),
      other = 
        rbeta(
          n = n_sims,
          shape1 = pred[[
            names(pred)[grepl('other',names(pred)) & grepl('shape1',names(pred))]
          ]][i],
          shape2 = pred[[
            names(pred)[grepl('other',names(pred)) & grepl('shape2',names(pred))]
          ]][i]
        ),
      sim_id = 1:n_sims, 
      training_data = '538_forecast'
    )
  
  f38_nat.pred <- rbindlist(list(f38_nat.pred,temp))
}

# save object
save(f38_nat.pred,file = 'data_generated/f38_predicted_vote_national.RData')


# double check that quantiles of preds match 
chck_lo = 
  merge(
    pred[,c('dte','voteshare_D_lo','voteshare_R_lo','voteshare_other_lo')],
    f38_nat.pred[,
                  lapply(.SD,function(x){100*quantile(x,0.1)}),
                  .SDcols= c('D','R','other'),
                  by = c('dte')
    ],
    by.x = c('dte'),
    by.y = c('dte'),
    all = TRUE)

chck_point = 
  merge(
    pred[,c('dte','voteshare_D','voteshare_R','voteshare_other')],
    f38_nat.pred[,
                  lapply(.SD,function(x){100*quantile(x,0.5)}),
                  .SDcols= c('D','R','other'),
                  by = c('dte')
    ],
    by.x = c('dte'),
    by.y = c('dte'),
    all = TRUE)

chck_hi = 
  merge(
    pred[,c('dte','voteshare_D_hi','voteshare_R_hi','voteshare_other_hi')],
    f38_nat.pred[,
                  lapply(.SD,function(x){100*quantile(x,0.9)}),
                  .SDcols= c('D','R','other'),
                  by = c('dte')
    ],
    by.x = c('dte'),
    by.y = c('dte'),
    all = TRUE)
