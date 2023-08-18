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

# create prediction function
pred_function <-
  function(
    alpha_star,

    beta_star,
    X,

    gamma_gender_star,
    gender_id,

    gamma_ethnicity_star,
    ethnicity_id,

    gamma_age_star,
    age_id,

    gamma_edu_star,
    edu_id,

    gamma_income_star,
    income_id,

    gamma_vote2016_star,
    vote2016_id,

    delta_star,
    dte_id,

    lambda_star,
    area_id

  ){

      alpha_star +

      as.matrix(X) %*% beta_star +
      gamma_gender_star[gender_id] +
      gamma_ethnicity_star[ethnicity_id] +
      gamma_age_star[age_id] +
      gamma_edu_star[edu_id] +
      gamma_income_star[income_id] +
      gamma_vote2016_star[vote2016_id] +

      delta_star[dte_id] +
      lambda_star[area_id];
  }

# load strat frame
load(file = 'data_generated/SF_extended_scaled.pred.RData')

# generate predictions
area.pred_list <- list() #data.table()

for(d in c('anes_abc.wapo','anes','turks','twitter_human','twitter_gpt_10tweets','twitter_gpt_5tweets')){

# load fit object
load(file = paste('data_generated/fit.object_',d,'.RData',sep=""))

# load training data
load(file = paste('data_generated/training.data_',d,'.RData',sep=""))

# load parameters of interest
load(file = paste('data_generated/model_pars_',d,'.RData',sep=""))


# extract simulations for model parametrs
pars_sims <-
  lapply(1:train_data_list$J,
    function(j){
      rstan::extract(fit_object[[j]], pars = pars, inc_warmup = FALSE)
    } )
names(pars_sims) <- train_data_list$Y_labs


for(s in 1:dim(pars_sims[[1]]$alpha_star)[1]){

# make predictions for each vote2016
pred <-
  sapply(1:train_data_list$J,
         function(j){

if(train_data_list$Y_labs[j]=="D"){
  keep <- !grepl('republican|libertarian|green|stay_home',names(SF_extended))
}
if(train_data_list$Y_labs[j]=="R"){
  keep <- !grepl('democrat|libertarian|green|stay_home',names(SF_extended))
}
if(train_data_list$Y_labs[j]=="L"){
  keep <- !grepl('democrat|republican|green|stay_home',names(SF_extended))
}
if(train_data_list$Y_labs[j]=="G"){
 keep <- !grepl('democrat|republican|libertarian|stay_home',names(SF_extended))
}
if(train_data_list$Y_labs[j]=="stay home"){
  keep <- !grepl('democrat|republican|libertarian|green',names(SF_extended))
}

           pred_function(

             alpha_star = pars_sims[[j]]$alpha_star[s],

             beta_star = pars_sims[[j]]$beta_star[s,],
             X = SF_extended[,grepl("LP",names(SF_extended)) & keep,with=F],

             gamma_gender_star = pars_sims[[j]]$gamma_gender_star[s,],
             gender_id = as.integer(as.factor(SF_extended$gender)),

             gamma_ethnicity_star = pars_sims[[j]]$gamma_ethnicity_star[s,],
             ethnicity_id = as.integer(as.factor(SF_extended$ethnicity)),

             gamma_age_star = pars_sims[[j]]$gamma_age_star[s,],
             age_id = as.integer(as.factor(SF_extended$age_bins)),

             gamma_edu_star = pars_sims[[j]]$gamma_edu_star[s,],
             edu_id = as.integer(as.factor(SF_extended$modeled_college_grad)),

             gamma_income_star = pars_sims[[j]]$gamma_income_star[s,],
             income_id = as.integer(as.factor(SF_extended$commercial_estimated_hh_income)),

             gamma_vote2016_star = pars_sims[[j]]$gamma_vote2016_star[s,],
             vote2016_id = as.integer(as.factor(SF_extended$vote2016)),

             delta_star = pars_sims[[j]]$delta_star[s,],
             dte_id = SF_extended$dte + 1,

             lambda_star = pars_sims[[j]]$lambda_star[s,],
             area_id = as.integer(as.factor(SF_extended$state_simple))
         ) } )

# assign names
colnames(pred) <- train_data_list$Y_labs
# convert to probabilities
pred <- exp(pred)/(1+exp(pred))#rowSums(exp(pred))

# get probabilities conditional on turnout
pred <- as.data.table(pred)

pred <-
cbind(
  pred[,!'stay home']/rowSums(pred[,!'stay home']),
  pred[,'stay home',with=F]
  )
pred$turnout <- 1-pred$`stay home`
pred <- pred[,!'stay home']

# bind with area-level data
temp <- cbind(SF_extended[,c('state_simple','dte','N'),with=F],pred)

area.pred <-
temp[,
     lapply(.SD,function(x){sum(x*N)/sum(N)}),
     by = c('state_simple','dte'),
     .SDcols = c(names(pred))
     ]

# give id number for simulation
area.pred$sim_id <- s
area.pred$training_data <- d

# stack
#area.pred_list <- rbindlist(list(area.pred_list,area.pred),fill = TRUE)

area.pred_list <- append(area.pred_list,list(area.pred))

# view
print(area.pred_list)

# monitor
print(
  paste(
    "simulating elections... already simulated:",
    round(s/dim(pars_sims[[1]]$alpha_star)[1]*100),"%"
    )
  )

#save(area.pred_list,file = 'data_generated/area_preds.RData',compress = TRUE)
}
}

load(file = 'data_generated/area_preds.RData')

area.pred_list <- rbindlist(area.pred_list)

# load past election result - this is used to calculate uniform swing
load(file = 'data_generated/observed_vote_2016.RData')

# store dataset on which to apply swing
uswing <- obs_dt

# calculate national 2016 votes
nat_votes_past <-
    uswing[,
    lapply(.SD,function(x){sum(x*turnout*Registered)}),
    .SDcols = c('R','D','L','G')]
nat_turnout_past <-
    uswing[,
    lapply(.SD,function(x){sum(x*Registered)}),
    .SDcols = c('turnout')]
nat_past <-
  c(
    unlist(nat_votes_past/nat_turnout_past[['turnout']]),
    turnout = nat_turnout_past[['turnout']]/sum(uswing$Registered))

# calculate national 2020 votes
load(file = 'data_generated/observed_vote.RData')

# calculate national 2016 votes
nat_votes <-
    obs_dt[,
    lapply(.SD,function(x){sum(x*turnout*registered)}),
    .SDcols = c('R','D','L','G')]
nat_turnout <-
    obs_dt[,
    lapply(.SD,function(x){sum(x*registered)}),
    .SDcols = c('turnout')]
nat <-
  c(
    unlist(nat_votes/nat_turnout[['turnout']]),
    turnout = nat_turnout[['turnout']]/sum(obs_dt$registered))

# calculate swing
swing <- nat - nat_past

for(j in names(swing)){
  uswing[[j]] <- uswing[[j]] + swing[j]
}

uswing$sim_id <- 1
uswing$training_data <- 'uniform_swing'
uswing$state_simple <- as.character(unlist(uswing$state_simple))
uswing$dte <- 0

# load past election result - this is another benchmark
load(file = 'data_generated/observed_vote_2016.RData')

obs_dt$sim_id <- 1
obs_dt$training_data <- 'last_election'
obs_dt$state_simple <- as.character(unlist(obs_dt$state_simple))
obs_dt$dte <- 0

# load raw ANES survey - this is the non-modeled benchmark
load(file = 'data_generated/raw_ANES2020_voteshare.RData')

ANES_raw$training_data <- 'raw_ANES'
ANES_raw$sim_id <- 1
ANES_raw$dte <- 0

# save into comparison object
area.pred_list <-
  rbindlist(
    list(
      area.pred_list,
      uswing[,names(area.pred_list),with = FALSE],
      obs_dt[,names(area.pred_list),with = FALSE],
      ANES_raw
    ),
    use.names=TRUE
  )

save(area.pred_list,file = 'data_generated/area_preds.RData',compress = TRUE)

# create specific object for 538 comparison (ignore turnout and join L + G )

# load 538 predictions as benchmark
load(file = 'data_generated/f38_predicted_vote.RData')
f38_area.pred$dte <- as.numeric(as.character(unlist(f38_area.pred$dte)))

# aggregate other vote to allow for f38 comparison

area.pred_list[,other:= 1-(D + R)]
area.pred_list <- area.pred_list[,!c('L','G','turnout')]

area.pred_list_f38 <-
  rbindlist(
    list(
      area.pred_list,
      f38_area.pred
    ),
    use.names=TRUE
  )

save(area.pred_list_f38,file = 'data_generated/area_preds_f38_comparison.RData',compress = TRUE)


