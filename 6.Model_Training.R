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
# ensure warnings are not converted into errors
options(warn = 1)
# set work directory
setwd(dir = "~/Desktop/Artificially Intelligent Opinion Polling/")

# load useful package
library(data.table)
library(dplyr)
library(mltools)

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

# Function to plot nodes and edges (if you want, over a map from a shapefile)
plot_network <- function(shape, 
                         nb_object,
                         plot.over.map = T,
                         map.border.col = adjustcolor('grey',0.5), 
                         map.plygon.col = adjustcolor('lightcoral',0.10),
                         map.border.lwd = 0.1,
                         node.cex=0.01,
                         edge.lwd=0.25,
                         node.pch=0,
                         edge.lty=1,
                         xlim = NA,
                         ylim = NA,
                         edge.col = adjustcolor('blue',0.25)){
  
  if(any(is.na(xlim))){
    
    n.poly <- length(shape[['geometry']])
    
    x_range <- 
      range(
        sapply(X = 1:n.poly,
               FUN = 
                 function(x){
                   range(
                     st_coordinates(
                       shape[['geometry']][[x]]
                     )[, "X"] 
                   )
                 }
        ) )
    
    xlim <- x_range 
  }
  
  if(any(is.na(ylim))){
    
    n.poly <- length(shape[['geometry']])
    
    y_range <-   
      range(
        sapply(X = 1:n.poly,
               FUN = 
                 function(x){
                   range(
                     st_coordinates(
                       shape[['geometry']][[x]]
                     )[, "Y"] 
                   )
                 }
        ) )
    
    ylim <- y_range
    
  }
  
  if(plot.over.map){
    connectedplot = 
      plot(st_geometry(shape),
           border = map.border.col,
           lwd = map.border.lwd,
           col = map.plygon.col,
           xlim = xlim ,
           ylim = ylim
      )
  }
  
  
  
  
  connectedplot = 
    plot(nb_object,
         st_coordinates(st_centroid(shape)),
         add = plot.over.map,
         col = edge.col,
         cex = node.cex,
         lwd = edge.lwd,
         pch = node.pch,
         lty = edge.lty,
         xlim = xlim ,
         ylim = ylim)
}

# # # SETUP SPATIAL STRUCTURE

# load US states map
shape_US <- sf::read_sf(dsn = 'data_auxiliary/cb_2018_us_state_500k/cb_2018_us_state_500k.shp')

# match names to FIPS names
shape_US$small.area <- usmap::fips_info()$full[match(shape_US$STATEFP,usmap::fips_info()$fips)]

# drop non relevant areas
shape_US <- shape_US[!is.na(shape_US$small.area),]

# get neighborhood objects - these will be useful later for the fitting of the model
nb <- spdep::poly2nb(shape_US, row.names=shape_US$small.area, queen=TRUE)

# get adjacency matrix
C <- shape2mat(shape_US, style = "B")

# prep icar data
icar.data <- geostan::prep_icar_data(C)

# plot US map
pdf(file = 'plots/spatial.structure.pdf',width = 5,height = 5)
plot_network(
  shape = shape_US,
  nb_object = nb,
  map.border.lwd = 0.5,
  map.border.col = 'black',
  edge.col = 'blue',
  xlim = c(-170,-70)
)
dev.off()

# load linenar predictor
load(file = "data_generated/AREA_by_CAMPAIGN_PREDICTOR.RData")

# scale linear predictor
OMNI_pred$dte <-
as.numeric(as.character(unlist(
  difftime(
    as.Date("03/11/2020","%d/%m/%Y") ,
    as.Date(OMNI_pred$date,"%Y-%m-%d"),
    units = 'days'
    ) ) ) )

temp <-
  OMNI_pred[,
            lapply(.SD,function(x){ (x - mean(x))/sd(x) } ),
            .SDcols =
              c(names(
                OMNI_pred[,!c(
                  "date",
                  "dte",
                  "state_simple",
                  "state_simple_abbreviation"
                ) ] ) ) ]

names(temp ) <- paste('LP_',names(temp),sep = '')

OMNI_pred_scale <-
  cbind(
    OMNI_pred[,c("date","dte","state_simple","state_simple_abbreviation")],
    temp
    )

# # # merge linear predictor with stratification frame

load(file = "data_generated/SF_extended.RData")

# change levels of income to be ordered in factor form
SF_extended$commercial_estimated_hh_income <-
  as.factor(as.character(unlist(SF_extended$commercial_estimated_hh_income)))

levels(SF_extended$commercial_estimated_hh_income)[
  which(levels(SF_extended$commercial_estimated_hh_income)=="[min, 25000)"
  )] = '1.[min, 25000)'
levels(SF_extended$commercial_estimated_hh_income)[
  which(levels(SF_extended$commercial_estimated_hh_income)=="[25000, 50000)")
] = '2.[25000, 50000)'
levels(SF_extended$commercial_estimated_hh_income)[
  which(levels(SF_extended$commercial_estimated_hh_income)=="[50000, 75000)")
] = '3.[50000, 75000)'
levels(SF_extended$commercial_estimated_hh_income)[
  which(levels(SF_extended$commercial_estimated_hh_income)=="[75000, 100000)")
] = '4.[75000, 100000)'
levels(SF_extended$commercial_estimated_hh_income)[
  which(levels(SF_extended$commercial_estimated_hh_income)=="[100000, max]")
] = '5.[100000, max]'

SF_extended$commercial_estimated_hh_income <-
  as.factor(as.character(unlist(SF_extended$commercial_estimated_hh_income)))

SF_extended$age_bins <-   as.factor(as.character(unlist(SF_extended$age_bins)))

levels(SF_extended$age_bins)[
  which(levels(SF_extended$age_bins)=="18-24"
  )] = '1.18-24'
levels(SF_extended$age_bins)[
  which(levels(SF_extended$age_bins)=="25-34")
] = '2.25-34'
levels(SF_extended$age_bins)[
  which(levels(SF_extended$age_bins)=="35-44")
] = '3.35-44'
levels(SF_extended$age_bins)[
  which(levels(SF_extended$age_bins)=="45-54")
] = '4.45-54'
levels(SF_extended$age_bins)[
  which(levels(SF_extended$age_bins)=="55-64")
] = '5.55-64'
levels(SF_extended$age_bins)[
  which(levels(SF_extended$age_bins)=="65+")
] = '6.65+'

SF_extended$age_bins <-
  as.factor(as.character(unlist(SF_extended$age_bins)))

SF_extended_original <- SF_extended

SF_extended$dte <- 0
SF_extended$date <- as.Date("03/11/2020","%d/%m/%Y")

for(dte in 1:30){
  SF_extended_temp <- SF_extended_original
  SF_extended_temp$dte <- dte
  SF_extended_temp$date <-
    rev(seq(
      as.Date("03/10/2020","%d/%m/%Y"),
      as.Date("03/11/2020","%d/%m/%Y"),
      by = 'days'
    ))[dte+1]
  SF_extended <- rbindlist(list(SF_extended, SF_extended_temp))
}

# # # augment with area level
SF_extended <-
  merge(
    SF_extended,
    OMNI_pred_scale,
    all.x = TRUE,
    by = c('state_simple','date','dte')
    )

save(
  SF_extended,
  file = 'data_generated/SF_extended_scaled.pred.RData',
  compress = TRUE
)

# merge linear predictor with surveys

# turk workers
dt_turks <- fread(file = "data_generated/ThreeDatasetsSystem/SURVEY/Turks_Survey.csv")

dt_turks <-
  merge(
    dt_turks,
    OMNI_pred_scale,
    all.x = TRUE,
    by = c('state_simple_abbreviation','dte')
    )

# twitter users
dt_twitter <- fread(file = "data_generated/ThreeDatasetsSystem/SURVEY/Twitter_Survey_Turks_Labels.csv")

dt_twitter <-
  merge(
    dt_twitter,
    OMNI_pred_scale,
    all.x = TRUE,
    by = c('state_simple_abbreviation','dte')
    )


# anes respondents
dt_anes <- fread(file = "data_generated/ThreeDatasetsSystem/SURVEY/ANES_Survey.csv")

dt_anes <-
  merge(
    dt_anes,
    OMNI_pred_scale,
    all.x = TRUE,
    by = c('state_simple_abbreviation','dte')
    )

# 10 tweets gpt labeled respondents
dt_gpt_10tweets <- fread(file = "data_generated/ThreeDatasetsSystem/SURVEY/Twitter_Survey_GPT_Labels_10_Tweets.csv")

dt_gpt_10tweets <-
  merge(
    dt_gpt_10tweets,
    OMNI_pred_scale,
    all.x = TRUE,
    by = c('state_simple_abbreviation','dte')
  )

# 5 tweets gpt labeled respondents
dt_gpt_5tweets <- fread(file = "data_generated/ThreeDatasetsSystem/SURVEY/Twitter_Survey_GPT_Labels_5_Tweets.csv")

dt_gpt_5tweets <-
  merge(
    dt_gpt_5tweets,
    OMNI_pred_scale,
    all.x = TRUE,
    by = c('state_simple_abbreviation','dte')
  )

# ANES + WaPo dataset
dt_anes_abc.wapo <- fread(file = "data_generated/ThreeDatasetsSystem/SURVEY/ANES_and_ABC.WaPo_Survey.csv")

dt_anes_abc.wapo <-
  merge(
    dt_anes_abc.wapo,
    OMNI_pred_scale,
    all.x = TRUE,
    by = c('state_simple_abbreviation','dte')
  )

# survey list
dt_list <- list(dt_anes_abc.wapo,dt_anes,dt_turks,dt_twitter,dt_gpt_10tweets,dt_gpt_5tweets)
names(dt_list) <- c('anes_abc.wapo','anes','turks','twitter_human','twitter_gpt_10tweets','twitter_gpt_5tweets')

for(d in 1:length(dt_list)){
  
drop.vars <-
    c(
    'state_simple_abbreviation',
    'user_id',
    'DedicatedWorker',
    'bot',
    'organization',
    'fakenews',
    'turned_out_2018',
    'party_code',
    'marital_status_code'
    )
keep <- names(dt_list[[d]])[!names(dt_list[[d]]) %in% drop.vars ]

dt_list[[d]] <- dt_list[[d]][,..keep]

# if not D,R,G,L or stay home - drop
dt_list[[d]] <- dt_list[[d]][dt_list[[d]]$vote2020 %in% c("R","D","L",'G',"stay home")]

dt_list[[d]]$state_simple <- as.character(unlist(dt_list[[d]]$state_simple))
if(length(which(dt_list[[d]]$state_simple==''))){
  dt_list[[d]]$state_simple[dt_list[[d]]$state_simple==''] = NA
}

dt_list[[d]]$gender <- as.character(unlist(dt_list[[d]]$gender))
if(length(which(dt_list[[d]]$gender==''))){
  dt_list[[d]]$gender[dt_list[[d]]$gender==''] = NA
}

dt_list[[d]]$ethnicity <- as.character(unlist(dt_list[[d]]$ethnicity))
if(length(which(dt_list[[d]]$ethnicity==''))){
  dt_list[[d]]$ethnicity[dt_list[[d]]$ethnicity==''] = NA
}

dt_list[[d]]$vote2016  <- as.character(unlist(dt_list[[d]]$vote2016))
if(length(which(dt_list[[d]]$vote2016==''))){
  dt_list[[d]]$vote2016[dt_list[[d]]$vote2016==''] = NA
}

dt_list[[d]]$age_bins <- as.character(unlist(dt_list[[d]]$age_bins))
if(length(which(dt_list[[d]]$age_bins==''))>0){
  dt_list[[d]]$age_bins[dt_list[[d]]$age_bins==''] = NA
}

dt_list[[d]]$age_bins <-   as.factor(as.character(unlist(dt_list[[d]]$age_bins)))

levels(dt_list[[d]]$age_bins)[
  which(levels(dt_list[[d]]$age_bins)=="18-24"
  )] = '1.18-24'
levels(dt_list[[d]]$age_bins)[
  which(levels(dt_list[[d]]$age_bins)=="25-34")
] = '2.25-34'
levels(dt_list[[d]]$age_bins)[
  which(levels(dt_list[[d]]$age_bins)=="35-44")
] = '3.35-44'
levels(dt_list[[d]]$age_bins)[
  which(levels(dt_list[[d]]$age_bins)=="45-54")
] = '4.45-54'
levels(dt_list[[d]]$age_bins)[
  which(levels(dt_list[[d]]$age_bins)=="55-64")
] = '5.55-64'
levels(dt_list[[d]]$age_bins)[
  which(levels(dt_list[[d]]$age_bins)=="65+")
] = '6.65+'

dt_list[[d]]$age_bins <-
  as.factor(as.character(unlist(dt_list[[d]]$age_bins)))

dt_list[[d]]$modeled_college_grad <- as.character(unlist(dt_list[[d]]$modeled_college_grad))
if(length(which(dt_list[[d]]$modeled_college_grad==''))){
  dt_list[[d]]$modeled_college_grad[dt_list[[d]]$modeled_college_grad==''] = NA
}

dt_list[[d]]$commercial_estimated_hh_income <- as.character(unlist(dt_list[[d]]$commercial_estimated_hh_income))
if(length(which(dt_list[[d]]$commercial_estimated_hh_income==''))){
  dt_list[[d]]$commercial_estimated_hh_income[dt_list[[d]]$commercial_estimated_hh_income==''] = NA
}
# order these so we can have a structured prior
dt_list[[d]]$commercial_estimated_hh_income <-
  as.factor(dt_list[[d]]$commercial_estimated_hh_income)

levels(dt_list[[d]]$commercial_estimated_hh_income)[
  which(levels(dt_list[[d]]$commercial_estimated_hh_income)=="[min, 25000)"
  )] = '1.[min, 25000)'
levels(dt_list[[d]]$commercial_estimated_hh_income)[
  which(levels(dt_list[[d]]$commercial_estimated_hh_income)=="[25000, 50000)")
] = '2.[25000, 50000)'
levels(dt_list[[d]]$commercial_estimated_hh_income)[
  which(levels(dt_list[[d]]$commercial_estimated_hh_income)=="[50000, 75000)")
] = '3.[50000, 75000)'
levels(dt_list[[d]]$commercial_estimated_hh_income)[
  which(levels(dt_list[[d]]$commercial_estimated_hh_income)=="[75000, 100000)")
] = '4.[75000, 100000)'
levels(dt_list[[d]]$commercial_estimated_hh_income)[
  which(levels(dt_list[[d]]$commercial_estimated_hh_income)=="[100000, max]")
] = '5.[100000, max]'

dt_list[[d]]$commercial_estimated_hh_income <-
  as.factor(as.character(unlist(dt_list[[d]]$commercial_estimated_hh_income)))

dt_list[[d]]$vote2020 <- as.character(unlist(dt_list[[d]]$vote2020))
if(length(which(dt_list[[d]]$vote2020==''))){
  dt_list[[d]]$vote2020[dt_list[[d]]$vote2020==''] = NA
}

# work with complete cases
dt_list[[d]] <- dt_list[[d]][complete.cases(dt_list[[d]])]

# turn dependent variable into factor
dt_list[[d]]$vote2020 <- as.factor(dt_list[[d]]$vote2020)

# prepare stan data
N <- dim(dt_list[[d]])[1]

Y <- dt_list[[d]]$vote2020

X <- dt_list[[d]][,grepl('LP_',names(dt_list[[d]])),with=F]

area_id <- match(dt_list[[d]]$state_simple,shape_US$small.area)

dte_id <- dt_list[[d]]$dte + 1
dte_N <- max(dte_id)

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

# these have to be in the same order
var.name.list <-
  c(
    'gender',
    'ethnicity',
    'age_bins',
    'modeled_college_grad',
    'commercial_estimated_hh_income',
    'vote2016'
  )

model.var.name.list <-
  c(
    'gender',
    'ethnicity',
    'age',
    'edu',
    'income',
    'vote2016'
  )

id_list <- list()

for(i in 1:length(var.name.list)) {

  temp <-
    get_id(
      dt = dt_list[[d]],
      SF = SF_extended,
      var.name = var.name.list[i],
      model.var.name.list[i]
    )

  id_list <- append(id_list, temp)
}

# calculate offset
load(file = 'data_generated/offset_components.RData')

n1 <- table(Y)
n0 <- N - n1

P1 <- n1/offset_components$N1[levels(Y)]
P0 <- n0/offset_components$N0[levels(Y)]

# # # (A) SET UP STAN DATA LIST

train_data_list =
  list(

    offset = log(P1/P0),

    # dependent var

    N = N,
    y = as.integer(Y),
    J = nlevels(Y),
    Y_labs = levels(Y),

    # fixed effects

    X = X,
    P = dim(X)[2],

    # time and area varying effects

    dte_id = dte_id,
    dte_N = dte_N,

    area_id = area_id,
    area_N =  dim(shape_US)[1],

    k = icar.data$k,
    group_size = icar.data$group_size,
    group_idx = icar.data$group_idx,
    N_edges = icar.data$n_edges,
    node1 = icar.data$node1,
    node2 = icar.data$node2,
    comp_id = icar.data$comp_id,

    inv_sqrt_scaling_factor = icar.data$inv_sqrt_scale_factor

  )

# add individual-level random effects

train_data_list <- append(train_data_list , id_list)

# # # (C) DEFINE MODEL PARAMETERS TO MONITOR

pars =
  c(# # # CHOICE MODEL

    # BASELINE EFFECT
    'alpha_star',

    # HORSESHOE EFFECTS
    'beta_star',

    # INDIVIDUAL-LEVEL EFFECTS
    'gamma_gender_star',
    'gamma_ethnicity_star',
    'gamma_age_star',
    'gamma_edu_star',
    'gamma_income_star',
    'gamma_vote2016_star',

    # CAMPAIGN EFFECT
    'delta_star',

    # BYM2 EFFECTS
    'psi',
    'phi',
    'spatial_scale',
    'lambda_star',
    'omega'
  )

# save object
save(pars,
     file =
        paste(
          'data_generated/model_pars_',
          names(dt_list)[d],
          '.RData',
          sep=''
          ),
     compress = T)


# # # (D) RUN STAN MODEL

fit_object <- list()
NUTS_time <- c()

for(j in 1:train_data_list$J){

train_data_list_temp <- train_data_list

train_data_list_temp$offset <- train_data_list_temp$offset[j]
train_data_list_temp$Y <- ifelse(train_data_list_temp$y==j,1,0)

if(train_data_list_temp$Y_labs[j]=="D"){
  train_data_list_temp$X <- train_data_list_temp$X[,!grepl('republican|libertarian|green|stay_home',names(train_data_list_temp$X)),with=F]
  train_data_list_temp$P <- dim(train_data_list_temp$X)[2]
}
if(train_data_list_temp$Y_labs[j]=="R"){
  train_data_list_temp$X <- train_data_list_temp$X[,!grepl('democrat|libertarian|green|stay_home',names(train_data_list_temp$X)),with=F]
  train_data_list_temp$P <- dim(train_data_list_temp$X)[2]
}
if(train_data_list_temp$Y_labs[j]=="L"){
  train_data_list_temp$X <- train_data_list_temp$X[,!grepl('democrat|republican|green|stay_home',names(train_data_list_temp$X)),with=F]
  train_data_list_temp$P <- dim(train_data_list_temp$X)[2]
}
if(train_data_list_temp$Y_labs[j]=="G"){
  train_data_list_temp$X <- train_data_list_temp$X[,!grepl('democrat|republican|libertarian|stay_home',names(train_data_list_temp$X)),with=F]
  train_data_list_temp$P <- dim(train_data_list_temp$X)[2]
}
if(train_data_list_temp$Y_labs[j]=="stay home"){
  train_data_list_temp$X <- train_data_list_temp$X[,!grepl('democrat|republican|libertarian|green',names(train_data_list_temp$X)),with=F]
  train_data_list_temp$P <- dim(train_data_list_temp$X)[2]
}

NUTS_time_temp <- Sys.time()

fit_object_temp <-
  stan(
    file =  "model.stan",
    pars = pars,
    data = train_data_list_temp,
    iter = 500,
    warmup = 250,
    refresh = 1,
    thin = 4,
    cores = 8,
    chains = 8,
    control = list(max_treedepth = 10,adapt_delta = 0.8),
    verbose = TRUE
  )

NUTS_time_temp <- Sys.time() - NUTS_time
NUTS_time <- c(NUTS_time,NUTS_time_temp)


fit_object <- append(fit_object,list(fit_object_temp))
}

names(fit_object) <- train_data_list$Y_labs

# clean trash
gc()

# save object
save(fit_object,
     file =
       paste(
         'data_generated/fit.object_',
         names(dt_list)[d],
         '.RData',
         sep=''
         ),
     compress = T)

# save time taken to fit
save(NUTS_time,
     file =
       paste(
         'data_generated/NUTS_time_',
         names(dt_list)[d],
         '.RData',
         sep=''
         ),
     compress = T)

# save training list
save(train_data_list,
     file =
        paste(
          'data_generated/training.data_',
          names(dt_list)[d],
          '.RData',
          sep=''
          ),
     compress = T)

}