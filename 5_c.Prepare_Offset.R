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

# offset - we use optimal aggregate of national level polls over the
# campaign to extract a campaign-wide baseline rathe for the 4 parties

f38_nat <- fread(file = 'data_auxiliary/f38_Model_Data_and_Predictions/presidential_national_toplines_2020.csv')
f38_nat <- f38_nat[,c('modeldate','national_voteshare_inc','national_voteshare_chal','nat_voteshare_other')]
names(f38_nat)[grepl('inc',names(f38_nat))] = 'R'
names(f38_nat)[grepl('chal',names(f38_nat))] = 'D'
names(f38_nat)[grepl('other',names(f38_nat))] = 'other'
f38_nat$modeldate <- as.Date(f38_nat$modeldate,'%m/%d/%Y')

# average across the campaign
vote2020 <- as.numeric(f38_nat[,lapply(.SD,mean),.SDcols = c('R','D','other')])
names(vote2020) <- c('R','D','other')

# split `other' share between G and L accoridng to last election split; further
# assume same national turnout as last election

load(file = 'data_generated/observed_vote_2016.RData')

obs_dt$n_turnout <- obs_dt$turnout*obs_dt$Registered
obs_dt$n_stay_home <- (1-obs_dt$turnout)*obs_dt$Registered
obs_dt$n_D <- obs_dt$D * obs_dt$n_turnout
obs_dt$n_R <- obs_dt$R * obs_dt$n_turnout
obs_dt$n_L <- obs_dt$L * obs_dt$n_turnout
obs_dt$n_G <- obs_dt$G * obs_dt$n_turnout

# aggregate
nat2016 <- obs_dt[,lapply(.SD,sum),.SDcols = c('n_stay_home','n_turnout','n_D','n_R','n_L','n_G')]
vote2020 <-
c(
vote2020/100 * nat2016$n_turnout,
`stay home` = nat2016$n_stay_home
)

vote2020 <- c(vote2020, L = as.numeric(vote2020['other'] * (nat2016$n_L)/(nat2016$n_L+nat2016$n_G)))
vote2020 <- c(vote2020, G = as.numeric(vote2020['other'] * (nat2016$n_G)/(nat2016$n_L+nat2016$n_G)))
vote2020 <- vote2020[-which(names(vote2020)=='other')]

# calculate N1 and N0 for offset
N1 <- vote2020
N0 <- sum(N1) - N1

offset_components <- list( N1 = N1, N0 = N0 )

# save offset components
save(offset_components,file = 'data_generated/offset_components.RData',compress = TRUE)
