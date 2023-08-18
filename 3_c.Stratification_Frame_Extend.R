# clean workspace
rm(list=ls())
# clean garbadge
gc()
# clear graphics device
# dev.off()
# set decimals to digits instead of scientific
options(scipen=999)
# set timeout limit (laxed)
options(timeout=10000)
# set work directory
setwd(dir = "~/Desktop/Artificially Intelligent Opinion Polling/")
# utils
library(data.table)
library(readxl)

# # # Derive past election distribution of choices

# estimates of 2016 turnout conditional on registration, accoridng to the CPS
# https://www.census.gov/data/tables/time-series/demo/voting-and-registration/p20-580.html

# load cps estimates from census website
reg <-
  as.data.table(
    read_xlsx(
      path = 'data_auxiliary/registration_and_turnout_2016.xlsx',
      skip = 3
      )
    )

# clean it up to get only the number of registered voters and the number of
# turned-out voters by state
reg <-
  reg[
    !is.na(STATE) & STATE!='UNITED STATES' & !is.na(Registered),
    as.character(unlist(reg[1])) %in% c('Total registered','Total voted')|
      names(reg) %in% c('STATE','Total Citizen Population'),
    with=F
    ]

# standardise names of states
reg$STATE <-
gsub(
  "(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2",    # Uppercase with Base R
  tolower(
    reg$STATE
  ),
  perl = TRUE
  )

# calculate turnout conditional on registration
reg$n_citizens <- as.numeric(reg$`Total Citizen Population`)
reg$n_registered <- as.numeric(reg$Registered)
reg$n_voted <- as.numeric(reg$Voted)

# keep ns
reg <- reg[,c('STATE','n_citizens','n_registered','n_voted')]

# load voting percentages by party
load(file = 'data_auxiliary/MIT_Election_Results/1976-2020-president.RData')
x <- as.data.table(x)
# focus on R, D, L and G & Other for parties
obs_dt <-
  x[
    year == 2016 &
    party_detailed %in% c('REPUBLICAN','DEMOCRAT')
    ]
# get percentages at the state level
obs_dt$pct_vote <- obs_dt $candidatevotes/obs_dt$totalvotes
# sum across parties
# (in case more than 1 party candidate running on the same ticket)
obs_dt <- obs_dt[,c('state','pct_vote','party_detailed'),with = F]
obs_dt <- obs_dt[,
                    lapply(.SD,sum,na.rm=TRUE),
                    by = c('state','party_detailed'),
                    .SDcols = c('pct_vote')
                ]
# calculate `other` vote share
obs_dt <-
  reshape(
    obs_dt,
    timevar = 'party_detailed',
    idvar = 'state',
    direction = 'wide'
    )
# replace NAs with 0s
for(i in paste('pct_vote.',c('REPUBLICAN','DEMOCRAT'),sep='')){
    obs_dt[[i]] <- ifelse(is.na(obs_dt[[i]]),0,obs_dt[[i]])
}
# add `others` column
obs_dt$pct_vote.OTHER <- 1- rowSums(obs_dt[,!'state'])

# standardise names of states
obs_dt$state <-
  gsub(
    "(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2",    # Uppercase with Base R
    tolower(
      obs_dt$state
    ),
    perl = TRUE
  )

# merge with turnout and registration data
obs_dt <-
  merge(
    obs_dt,
    reg,
    by.x = 'state',
    by.y = 'STATE',
    all = TRUE
    )

obs_dt$state[obs_dt$state=="District Of Columbia"] = "District of Columbia"

names(obs_dt) <- gsub('pct_vote.','',names(obs_dt))
names(obs_dt)[names(obs_dt)=='DEMOCRAT'] = "D"
names(obs_dt)[names(obs_dt)=='REPUBLICAN'] = 'R'
names(obs_dt)[names(obs_dt)=='OTHER'] = 'other'
names(obs_dt)[names(obs_dt)=='state'] = 'state_simple'


# calculate n votes
obs_dt$n_R <- obs_dt$R*obs_dt$n_voted
obs_dt$n_D <- obs_dt$D*obs_dt$n_voted
obs_dt$n_other <- obs_dt$other*obs_dt$n_voted

# keep only counts
obs_dt <- obs_dt[,grepl('state|n_',names(obs_dt)),with = F]
obs_dt$n_stay.home <- obs_dt$n_registered - obs_dt$n_voted
obs_dt$n_did.not.register <- obs_dt$n_citizens - obs_dt$n_registered

# calculate probability distirbution
choice_dist_2016 <- obs_dt
choice_dist_2016 <- choice_dist_2016[,!'n_citizens']
choice_dist_2016 <- choice_dist_2016[,!'n_registered']
choice_dist_2016 <- choice_dist_2016[,!'n_voted']
choice_dist_2016 <- cbind(choice_dist_2016[,'state_simple'],choice_dist_2016[,!'state_simple']/rowSums(choice_dist_2016[,!'state_simple']))

names(choice_dist_2016 ) <- gsub('n_','',names(choice_dist_2016 ))

# melt
choice_dist_2016 <- melt(choice_dist_2016,id.vars = 'state_simple',value.name = 'vote2016_share',variable.name = 'vote2016')

# load stratification frame
load(file = 'data_generated/SF.RData')

# apply state-level marginals
SF_extended <- merge(SF,choice_dist_2016, by = 'state_simple',all.x = TRUE,allow.cartesian = TRUE)

SF_extended$N <- SF_extended $N * SF_extended$vote2016_share
SF_extended <- SF_extended[,!'vote2016_share']

# drop unregistered
SF_extended <- SF_extended[vote2016!='did.not.register']

SF_extended$vote2016 <- gsub('\\.',' ',SF_extended$vote2016)

# save extended frame
save(SF_extended,file = 'data_generated/SF_extended.RData',compress = TRUE)
