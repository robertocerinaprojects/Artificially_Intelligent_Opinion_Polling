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


# estimates of 2020 turnout conditional on registration, accoridng to the CPS
# https://www.census.gov/data/tables/time-series/demo/voting-and-registration/p20-585.html
library(readxl)

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
      names(reg) == 'STATE',
    with = FALSE
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
reg$Registered <- as.numeric(reg$Registered)
reg$Voted <- as.numeric(reg$Voted)
reg$turnout <- reg$Voted/reg$Registered

# load voting percentages by party
load(file = 'data_auxiliary/MIT_Election_Results/1976-2020-president.RData')

x <- as.data.table(x)

obs_dt <-
  x[
    year == 2016 &
    party_detailed %in% c('REPUBLICAN','DEMOCRAT','LIBERTARIAN','GREEN')
    ]

obs_dt$pct_vote <- obs_dt $candidatevotes/obs_dt$totalvotes

tmp <- obs_dt[,c('state','pct_vote','party_detailed'),with = F]
tmp <- tmp[,lapply(.SD,sum,na.rm=TRUE),by = c('state','party_detailed'),.SDcols = c('pct_vote')]

obs_dt <-
  reshape(
    tmp,
    timevar = 'party_detailed',
    idvar = 'state',
    direction = 'wide'
    )

# standardise names of states
obs_dt$state <-
  gsub(
    "(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2",    # Uppercase with Base R
    tolower(
      obs_dt$state
    ),
    perl = TRUE
  )

obs_dt$pct_vote.LIBERTARIAN[is.na(obs_dt$pct_vote.LIBERTARIAN)] <- 0
obs_dt$pct_vote.GREEN[is.na(obs_dt$pct_vote.GREEN)] <- 0

# get final results data
obs_dt <-
  merge(
    obs_dt,
    reg[,c('STATE','turnout','Registered')],
    by.x = 'state',
    by.y = 'STATE',
    all = TRUE
    )

obs_dt$state[obs_dt$state=="District Of Columbia"] = "District of Columbia"

names(obs_dt) <- gsub('pct_vote.','',names(obs_dt))
names(obs_dt)[names(obs_dt)=='DEMOCRAT'] = "D"
names(obs_dt)[names(obs_dt)=='REPUBLICAN'] = 'R'
names(obs_dt)[names(obs_dt)=='LIBERTARIAN'] = 'L'
names(obs_dt)[names(obs_dt)=='GREEN'] = 'G'
names(obs_dt)[names(obs_dt)=='state'] = 'state_simple'

# save results data
save(obs_dt, file = 'data_generated/observed_vote_2016.RData',compress = TRUE)

